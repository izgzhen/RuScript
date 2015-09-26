{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Parser
import Control.Applicative ((<|>))
import Control.Monad.RWS
import Control.Monad.Except
import qualified Data.Map as M

-- Bytecode compiler for ruscript language

-- XXX: Maybe String should be put into the data section and left just a reference in the text ....
--- But this is not difficult to modify whatsoever

-- XXX: Maybe some debug info ... like line number, can be forged into the monad stack

data SCode = SPushG Int
           | SPushL Int
           | SAdd
           | SCall Int String Int
           | SRet
           | SNew Int
           | SPushInt Int
           | SPushStr String
           | SFrameStart
           | SFrameEnd Int
           | SClass Int Int
           | SPopG Int -- Pop the stack top to the globals[i]
           | SPopL Int -- Pop the stack top to the locals[i]
           deriving (Show, Eq)

type Config = ()

data State = State {
  globals :: M.Map String Int,
  locals         :: Maybe (M.Map String Int),
  visibleGlobals :: Maybe [String],
  nextId  :: Int
}

type Compiler = ExceptT String (RWS Config [SCode] State)

compile :: Source -> Compiler ()
compile [] = return ()
compile (s:ss) = case s of
  Assignment lhs expr -> withGlobals lhs $ \i -> do
        pushExpr expr
        emit $ SPopG i
        compile ss
  ClassDecl name attrs methods -> withGlobals name $ \i -> do
        tell (map SPushStr attrs)
        mapM_ emitMethod methods
        emit $ SClass (length attrs) (length methods)
        compile ss
  Return expr -> ifInMethod $ do
        pushExpr expr
        emit SRet
        compile ss

-- stack top should reside the computed value
pushExpr :: Expr -> Compiler ()
pushExpr (Single tm) = pushTerm tm
pushExpr (Plus tm1 tm2) = do
    pushTerm tm1
    pushTerm tm2
    emit SAdd

pushTerm tm = case tm of
    Var x    -> pushVar x
    LitInt i -> emit $ SPushInt i

withGlobals :: String -> (Int -> Compiler ()) -> Compiler ()
withGlobals name f = do
    glbs <- globals <$> get
    case M.lookup name glbs of
        Just i  -> f i
        Nothing -> throwError $ "unable to resolve name: " ++ name

emitMethod :: MethodDecl -> Compiler ()
emitMethod (MethodDecl name args glbs src) = do
  enterMethod $ do
    addVisibleGlobals glbs
    mapM_ (\x -> withNextId $ \id -> addLocal x id) args
    emit SFrameStart
    compile src
  emit $ SFrameEnd (length args)
  emit $ SPushStr name

ifInMethod :: Compiler () -> Compiler ()
ifInMethod f = do
  maybeLocals <- locals <$> get
  if maybeLocals /= Nothing
    then f
    else throwError "Not in method scope"

emit :: SCode -> Compiler ()
emit x = tell [x]

pushVar x = pushGlobal x <|> pushLocal x

pushGlobal x = do
  glbs <- globals <$> get
  case M.lookup x glbs of
      Just i  -> emit $ SPushG i
      Nothing -> withNextId (\id -> emit (SPushG id) >> addGlobal x id)

pushLocal x = do
  m <- locals <$> get
  case m of
    Just lcs -> case M.lookup x lcs of
        Just i  -> emit $ SPushL i
        Nothing -> withNextId (\id -> emit (SPushL id) >> addLocal x id)
    Nothing  -> throwError "Not in local scope"

addVisibleGlobals glbs = modify $ \(State g l v n) -> State g l (f v) n
  where
    f Nothing = Just glbs
    f (Just gs) = Just (gs ++ glbs)

-- XXX: It seems stupid to split counter and namer
addLocal :: String -> Int -> Compiler ()
addLocal name i = modify $ \(State g (Just l) v n) -> State g (Just (M.insert name i l)) v n

addGlobal :: String -> Int -> Compiler ()
addGlobal name i = modify $ \(State g l v n) -> State (M.insert name i g) l v n

withNextId f = do
  id <- nextId <$> get
  modify $ \(State g l v n) -> State g l v (n + 1)
  f id

enterMethod f = do
  modify $ \(State g Nothing Nothing n) -> State g (Just M.empty) (Just []) n

