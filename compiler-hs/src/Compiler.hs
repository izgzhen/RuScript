{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Parser
import Control.Applicative ((<|>))
import Control.Monad.RWS
import Control.Monad.Except
import qualified Data.Map as M
import Control.Monad
import NameSpace
-- Bytecode compiler for ruscript language

-- XXX: Maybe String should be put into the data section and left just a reference in the text ....
--- But this is not difficult to modify whatsoever

-- XXX: Maybe some debug info ... like line number, can be forged into the monad stack

data SCode = SPushL Int
           | SPushG Int
           | SPopG Int
           | SAdd
           | SCall Int String Int
           | SRet
           | SNew Int
           | SPushInt Int
           | SPushStr String
           | SFrameEnd
           | SClass Int Int
           | SPrint
           | SPopL Int
           deriving (Show, Eq)

type Config = ()

data Scope = Scope {
  globals        :: NameSpace,
  locals         :: Maybe NameSpace,
  visibleGlobals :: Maybe [String]
} deriving (Show)

initConfig = ()

initScope = Scope {
  globals = initNameSpace,
  locals  = Nothing,
  visibleGlobals = Nothing
}

type Compiler = ExceptT String (RWS Config [SCode] Scope)

runCompiler src = runRWS (runExceptT (compile src)) initConfig initScope

compile :: Source -> Compiler ()
compile [] = return ()
compile (s:ss) = case s of
  Assignment lhs expr -> withGlobals lhs $ \i -> do
        pushExpr expr
        emit $ SPopG i
        compile ss
  ClassDecl name attrs methods -> withGlobals name $ \i -> do
        emit $ SClass (length attrs) (length methods)
        tell (map SPushStr attrs)
        mapM_ emitMethod methods
        compile ss
  Print expr -> do
        pushExpr expr
        emit SPrint
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
    LitStr s -> emit $ SPushStr s
    New className -> withGlobals className $ emit . SNew
    call@(Call _ _ _) -> compileCall call

compileCall (Call receiver method params) = withGlobals receiver $ \recvi -> do
    enterMethod $ do
      prms <- foldM 
              (\pis p -> do
                  i <- addAnonyLocal
                  pushExpr p
                  emit $ SPopL i
                  return (i : pis)
                )
              []
              params
      mapM_ (emit . SPushL) prms
      emit $ SCall recvi method $ length prms

withGlobals :: String -> (Int -> Compiler ()) -> Compiler ()
withGlobals name f = do
    glbs <- globals <$> get
    case lookupName name glbs of
        Just i  -> f i
        Nothing -> addGlobal name >>= f

emitMethod :: MethodDecl -> Compiler ()
emitMethod (MethodDecl name args glbs src) = do
  enterMethod $ do
    addVisibleGlobals glbs
    mapM_ addLocal args
    compile src
  emit $ SFrameEnd
  emit $ SPushStr name

ifInMethod :: Compiler () -> Compiler ()
ifInMethod f = do
  maybeLocals <- locals <$> get
  if maybeLocals /= Nothing
    then f
    else throwError "Not in method scope"

emit :: SCode -> Compiler ()
emit x = tell [x]

pushVar x = do
  g <- pushGlobal x
  l <- pushLocal x
  case (g <|> l) of
    Nothing -> throwError $ "Can't find " ++ x ++ " in scope"
    Just inst -> emit inst

pushGlobal x = do
  glbs <- globals <$> get
  case lookupName x glbs of
      Just i  -> return $ Just $ SPushG i
      Nothing -> return Nothing

pushLocal x = do
  m <- locals <$> get
  case m of
    Just lcs -> case lookupName x lcs of
        Just i  -> return $ Just $ SPushL i
        Nothing -> return Nothing
    Nothing  -> return Nothing

addVisibleGlobals glbs = modify $ \(Scope g l v) -> Scope g l (f v)
  where
    f Nothing = Just glbs
    f (Just gs) = Just (gs ++ glbs)

addLocal :: String -> Compiler Int
addLocal name = do
  Scope g (Just l) v <- get
  let (l', nid) = insertName name l
  put $ Scope g (Just l') v
  return nid

addAnonyLocal :: Compiler Int
addAnonyLocal = do
  Scope g (Just l) v <- get
  let (l', nid) = insertAnony l
  put $ Scope g (Just l') v
  return nid

addGlobal :: String -> Compiler Int
addGlobal name = do
  Scope g l v <- get
  let (g', nid) = insertName name g
  put $ Scope g' l v
  return nid


enterMethod f = do
  modify $ \(Scope g Nothing Nothing) -> Scope g (Just initNameSpace) (Just [])
  f
  modify $ \(Scope g _ _) -> Scope g Nothing Nothing


