{-# LANGUAGE FlexibleContexts #-}

module Compiler where

import Parser
import Control.Applicative ((<|>))
import Control.Monad.RWS
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import NameSpace
import SCode

type Config = ()

data Scope = Scope {
  globals        :: NameSpace,
  locals         :: Maybe (NameSpace, NameSet),
  classes        :: NameSpace,
  classScope     :: Maybe ClassScope
} deriving (Show)

initConfig = ()

initScope = Scope {
  globals = initNameSpace,
  locals  = Nothing,
  classes = initNameSpace,
  classScope = Nothing
}

data ClassScope = ClassScope {
  attrNameSpace   :: NameSpace,
  methodNameSpace :: NameSpace
} deriving (Show)

type NameSet = S.Set String


type Compiler = ExceptT String (RWS Config [SCode] Scope)
runCompiler src = runRWS (runExceptT (compile src)) initConfig initScope

data Scoped = G Int | L Int | A Int deriving (Show)

data Context = TopLevel | InMethod deriving (Show)

newtype ClassId = ClassId Int deriving (Show)

emit :: SCode -> Compiler ()
emit x = tell [x]

pop :: Scoped -> Compiler ()
pop (G i) = emit $ SPopG i
pop (L i) = emit $ SPopL i
pop (A i) = throwError "Can't write to attributes now"

push :: Scoped -> Compiler ()
push (G i) = emit $ SPushG i
push (L i) = emit $ SPushL i
push (A i) = emit $ SPushA i

compile :: Source -> Compiler ()
compile [] = return ()
compile (s:ss) = case s of
  Assignment lhs expr -> writeVar lhs $ \i -> do
        pushExpr expr
        pop i
        compile ss
  ClassDecl name attrs methods -> emitClass name attrs methods >> compile ss
  Print expr -> do
        pushExpr expr
        emit SPrint
        compile ss
  Return expr -> inMethod $ \_ _ -> do
        pushExpr expr
        emit SRet
        compile ss

-- Class Definition Generator

emitClass :: String -> [String] -> [MethodDecl] -> Compiler ()
emitClass name attrs methods = do
    void $ addClass name
    emit $ SClass (length attrs) (length methods)
    let cscope = ClassScope (collectNames attrs)
                            (collectNames $ map getMethodName methods)
    enterClass cscope $ mapM_ emitMethod methods

emitMethod :: MethodDecl -> Compiler ()
emitMethod (MethodDecl name args glbs src) = inClass $ \_ -> do
    enterMethod (S.fromList glbs) args $ compile src
    emit $ SFrameEnd
    emit $ SPushStr name

-- Expression & Term Evaluation Generator

pushExpr :: Expr -> Compiler ()
pushExpr (Single tm) = pushTerm tm
pushExpr (Plus tm1 tm2) = do
    pushTerm tm1
    pushTerm tm2
    emit SAdd

pushTerm :: Term -> Compiler ()
pushTerm tm = case tm of
    TVar x    -> pushVar x
    TLitInt i -> emit $ SPushInt i
    TLitStr s -> emit $ SPushStr s
    TNew new    -> compileNew    new
    TCall call  -> compileCall   call
    TAccess acc -> compileAccess acc

compileNew :: New -> Compiler ()
compileNew (New className params) = readClass className $ \i -> do
    mapM_ pushExpr params
    new i $ length params
  

compileCall :: Call -> Compiler ()
compileCall (Call "self" method params) = inClass $ \cs -> do
    case lookupName method (methodNameSpace cs) of
        Just i  -> do
          mapM_ pushExpr params
          emit SPushSelf  -- Push self on top of stack
          selfi <- addLocal "self"
          pop selfi
          call selfi method $ length params
        Nothing -> throwError $ method ++ " is not defined as methods"

compileCall (Call receiver method params) = readVar receiver $ \recvi -> do
    mapM_ pushExpr params
    call recvi method $ length params

compileAccess :: Access -> Compiler ()
compileAccess (Access "self" accessor) = inClass $ \cs -> do
    case lookupName accessor (attrNameSpace cs) of
        Just i  -> do
          emit SPushSelf  -- Push self on top of stack
          emit $ SPushA i -- Push attrs[i] of stacktop on top of stack
        Nothing -> throwError $ accessor ++ " is not defined as attributes" 

compileAccess (Access receiver accessor) = readVar receiver $ \recvi -> do
    push recvi
    emit $ SPushAStr accessor

pushVar :: String -> Compiler ()
pushVar x = writeVar x $ \scoped ->
  case scoped of
    G i -> emit $ SPushG i
    L i -> emit $ SPushL i
    A i -> emit $ SPushA i

call :: Scoped -> String -> Int -> Compiler ()
call scoped m narg = case scoped of
      G i -> emit $ SCallG i m narg
      L i -> emit $ SCallL i m narg
      _   -> error "can't call on attributes now"

new :: ClassId -> Int -> Compiler ()
new (ClassId i) n = emit $ SNew i n

-- Register Allocator

addVar :: String -> Compiler Scoped
addVar name = getContext >>= \ctx -> do
  case ctx of
    InMethod -> addLocal name
    TopLevel -> addGlobal name

addLocal :: String -> Compiler Scoped
addLocal name = inMethod $ \lcs vS -> do
  scope <- get
  let (l', nid) = insertName name lcs
  put $ scope { locals = Just (l', vS)}
  return $ L nid

addGlobal :: String -> Compiler Scoped
addGlobal name = do
  scope <- get
  let (g', nid) = insertName name $ globals scope
  put $ scope { globals = g'}
  return $ G nid

addAnonyLocal :: Compiler Scoped
addAnonyLocal = inMethod $ \lcs vS -> do
  scope <- get
  let (l', nid) = insertAnony lcs
  put $ scope { locals = Just (l', vS)}
  return $ L nid

-- Class Initializer

addClass :: String -> Compiler ClassId
addClass name = do
  scope <- get
  let (c', nid) = insertName name $ classes scope
  put $ scope { classes = c'}
  return $ ClassId nid

-- Class Scoping

readClass :: String -> (ClassId -> Compiler ()) -> Compiler ()
readClass name f = do
    clss <- classes <$> get
    case lookupName name clss of
        Just i  -> f $ ClassId i
        Nothing -> throwError $ "can't find class " ++ name

-- Register Scoping

writeVar :: String -> (Scoped -> Compiler a) -> Compiler a
writeVar name f = scopeVar name $ \mScoped -> do
    case mScoped of
        Nothing -> addVar name >>= f
        Just scoped -> f scoped

readVar :: String -> (Scoped -> Compiler ()) -> Compiler ()
readVar name f = scopeVar name $ \mScoped -> do
    case mScoped of
        Nothing -> throwError $ "can't find " ++ name ++ " in scope"
        Just scoped -> f scoped

scopeVar :: String -> (Maybe Scoped -> Compiler a) -> Compiler a
scopeVar name f = do
    maybeLocals <- locals <$> get
    case maybeLocals of
        Nothing -> f =<< scopeGlobal name
        Just (locals, vGlobals) ->
            if name `S.member` vGlobals then
                scopeGlobal name >>= f
                else scopeLocal locals name >>= f

scopeGlobal :: String -> Compiler (Maybe Scoped)
scopeGlobal name = do
    glbs <- globals <$> get
    case lookupName name glbs of
        Just i  -> return $ Just $ G i
        Nothing -> return Nothing

scopeLocal :: NameSpace -> String -> Compiler (Maybe Scoped)
scopeLocal locals name = do
    case lookupName name locals of
        Nothing -> return $ Nothing
        Just i  -> return $ Just $ L i

-- Context Helpers

enterMethod :: NameSet -> [String] -> Compiler a -> Compiler a
enterMethod glbs args f = do
    modify $ \scope -> scope { locals = Just (initNameSpace, glbs) }
    mapM_ addLocal args
    x <- f
    modify $ \scope -> scope { locals = Nothing }
    return x

inMethod :: (NameSpace -> NameSet -> Compiler a) -> Compiler a
inMethod f = do
  maybeLocals <- locals <$> get
  case maybeLocals of
    Just (locals, vGlobals) -> f locals vGlobals
    Nothing -> throwError "Not in method definition scope"

enterClass :: ClassScope -> Compiler a -> Compiler a
enterClass s f = do
    modify $ \scope -> scope { classScope = Just s }
    x <- f
    modify $ \scope -> scope { classScope = Nothing }
    return x

inClass :: (ClassScope -> Compiler a) -> Compiler a
inClass f = do
  maybeCS <- classScope <$> get
  case maybeCS of
    Just cs -> f cs
    Nothing -> throwError "Not in class definition scope"


getContext :: Compiler Context
getContext = do
  maybeLocals <- locals <$> get
  if maybeLocals == Nothing then return TopLevel
    else return InMethod
