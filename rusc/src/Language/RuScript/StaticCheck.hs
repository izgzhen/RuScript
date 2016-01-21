{-# LANGUAGE TemplateHaskell, RankNTypes,
             TypeSynonymInstances, FlexibleInstances,
             FlexibleContexts, LambdaCase #-}

module Language.RuScript.StaticCheck where

{-
    Static checking of source code

    1. Type check
    2. Class inheritance (including virtual method)
    3. Visibility

    However, there should be some generic functions.
-}

import Language.RuScript.AST

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Either
import qualified Data.List as L

data StaticState = StaticState {
  _localTable  :: M.Map Name Type
, _funcTable   :: M.Map Name FnSig
, _classTable  :: M.Map Name ClassType
, _enclosedRet :: Maybe Type
}

data ClassType = ClassType {
  _mInherit  :: Maybe Name
, _attrTable :: M.Map Name Type
, _mtdTable  :: M.Map Name FnSig
}

initStaticState :: StaticState
initStaticState = StaticState M.empty M.empty M.empty Nothing

makeLenses ''StaticState
makeLenses ''ClassType

type Static = ExceptT String (State StaticState)

checkProgram :: Program -> Either String ()
checkProgram prog = evalState (runExceptT (check prog)) initStaticState

class Check a where
    check :: a -> Static ()

instance Check a => Check [a] where
    check = mapM_ check

instance Check Program where
    check (Program mix) = do
        let stmts = lefts mix
        let decls = rights mix
        check decls
        check stmts

instance Check Declaration where
    check (FnDecl sig stmts) = do
        addFnSig sig
        inFunc sig $ check stmts

    check (ClassDecl name mInherit attrs methods) = do
        addEmptyClass name mInherit
        mapM_ (\(_, binding) -> addAttr name binding) attrs
        mapM_ (\(_, method)  -> addMethod name method) methods

instance Check Statement where
    check (SVar binding mExpr) = do
        addBinding binding
        case mExpr of
            Just expr -> do
                ty <- infer expr
                assert (snd binding == ty) "Variable binding's type don't match"
            Nothing -> return ()

    check (SAssign lhs expr) = do
        ty1 <- infer lhs
        ty2 <- infer expr
        assertEq ty1 ty2

    check (SBlock block) = check block

    check (SInvoke recv method params) = do
        ty <- infer recv
        sig@(FnSig _ _ Nothing) <- getSigFromType method ty
        checkFunc sig params


    check (SCall f params) = do
        sig@(FnSig _ _ Nothing) <- getSigOfFunc f
        checkFunc sig params

    check (SReturn expr) = do
        ty <- getEncloseFuncRetTy
        ty' <- infer expr
        assertEq ty ty'

    check SBreak = return ()

checkFunc :: FnSig -> [Expr] -> Static ()
checkFunc (FnSig _ bindings _) params = do
    tys <- mapM infer params
    assert (length tys == length bindings) "length of params is not equal to type signature"
    mapM_ (uncurry assertEq) $ zip (map snd bindings) tys

instance Check Block where
    check (Branch expr ss1 ss2) = do
        ty <- infer expr
        assert (ty == TyBool) "Type of 'if' condition is not Bool"
        check ss1
        check ss2

    check (Loop expr ss) = do
        ty <- infer expr
        assert (ty == TyBool) "Type of 'while' condition is not Bool"
        check ss

class Infer a where
    infer :: a -> Static Type

instance Infer Expr where
    infer (EVar x) = lookUpLocalVar x
    infer (EGet x attr) = do
        ty <- infer x
        lookUpAttr ty attr

    infer (EInvoke x f params) = do
        ty <- infer x
        sig@(FnSig _ _ (Just ty)) <- getSigFromType f ty
        checkFunc sig params
        return ty

    infer (ECall f params) = do
        sig@(FnSig _ _ (Just ty)) <- getSigOfFunc f
        checkFunc sig params
        return ty

    infer (ENew cls params) = do
        attrs <- getAttrs cls
        tys <- mapM infer params
        mapM_ (uncurry assertEq) $ zip (map snd attrs) tys
        return $ TyClass cls

    infer (ELit lit) = case lit of
        LStr  _ -> return TyStr
        LInt  _ -> return TyInt
        LBool _ -> return TyBool

instance Infer Name where
    infer = lookUpLocalVar

instance Infer LHS where
    infer (LVar x) = lookUpLocalVar x
    infer (LAttr x attr) = do
        ty <- infer x
        lookUpAttr ty attr


addFnSig :: FnSig -> Static ()
addFnSig sig@(FnSig name _ _) = funcTable %= M.insert name sig


inMethod :: Name -> FnSig -> Static a -> Static a
inMethod cls (FnSig _ bindings mRet) m = do
    case mRet of
        Just ty -> enclosedRet .= Just ty
        Nothing -> return ()

    old <- use localTable
    localTable .= M.fromList (bindings ++ [("self", TyClass cls)])
    ret <- m

    enclosedRet .= Nothing
    localTable .= old
    return ret


inFunc :: FnSig -> Static a -> Static a
inFunc (FnSig _ bindings mRet) m = do
    case mRet of
        Just ty -> enclosedRet .= Just ty
        Nothing -> return ()

    old <- use localTable
    localTable .= M.fromList bindings
    ret <- m

    enclosedRet .= Nothing
    localTable .= old
    return ret


addEmptyClass :: Name -> (Maybe Name) -> Static ()
addEmptyClass name mInherit =
    classTable %= M.insert name (ClassType mInherit M.empty M.empty)

addAttr :: Name -> Binding -> Static ()
addAttr name (x, ty) = classTable %= M.update (Just . over attrTable (M.insert x ty)) name

addMethod :: Name -> Method -> Static ()
addMethod name = \case
    Virtual sig        -> addMethod' sig
    Concrete sig stmts -> do
        addMethod' sig
        inMethod name sig $ check stmts
    where
        addMethod' :: FnSig -> Static ()
        addMethod' sig@(FnSig x _ _) =
            classTable %= M.update (Just . over mtdTable (M.insert x sig)) name

assertEq :: (MonadError String m, Eq a, Show a) => a -> a -> m ()
assertEq a b = assert (a == b) $ show a ++ " and " ++ show b ++ " are not equal"

assert :: MonadError e m => Bool -> e -> m ()
assert b e = if b then return () else throwError e

addBinding :: Binding -> Static ()
addBinding (x, ty) = localTable %= M.insert x ty


getSigFromType :: Name -> Type -> Static FnSig
getSigFromType method = \case
    TyClass cls -> do
        t <- use classTable
        case M.lookup cls t of
            Just table -> case M.lookup method $ _mtdTable table of
                Just sig -> return sig
                Nothing -> throwError $ "has no idea of method " ++ method
            Nothing -> throwError $ "has no idea of class " ++ cls
    other       -> queryBuiltinMethod method other


getSigOfFunc :: Name -> Static FnSig
getSigOfFunc f = do
    t <- use funcTable
    case M.lookup f t of
        Just sig -> return sig
        Nothing  -> throwError $ "can't find signature of " ++ f

getEncloseFuncRetTy :: Static Type
getEncloseFuncRetTy = do
    e <- use enclosedRet
    case e of
        Just retty -> return retty
        Nothing -> throwError "can't find return type of enclosed function environment"

lookUpLocalVar :: Name -> Static Type
lookUpLocalVar x = do
    t <- use localTable
    case M.lookup x t of
        Just ty -> return ty
        Nothing -> throwError $ "can't find type of variable " ++ x

lookUpAttr :: Type -> Name -> Static Type
lookUpAttr ty name = do
    case ty of
        TyClass cls -> do
            bindings <- getAttrs cls
            case L.lookup name bindings of
                Just ty -> return ty
                Nothing -> throwError $ "can't find type of attribute " ++ name ++ " of class " ++ cls
        other -> queryBuiltinAttr name other

getAttrs :: Name -> Static [Binding]
getAttrs cls = do
    t <- use classTable
    case M.lookup cls t of
        Just (ClassType _ attrs _) -> return $ M.toList attrs
        _ -> throwError $ "can't get attributes of class " ++ cls

builtInMethods :: M.Map (Type, Name) FnSig
builtInMethods = M.fromList [
      ((TyInt, "add"), FnSig "add" [("x", TyInt)] (Just TyInt))
    ]

builtInAttrs :: M.Map (Type, Name) Type
builtInAttrs = M.empty

queryBuiltinMethod :: Name -> Type -> Static FnSig
queryBuiltinMethod name ty =
    case M.lookup (ty, name) builtInMethods of
        Just sig -> return sig
        Nothing  -> throwError $ show ty ++ " doesn't have \"" ++ name ++ "\" method builtin"


queryBuiltinAttr :: Name -> Type -> Static Type
queryBuiltinAttr name ty = 
    case M.lookup (ty, name) builtInAttrs of
        Just ty -> return ty
        Nothing -> throwError $ show ty ++ " doesn't have \"" ++ name ++ "\" method builtin"

