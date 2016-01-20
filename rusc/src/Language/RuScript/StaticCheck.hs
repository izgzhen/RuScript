{-# LANGUAGE TemplateHaskell, RankNTypes,
             TypeSynonymInstances, FlexibleInstances,
             FlexibleContexts #-}

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

data StaticState = StaticState {
  _localTable :: M.Map Name Type
, _funcTable  :: M.Map Name FnSig
, _classTable :: M.Map Name ClassType
, _enclosed   :: Maybe FnSig
}

data ClassType = ClassType {
  _mInherit  :: Maybe Name
, _attrTable :: M.Map Name Type
, _mtdTable  :: M.Map Name FnSig
}

makeLenses ''StaticState
makeLenses ''ClassType

type Static = ExceptT String (State StaticState)

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
        inLocal sig $ check stmts

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
checkFunc sig params = case sig of
    FnSig _ bindings _ -> do
        tys <- mapM infer params
        assert (length tys == length bindings) "length of params is not equal to type signature"
        mapM_ (uncurry assertEq) $ zip (map snd bindings) tys
    _ -> throwError "error in check SInvoke"

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
addFnSig = undefined

inLocal :: FnSig -> Static a -> Static a
inLocal sig m = do
    enclosed .= Just sig
    ret <- m
    enclosed .= Nothing
    return ret

addEmptyClass :: Name -> (Maybe Name) -> Static ()
addEmptyClass = undefined

addAttr :: Name -> Binding -> Static ()
addAttr = undefined

addMethod :: Name -> Method -> Static ()
addMethod = undefined

assertEq :: (MonadError String m, Eq a, Show a) => a -> a -> m ()
assertEq a b = assert (a == b) $ show a ++ " and " ++ show b ++ " are not equal"

assert :: MonadError e m => Bool -> e -> m ()
assert b e = if b then return () else throwError e

addBinding :: Binding -> Static ()
addBinding = undefined


getSigFromType :: Name -> Type -> Static FnSig
getSigFromType = undefined

getSigOfFunc :: Name -> Static FnSig
getSigOfFunc = undefined

getEncloseFuncRetTy :: Static Type
getEncloseFuncRetTy = undefined


lookUpLocalVar :: Name -> Static Type
lookUpLocalVar = undefined

lookUpAttr :: Type -> Name -> Static Type
lookUpAttr = undefined


getAttrs :: Name -> Static [Binding]
getAttrs = undefined



