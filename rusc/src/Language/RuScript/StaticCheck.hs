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
, _funcTable   :: M.Map Qualified FnSig
, _classTable  :: M.Map Qualified ClassType
, _enclosedRet :: Maybe Type
}

data ClassType = ClassType {
  _mInherit  :: Maybe Qualified
, _attrTable :: M.Map Name (Visibility, Type)
, _mtdTable  :: M.Map Name (Visibility, FnSig)
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
        -- note the order: we must load the information in
        -- declarations first
        check decls
        check stmts

instance Check Declaration where
    check (FnDecl sig stmts) = do
        addFnSig sig
        inFunc sig $ check stmts

    check (ClassDecl name mFather attrs methods) = do
        addEmptyClass name mFather
        mapM_ (\(vis, binding) -> addAttr   name vis binding) attrs
        mapM_ (\(vis, method)  -> addMethod name vis method) methods

    check (ImportDecl _) = return ()

instance Check Statement where
    check (SVar binding mExpr) = do
        addBinding binding
        case mExpr of
            Just expr -> do
                ty <- infer expr
                snd binding `isSubTypeOf` ty
            Nothing -> return ()

    check (SAssign lhs expr) = do
        ty1 <- infer lhs
        ty2 <- infer expr
        ty1 `isSubTypeOf` ty2

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
        ty `isSubTypeOf` ty'

    check SBreak = return ()

checkFunc :: FnSig -> [Expr] -> Static ()
checkFunc (FnSig _ bindings _) params = do
    tys <- mapM infer params
    assert (length tys == length bindings) "length of params is not equal to type signature"
    mapM_ (uncurry isSubTypeOf) $ zip (map snd bindings) tys

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
        checkVis x ty attr
        snd <$> lookUpAttr ty attr

    infer (EInvoke x f params) = do
        ty <- infer x
        sig@(FnSig _ _ (Just retty)) <- getSigFromType f ty
        checkFunc sig params
        return retty

    infer (ECall f params) = do
        sig@(FnSig _ _ (Just ty)) <- getSigOfFunc f
        checkFunc sig params
        return ty

    infer (ENew cls params) = do
        attrs <- getAttrs cls
        tys <- mapM infer params
        mapM_ (uncurry isSubTypeOf) $ zip (map (snd . snd) attrs) tys
        return $ TyClass cls

    infer (ELit lit) = case lit of
        LStr  _ -> return TyStr
        LInt  _ -> return TyInt
        LBool _ -> return TyBool
        LList   -> return $ TyList TyBot
        LNil    -> return TyNil

instance Infer Name where
    infer = lookUpLocalVar

instance Infer LHS where
    infer (LVar x) = lookUpLocalVar x
    infer (LAttr x attr) = do
        ty <- infer x
        checkVis x ty attr
        snd <$> lookUpAttr ty attr


-- Check visibility rule: If `x` is `this`, no check;
-- else, check by querying the class table
checkVis :: Name -> Type -> Name -> Static ()
checkVis "this" _ _ = return ()
checkVis _ ty attrName = do
    (vis, _) <- lookUpAttr ty attrName
    case vis of
        Public  -> return ()
        Private -> throwError $ "accessing private attribute " ++ attrName

addFnSig :: FnSig -> Static ()
addFnSig sig@(FnSig name _ _) = funcTable %= M.insert name sig


inMethod :: Qualified -> FnSig -> Static a -> Static a
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


addEmptyClass :: Qualified -> (Maybe Qualified) -> Static ()
addEmptyClass name mFather =
    classTable %= M.insert name (ClassType mFather M.empty M.empty)

addAttr :: Qualified -> Visibility -> Binding -> Static ()
addAttr name vis (x, ty) =
    classTable %= M.update (Just . over attrTable (M.insert x (vis, ty))) name

addMethod :: Qualified -> Visibility -> Method -> Static ()
addMethod name vis = \case
    Virtual sig        -> addMethod' sig
    Concrete sig stmts -> do
        addMethod' sig
        inMethod name sig $ check stmts
    where
        addMethod' :: FnSig -> Static ()
        addMethod' sig@(FnSig (Qualified _ x) _ _) =
            classTable %= M.update (Just . over mtdTable (M.insert x (vis, sig))) name

-- isSubTypeOf
isSubTypeOf :: Type -> Type -> Static ()
isSubTypeOf t1 t2
    | t1 == t2  = return ()
    | otherwise =
        case t2 of
            TyClass cls -> do
                t <- lookupClass cls
                case (_mInherit t) of
                    Just inherit
                        | TyClass inherit == t1 -> return ()
                        | otherwise             -> isSubTypeOf t1 (TyClass inherit)
                    Nothing      -> err
            TyList TyBot ->
                case t1 of
                    TyList _ -> return ()
                    _        -> err
            _ -> err
  where
    err = throwError $ show t2 ++ " can't be subtype of " ++ show t1


assertEq :: (MonadError String m, Eq a, Show a) => a -> a -> m ()
assertEq a b = assert (a == b) $ show a ++ " and " ++ show b ++ " are not equal"

assert :: MonadError e m => Bool -> e -> m ()
assert b e = if b then return () else throwError e

addBinding :: Binding -> Static ()
addBinding (x, ty) = localTable %= M.insert x ty


getSigFromType :: Name -> Type -> Static FnSig
getSigFromType method = \case
    TyClass cls -> do
        table <- lookupClass cls
        case M.lookup method $ _mtdTable table of
            Just (_, sig) -> return sig
            Nothing       -> throwError $ "has no idea of method " ++ method
    other       -> queryBuiltinMethod method other

lookupClass :: Qualified -> Static ClassType
lookupClass cls = do
    t <- use classTable
    case M.lookup cls t of
        Just table -> return table
        Nothing    -> throwError $ "I has no idea of class " ++ show cls


getSigOfFunc :: Qualified -> Static FnSig
getSigOfFunc f = do
    t <- use funcTable
    case M.lookup f t of
        Just sig -> return sig
        Nothing  -> throwError $ "can't find signature of " ++ show f

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

lookUpAttr :: Type -> Name -> Static (Visibility, Type)
lookUpAttr ty name = do
    case ty of
        TyClass cls -> do
            bindings <- getAttrs cls
            case L.lookup name bindings of
                Just attr -> return attr
                Nothing -> throwError $ "can't find type of attribute " ++ name ++ " of class " ++ show cls
        other -> (,) Public <$> queryBuiltinAttr name other

getAttrs :: Qualified -> Static [(Name, (Visibility, Type))]
getAttrs cls = do
    t <- use classTable
    case M.lookup cls t of
        Just (ClassType _ attrs _) -> return $ M.toList attrs
        _ -> throwError $ "can't get attributes of class " ++ show cls

builtInMethods :: M.Map (Type, Name) FnSig
builtInMethods = M.fromList [
      ((TyInt, "add"), FnSig (Qualified [] "add") [("x", TyInt)] (Just TyInt))
    , ((TyInt, "print"), printSig)
    , ((TyBool, "not"), FnSig (Qualified [] "not") [] Nothing)
    , ((TyInt, "le"), FnSig (Qualified [] "le") [("x", TyInt)] (Just TyBool))
    , ((TyStr, "print"), printSig)
    , ((TyNil, "print"), printSig)
    ]
    where
        printSig = FnSig (Qualified [] "print") [] Nothing

builtInAttrs :: M.Map (Type, Name) Type
builtInAttrs = M.empty

queryBuiltinMethod :: Name -> Type -> Static FnSig
queryBuiltinMethod name ty =
    case M.lookup (ty, name) builtInMethods of
        Just sig -> return sig
        Nothing  ->
            case ty of
                TyList eTy -> case name of
                    "cons"  -> return $ FnSig (Qualified [] "cons") [("x", eTy)] (Just (TyList eTy))
                    "print" -> return $ FnSig (Qualified [] "print") [] Nothing
                    _      -> err
                _ -> err
    where
        err = throwError $ show ty ++ " doesn't have \"" ++ name ++ "\" method builtin"


queryBuiltinAttr :: Name -> Type -> Static Type
queryBuiltinAttr name ty = 
    case M.lookup (ty, name) builtInAttrs of
        Just attrty -> return attrty
        Nothing -> throwError $ show ty ++ " doesn't have \"" ++ name ++ "\" method builtin"

