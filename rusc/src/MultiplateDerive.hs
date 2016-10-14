{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module MultiplateDerive where

import Language.Haskell.TH hiding (varE)
import Data.Char (toLower)
import Control.Monad (forM)

temPlate :: String -> [String] -> Q [Dec]
temPlate plateName tyNames = do
  decls <- forM tyNames $ \tyName -> do
      Just ty <- lookupTypeName tyName
      reify ty >>= \case
          TyConI (DataD _ _ _ _ constrs _) -> return (tyName, constrs)
          other -> error $ "Can't read data type declaration: " ++ show other

  clauses <- mapM (genInstance (extractNames decls)) decls

  let mkDecl = FunD (mkName "mkPlate")
                    [Clause [VarP $ mkName "build"]
                            (NormalB $ foldl AppE (ConE (mkName plateName))
                                              $ map (\n -> AppE (VarE $ mkName "build")
                                                                (VarE $ mkName (lowerHead n ++ "_")))
                                                    tyNames)
                            []]
  let instDecl = InstanceD Nothing []
                           (AppT (ConT $ mkName "Multiplate") (ConT $ mkName plateName))
                           [FunD (mkName "multiplate")
                                 [Clause [VarP $ mkName "child"]
                                         (NormalB $ foldl AppE (ConE (mkName plateName))
                                                           $ map (\n -> VarE . mkName $ "build" ++ n)
                                                                 tyNames)
                                         clauses]
                           , mkDecl]

  return $ [ platDecl, instDecl ]
    where
        platDecl = DataD [] (mkName plateName) [binder] Nothing [constr] []
        binder   = PlainTV (mkName "f")
        constr   = RecC (mkName plateName) $ map (\s -> (mkName (lowerHead s ++ "_"),
                                                         Bang NoSourceUnpackedness NoSourceStrictness,
                                                         toType s)) tyNames
        toType s = let ty = ConT $ mkName s
                   in  AppT (AppT ArrowT ty) (AppT (VarT (mkName "f")) ty)
        extractNames = map fst

        genInstance :: [String] -> (String, [Con]) -> Q Dec
        genInstance recTyNames (tyName, constrs) = do
          let funName = mkName $ "build" ++ tyName
          clauses <- forM constrs $ \case
              NormalC name tys -> genClause recTyNames name tys snd
              RecC name tys    -> genClause recTyNames name tys (\(_, _, ty) -> ty)
              other            -> error $ "genInstance bad constr: " ++ show other
          return $ FunD funName clauses

        genClause :: [String] -> Name -> [a] -> (a -> Type) -> Q Clause
        genClause recTyNames name tys tyNameSel = do
          let conName = mkName $ nameBase name
          let numTys = length tys
          newNames <- sequence $ take numTys $ repeat (newName "x") :: Q [Name]
          let pairs = zip newNames $ map tyNameSel tys
          segments <- mapM (\(v, ty) -> build recTyNames (\f -> f (VarE v)) ty) pairs
          let bodys = NormalB $ appChain (ConE name) segments
          return $ Clause [ConP conName (map VarP newNames)]
                          bodys
                          []


---- Core

build :: [String] -> ((Exp -> Exp) -> Exp) -> Type -> Q Exp
-- <var> :: <ty>, return expr s.t. <expr> :: Applicative f => f <ty>
build astTys withDestr ty = case ty of
    -- <var> :: a
    ConT name -> do
        let baseName = nameBase name
        if baseName `elem` astTys
            then return $ buildE (selectorE baseName) (withDestr id)
            -- build <astTyConstr> <var> :: f a
            else return $ pureE (withDestr id)
            -- pure <var> :: f a

    AppT t ty -> do
        case getTuple t of
            Just depth -> do
                -- <var> :: (t1, t2, ..., t_{depth}), <ty> ~ t_{depth}
                let tupleConE = getTupleConE depth -- 2: (,) ...
                let ts = getRTs t ++ [ty] -- [Type], length = depth
                let components = map withDestr $ map (AppE . getSelE) [1..depth]
                es <- mapM (\(t, e) -> build astTys (\f -> f e) t) $ zip ts components
                return $ appChain tupleConE es -- :: f (t1, ..., t_{depth})
            Nothing -> do
                -- <var> :: Traversable t => t a
                x <- newName "x"
                f <- lamE x <$> build astTys (\f -> f (VarE x)) ty -- <f> :: a -> f a
                let g = wrapE f -- <g> :: t a -> f (t a)
                return $ AppE g (withDestr id) -- :: f (t a)

    other -> error $ "illegal: " ++ show other

    where -- Helpers, a lot
        constrE :: String -> Exp -- lowerHead
        constrE = ConE . mkName
        selectorE :: String -> Exp
        selectorE s = varE (lowerHead s ++ "_")
        getTuple :: Type -> Maybe Int
        getTuple (TupleT i) = Just i
        getTuple (AppT f _) = getTuple f
        getTuple _          = Nothing
        getTupleConE :: Int -> Exp
        getTupleConE i = constrE $ "(" ++ take (i - 1) (repeat ',') ++ ")"
        getRTs :: Type -> [Type]
        getRTs (AppT l r) = getRTs l ++ [r]
        getRTs _          = []
        getSelE :: Int -> Exp -- Data.Tuple.Select
        getSelE i = varE $ "sel" ++ show i
        wrapE :: Exp -> Exp
        wrapE f = InfixE (Just $ varE "sequenceA") (varE ".") (Just $ AppE (varE "fmap") f)
        lamE :: Name -> Exp -> Exp
        lamE x e = LamE [VarP x] e

lowerHead :: String -> String
lowerHead "" = ""
lowerHead (s:ss) = toLower s : ss


appChain :: Exp -> [Exp] -> Exp
appChain conE [] = pureE conE
appChain conE (fd:fds) =
    let header = InfixE (Just conE) (varE "<$>") (Just fd)
    in  foldl (\e1 e2 -> InfixE (Just e1) (varE "<*>") (Just e2)) header fds

buildE :: Exp -> Exp -> Exp
buildE constrE e = AppE (AppE constrE (varE "child")) e

pureE :: Exp -> Exp
pureE e = AppE (varE "pure") e

varE :: String -> Exp
varE = VarE . mkName

