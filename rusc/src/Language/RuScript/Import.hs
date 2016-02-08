{-# LANGUAGE LambdaCase #-}
-- Import system

module Language.RuScript.Import where

import System.FilePath ((</>))
import System.IO
import System.Exit
import Data.Either (lefts, rights)
import Data.Generics.Multiplate
import Data.Functor.Identity
import Control.Arrow (second)

import Language.RuScript.AST
import Language.RuScript.StaticCheck
import Language.RuScript.Parser
import Language.RuScript.Traversal

-- all exposed (which is just everything) declarations as AST will be brought out
importModule :: FilePath -> [String] -> IO [Declaration]
importModule includeDir segments = do
    let path = includeDir </> foldr (</>) "" segments ++ ".rus"
    source <- readFile path
    case parseProgram source of
        Left err -> exitError $ "Error in parsing: " ++ show err
        Right ast -> do
            case checkProgram ast of
                Left err -> exitError $ "Error in checking: " ++ err
                Right _  -> do
                    let (otherDecls, imports) = splitDecls ast
                    decls <- concat <$> mapM (importModule includeDir) imports
                    return $ map (qualify segments) otherDecls ++ decls

exitError :: String -> IO a
exitError s = hPutStrLn stderr s >> exitFailure

splitDecls :: Program -> ([Declaration], [[String]])
splitDecls (Program mix) =
    let decls = rights mix
        sss   = flip map decls $ \case
                    ImportDecl s -> Right s
                    other        -> Left other
    in  (lefts sss, rights sss)

resolveDeps :: FilePath -> Program -> IO Program
resolveDeps includeDir program@(Program mixed) = do
    let (_, imports) = splitDecls program
    decls <- concat <$> mapM (importModule includeDir) imports
    return $ Program (mixed ++ map Right decls)

class Qualifiable a where
    qualify :: [String] -> a -> a

instance Qualifiable a => Qualifiable [a] where
    qualify qualifier xs = map (qualify qualifier) xs

instance Qualifiable Declaration where
    qualify qualifier = \case
        ImportDecl _ -> error "unexpected ImportDecl in qualification process"
        FnDecl sig stmts -> FnDecl (qualify qualifier sig) (qualify qualifier stmts)
        ClassDecl (Qualified _ name) mInherit attrs mtds ->
            let mInherit' = qualify qualifier <$> mInherit
                mtds' = flip map mtds $ \(vis, mtd) -> case mtd of
                            Virtual sig       -> (vis, Virtual  (qualify qualifier sig))
                            Concrete sig body -> (vis, Concrete (qualify qualifier sig)
                                                                (qualify qualifier body))
            in  ClassDecl (Qualified qualifier name) mInherit' attrs mtds'

instance Qualifiable Statement where
    qualify qualifier = mapper (qualifyPlate qualifier) statement_

instance Qualifiable Type where
    qualify qualifier = mapper (qualifyPlate qualifier) type_

instance Qualifiable Qualified where
    qualify qualifier = \case
        Qualified [] name -> Qualified qualifier name
        other             -> other

instance Qualifiable FnSig where
    qualify qualifier (FnSig qualified bindings mRetTy) =
        FnSig (qualify qualifier qualified)
              (map (second $ qualify qualifier) bindings)
              (qualify qualifier <$> mRetTy)

qualifyPlate :: [String] -> Plate Identity
qualifyPlate qualifier = purePlate { qualified_ = pure . qualify qualifier }

