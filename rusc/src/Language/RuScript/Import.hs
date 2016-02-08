{-# LANGUAGE LambdaCase #-}
-- Import system

module Language.RuScript.Import where

import System.FilePath ((</>))
import System.IO
import System.Exit
import Data.Either (lefts, rights)

import Language.RuScript.AST
import Language.RuScript.StaticCheck
import Language.RuScript.Parser

-- all exposed (which is just everything) declarations as AST will be brought out
importModule :: [String] -> IO [Declaration]
importModule segments = do
    let path = foldr (</>) "" segments ++ ".rus"
    source <- readFile path
    case parseProgram source of
        Left err -> exitError $ "Error in parsing: " ++ show err
        Right ast -> do
            case checkProgram ast of
                Left err -> exitError $ "Error in checking: " ++ err
                Right _  -> do
                    let (otherDecls, imports) = splitDecls ast
                    decls <- concat <$> mapM importModule imports
                    return $ map (qualify segments) otherDecls ++ decls

exitError :: String -> IO a
exitError s = hPutStrLn stderr s >> exitFailure

qualify :: [String] -> Declaration -> Declaration
qualify qualifier = \case
            ImportDecl _ -> error "unexpected ImportDecl in qualification process"
            FnDecl (FnSig (Qualified _ name) bindings mTy) stmts ->
                FnDecl (FnSig (Qualified qualifier name) bindings mTy) stmts
            ClassDecl (Qualified _ name) mInherit attrs mtds ->
                let mInherit' = (\(Qualified _ iname) -> Qualified qualifier iname) <$> mInherit
                in  ClassDecl (Qualified qualifier name) mInherit' attrs mtds                

splitDecls :: Program -> ([Declaration], [[String]])
splitDecls (Program mix) =
    let decls = rights mix
        sss   = flip map decls $ \case
                    ImportDecl s -> Right s
                    other        -> Left other
    in  (lefts sss, rights sss)

resolveDeps :: Program -> IO Program
resolveDeps program@(Program mixed) = do
    case checkProgram program of
        Left err -> exitError $ "Error in checking: " ++ err
        Right _  -> do
            let (_, imports) = splitDecls program
            decls <- concat <$> mapM importModule imports
            return $ Program (mixed ++ map Right decls)

