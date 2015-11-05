{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as C

type Source = [Statement]

showSource :: Source -> String
showSource = unlines . map show

data Statement = Assignment String Expr
               | ClassDecl String [String] [MethodDecl]
               | Print Expr
               | Return Expr

instance Show Statement where
  show (Assignment name expr) = name ++ " = " ++ show expr ++ ";"
  show (ClassDecl name attrs methods) = "class " ++ name ++ " {\n" ++
                                        unlines (map (\a -> "\t" ++ a ++ ";") $ attrs ++ map show methods) ++
                                        "};"
  show (Print expr) = "print " ++ show expr ++ ";"
  show (Return expr) = "return " ++ show expr ++ ";"

data MethodDecl = MethodDecl String [String] [String] [Statement]

getMethodName (MethodDecl n _ _ _) = n

instance Show MethodDecl where
  show (MethodDecl name args globals stmts) = "fn " ++ name ++ " (" ++ sepShow args ++ ") {\n" ++
                                              unlines ( map ("\t\t" ++)
                                                   (map ("global " ++) globals ++
                                                    map show stmts)) ++ 
                                              "\t}"

sepShow [] = ""
sepShow [x] = x
sepShow (x:y:xs) = x ++ ", " ++ sepShow (y:xs)

data Expr = Single Term
          | Plus Term Term

instance Show Expr where
    show (Single t) = show t
    show (Plus t1 t2) = show t1 ++ " + " ++ show t2

data Term = Var String
          | LitInt Int
          | LitStr String
          | New String
          | Call String String [Expr]
          | Access String String

instance Show Term where
    show (LitStr s) = "\"" ++ s ++ "\""
    show (LitInt i) = show i
    show (Var v) = v
    show (New name) = "new " ++ name
    show (Call objName methodName params) = objName ++ "." ++ methodName ++ "(" ++ sepShow (map show params) ++ ")"
    show (Access objName attrName) = objName ++ "." ++ attrName


--------- Parser ------------

parseSrc = testParser pSrc

testParser p = parse p ""

pSrc :: CharParser () Source
pSrc = many (whiteSpace *> pStmt <* (char ';' <* whiteSpace))

pStmt = Assignment <$> (pIdent <* pEq) <*> pExpr
    <|> pClassDecl
    <|> Print <$> (reserved "print" *> whiteSpace *> pExpr)
    <|> Return <$> (reserved "return" *> whiteSpace *> pExpr)

-- Helper

pClassDecl = do
    reserved "class"
    className <- pIdent
    (as, ms) <- braces $ do
        as <- many (pIdent <* char ';' <* whiteSpace)
        ms <- many pMethodDecl
        return (as, ms)
    return $ ClassDecl className as ms

pMethodDecl = do
    reserved "fn"
    whiteSpace
    name <- pIdent
    whiteSpace
    args <- pArgs
    whiteSpace
    braces $ MethodDecl name args
                        <$> many (reserved "global" *> (whiteSpace *> pIdent <* char ';') <* whiteSpace)
                        <*> pSrc

pArgs = whiteSpace *> (parens $ pIdent `sepEndBy` (char ',' <* whiteSpace))

pExpr = Plus   <$> try (whiteSpace *> pTerm <* pPlus) <*> (whiteSpace *> pTerm)
    <|> Single <$> (whiteSpace *> pTerm)

pTerm =  LitStr <$> parseString
     <|> New <$> (reserved "new" *> pIdent)
     <|> try pCall
     <|> try pAccess
     <|> Var <$> pIdent
     <|> (LitInt . fromIntegral) <$> pInt

pCall = Call <$> (pIdent <* char '.') <*> pIdent <*> parens (pExpr `sepEndBy` (char ',' <* whiteSpace))

pAccess = Access <$> (pIdent <* char '.') <*> pIdent

--------- LangDef -----------

pEq   = whiteSpace *> reservedOp "="
pPlus = whiteSpace *> reservedOp "+"

reservedNames = ["class", "fn", "global", "new", "return", "print"]
reservedOpNames = ["=", "+"]

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef {
  Tok.commentStart = "#-"
, Tok.commentEnd   = "-#"
, Tok.commentLine  = "#"
, Tok.nestedComments = True
, Tok.identStart   = letter
, Tok.identLetter  = alphaNum <|> oneOf "_'"
, Tok.opStart      = oneOf ":!#$%&*+./<=>?@\\^|-~"
, Tok.opLetter     = oneOf ":!#$%&*+./<=>?@\\^|-~"
, Tok.reservedNames = reservedNames
, Tok.reservedOpNames = reservedOpNames
, Tok.caseSensitive = True
}

lexer = Tok.makeTokenParser langDef

parens     = Tok.parens lexer
braces     = Tok.braces lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
whiteSpace = Tok.whiteSpace lexer
pIdent     = Tok.identifier lexer
pInt       = Tok.integer lexer

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser String
parseString = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

