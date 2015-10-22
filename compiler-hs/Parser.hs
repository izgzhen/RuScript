{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Tok

type Source = [Statement]

data Statement = Assignment String Expr
               | ClassDecl String [String] [MethodDecl]
               | Print Expr
               | Return Expr
               deriving (Show)


data MethodDecl = MethodDecl String [String] [String] [Statement] deriving (Show)

data Expr = Single Term
          | Plus Term Term
          | New String
          deriving (Show)

data Term = Var String
          | LitInt Int
          deriving (Show)

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
        as <- many pIdent
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

pArgs = whiteSpace *> (parens $ pIdent `sepEndBy1` (char ',' <* whiteSpace))

pExpr = New    <$> (reserved "new" *> pIdent)
    <|> Plus   <$> try (whiteSpace *> pTerm <* pPlus) <*> (whiteSpace *> pTerm)
    <|> Single <$> (whiteSpace *> pTerm)

pTerm =  Var <$> pIdent
     <|> (LitInt . fromIntegral) <$> pInt

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
