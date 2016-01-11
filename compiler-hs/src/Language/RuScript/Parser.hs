{-# LANGUAGE FlexibleContexts #-}

module Language.RuScript.Parser where

import Language.RuScript.AST

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as C

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

pTerm =  TLitStr <$> parseString
     <|> TNew <$> pNew
     <|> try (TCall <$> pCall)
     <|> try (TAccess <$> pAccess)
     <|> TVar <$> pIdent
     <|> (TLitInt . fromIntegral) <$> pInt

pNew  = New <$> (reserved "new" *> pIdent) <*> parens (pExpr `sepEndBy` (char ',' <* whiteSpace))

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

