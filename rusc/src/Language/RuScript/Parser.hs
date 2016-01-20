{-# LANGUAGE FlexibleContexts #-}

module Language.RuScript.Parser where

import Language.RuScript.AST

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as C

parseSrc = testParser pSrc

testParser p = parse p ""

type MyParser = CharParser ()

pSrc :: MyParser Program
pSrc = Program <$> many (whiteSpace *> pTopLevel <* whiteSpace)


pTopLevel :: MyParser (Either Statement Declaration)
pTopLevel = Left <$> pStatement
        <|> Right <$> (pFuncDecl <|> pClassDecl)

pStatement :: MyParser Statement
pStatement = SVar <$> (reserved "var" *> pBinding) <*> pMaybeEqualExpr
         <|> SAssign <$> (pIdent <* pEq) <*> (pExpr <* char ';')
         <|> reserved "return" *> char ';' *> return SReturn
         <|> reserved "break" *> char ';' *> return SBreak
         <|> SBBlock <$> pBBlock

pMaybeEqualExpr :: MyParser (Maybe Expr)
pMaybeEqualExpr = try (Just <$> (char '=' *> pExpr))
              <|> return Nothing


pBBlock = pBranch <|> pLoop

pBranch = do
    reserved "if"
    whiteSpace
    cond <- parens pExpr
    whiteSpace
    cb1 <- braces (many pStatement)
    whiteSpace
    reserved "then"
    cb2 <- braces (many pStatement)
    return $ Branch cond cb1 cb2

pLoop = do
    reserved "while"
    whiteSpace
    cond <- parens pExpr
    whiteSpace
    cb <- braces (many pStatement)
    return $ Loop cond cb

pClassDecl :: MyParser Declaration
pClassDecl = do
    reserved "class"
    className <- pIdent
    mInherits <- try pInherit
    (as, ms) <- braces $ do
        as <- many pAttr
        ms <- many ((,) <$> pVisibility <*> pMethod)
        return (as, ms)
    return $ ClassDecl className mInherits as ms

pVisibility :: MyParser Visibility
pVisibility = try (return Private <* reserved "private")
          <|> return Public

pAttr = (,) <$> pVisibility <*> pBinding

pInherit :: MyParser (Maybe String)
pInherit = Just <$> (reserved "inherits" *> pIdent)
       <|> return Nothing

pMethod :: MyParser Method
pMethod = Virtual <$> (reserved "virtual" *> pFnSig)
      <|> Concrete <$> pFnSig <*> braces (many pStatement)

pFuncDecl :: MyParser Declaration
pFuncDecl = FnDecl <$> pFnSig <*> braces (many pStatement)

pFnSig :: MyParser FnSig
pFnSig = do
    reserved "fn"
    whiteSpace
    name <- pIdent
    whiteSpace
    args <- pArgs
    whiteSpace
    return $ FnSig name args

pArgs = whiteSpace *> (parens $ pBinding `sepEndBy` (char ',' <* whiteSpace))

pBinding :: MyParser Binding
pBinding = (,) <$> (pIdent <* whiteSpace) <*> (char ':' *> (whiteSpace *> pType))

pType :: MyParser Type
pType = reserved "Int" *> return TyInt
    <|> reserved "Bool" *> return TyBool
    <|> reserved "Str" *> return TyStr
    <|> TyClass <$> pIdent

pExpr =  ELit <$> pLit
     <|> pNew
     <|> try pInvoke
     <|> try pGet
     <|> EVar <$> pIdent

pNew  = ENew <$> (reserved "new" *> pIdent) <*> parens (pExpr `sepEndBy` (char ',' <* whiteSpace))

pInvoke = EInvoke <$> (pIdent <* char '.') <*> pIdent <*> parens (pExpr `sepEndBy` (char ',' <* whiteSpace))

pGet = EGet <$> (pIdent <* char '.') <*> pIdent

pLit = LStr <$> parseString
   <|> (\s -> LInt (read s :: Int)) <$> many digit
   <|> LBool <$> (reserved "True"  *> return True
              <|> reserved "False" *> return False)

--------- LangDef -----------

pEq   = whiteSpace *> reservedOp "="

reservedNames = ["class", "fn", "new", "return", "break", "while", "if", "then", "private", "virtual", "var"]
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

