{-# LANGUAGE FlexibleContexts #-}

module Language.RuScript.Parser where

import Language.RuScript.AST
import Language.RuScript.Desugar

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Tok

testParse :: MyParser a -> String -> Either ParseError a
testParse p = parse p ""

parseProgram :: String -> Either ParseError Program
parseProgram = parse pProgram ""

-- testParser p = parse p ""

type MyParser = CharParser ()

pProgram :: MyParser Program
pProgram = Program <$> many (whiteSpace *> pTopLevel <* whiteSpace)


pTopLevel :: MyParser (Either Statement Declaration)
pTopLevel = Left <$> pStatement
        <|> Right <$> (pFuncDecl <|> pClassDecl <|> pImportDecl)

pImportDecl :: MyParser Declaration
pImportDecl = ImportDecl <$> (reserved "import" *> whiteSpace *> (pIdent `sepEndBy` char '.'))

pQualified :: MyParser (Qualified Name)
pQualified = f <$> pIdent `sepEndBy` char '.'
  where
    f []  = error "parsed nothing out as qualified name"
    f xs  = let xs' = reverse xs
            in  Qualified (reverse $ tail xs') (head xs')

pStatement :: MyParser Statement
pStatement = SVar <$> (reserved "var" *> pBinding) <*> pMaybeEqualExpr <* char ';' <* whiteSpace
         <|> try (SAssign <$> (pLHS <* pEq) <*> (pExpr <* char ';') <* whiteSpace)
         <|> SReturn <$> (reserved "return" *> pExpr <* char ';' <* whiteSpace)
         <|> reserved "break" *> char ';' *> return SBreak <* whiteSpace
         <|> SBlock <$> (pBranch <|> pLoop)
         <|> try (SInvoke <$> (EVar <$> pIdent <* char '.') <*> pIdent <*> pParams <* char ';' <* whiteSpace)
         <|> try (SInvoke <$> (parens pExpr <* char '.') <*> pIdent <*> pParams <* char ';' <* whiteSpace)
         <|> SCall <$> pQualified <*> pParams <* char ';' <* whiteSpace

pLHS :: MyParser LHS
pLHS = try (LAttr <$> (pIdent <* char '.') <*> pIdent)
   <|> LVar <$> pIdent

pMaybeEqualExpr :: MyParser (Maybe Expr)
pMaybeEqualExpr = try (Just <$> (whiteSpace *> char '=' *> whiteSpace *> pExpr))
              <|> return Nothing

pBranch :: MyParser Block
pBranch = do
    reserved "if"
    whiteSpace
    cond <- parens pExpr
    whiteSpace
    cb1 <- braces (many pStatement)
    whiteSpace
    reserved "else"
    cb2 <- braces (many pStatement)
    return $ Branch cond cb1 cb2

pLoop :: MyParser Block
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
    whiteSpace
    className <- pIdent
    whiteSpace
    mInherits <- try pInherit
    (as, ms) <- braces $ do
        as <- many $ try ((,) <$> pVisibility <*> pBinding)
        ms <- many ((,) <$> pVisibility <*> pMethod)
        return (as, ms)
    return $ ClassDecl className mInherits as ms

pVisibility :: MyParser Visibility
pVisibility = try (return Private <* reserved "private")
          <|> return Public

pInherit :: MyParser (Maybe (Qualified String))
pInherit = Just <$> (reserved "inherits" *> pQualified)
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
    args <- whiteSpace *> (parens $ pBinding `sepEndBy` (char ',' <* whiteSpace))
    whiteSpace
    mty <- pMaybeRetType
    whiteSpace
    return $ FnSig (Qualified [] name) args mty

pMaybeRetType :: MyParser (Maybe Type)
pMaybeRetType = try (Just <$> (string "->" *> whiteSpace *> pType))
            <|> return Nothing

pBinding :: MyParser Binding
pBinding = (,) <$> (pIdent <* whiteSpace) <*> (char ':' *> (whiteSpace *> pType))

pType :: MyParser Type
pType = reserved "Int" *> return TyInt
    <|> reserved "Bool" *> return TyBool
    <|> reserved "Str" *> return TyStr
    <|> reserved "Nil" *> return TyNil
    <|> TyList <$> (string "List<" *> pType <* char '>')
    <|> TyClass <$> pQualified

pExpr :: MyParser Expr
pExpr = pNew
    <|> try (EInvoke <$> (EVar <$> pIdent <* char '.') <*> pIdent <*> pParams)
    <|> try (EInvoke <$> (parens pExpr <* char '.') <*> pIdent <*> pParams)
    <|> try pGet
    <|> ELit <$> pLit
    <|> try (ECall <$> pQualified <*> pParams)
    <|> EVar <$> pIdent
    <|> desugarTerm <$> parens pTerm

pBinary :: (Expr -> Expr -> Term) -> MyParser String -> MyParser Term
pBinary cons pOp = cons <$> pExpr <* (whiteSpace *> pOp <* whiteSpace) <*> pExpr

pTerm = try (pBinary TPlus (string "+"))
    <|> try (pBinary TLE   (string "<="))

pNew :: MyParser Expr
pNew  = ENew <$> (reserved "new" *> pQualified) <*> pParams

pParams :: MyParser [Expr]
pParams = parens (pExpr `sepEndBy` (char ',' <* whiteSpace))

pGet :: MyParser Expr
pGet = EGet <$> (pIdent <* char '.') <*> pIdent

pLit :: MyParser Literal
pLit = try (LBool <$> pBool)
   <|> LStr <$> parseString
   <|> LInt . fromInteger <$> pInt
   <|> string "[]" *> return LList
   <|> reserved "nil" *> return LNil

pBool :: MyParser Bool
pBool = reserved "True"  *> return True
    <|> reserved "False" *> return False

--------- LangDef -----------

pEq :: MyParser ()
pEq   = whiteSpace *> reservedOp "="

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
, Tok.reservedNames = ["class", "fn", "new", "return", "break", "while", "if", "else", "private", "virtual", "var"]
, Tok.reservedOpNames = ["="]
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

