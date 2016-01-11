module Language.RuScript.AST where

-- AST ADT

type CodeBlock = [Statement]

data Statement = Assignment String Expr
               | ClassDecl String [String] [MethodDecl]
               | Print Expr
               | Return Expr
               | IfThenElse Expr CodeBlock (Maybe CodeBlock)
               | While Expr CodeBlock


data MethodDecl = MethodDecl String [String] [String] CodeBlock



data Expr = Single Term
          | Plus Term Term


data Term = TVar String
          | TLitInt Int
          | TLitStr String
          | TNew New
          | TCall Call
          | TAccess Access

data New    = New String [Expr]
data Call   = Call String String [Expr]
data Access = Access String String

-- Pretty Printing

instance Show Statement where
  show (Assignment name expr) = name ++ " = " ++ show expr ++ ";"
  show (ClassDecl name attrs methods) = "class " ++ name ++ " {\n" ++
                                        unlines (map (\a -> "\t" ++ a ++ ";") $ attrs ++ map show methods) ++
                                        "};"
  show (Print expr) = "print " ++ show expr ++ ";"
  show (Return expr) = "return " ++ show expr ++ ";"
  show (IfThenElse condExpr contCb maybeAltCb) =
    ("if " ++ show condExpr ++ "{\n" ++ unlines (map show contCb) ++ "}\n") ++
    (case maybeAltCb of
        Just altCb -> "{\n" ++ unlines (map show altCb) ++ "}\n"
        Nothing    -> "")
  show (While condExpr loopCb) = ("while " ++ show condExpr ++ "{\n" ++ unlines (map show loopCb) ++ "}\n")


instance Show Term where
    show (TLitStr s) = "\"" ++ s ++ "\""
    show (TLitInt i) = show i
    show (TVar v) = v
    show (TNew new) = show new
    show (TCall call) = show call 
    show (TAccess acc) = show acc

instance Show Call where
    show (Call objName methodName params) = objName ++ "." ++ methodName ++ "(" ++ sepShow (map show params) ++ ")"

instance Show Access where
    show (Access objName attrName) = objName ++ "." ++ attrName

instance Show New where
    show (New className params) = "new " ++ className ++ "(" ++ sepShow (map show params) ++ ")"


instance Show MethodDecl where
  show (MethodDecl name args globals stmts) = "fn " ++ name ++ " (" ++ sepShow args ++ ") {\n" ++
                                              unlines ( map ("\t\t" ++)
                                                   (map ("global " ++) globals ++
                                                    map show stmts)) ++ 
                                              "\t}"

instance Show Expr where
    show (Single t) = show t
    show (Plus t1 t2) = show t1 ++ " + " ++ show t2

sepShow [] = ""
sepShow [x] = x
sepShow (x:y:xs) = x ++ ", " ++ sepShow (y:xs)

showSource :: CodeBlock -> String
showSource = unlines . map show

getMethodName (MethodDecl n _ _ _) = n
