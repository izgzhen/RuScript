module Language.RuScript.AST where

type Name = String
type Binding = (Name, Type)

-- Type
data Type = TyInt | TyBool | TyStr | TyClass Name deriving (Show, Eq)

-- Basic Block
data Block = Branch Expr [Statement] [Statement]
           | Loop Expr [Statement]
           deriving (Show, Eq)

-- Expression
data Expr = EVar Name
          | EGet Name Name
          | EInvoke Name Name [Expr]
          | ENew Name [Expr]
          | ELit Literal
          deriving (Show, Eq)

-- Literal Value
data Literal = LStr String
             | LBool Bool
             | LInt Int
             deriving (Show, Eq)

-- Linear statement
data Statement = SVar Binding (Maybe Expr)
               | SAssign Name Expr
               | SBBlock Block
               | SReturn
               | SBreak
               deriving (Show, Eq)

-- Top-level construct
data FnSig = FnSig Name [Binding] deriving (Show, Eq)

data Declaration = FnDecl FnSig [Statement]
                 | ClassDecl Name (Maybe Name) [(Visibility, Attr)] [(Visibility, Method)]
                 deriving (Show, Eq)

data Visibility = Public | Private deriving (Show, Eq)

type Attr = Binding

data Method = Virtual  FnSig
            | Concrete FnSig [Statement]
            deriving (Show, Eq)

data Program = Program [Either Statement Declaration] deriving (Show, Eq)
