module Language.RuScript.AST where

type Name = String
type Binding = (Name, Type)

-- Type
data Type = TyInt | TyBool | TyStr | TyClass Name deriving (Show, Eq, Ord)

-- Basic Block
data Block = Branch Expr [Statement] [Statement]
           | Loop Expr [Statement]
           deriving (Show, Eq)

-- Expression
data Expr = EVar Name
          | EGet Name Name
          | EInvoke Name Name [Expr]
          | ECall Name [Expr]
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
               | SAssign LHS Expr
               | SBlock Block
               | SInvoke Name Name [Expr]
               | SCall Name [Expr]
               | SReturn Expr
               | SBreak
               deriving (Show, Eq)

-- Left hand side
data LHS = LVar  Name
         | LAttr Name Name
         deriving (Show, Eq)

-- Top-level construct
data FnSig = FnSig Name [Binding] (Maybe Type) deriving (Show, Eq)

data Declaration = FnDecl FnSig [Statement]
                 | ClassDecl Name (Maybe Name) [(Visibility, Attr)] [(Visibility, Method)]
                 deriving (Show, Eq)

data Visibility = Public | Private deriving (Show, Eq)

type Attr = Binding

data Method = Virtual  FnSig
            | Concrete FnSig [Statement]
            deriving (Show, Eq)

data Program = Program [Either Statement Declaration] deriving (Show, Eq)
