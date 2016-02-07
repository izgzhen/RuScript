module Language.RuScript.AST where

type Name = String
type Binding = (Name, Type)

-- Type
data Type = TyNil
          | TyInt
          | TyBool
          | TyStr
          | TyList Type
          | TyClass Name
          | TyBot -- undetermined type
          deriving (Show, Eq, Ord)

-- Basic Block
data Block = Branch Expr [Statement] [Statement]
           | Loop Expr [Statement]
           deriving (Show, Eq)

-- Expression
data Expr = EVar Name
          | EGet Name Name
          | EInvoke Expr Name [Expr]
          | ECall Name [Expr]
          | ENew Name [Expr]
          | ELit Literal
          | ETerm Term
          deriving (Show, Eq)

-- Term. It should be desugared into invoke
data Term = TPlus Expr Expr
          | TLE Expr Expr
          deriving (Show, Eq)

-- Literal Value Constructor
data Literal = LStr String
             | LBool Bool
             | LInt Int
             | LList
             | LNil
             deriving (Show, Eq)

-- Linear statement
data Statement = SVar Binding (Maybe Expr)
               | SAssign LHS Expr
               | SBlock Block
               | SInvoke Expr Name [Expr]
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
