{-# LANGUAGE FlexibleInstances #-}

module Language.RuScript.AST where

type Name = String

data Qualified a = Qualified [String] a deriving (Eq, Ord)

type Binding = (Name, Type)

-- Type
data Type = TyNil
          | TyInt
          | TyBool
          | TyStr
          | TyList Type
          | TyClass (Qualified Name)
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
          | ECall (Qualified Name) [Expr]
          | ENew (Qualified Name) [Expr]
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
               | SCall (Qualified Name) [Expr]
               | SReturn Expr
               | SBreak
               deriving (Show, Eq)

-- Left hand side
data LHS = -- Variable, e.g. x
           LVar  Name
           -- Setter e.g. x.y
         | LAttr Name Name
         deriving (Show, Eq)

-- Top-level construct
data FnSig = FnSig (Qualified Name) [Binding] (Maybe Type) deriving (Show, Eq)

data Declaration = -- Function declaration
                   FnDecl FnSig [Statement]
                   -- Class declaration
                 | ClassDecl (Qualified Name) (Maybe (Qualified Name)) [(Visibility, Attr)] [(Visibility, Method)]
                   -- e.g. after `import std`, the class `std.pair` can be used
                 | ImportDecl [String]
                   deriving (Show, Eq)

data Visibility = Public | Private deriving (Show, Eq)

type Attr = Binding

data Method = Virtual  FnSig
            | Concrete FnSig [Statement]
            deriving (Show, Eq)

data Program = Program [Either Statement Declaration] deriving (Show, Eq)


instance Show (Qualified String) where
  show (Qualified [] x) = x
  show (Qualified ss x) = splitByDot ss ++ "." ++ x
    where
      splitByDot []     = error "unexpected"
      splitByDot [a]    = a
      splitByDot (a:as) = a ++ "." ++ splitByDot as


