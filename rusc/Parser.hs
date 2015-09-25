module Parser where

type Source = [Statement]

data Statement = Assignment String Expr
               | ClassDecl String [String] [MethodDecl]


data MethodDecl = MethodDecl String [String] [String] [Statement] Expr

data Expr = Plus Term Term
          | New String

data Term = Var String
          | LitInt Int

