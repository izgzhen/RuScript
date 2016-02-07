module Language.RuScript.Desugar where

import Language.RuScript.AST

desugarTerm :: Term -> Expr
desugarTerm (TPlus e1 e2) = EInvoke e1 "add" [e2]
desugarTerm (TLE e1 e2)   = EInvoke e1 "le" [e2]
