{-# LANGUAGE TemplateHaskell #-}
-- Construct traversal utilities by Multiplate

module Language.RuScript.Traversal where

import MultiplateDerive
import Language.RuScript.AST

import Data.Generics.Multiplate
import Data.Functor.Constant
import Data.Functor.Identity


temPlate "Plate" [ "Qualified", "Type", "Block", "Expr", "Literal", "Statement", "LHS" ]

mapper :: Plate Identity -> Statement -> Statement
mapper plate = traverseFor statement_ (mapFamily plate)
