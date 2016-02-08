{-# LANGUAGE TemplateHaskell, RankNTypes #-}
-- Construct traversal utilities by Multiplate

module Language.RuScript.Traversal where

import MultiplateDerive
import Language.RuScript.AST

import Data.Generics.Multiplate
import Data.Functor.Constant
import Data.Functor.Identity


temPlate "Plate" [ "Qualified", "Type", "Block", "Expr", "Literal", "Statement", "LHS", "FnSig" ]

mapper :: Plate Identity -> (forall f. Plate f -> a -> f a) -> a -> a
mapper plate selector = traverseFor selector (mapFamily plate)
