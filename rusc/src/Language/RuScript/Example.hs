module Language.RuScript.Example where

import Language.RuScript.Codegen
import Language.RuScript.AST

{-
    // Example 1

    var x : Int = 1
    var y : Int = 2
    if x.equal(y) {
        x = 3
    } else {
        x = 4
    }

    var z: Bool = True

    while z {
        x = x.add(1)
    }
-}

example1 :: Program
example1 = Program $ map Left stmts
    where
        stmts = [ SVar ("x", TyInt) (Just $ ELit (LInt 1))
                , SVar ("y", TyInt) (Just $ ELit (LInt 2))
                , SBBlock $
                    Branch
                        (EInvoke "x" "equal" [EVar "y"])
                        [SAssign "x" (ELit $ LInt 3)]
                        [SAssign "x" (ELit $ LInt 4)]
                , SVar ("z", TyBool) (Just $ ELit (LBool True))
                , SBBlock $
                    Loop
                        (EVar "z")
                        [SAssign "x" (EInvoke "x" "add" [ELit $ LInt 1])]
                ]

