Extraction Language Haskell.

Require Import Coq.Unicode.Utf8.
(* Require Import ZArith_base. *)

Inductive ByteCode (z: Set) :=
| PUSH : z → ByteCode
| POP : z → ByteCode
.

Extraction "Optimize'.hs" ByteCode.

