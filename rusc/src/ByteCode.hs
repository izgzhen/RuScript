module ByteCode (
  ByteCode(..)
, Label(LabelRel)
, initLabel
, genLabel
, Pos
, Address
) where

data ByteCode = CALL Int
              | INVOKE String
              | RET
              -- * Address should be `Pos` after pass one
              | JUMP Address
              | JUMPT Address
              | JUMPF Address
              -- *
              | PUSH Int
              | POP Int
              | NEW Int
              | PUSHA String
              | POPA String
              | PUSHSTR String
              | PUSHINT Int
              | PUSHBOOL Int
              | CLASS Int Int Int
              | SFUNC
              | EBODY Int
              deriving (Show, Eq)


-- Label

data Label = Label Int          -- Label no.
           | LabelRel Label Int -- Relative address to a label
           deriving (Show, Eq)

initLabel :: Label
initLabel = Label 0

genLabel :: Label -> (Label, Label)
genLabel (Label i) = (Label i, Label $ i + 1)

-- Position in the bytecode array
type Pos            = Int

-- Abstract address is either a absolute position, or a label
type Address        = Either Pos Label

