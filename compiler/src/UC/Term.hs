module UC.Term where

import Core.Syntax (Mult)
import Core.Term (Prim, PrimType)

type Name = String

-- Types.
data Type
  = TPrim PrimType -- Primitive types.
  | TFunc [(Mult, Type)] Type -- Function types.
  | TProd Mult Type Mult Type -- Product types, each side with its multiplicity.
  deriving (Eq, Show)

-- Expressions.
data Nf b v
  = ENeu (Ne b v)
  | ELam [(b, Type)] (Ne b v)
  deriving (Show)

data Ne b v
  = EVar v -- Variables as a de Bruijn level.
  | EPrim (Prim (Ne b v)) -- A primitive operation (arithmetic, constants, primitive IO, etc.)
  | EApp [Type] Type (Ne b v) [Nf b v] -- Function application.
  | ELetRec b Type (Nf b v) (Ne b v) -- Recursive let binding.
  | ELetPair b b Mult Type Type (Ne b v) (Ne b v) -- Pair destructuring via let binding (non-recursive).
  | EPair (Ne b v) (Ne b v) -- Product constructor.
  | EIf (Ne b v) (Ne b v) (Ne b v) -- If expression.
  deriving (Show)