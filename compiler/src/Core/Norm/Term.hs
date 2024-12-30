-- Type-annotated normal forms for the core language. It is the result of partial
-- evaluation.
module Core.Norm.Term where

import Core.Syntax (Mult)
import Core.Term (Prim, PrimType)

-- Types.
data Type
  = TPrim PrimType -- Primitive types.
  | TFunc Mult Type Type -- Function types.
  | TProd Mult Type Mult Type -- Product types, each side with its multiplicity.
  deriving (Eq, Show)

-- Expressions.
data Nf
  = ENeu Ne
  | ELam Type Nf
  deriving (Show)

data Ne
  = EVar Int -- Variables as a de Bruijn level.
  | EPrim (Prim Ne) -- A primitive operation (arithmetic, constants, primitive IO, etc.)
  | EApp Type Type Ne Nf -- Function application. A, B, Expr (A -> B), Expr A.
  | ELetRec Type Nf Ne -- Recursive let binding.
  | ELetPair Mult Type Type Ne Ne -- Pair destructuring via let binding (non-recursive).
  | EPair Ne Ne -- Product constructor.
  | EIf Ne Ne Ne -- If expression.
  deriving (Show)