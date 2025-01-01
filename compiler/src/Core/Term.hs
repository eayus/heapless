-- Type-annotated AST for the core language. It is the result of elaboration.
module Core.Term where

import Core.Syntax (Kind, Mult)

-- Builtin primitive types.
data PrimType
  = TInt
  | TBool
  | TWorld
  | TUnit
  deriving (Eq, Show)

-- Types.
data Type
  = TVar Int -- Type variables as a de Bruijn level.
  | TPrim PrimType -- Primitive types.
  | TFunc Mult Type Type -- Function types.
  | TProd Mult Type Mult Type -- Product types, each side with its multiplicity.
  | TForall Kind Type -- Universal quantification.
  deriving (Eq, Show)

-- Primitive operations.
data Prim e
  = PAdd e e
  | PSub e e
  | PEql e e
  | PReadInt e
  | PPrintInt e e
  | PBool Bool
  | PInt Integer
  | PUnit
  deriving (Functor, Foldable, Traversable, Show)

-- Expressions.
data Expr
  = EVar Int -- Variables as a de Bruijn level.
  | EPrim (Prim Expr) -- A primitive operation (arithmetic, constants, primitive IO, etc.)
  | ELam Type Expr -- Lambda abstractions.
  | EApp Type Type Expr Expr -- Function application. A, B, Expr (A -> B), Expr A.
  | ETyLam Expr -- Type abstraction (capital lambda).
  | ETyApp Expr Type -- Type application (instantiation).
  | ELet Mult Type Expr Expr -- Let biniding (non-recursive).
  | ELetRec Type Expr Expr -- Recursive let binding.
  | ELetPair Mult Type Type Expr Expr -- Pair destructuring via let binding (non-recursive).
  | EPair Expr Expr -- Product constructor.
  | EIf Expr Expr Expr -- If expression.
  deriving (Show)