-- Type-annotated AST for the core language. It is the result of elaboration.
module Core.Term where

import Core.Syntax (Kind, Mult)

data IntSize
  = I64
  | I32
  | I16
  | I8
  deriving (Bounded, Enum, Eq, Show)

-- Builtin primitive types.
data PrimType
  = TInt IntSize
  | TBool
  | TWorld
  | TUnit
  | TOrd Int
  deriving (Eq, Show)

-- Types.
data Type
  = TVar Int -- Type variables as a de Bruijn level.
  | TPrim PrimType -- Primitive types.
  | TFunc Mult Type Type -- Function types.
  | TProd Mult Type Mult Type -- Product types, each side with its multiplicity.
  | TForall Kind Type -- Universal quantification.
  | TLam Type
  | TApp Type Type
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
  | ETyLet Kind Type Expr -- Type alias.
  | ELet Mult Type Expr Expr -- Let biniding (non-recursive).
  | ELetRec Type Expr Expr Expr -- Recursive let binding. First expression is a proof of O2.
  | ELetPair Mult Type Type Expr Expr -- Pair destructuring via let binding (non-recursive).
  | EPair Expr Expr -- Product constructor.
  | EIf Expr Expr Expr -- If expression.
  deriving (Show)