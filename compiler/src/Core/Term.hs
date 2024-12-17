-- Type-annotated AST for the core language. It is the result of elaboration.
module Core.Term where

import Core.Syntax (Kind, Mult)

-- Builtin primitive types.
data PrimType
  = TInt
  | TBool
  | TWorld

-- Types.
data Type
  = TVar Int -- Type variables as a de Bruijn level.
  | TPrim PrimType -- Primitive types.
  | TFunc Mult Type Type -- Function types.
  | TProd Mult Type Mult Type -- Product types, each side with its multiplicity.
  | TForall Kind Type -- Universal quantification.

-- Primitive operations.
data Prim
  = PAdd Expr Expr
  | PSub Expr Expr
  | PReadInt
  | PPrintInt Expr
  | PBool Bool
  | PInt Integer

-- Expressions.
data Expr
  = EVar Int -- Variables as a de Bruijn level.
  | EPrim Prim -- A primitive operation (arithmetic, constants, primitive IO, etc.)
  | ELam Type Expr -- Lambda abstractions.
  | EApp Expr Expr -- Function application.
  | ETyLam Expr -- Type abstraction (capital lambda).
  | ETyApp Expr Type -- Type application (instantiation).
  | ELet Mult Type Expr Expr -- Let biniding (non-recursive).
  | ELetRec Type Expr Expr -- Recursive let binding.
  | ELetPair Mult Type Expr Expr -- Pair destructuring via let binding (non-recursive).
  | EPair Expr Expr -- Product constructor.
  | EIf Expr Expr Expr -- If expression.