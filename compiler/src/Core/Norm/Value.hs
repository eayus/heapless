-- Semantic values for the core language. The types are almost identical to the type annotated
-- terms, except that closures are used when expressions are bound.
module Core.Norm.Value where

import Core.Syntax (Kind, Mult (..))
import Core.Term (Prim, PrimType (..))

-- Types.
data Type
  = TVar Int -- Type variables as a de Bruijn level.
  | TPrim PrimType -- Primitive types.
  | TFunc Mult Type Type -- Function types.
  | TProd Mult Type Mult Type -- Product types, each side with its multiplicity.
  | TForall Kind (Type -> Type) -- Universal quantification.

-- Expressions.
data Expr
  = EVar Int -- Variables as a de Bruijn level.
  | EPrim (Prim Expr) -- A primitive operation (arithmetic, constants, primitive IO, etc.)
  | ELam Type (Expr -> Expr) -- Lambda abstractions.
  | EApp Type Type Expr Expr -- Function application. A, B, Expr (A -> B), Expr A.
  | ETyLam (Type -> Expr) -- Type abstraction (capital lambda).
  | ETyApp Expr Type -- Type application (instantiation).
  | ELet Mult Type Expr (Expr -> Expr) -- Let biniding (non-recursive).
  | ELetRec Type (Expr -> Expr) (Expr -> Expr) -- Recursive let binding.
  | ELetPair Mult Type Expr ((Expr, Expr) -> Expr) -- Pair destructuring via let binding (non-recursive).
  | EPair Expr Expr -- Product constructor.
  | EIf Expr Expr Expr -- If expression.

mainType :: Type
mainType = TFunc One (TPrim TWorld) (TPrim TWorld)