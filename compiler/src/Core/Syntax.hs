-- AST for the core language which is the direct result of parsing.
module Core.Syntax where

-- Identifiers.
type Ident = String

-- Multiplicity (used for linearity).
data Mult
  = One -- Linear.
  | Many -- Unrestricted.
  deriving (Eq, Show)

-- Kinds of types.
data Kind
  = KStar
  | KFunc Kind Kind
  deriving (Eq, Show)

-- Types.
data Type
  = TName Ident -- Type variables AND builtin types.
  | TFunc Mult Type Type -- Function types.
  | TProd Mult Type Mult Type -- Product types, each side with its multiplicity.
  | TForall Ident Kind Type -- Universal quantification.
  | TLam Ident Type
  | TApp Type Type
  deriving (Show)

-- Binary operators.
data BinOp
  = Add
  | Sub
  | Eql
  deriving (Show)

-- Expressions.
data Expr
  = EVar Ident -- Variables.
  | ELam Ident Expr -- Lambda abstractions.
  | EApp Expr Expr -- Function application.
  | ETyLam Ident Expr -- Type abstraction (capital lambda).
  | ETyApp Expr Type -- Type application (instantiation).
  | ETyLet Ident Kind Type Expr -- Type alias.
  | ELet Mult Ident (Maybe Type) Expr Expr -- Let biniding (non-recursive).
  | ELetRec Ident Type Expr Expr -- Recursive let binding.
  | ELetPair Mult (Ident, Ident) (Maybe Type) Expr Expr -- Pair destructuring via let binding (non-recursive).
  | EPair Expr Expr -- Product constructor.
  | EBin BinOp Expr Expr -- Binary expression (+, -, etc.).
  | EPrim Ident [Expr] -- Primitive functions (IO primitives, etc.)
  | EIf Expr Expr Expr -- If expression.
  | EBool Bool -- Boolean literal.
  | EInt Integer -- Integer literal.
  deriving (Show)