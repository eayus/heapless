module Surface.Syntax where

type Ident = String

data BinOp
  = BLT
  | BLTE
  | BGT
  | BGTE
  | BAdd
  | BSub
  | BMul
  deriving (Show)

data Type
  = TVar Ident -- Lowercase type variable
  | TCon Ident -- Uppercase type constructor
  | TArr Type Type -- Arrow type
  | TMeta Ident -- Meta variable, only used during type checking (user cannot create this)
  deriving (Eq, Show)

data Rec = Rec | NoRec
  deriving (Eq, Show)

data Expr
  = EVar Ident
  | ELam [Ident] Expr
  | EApp Expr Expr
  | ELet Rec Ident (Maybe Type) Expr Expr
  | EInt Integer
  | EIf Expr Expr Expr
  | EBin BinOp Expr Expr
  deriving (Show)

data Top
  = TLet Rec Ident Type Expr
  deriving (Show)

type Prog = [Top]