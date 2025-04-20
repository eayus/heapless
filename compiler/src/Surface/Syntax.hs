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
  | BAnd
  | BOr
  | BEq
  deriving (Show)

data Type
  = TVar Ident -- Lowercase type variable
  | TCon Ident -- Uppercase type constructor
  | TArr Type Type -- Arrow type
  | TApp Type Type -- Type-level application (e.g. "m a")
  | TMeta Ident -- Meta variable, only used during type checking (user cannot create this)
  deriving (Eq, Show)

data Kind
  = Star Int
  | KFunc Kind Kind
  deriving (Eq, Show)

data Scheme = Forall
  { schTypeVars :: [(Ident, Kind)],
    schClassConstraints :: [(Ident, Type)], -- Class name, type var
    schBody :: Type
  }
  deriving (Show)

data Rec = Rec | NoRec
  deriving (Eq, Show)

data Pat = Pat
  { con :: Ident,
    args :: [Ident]
  }
  deriving (Show)

data Expr
  = EVar Ident
  | ELam [Ident] Expr
  | EApp Expr Expr
  | ELet Rec Ident (Maybe Scheme) Expr Expr
  | EInt Integer
  | EStr String
  | EChar Char
  | ECon Ident
  | EIf Expr Expr Expr
  | EBin BinOp Expr Expr
  | EDo [(Ident, Expr)] Expr
  | EFold Expr [(Pat, Expr)]
  deriving (Show)

data Constr = Constr Ident [Type]
  deriving (Show)

data Stage = RT | CT
  deriving (Show)

data Class = Class
  { tvar :: Ident,
    tvarKind :: Kind,
    sigs :: [(Ident, Scheme)]
  }
  deriving (Show)

data Top
  = TLet Rec Ident Scheme Expr
  | TData Rec Ident Stage [Constr]
  | TClass Ident Class
  | TInst Ident Type [(Ident, Expr)]
  deriving (Show)

type Prog = [Top]