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

newtype Kind = Star Int
  deriving (Eq, Show)

data Scheme = Forall
  { schTypeVars :: [(Ident, Kind)],
    schClassConstraints :: [(Ident, Type)], -- Class name, type var
    schBody :: Type
  }
  deriving (Show)

data Rec = Rec | NoRec
  deriving (Eq, Show)

data Expr
  = EVar Ident
  | ELam [Ident] Expr
  | EApp Expr Expr
  | ELet Rec Ident (Maybe Scheme) Expr Expr
  | EInt Integer
  | EStr String
  | ECon Ident
  | EIf Expr Expr Expr
  | EBin BinOp Expr Expr
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
  | TData Ident Stage [Constr]
  | TClass Ident Class
  | TInst Ident Type [(Ident, Expr)]
  deriving (Show)

type Prog = [Top]