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
  | BBitOr
  | BShiftR
  deriving (Show)

data Type
  = TVar Ident -- Type variable or type constructor
  | TArr Type Type -- Arrow type
  | TApp Type Type -- Type-level application (e.g. "m a")
  | TMeta Ident -- Meta variable, only used during type checking (user cannot create this)
  deriving (Eq, Show)

data Sig = Sig {var :: Ident, scheme :: Scheme}
  deriving (Show)

pattern TInt :: Type
pattern TInt = TVar "Int"

pattern TStr :: Type
pattern TStr = TVar "Str"

pattern TChar :: Type
pattern TChar = TVar "Char"

pattern TBool :: Type
pattern TBool = TVar "Bool"

data Kind
  = Star Int
  | KFunc Kind Kind
  deriving (Eq, Show)

data InstanceSig = InstanceSig
  { className :: Ident,
    args :: [Type]
  }
  deriving (Eq, Show)

data Scheme = Forall
  { typeVars :: [(Ident, Kind)],
    instances :: [InstanceSig], -- Class name, type vars
    schBody :: Type
  }
  deriving (Show)

pattern Mono :: Type -> Scheme
pattern Mono a = Forall [] [] a

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
  | EIf Expr Expr Expr
  | EBin BinOp Expr Expr
  | EDo [(Ident, Expr)] Expr
  | EFold Expr [(Pat, Expr)]
  deriving (Show)

data Constr = Constr Ident [Type]
  deriving (Show)

data Stage = RT | CT
  deriving (Show)

-- Rename to ClassDef?
data Class = Class
  { tvars :: [(Ident, Kind)],
    sigs :: [Sig]
  }
  deriving (Show)

data Instance = Instance
  { sig :: InstanceSig,
    defs :: [(Ident, Expr)]
  }
  deriving (Show)

data Top
  = TLet Rec Ident Scheme Expr
  | TData DataDef
  | TClass Ident Class
  | TInst Instance
  | TInclude FilePath
  deriving (Show)

data DataDef = DataDef
  { name :: Ident,
    recursive :: Rec,
    stage :: Stage,
    typeVars :: [(Ident, Kind)],
    constructors :: [Constr]
  }
  deriving (Show)

type Prog = [Top]

-- Constraints

data Ctxt = Ctxt
  { vars :: [(Ident, Scheme)],
    typeVars :: [(Ident, Kind)],
    classDefs :: [(Ident, Class)],
    instances :: [InstanceSig]
  }

data CTypeEquality = CTypeEquality
  { lhs :: Type,
    rhs :: Type
  }

data CTypeKind = CTypeKind
  { ctxt :: Ctxt,
    ty :: Type,
    kind :: Kind
  }

data CInstance = CInstance
  { ctxt :: Ctxt,
    sig :: InstanceSig
  }