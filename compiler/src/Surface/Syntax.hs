module Surface.Syntax where

-- Terms are indexed by whether they are either:
--   a) Syntactic terms, produced by the parser.
--   b) Typed terms which contain meta variables, produced by the typechecker.
--   b) Fully typed terms, after meta variables have been resolved.
data Ano
  = Syn
  | Met
  | Typ

-- Meta Variables

data Meta :: Ano -> * where
  Meta :: Ident -> Meta Met

deriving instance Eq (Meta ano)

deriving instance Show (Meta ano)

-- Kinds

data Kind
  = Star Int
  | KFunc Kind Kind
  deriving (Eq, Show)

-- Types

data Type (ano :: Ano)
  = TVar Ident
  | TArr (Type ano) (Type ano)
  | TApp (Type ano) (Type ano)
  | TMeta (Meta ano)
  deriving (Eq, Show)

data Scheme ano = Forall
  { typeVars :: [(Ident, Kind)],
    instances :: [InstanceSig ano], -- Class name, type vars
    schBody :: Type ano
  }
  deriving (Show)

data Sig ano = Sig {var :: Ident, scheme :: Scheme ano}
  deriving (Show)

-- Patterns and Clauses

data Pat = Pat
  { con :: Ident,
    args :: [Ident]
  }
  deriving (Show)

data Clause ano = Clause
  { pat :: Pat,
    rhs :: Expr ano
  }
  deriving (Show)

data PatternSig = PatternSig
  { conName :: Ident,
    params :: [Type Met],
    retType :: Ident
  }

-- Expressions

data Expr ano
  = EVar Ident
  | ELam [Ident] (Expr ano)
  | EApp (Expr ano) (Expr ano)
  | ELet Rec Ident (Maybe (Scheme ano)) (Expr ano) (Expr ano)
  | EInt Integer
  | EStr String
  | EChar Char
  | EIf (Expr ano) (Expr ano) (Expr ano)
  | EBin BinOp (Expr ano) (Expr ano)
  | EDo [(Ident, Expr ano)] (Expr ano)
  | EFold (Expr ano) [(Pat, Expr ano)]
  | ECase (Expr ano) [Clause ano]
  deriving (Show)

-- Typeclasses

data InstanceSig ano = InstanceSig
  { className :: Ident,
    args :: [Type ano]
  }
  deriving (Eq, Show)

-- Rename to ClassDef?
data Class ano = Class
  { tvars :: [(Ident, Kind)],
    sigs :: [Sig ano]
  }
  deriving (Show)

data Instance ano = Instance
  { sig :: InstanceSig ano,
    defs :: [(Ident, Expr ano)]
  }
  deriving (Show)

-- Data Types

data DataDef ano = DataDef
  { name :: Ident,
    recursive :: Rec,
    stage :: Stage,
    typeVars :: [(Ident, Kind)],
    constructors :: [Constr ano]
  }
  deriving (Show)

data Constr ano = Constr Ident [Type ano]
  deriving (Show)

data Rec = Rec | NoRec
  deriving (Eq, Show)

data Stage = RT | CT
  deriving (Show)

-- Programs

data Top ano
  = TLet Rec Ident (Scheme ano) (Expr ano)
  | TData (DataDef ano)
  | TClass Ident (Class ano)
  | TInst (Instance ano)
  | TInclude FilePath
  deriving (Show)

type Prog ano = [Top ano]

-- Contexts

data Ctxt = Ctxt
  { vars :: [(Ident, Scheme Met)],
    typeVars :: [(Ident, Kind)],
    classDefs :: [(Ident, Class Met)],
    instances :: [InstanceSig Met],
    patterns :: [PatternSig]
  }

-- Constraints

data CTypeEquality = CTypeEquality
  { lhs :: Type Met,
    rhs :: Type Met
  }

data CTypeKind = CTypeKind
  { ctxt :: Ctxt,
    ty :: Type Met,
    kind :: Kind
  }

data CInstance = CInstance
  { ctxt :: Ctxt,
    sig :: InstanceSig Met
  }

-- Utilities

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

-- pattern TInt :: Type
-- pattern TInt = TVar "Int"

-- pattern TStr :: Type
-- pattern TStr = TVar "Str"

-- pattern TChar :: Type
-- pattern TChar = TVar "Char"

-- pattern TBool :: Type
-- pattern TBool = TVar "Bool"

-- pattern Mono :: Type -> Scheme
-- pattern Mono a = Forall [] [] a
