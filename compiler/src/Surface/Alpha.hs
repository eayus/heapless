module Surface.Alpha where

import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Surface.Syntax

type Subst = M.HashMap Ident Type

-- To avoid so much duplication, perhaps we should have a single constructor
-- for variables of all kinds (type, meta, etc.).
-- i.e.
--   | EVar Variable
--
--   data Variable
--     = TVar ..
--     | TMeta ..

typeFrees :: Type -> S.HashSet Ident
typeFrees = \case
  TVar x -> S.singleton x
  TArr a b -> typeFrees a `S.union` typeFrees b
  TMeta _ -> S.empty
  TApp a b -> typeFrees a `S.union` typeFrees b

substVar :: Subst -> Type -> Type
substVar sub = \case
  TVar x -> case M.lookup x sub of
    Just a -> a
    Nothing -> TVar x
  TArr a b -> TArr (substVar sub a) (substVar sub b)
  TApp a b -> TApp (substVar sub a) (substVar sub b)
  TMeta n -> TMeta n

substMeta :: Subst -> Type -> Type
substMeta sub = \case
  TMeta x -> case M.lookup x sub of
    Just a -> a
    Nothing -> TMeta x
  TArr a b -> TArr (substMeta sub a) (substMeta sub b)
  TApp a b -> TApp (substMeta sub a) (substMeta sub b)
  TVar x -> TVar x
substSchemeMeta :: Subst -> Scheme -> Scheme
substSchemeMeta sub (Forall xs cs a) = Forall xs cs $ substMeta sub a

metaFrees :: Type -> S.HashSet Ident
metaFrees = \case
  TVar _ -> S.empty
  TArr a b -> metaFrees a `S.union` metaFrees b
  TApp a b -> metaFrees a `S.union` metaFrees b
  TMeta x -> S.singleton x

-- For substituting metas
class TypeVars a where
  substTypeVars :: Subst -> a -> a

instance TypeVars Type where
  substTypeVars = substVar

instance TypeVars InstanceSig where
  substTypeVars sub (InstanceSig c as) = InstanceSig c $ map (substTypeVars sub) as

-- TODO: add sanity checks to prevent capturing
instance TypeVars Scheme where
  substTypeVars sub (Forall as cs a) = Forall as (substTypeVars sub cs) (substTypeVars sub a)

instance TypeVars Sig where
  substTypeVars sub (Sig x a) = Sig x (substTypeVars sub a)

instance (TypeVars a) => TypeVars [a] where
  substTypeVars sub = map (substTypeVars sub)

class Metas a where
  substMetas :: Subst -> a -> a

instance Metas Type where
  substMetas = substMeta

instance (Metas a) => Metas [a] where
  substMetas sub = map (substMetas sub)

instance Metas InstanceSig where
  substMetas sub (InstanceSig c as) = InstanceSig c (map (substMetas sub) as)

instance Metas CTypeKind where
  substMetas sub (CTypeKind ctxt a k) = CTypeKind ctxt (substMetas sub a) k

instance Metas CInstance where
  substMetas sub (CInstance ctxt sig) = CInstance ctxt (substMetas sub sig)

(@@) :: Subst -> Subst -> Subst
s0 @@ s1 = M.map (substMeta s0) s1 `M.union` s0