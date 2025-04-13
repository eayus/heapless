module Surface.Alpha where

import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Surface.Syntax

type Subst = M.HashMap Ident Type

typeFrees :: Type -> S.HashSet Ident
typeFrees = \case
  TVar x -> S.singleton x
  TArr a b -> typeFrees a `S.union` typeFrees b
  TMeta _ -> S.empty
  TCon _ -> S.empty

substVar :: Subst -> Type -> Type
substVar sub = \case
  TVar x -> case M.lookup x sub of
    Just a -> a
    Nothing -> TVar x
  TArr a b -> TArr (substVar sub a) (substVar sub b)
  TMeta n -> TMeta n
  TCon x -> TCon x

substMeta :: Subst -> Type -> Type
substMeta sub = \case
  TMeta x -> case M.lookup x sub of
    Just a -> a
    Nothing -> TMeta x
  TArr a b -> TArr (substMeta sub a) (substMeta sub b)
  TVar x -> TVar x
  TCon x -> TCon x

metaFrees :: Type -> S.HashSet Ident
metaFrees = \case
  TVar _ -> S.empty
  TArr a b -> metaFrees a `S.union` metaFrees b
  TMeta x -> S.singleton x
  TCon _ -> S.empty