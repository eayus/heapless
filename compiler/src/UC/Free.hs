module UC.Free where

import Data.Foldable
import Data.HashSet qualified as S
import UC.Term

freesNf :: Nf Name Name -> S.HashSet Name
freesNf = \case
  ENeu t -> freesNe t
  ELam ps t -> foldr (\(x, _) -> S.delete x) (freesNe t) ps

freesNe :: Ne Name Name -> S.HashSet Name
freesNe = \case
  EVar x -> S.singleton x
  EPrim p -> S.unions $ map freesNe $ toList p
  EApp _ _ t us -> S.unions $ freesNe t : map freesNf us
  ELetRec x _ t u -> S.delete x $ freesNf t `S.union` freesNe u
  ELetPair x y _ _ t u -> freesNe t `S.union` S.delete x (S.delete y $ freesNe u)
  EPair t u -> freesNe t `S.union` freesNe u
  EIf t u v -> S.unions [freesNe t, freesNe u, freesNe v]