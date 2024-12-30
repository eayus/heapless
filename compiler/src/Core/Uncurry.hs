module Core.Uncurry where

import Core.Norm.Term as C
import UC.Term as U

ucNf :: C.Nf -> U.Nf () Int
ucNf = \case
  C.ENeu t -> U.ENeu $ ucNe t
  C.ELam a t -> case ucNf t of
    U.ELam as t' -> U.ELam (((), ucType a) : as) t'
    U.ENeu t -> U.ELam [((), ucType a)] t

ucNe :: C.Ne -> U.Ne () Int
ucNe = \case
  C.EVar n -> U.EVar n
  C.EPrim p -> U.EPrim (fmap ucNe p)
  C.EApp a b t u -> case ucNe t of
    U.EApp as b' f args -> U.EApp (as ++ [ucType a]) b' f (args ++ [ucNf u])
    t' -> U.EApp [ucType a] (ucType b) t' [ucNf u]
  C.ELetRec a t u -> U.ELetRec () (ucType a) (ucNf t) (ucNe u)
  C.ELetPair p a b t u -> U.ELetPair () () p (ucType a) (ucType b) (ucNe t) (ucNe u)
  C.EPair t u -> U.EPair (ucNe t) (ucNe u)
  C.EIf t u v -> U.EIf (ucNe t) (ucNe u) (ucNe v)

ucType :: C.Type -> U.Type
ucType = \case
  C.TPrim a -> U.TPrim a
  C.TFunc q a b -> case ucType b of
    U.TFunc xs b' -> U.TFunc ((q, ucType a) : xs) b'
    b' -> U.TFunc [(q, ucType a)] b'
  C.TProd q a p b -> U.TProd q (ucType a) p (ucType b)