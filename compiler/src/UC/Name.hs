module UC.Name where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List (singleton)
import UC.Term

type Namer = StateT [Name] (Reader [Name])

nameMain :: Nf () Int -> Nf Name Name
nameMain t = runReader (evalStateT (nameNf t) supply) []
  where
    supply = map singleton ['a' ..]

nameNf :: Nf () Int -> Namer (Nf Name Name)
nameNf = \case
  ENeu t -> ENeu <$> nameNe t
  ELam ps t -> do
    let as = map snd ps
    xs <- replicateM (length as) fresh
    let ps' = zip xs as
    ELam ps' <$> local (reverse xs ++) (nameNe t)

nameNe :: Ne () Int -> Namer (Ne Name Name)
nameNe = \case
  EVar x -> asks (EVar . (!! x) . reverse)
  EPrim p -> EPrim <$> mapM nameNe p
  EApp as b t us -> liftA2 (EApp as b) (nameNe t) (mapM nameNf us)
  ELetRec () a t u -> do
    x <- fresh
    t' <- local (x :) $ nameNf t
    u' <- local (x :) $ nameNe u
    pure $ ELetRec x a t' u'
  ELetPair () () q a t u -> do
    x <- fresh
    y <- fresh
    t' <- nameNe t
    u' <- local ([x, y] ++) $ nameNe u
    pure $ ELetPair x y q a t' u'
  EPair t u -> liftA2 EPair (nameNe t) (nameNe u)
  EIf t u v -> liftA3 EIf (nameNe t) (nameNe u) (nameNe v)

fresh :: Namer Name
fresh = do
  xs <- get
  put (tail xs)
  pure (head xs)