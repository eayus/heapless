module UC.Free where

import Control.Applicative
import Control.Monad.Reader
import Data.Foldable
import Data.HashMap.Strict qualified as M
import UC.Term

type Frees = M.HashMap Name Type

freesNf :: (MonadReader [(Name, Type)] m) => Nf Name Name -> m Frees
freesNf = \case
  ENeu t -> freesNe t
  ELam ps t -> foldr (\(x, _) -> fmap $ M.delete x) (local (ps ++) $ freesNe t) ps

freesNe :: (MonadReader [(Name, Type)] m) => Ne Name Name -> m Frees
freesNe = \case
  EVar x -> do
    scope <- ask
    case lookup x scope of
      Nothing -> error $ "Undefined var in scope " ++ x ++ "\n" ++ show scope
      Just a -> pure $ M.singleton x a
  EPrim p -> M.unions <$> mapM freesNe (toList p)
  EApp _ _ t us -> do
    t' <- freesNe t
    us' <- mapM freesNf us
    pure $ M.unions $ t' : us'
  ELetRec x a t u -> do
    M.delete x <$> local ((x, a) :) (liftA2 M.union (freesNf t) (freesNe u))
  ELetPair x y _ a b t u -> do
    t' <- freesNe t
    u' <- local ([(x, a), (y, b)] ++) $ freesNe u
    pure $ t' `M.union` M.delete x (M.delete y u')
  EPair t u -> liftA2 M.union (freesNe t) (freesNe u)
  EIf t u v -> M.unions <$> mapM freesNe [t, u, v]