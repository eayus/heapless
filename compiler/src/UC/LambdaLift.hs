module UC.LambdaLift where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import LL.Term qualified as L
import UC.Term qualified as U

type Lifter = StateT [U.Name] (Writer [L.Func])

llMain :: U.Nf U.Name U.Name -> L.Prog
llMain = \case
  U.ELam [_] t -> do
    let (t', fs) = runWriter $ ll t
    L.Prog fs t'
  _ -> undefined

llNf :: U.Nf U.Name U.Name -> Lifter L.Expr
llNf = \case
  U.ENeu t -> ll t
  u@(U.ELam ps t) -> _

ll :: U.Ne U.Name U.Name -> Lifter L.Expr
ll = \case
  U.EVar x -> pure $ L.EVar x
  U.EPrim p -> L.EPrim <$> mapM ll p
  U.EApp as b t us -> liftA2 (L.EApp as b) (ll t) (mapM llNf us)
  U.ELetRec x a t u -> case t of
    U.ELam as t' -> _
    U.ENeu {} -> undefined
  U.ELetPair x y q a t u -> liftA2 (L.ELetPair x y q a) (ll t) (ll u)
  U.EPair t u -> liftA2 L.EPair (ll t) (ll u)
  U.EIf t u v -> liftA3 L.EIf (ll t) (ll u) (ll v)

fresh :: Lifter Name
fresh = do
  xs <- get
  put (tail xs)
  pure (head xs)