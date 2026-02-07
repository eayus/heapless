module UC.LambdaLift where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.HashMap.Strict qualified as M
import Data.List (singleton)
import Data.Maybe (fromJust)
import LL.Term qualified as L
import UC.Free
import UC.Term qualified as U

-- First reader is scope, second reader is function names mapped to extra params.
type Lifter = ReaderT [(U.Name, U.Type)] (ReaderT [(U.Name, [U.Name])] (StateT [U.Name] (Writer [L.Func])))

llMain :: U.Nf U.Name U.Name -> L.Prog
llMain = \case
  U.ELam ps t -> do
    let lamNames = map (("lam" ++) . show) [0 ..]
    let (t', fs) = runWriter $ flip evalStateT lamNames $ flip runReaderT [] $ flip runReaderT ps $ ll t
    L.Prog fs t'
  _ -> undefined

llNf :: U.Nf U.Name U.Name -> Lifter L.Expr
llNf = \case
  U.ENeu t -> ll t
  U.ELam {} -> undefined -- lambdas cannot be arguments at this point.

ll :: U.Ne U.Name U.Name -> Lifter L.Expr
ll = \case
  U.EVar x -> pure $ L.EVar x
  U.EPrim p -> L.EPrim <$> mapM ll p
  U.EApp as b t us ->
    case t of
      U.EVar v -> do
        us' <- mapM llNf us
        extraMap <- lift ask
        let extras = fromJust $ lookup v extraMap
        pure $ L.EApp as b (L.EVar v) (map L.EVar extras ++ us')
      _ -> undefined
  U.ELetRec x a t u -> case t of
    U.ELam as body -> do
      let ret = retType a
      captured <- M.toList . M.delete x <$> local ((x, a) :) (freesNf t)
      mapReaderT (local ((x, map fst captured) :)) $ local ((x, a) :) $ do
        body' <- ll body
        tell $ singleton $ L.LetRec x (captured ++ as) ret body'
        ll u
    U.ENeu {} -> undefined
  U.ELetPair x y q a b t u -> local ([(x, a), (y, b)] ++) $ liftA2 (L.ELetPair x y q a b) (ll t) (ll u)
  U.EPair t u -> liftA2 L.EPair (ll t) (ll u)
  U.EIf t u v -> liftA3 L.EIf (ll t) (ll u) (ll v)

-- TODO: This code is a little duplicated in UC.Name too.
fresh :: Lifter U.Name
fresh = get >>= \case
  x : xs -> x <$ put xs
  [] -> error "ran out of names!"

retType :: U.Type -> U.Type
retType = \case
  U.TFunc _ ret -> ret
  _ -> error "expected function type"
