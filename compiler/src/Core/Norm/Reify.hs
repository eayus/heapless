-- Reify (read back) semantic core values into core terms.
module Core.Norm.Reify where

import Core.Norm.Value qualified as V
import Core.Term qualified as T
import Data.Function (fix)

-- TODO: eta expansion.

reifyExpr :: Int -> Int -> V.Expr -> T.Expr
reifyExpr tlen len =
  let re = reifyExpr tlen len
      tre = reifyType tlen
      frc f = reifyExpr tlen (len + 1) $ f $ V.EVar len
      --   tfrc f = reifyExpr (tlen + 1) len $ f $ V.TVar len
      frcPair f = reifyExpr tlen (len + 2) $ f (V.EVar len, V.EVar (len + 1))
   in \case
        V.EVar l -> T.EVar l
        V.EPrim t -> T.EPrim (fmap re t)
        V.ELam a f -> T.ELam (tre a) (frc f)
        V.EApp t u -> T.EApp (re t) (re u)
        V.ELetRec a f g -> T.ELetRec (tre a) (frc f) (frc g)
        V.ELetPair q a t u -> T.ELetPair q (tre a) (re t) (frcPair u)
        V.EPair t u -> T.EPair (re t) (re u)
        V.EIf t u v -> T.EIf (re t) (re u) (re v)
        V.ELet {} -> undefined
        V.ETyLam {} -> undefined
        V.ETyApp {} -> undefined

-- V.ETyLam f -> T.ETyLam (tfrc f)
-- V.ETyApp t a -> T.ETyApp (re t) (tre a)

reifyType :: Int -> V.Type -> T.Type
reifyType len = fix $ \re -> \case
  V.TVar l -> T.TVar l
  V.TPrim p -> T.TPrim p
  V.TFunc q a b -> T.TFunc q (re a) (re b)
  V.TProd q a p b -> T.TProd q (re a) p (re b)
  V.TForall k f -> T.TForall k $ reifyType (len + 1) $ f $ V.TVar len