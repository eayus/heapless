-- Reify (read back) semantic core values into core terms.
module Core.Norm.Reify where

import Core.Norm.Eval
import Core.Norm.Value qualified as V
import Core.Term qualified as T
import Core.Norm.Term as N
import Data.Function (fix)

partialEval :: T.Expr -> N.Nf
partialEval t = reifyNf 0 (evalExpr [] [] t) V.mainType

reifyNf :: Int -> V.Expr -> V.Type -> N.Nf
reifyNf len t = \case
  V.TFunc _ a b -> N.ELam (reifyType' a) $ reifyNf (len + 1) (app a b t $ V.EVar len) b
  V.TForall {} -> undefined
  V.TVar {} -> undefined
  _ -> N.ENeu $ reifyExpr len t

reifyExpr :: Int -> V.Expr -> N.Ne
reifyExpr len =
  let re = reifyExpr len
      reNf = reifyNf len
      tre = reifyType'
      frc f = reifyExpr (len + 1) $ f $ V.EVar len
      frcNf f = reifyNf (len + 1) $ f $ V.EVar len
      --   tfrc f = reifyExpr (tlen + 1) len $ f $ V.TVar len
      frcPair f = reifyExpr (len + 2) $ f (V.EVar len, V.EVar (len + 1))
   in \case
        V.EVar l -> N.EVar l
        V.EPrim t -> N.EPrim (fmap re t)
        V.EApp a b t u -> N.EApp (tre a) (tre b) (re t) (reNf u a)
        V.ELetRec a f g -> N.ELetRec (tre a) (frcNf f a) (frc g)
        V.ELetPair q a b t u -> N.ELetPair q (tre a) (tre b) (re t) (frcPair u)
        V.EPair t u -> N.EPair (re t) (re u)
        V.EIf t u v -> N.EIf (re t) (re u) (re v)
        V.ELam {} -> undefined -- V.ELam a f -> T.ELam (tre a) (frc f)
        V.ELet {} -> undefined
        V.ETyLam {} -> undefined -- V.ETyLam f -> T.ETyLam (tfrc f)
        V.ETyApp {} -> undefined -- V.ETyApp t a -> T.ETyApp (re t) (tre a)

-- Reification for partial evaluation.
reifyType' :: V.Type -> N.Type
reifyType' = fix $ \re -> \case
  V.TPrim p -> N.TPrim p
  V.TFunc q a b -> N.TFunc q (re a) (re b)
  V.TProd q a p b -> N.TProd q (re a) p (re b)
  V.TVar {} -> undefined
  V.TForall {} -> undefined

-- Reification for type checking
reifyType :: Int -> V.Type -> T.Type
reifyType len = fix $ \re -> \case
  V.TVar l -> T.TVar l
  V.TPrim p -> T.TPrim p
  V.TFunc q a b -> T.TFunc q (re a) (re b)
  V.TProd q a p b -> T.TProd q (re a) p (re b)
  V.TForall k f -> T.TForall k $ reifyType (len + 1) $ f $ V.TVar len