-- Evaluate core terms into core values.
module Core.Norm.Eval where

import Core.Norm.Value qualified as V
import Core.Term qualified as T
import Data.Function (fix)

evalExpr :: [V.Type] -> [V.Expr] -> T.Expr -> V.Expr
evalExpr tenv env =
  let tev = evalType tenv
      ev = evalExpr tenv env
      sus t u = evalExpr tenv (u : env) t
      tsus t a = evalExpr (a : tenv) env t
      susPair t (v, w) = evalExpr tenv (w : v : env) t
   in \case
        T.EVar l -> level l env
        T.EPrim t -> V.EPrim (fmap ev t)
        T.ELam a t -> V.ELam (tev a) (sus t)
        T.EApp t u -> app (ev t) (ev u)
        T.ETyLam t -> V.ETyLam (tsus t)
        T.ETyApp t a -> tyApp (ev t) (tev a)
        T.ELet _ _ t u -> sus u (ev t)
        T.ELetRec a t u -> V.ELetRec (tev a) (sus t) (sus u)
        T.ELetPair q a t u -> V.ELetPair q (tev a) (ev t) (susPair u)
        T.EPair t u -> V.EPair (ev t) (ev u)
        T.EIf t u v -> V.EIf (ev t) (ev u) (ev v)

evalType :: [V.Type] -> T.Type -> V.Type
evalType env = fix $ \ev -> \case
  T.TVar l -> level l env
  T.TPrim p -> V.TPrim p
  T.TFunc q a b -> V.TFunc q (ev a) (ev b)
  T.TProd q a p b -> V.TProd q (ev a) p (ev b)
  T.TForall k a -> V.TForall k $ \b -> evalType (b : env) a

app :: V.Expr -> V.Expr -> V.Expr
app = \case
  V.ELam _ f -> f
  V.EIf t u v -> \rhs -> V.EIf t (app u rhs) (app v rhs)
  V.ELet q a t u -> \rhs -> V.ELet q a t $ \arg -> app (u arg) rhs
  V.ELetRec a t u -> \rhs -> V.ELetRec a t $ \arg -> app (u arg) rhs
  V.ELetPair q a t u -> \rhs -> V.ELetPair q a t $ \arg -> app (u arg) rhs
  lhs -> V.EApp lhs

tyApp :: V.Expr -> V.Type -> V.Expr
tyApp = \case
  V.ETyLam f -> f
  V.EIf t u v -> \rhs -> V.EIf t (tyApp u rhs) (tyApp v rhs)
  V.ELet q a t u -> \rhs -> V.ELet q a t $ \arg -> tyApp (u arg) rhs
  V.ELetRec a t u -> \rhs -> V.ELetRec a t $ \arg -> tyApp (u arg) rhs
  V.ELetPair q a t u -> \rhs -> V.ELetPair q a t $ \arg -> tyApp (u arg) rhs
  lhs -> V.ETyApp lhs

level :: Int -> [a] -> a
level l xs = reverse xs !! l