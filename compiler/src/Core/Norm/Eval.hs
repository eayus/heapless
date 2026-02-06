-- Evaluate core terms into core values.
module Core.Norm.Eval where

import Core.Norm.Value qualified as V
import Core.Syntax qualified as S
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
        T.EApp a b t u -> app (tev a) (tev b) (ev t) (ev u)
        T.ETyLam t -> V.ETyLam (tsus t)
        T.ETyApp t a -> tyApp (ev t) (tev a)
        T.ETyLet _ a t -> tsus t (tev a)
        T.ELet _ _ t u -> sus u (ev t)
        T.ELetRec a p t u -> V.ELetRec (tev a) (ev p) (sus t) (sus u)
        T.ELetPair q a b t u -> letPair q (tev a) (tev b) (ev t) (susPair u)
        T.EPair t u -> V.EPair (ev t) (ev u)
        T.EIf t u v -> V.EIf (ev t) (ev u) (ev v)

letPair :: S.Mult -> V.Type -> V.Type -> V.Expr -> ((V.Expr, V.Expr) -> V.Expr) -> V.Expr
letPair q a b = \case
  V.EPair t u -> \f -> f (t, u)
  t -> V.ELetPair q a b t

evalType :: [V.Type] -> T.Type -> V.Type
evalType env = fix $ \ev -> \case
  T.TVar l -> level l env
  T.TPrim p -> V.TPrim p
  T.TFunc q a b -> V.TFunc q (ev a) (ev b)
  T.TProd q a p b -> V.TProd q (ev a) p (ev b)
  T.TForall k a -> V.TForall k $ \b -> evalType (b : env) a
  T.TLam a -> V.TLam $ \b -> evalType (b : env) a
  T.TApp a b -> case ev a of
    V.TLam f -> f (ev b)
    a' -> V.TApp a' (ev b)

app :: V.Type -> V.Type -> V.Expr -> V.Expr -> V.Expr
app a b = \case
  V.ELam _ f -> f
  V.EIf t u v -> \rhs -> V.EIf t (app a b u rhs) (app a b v rhs)
  V.ELet q c t u -> \rhs -> V.ELet q c t $ \arg -> app a b (u arg) rhs
  V.ELetRec c p t u -> \rhs -> V.ELetRec c p t $ \arg -> app a b (u arg) rhs
  V.ELetPair q c d t u -> \rhs -> V.ELetPair q c d t $ \arg -> app a b (u arg) rhs
  lhs -> V.EApp a b lhs

tyApp :: V.Expr -> V.Type -> V.Expr
tyApp = \case
  V.ETyLam f -> f
  V.EIf t u v -> \rhs -> V.EIf t (tyApp u rhs) (tyApp v rhs)
  V.ELet q a t u -> \rhs -> V.ELet q a t $ \arg -> tyApp (u arg) rhs
  V.ELetRec a p t u -> \rhs -> V.ELetRec a p t $ \arg -> tyApp (u arg) rhs
  V.ELetPair q a b t u -> \rhs -> V.ELetPair q a b t $ \arg -> tyApp (u arg) rhs
  lhs -> V.ETyApp lhs

level :: Int -> [a] -> a
level l xs = reverse xs !! l