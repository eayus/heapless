-- Type check syntax and elaborate to a typed core representation.
module Core.Check where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Core.Norm.Eval qualified as E
import Core.Norm.Reify qualified as R
import Core.Norm.Value qualified as V
import Core.Syntax qualified as S
import Core.Term qualified as T
import Data.HashMap.Strict qualified as M

data Ctxt = Ctxt
  { typeVars :: [(S.Ident, S.Kind)],
    exprVars :: [(S.Ident, V.Type)]
  }

newtype Uses = Uses (M.HashMap S.Ident S.Mult)

type TC = ExceptT String (WriterT Uses (Reader Ctxt))

addMult :: S.Mult -> S.Mult -> S.Mult
addMult _ _ = S.Many

mulMult :: S.Mult -> S.Mult -> S.Mult
mulMult S.One S.One = S.One
mulMult _ _ = S.Many

maxMult :: S.Mult -> S.Mult -> S.Mult
maxMult S.One S.One = S.One
maxMult _ _ = S.Many

instance Semigroup Uses where
  Uses x <> Uses y = Uses $ M.unionWith addMult x y

instance Monoid Uses where
  mempty = Uses M.empty

useVar :: S.Ident -> TC ()
useVar x = tell $ Uses $ M.singleton x S.One

-- TODO: Can we change the monad stack order to avoid rewrapping the ExceptT.
typecheck :: (MonadError String m) => S.Expr -> m T.Expr
typecheck t = do
  let x = runExceptT (checkExpr t mainType)
  let y = runWriterT x
  let (z, _) = runReader y initCtxt
  liftEither z

mainType :: V.Type
mainType = V.TFunc S.One (V.TPrim T.TWorld) (V.TPrim T.TWorld)

initCtxt :: Ctxt
initCtxt = Ctxt [] []

inferExpr :: S.Expr -> TC (V.Type, T.Expr)
inferExpr = \case
  S.EVar x -> do
    vars <- asks exprVars
    (a, l) <- lookupVar x vars
    useVar x
    pure (a, T.EVar l)
  S.ELam {} -> throwError "Can't infer the type for a lambda abstraction"
  S.EApp t u ->
    inferExpr t >>= \case
      (V.TFunc q a b, t') -> do
        u' <- multiplyUses q $ checkExpr u a
        pure (b, T.EApp t' u')
      _ -> throwError "Can't apply a term which is not a function"
  S.ETyLam {} -> throwError "Can't infer the type for a type abstraction"
  S.ETyApp t a ->
    inferExpr t >>= \case
      (V.TForall k f, t') -> do
        a' <- checkType a k
        va <- evalType a'
        pure (f va, T.ETyApp t' a')
      _ -> throwError "Can't type apply a which is not polymorphic"
  S.ELet q x a t u -> do
    a' <- mapM (fmap snd . inferType) a
    va <- mapM evalType a'
    (va', t') <- multiplyUses q $ tryCheckExpr t va
    (b, u') <- bindExpr x q va' $ inferExpr u
    a'' <- reifyType va'
    pure (b, T.ELet q a'' t' u')
  S.ELetRec x a t u -> do
    a' <- checkType a $ S.KStar 2
    va <- evalType a'
    bindExpr x S.Many va $ do
      t' <- checkExpr t va
      (b, u') <- inferExpr u
      pure (b, T.ELetRec a' t' u')
  S.ELetPair q (x, y) a t u -> do
    a' <- mapM (fmap snd . inferType) a
    va <- mapM evalType a'
    (va', t') <- multiplyUses q $ tryCheckExpr t va
    case va' of
      V.TProd lq la rq ra -> do
        (b, u') <- bindExpr x (mulMult q lq) la $ bindExpr y (mulMult q rq) ra $ inferExpr u
        a'' <- reifyType va'
        pure (b, T.ELetPair q a'' t' u')
      _ -> throwError "Can't use pair pattern for non product type"
  t@(S.EPair {}) -> throwError $ "Can't infer the type for the pair " ++ show t
  S.EBin op t u -> case op of
    S.Add -> do
      t' <- checkExpr t $ V.TPrim T.TInt
      u' <- checkExpr u $ V.TPrim T.TInt
      pure (V.TPrim T.TInt, T.EPrim $ T.PAdd t' u')
    S.Sub -> do
      t' <- checkExpr t $ V.TPrim T.TInt
      u' <- checkExpr u $ V.TPrim T.TInt
      pure (V.TPrim T.TInt, T.EPrim $ T.PSub t' u')
  S.EPrim x ts -> case x of
    "printInt" -> case ts of
      [t, u] -> do
        t' <- checkExpr t $ V.TPrim T.TInt
        u' <- checkExpr u $ V.TPrim T.TWorld
        pure (V.TPrim T.TWorld, T.EPrim $ T.PPrintInt t' u')
      _ -> throwError $ "Wrong num args supplied to prim " ++ show x
    "readInt" -> case ts of
      [t] -> do
        t' <- checkExpr t $ V.TPrim T.TWorld
        pure (V.TProd S.Many (V.TPrim T.TInt) S.One (V.TPrim T.TWorld), T.EPrim $ T.PReadInt t')
      _ -> throwError $ "Wrong num args supplied to prim " ++ show x
    _ -> throwError $ "Unknown prim op named " ++ show x
  S.EIf t u v -> do
    t' <- checkExpr t $ V.TPrim T.TBool
    ((a, u'), (b, v')) <- alternateUses (inferExpr u) (inferExpr v)
    ensureEqual a b
    pure (a, T.EIf t' u' v')
  S.EBool b -> pure (V.TPrim T.TBool, T.EPrim $ T.PBool b)
  S.EInt n -> pure (V.TPrim T.TInt, T.EPrim $ T.PInt n)

checkExpr :: S.Expr -> V.Type -> TC T.Expr
checkExpr = \case
  S.ELam x t -> \case
    V.TFunc q a b -> do
      t' <- bindExpr x q a $ checkExpr t b
      a' <- reifyType a
      pure $ T.ELam a' t'
    _ -> throwError "Lambdas can only have function types"
  S.ETyLam x t -> \case
    V.TForall k f -> do
      len <- asks (length . typeVars)
      t' <- bindType x k $ checkExpr t $ f $ V.TVar len
      pure $ T.ETyLam t'
    _ -> throwError "Type abstractions can only have polymorphic types"
  S.EPair t u -> \case
    V.TProd q a p b -> do
      t' <- multiplyUses q $ checkExpr t a
      u' <- multiplyUses p $ checkExpr u b
      pure $ T.EPair t' u'
    _ -> throwError "Pairs must have product types"
  t -> \a -> do
    (b, t') <- inferExpr t
    withExceptT (("When checking the term " ++ show t ++ "\n") ++) $ ensureEqual a b
    pure t'

ensureEqual :: V.Type -> V.Type -> TC ()
ensureEqual a b = do
  a' <- reifyType a
  b' <- reifyType b
  ctxt <- asks exprVars
  ctxt' <- mapM (mapM reifyType) ctxt
  unless (a' == b') $ throwError $ "In the context: " ++ show ctxt' ++ "\nType mismatch between " ++ show a' ++ " and " ++ show b'

tryCheckExpr :: S.Expr -> Maybe V.Type -> TC (V.Type, T.Expr)
tryCheckExpr t = \case
  Nothing -> inferExpr t
  Just a -> do
    t' <- checkExpr t a
    pure (a, t')

inferType :: S.Type -> TC (S.Kind, T.Type)
inferType = \case
  S.TName x | Just p <- isPrimType x -> pure (S.KStar 0, T.TPrim p)
  S.TName x -> do
    vars <- asks typeVars
    (k, l) <- lookupVar x vars
    pure (k, T.TVar l)
  S.TFunc q a b -> do
    (ka, a') <- inferType a
    (kb, b') <- inferType b
    pure (min (max (succ ka) kb) (S.KStar 3), T.TFunc q a' b')
  S.TProd q a p b -> do
    -- TODO: For now, only basic types can appear in pairs, but this restriction can be lifted.
    a' <- checkType a $ S.KStar 1
    b' <- checkType b $ S.KStar 1
    pure (S.KStar 1, T.TProd q a' p b')
  S.TForall x k a -> do
    (_, a') <- bindType x k $ inferType a
    pure (S.KStar 3, T.TForall k a')

isPrimType :: S.Ident -> Maybe T.PrimType
isPrimType = \case
  "World" -> Just T.TWorld
  "Int" -> Just T.TInt
  "Bool" -> Just T.TBool
  _ -> Nothing

checkType :: S.Type -> S.Kind -> TC T.Type
checkType a k = do
  (k', a') <- inferType a
  unless (k >= k') $ throwError $ "Kind mismatch between " ++ show k ++ " and " ++ show k'
  pure a'

evalType :: T.Type -> TC V.Type
evalType a = do
  len <- asks (length . typeVars)
  let env = map V.TVar $ reverse [0 .. len - 1]
  pure $ E.evalType env a

reifyType :: V.Type -> TC T.Type
reifyType a = do
  len <- asks (length . typeVars)
  pure $ R.reifyType len a

bindExpr :: S.Ident -> S.Mult -> V.Type -> TC a -> TC a
bindExpr x q a m = do
  names <- asks (map fst . exprVars)
  when (x `elem` names) $ throwError $ "Variable " ++ show x ++ " is shadowed"
  (res, Uses u) <- censor (\(Uses u) -> Uses $ M.delete x u) $ listen $ local (\ctxt -> ctxt {exprVars = (x, a) : exprVars ctxt}) m
  case (q, M.lookup x u) of
    (S.One, Just S.One) -> pure ()
    (S.One, _) -> throwError "Linear variable not used once"
    (S.Many, _) -> pure ()
  pure res

bindType :: S.Ident -> S.Kind -> TC a -> TC a
bindType x k m = do
  names <- asks (map fst . typeVars)
  when (x `elem` names) $ throwError $ "Type variable " ++ show x ++ " is shadowed"
  local (\ctxt -> ctxt {typeVars = (x, k) : typeVars ctxt}) m

lookupVar :: S.Ident -> [(S.Ident, a)] -> TC (a, Int)
lookupVar x = \case
  [] -> throwError $ "Undefined variable " ++ show x
  (y, a) : xs
    | x == y -> pure (a, length xs)
    | otherwise -> lookupVar x xs

multiplyUses :: S.Mult -> TC a -> TC a
multiplyUses q = censor $ \(Uses x) -> Uses $ fmap (mulMult q) x

alternateUses :: TC a -> TC b -> TC (a, b)
alternateUses ma mb = do
  (x, Uses qs) <- censor (const $ Uses M.empty) $ listen ma
  (y, Uses rs) <- censor (const $ Uses M.empty) $ listen mb
  let ss = M.unionWith maxMult qs rs
  censor (const $ Uses ss) $ pure (x, y)