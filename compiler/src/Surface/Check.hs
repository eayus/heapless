module Surface.Check where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List
import Surface.Alpha
import Surface.Syntax
import Debug.Trace (traceShowM)

data Ctxt = Ctxt
  { vars :: [(Ident, Scheme)],
    typeVars :: [(Ident, Kind)]
  }

data MCtxt = MCtxt
  { freshNames :: [Ident],
    metaVars :: [Ident], -- Meta variables in scope.
    constraints :: [(Type, Type)], -- Type equalities
    kindConstraints :: [(Ctxt, Type, Kind)]
  }

type TC a = StateT Ctxt (StateT MCtxt (Except String)) a

typecheck :: (MonadError String m) => Prog -> m ()
typecheck p = runTC (checkProg p)

runTC :: (MonadError String m) => TC () -> m ()
runTC m0 = do
  let m1 = evalStateT m0 initCtxt
  let m2 = runStateT m1 initMCtxt
  (_, _) <- liftEither $ runExcept m2
  pure ()

initCtxt :: Ctxt
initCtxt = Ctxt [] []

initMCtxt :: MCtxt
initMCtxt = MCtxt nameSupply [] [] []
  where
    nameSupply = map singleton ['a' ..]

substCtxtMeta :: Subst -> Ctxt -> Ctxt
substCtxtMeta sub (Ctxt vars tvars) = Ctxt (map (second (substSchemeMeta sub)) vars) tvars

checkProg :: Prog -> TC ()
checkProg = mapM_ checkTop

checkTop :: Top -> TC ()
checkTop (TLet r x a t) = do
  checkPolyLet r x a t
  mctxt <- lift get
  withError (("When checking the top level definiton " ++ x ++ "\n") ++) $ do
    -- TODO: Check for unsolved metas?
    sub <- solveConstraints (constraints mctxt)
    let kindCons = map (\(ctxt, a, k) -> (substCtxtMeta sub ctxt, substMeta sub a, k)) $ kindConstraints mctxt
    forM_ kindCons $ \(ctxt, a, k) -> liftEither $ runExcept $ runStateT (evalStateT (checkType a k) ctxt) undefined
  lift $ put mctxt {constraints = []}

checkExpr :: Expr -> Type -> TC ()
checkExpr t a = do
  b <- inferExpr t
  unify a b

inferExpr :: Expr -> TC Type
inferExpr = \case
  EVar x -> do
    ctxt <- get
    case lookup x $ vars ctxt of
      Just (Forall xs a) -> do
        sub <-
          mapM
            ( \(y, k) -> do
                m <- meta
                hasKind m k
                pure (y, m)
            )
            xs
        pure $ substVar (M.fromList sub) a
      Nothing -> throwError $ "Undefined variable " ++ x
  ELam xs t -> locally $ do
    as <- mapM (\x -> do a <- meta; extend x $ Forall [] a; pure a) xs
    b <- inferExpr t
    pure $ foldr TArr b as
  EApp t u -> do
    a <- inferExpr u
    b <- meta
    checkExpr t (TArr a b)
    pure b
  ELet Rec _ Nothing _ _ -> throwError "Recursive let bindings must have type annotation"
  ELet NoRec x Nothing t u -> do
    a <- inferExpr t
    locally $ do
      extend x (Forall [] a)
      inferExpr u
  ELet r x (Just a) t u -> locally $ do
    checkPolyLet r x a t
    inferExpr u
  EInt _ -> pure $ TCon "Int"
  EIf x y z -> do
    checkExpr x $ TCon "Bool"
    a <- meta
    checkExpr y a
    checkExpr z a
    pure a
  EBin BLT x y -> checkIntCmp x y
  EBin BLTE x y -> checkIntCmp x y
  EBin BGT x y -> checkIntCmp x y
  EBin BGTE x y -> checkIntCmp x y
  EBin BAdd x y -> checkArith x y
  EBin BSub x y -> checkArith x y
  EBin BMul x y -> checkArith x y

checkIntCmp :: Expr -> Expr -> TC Type
checkIntCmp x y = do
  checkExpr x $ TCon "Int"
  checkExpr y $ TCon "Int"
  pure $ TCon "Bool"

checkArith :: Expr -> Expr -> TC Type
checkArith x y = do
  checkExpr x $ TCon "Int"
  checkExpr y $ TCon "Int"
  pure $ TCon "Int"

checkPolyLet :: Rec -> Ident -> Scheme -> Expr -> TC ()
checkPolyLet r x sch@(Forall xs a) t = do
  locally $ do
    mapM_ (uncurry extendType) xs
    Star o <- inferType a
    when (r == Rec) $ do
      when (o == 3) $ throwError "Higher-order functions are not allowed to be recursive"
      extend x sch
    checkExpr t a
  extend x sch

-- Only to be used once meta variables have been solved.
checkType :: Type -> Kind -> TC ()
checkType a k0 = do
  k1 <- inferType a
  unless (k0 == k1) $ throwError $ "Expected type " ++ show a ++ "to have kind " ++ show k0 ++ " but it has kind " ++ show k1

inferType :: Type -> TC Kind
inferType = \case
  TVar x -> do
    ctxt <- get
    case lookup x (typeVars ctxt) of
      Just k -> pure k
      Nothing -> throwError $ "Undefined type variable " ++ x
  TCon x -> do
    unless (x `elem` ["Int", "Bool"]) (throwError $ "Undefined type constructor " ++ x)
    pure $ Star 1
  TArr a b -> do
    Star i <- inferType a
    Star j <- inferType b
    pure $ Star $ max (suc i) j
  TMeta {} -> undefined

suc :: Int -> Int
suc i = min (i + 1) 3

extend :: Ident -> Scheme -> TC ()
extend x a = modify $ \ctxt -> ctxt {vars = (x, a) : vars ctxt}

extendType :: Ident -> Kind -> TC ()
extendType x a = modify $ \ctxt -> ctxt {typeVars = (x, a) : typeVars ctxt}

hasKind :: Type -> Kind -> TC ()
hasKind a k = do
  ctxt <- get
  mctxt <- lift get
  lift $ put $ mctxt {kindConstraints = (ctxt, a, k) : kindConstraints mctxt}

locally :: TC a -> TC a
locally m = do
  ctxt <- get
  x <- m
  put ctxt
  pure x

meta :: TC Type
meta = do
  mctxt <- lift get
  let names = freshNames mctxt
  lift $ put $ mctxt {freshNames = tail names, metaVars = head names : metaVars mctxt}
  pure $ TMeta $ head names

solveConstraints :: (MonadError String m) => [(Type, Type)] -> m Subst
solveConstraints = \case
  [] -> pure M.empty
  (a0, a1) : cs -> do
    s0 <- solveConstraints cs
    s1 <- mgu (substMeta s0 a0) (substMeta s0 a1)
    pure $ s1 @@ s0

unify :: Type -> Type -> TC ()
unify a b = do
  mctxt <- lift get
  lift $ put $ mctxt {constraints = (a, b) : constraints mctxt}

mgu :: (MonadError String m) => Type -> Type -> m Subst
mgu (TMeta x) a = setMeta x a
mgu a (TMeta x) = setMeta x a
mgu (TVar x0) (TVar x1) | x0 == x1 = pure M.empty
mgu (TCon x0) (TCon x1) | x0 == x1 = pure M.empty
mgu (TArr a0 b0) (TArr a1 b1) = do
  s0 <- mgu a0 a1
  s1 <- mgu (substMeta s0 b0) (substMeta s0 b1)
  pure $ s1 @@ s0
mgu a b = throwError $ "Mismatch between\n" ++ show a ++ "\n" ++ show b

setMeta :: (MonadError String m) => Ident -> Type -> m Subst
setMeta x a
  | a == TMeta x = pure M.empty
  | x `S.member` metaFrees a = throwError "Circular type"
  | otherwise = pure $ M.singleton x a

(@@) :: Subst -> Subst -> Subst
s0 @@ s1 = M.map (substMeta s0) s1 `M.union` s0

-- These should be in base now?
tryError :: (MonadError e m) => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

withError :: (MonadError e m) => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure