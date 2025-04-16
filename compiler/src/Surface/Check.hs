module Surface.Check where

import Control.Monad.Except
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List
import Surface.Alpha
import Surface.Syntax

newtype Kind = Star Int

data Scheme = Forall (M.HashMap Ident Kind) Type

data Ctxt = Ctxt
  { vars :: [(Ident, Scheme)],
    typeVars :: S.HashSet Ident
  }

data MCtxt = MCtxt
  { freshNames :: [Ident],
    usedNames :: [Ident],
    constraints :: [(Type, Type)] -- Type equalities
  }

type TC a = StateT Ctxt (StateT MCtxt (Except String)) a

typecheck :: (MonadError String m) => Prog -> m ()
typecheck p = runTC (checkProg p)

runTC :: (MonadError String m) => TC () -> m ()
runTC m0 = do
  let m1 = evalStateT m0 initCtxt
  let m2 = runStateT m1 initMCtxt
  (_, mctxt) <- liftEither $ runExcept m2
  _ <- solveConstraints $ constraints mctxt
  pure ()

initCtxt :: Ctxt
initCtxt = Ctxt [] S.empty

initMCtxt :: MCtxt
initMCtxt = MCtxt nameSupply [] []
  where
    nameSupply = map singleton ['a' ..]

checkProg :: Prog -> TC ()
checkProg = mapM_ checkTop

checkTop :: Top -> TC ()
checkTop (TLet r x a t) = do
  checkPolyLet r x a t
  mctxt <- lift get
  _ <- withError (("When checking the top level definiton " ++ x ++ "\n") ++) $ solveConstraints (constraints mctxt)
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
        sub <- mapM (const meta) $ S.toMap xs
        pure $ substVar sub a
      Nothing -> throwError $ "Undefined variable " ++ x
  ELam xs t -> locally $ do
    as <- mapM (\x -> do a <- meta; extend x $ Forall S.empty a; pure a) xs
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
      extend x (Forall S.empty a)
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

checkPolyLet :: Rec -> Ident -> Type -> Expr -> TC ()
checkPolyLet r x a t = do
  ctxt <- get
  let quantifiedTypeVars = typeFrees a `S.difference` typeVars ctxt
  locally $ do
    mapM_ extendType quantifiedTypeVars
    when (r == Rec) $ extend x (Forall quantifiedTypeVars a)
    checkExpr t a
  extend x $ Forall quantifiedTypeVars a

extend :: Ident -> Scheme -> TC ()
extend x a = modify $ \ctxt -> ctxt {vars = (x, a) : vars ctxt}

extendType :: Ident -> TC ()
extendType x = modify $ \ctxt -> ctxt {typeVars = S.insert x $ typeVars ctxt}

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
  lift $ put $ mctxt {freshNames = tail names, usedNames = head names : usedNames mctxt}
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
