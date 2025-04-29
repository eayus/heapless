module Surface.Check where

import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List
import Data.Maybe
import Surface.Alpha
import Surface.Syntax

data Ctxt = Ctxt
  { vars :: [(Ident, Scheme)],
    typeVars :: [(Ident, Kind)],
    typeCons :: [(Ident, Kind)],
    dataCons :: [(Ident, Scheme)],
    classDefs :: [(Ident, Class)],
    instances :: [(Ident, [Type])] -- Class name, types (multiple types because we have multiparam typeclasses)
  }
  deriving (Show)

data MCtxt = MCtxt
  { freshNames :: [Ident],
    metaVars :: [Ident], -- Meta variables in scope.
    constraints :: [(Type, Type)], -- Type equalities
    kindConstraints :: [(Ctxt, Type, Kind)], -- A type has some kind.
    mClassConstraints :: [(Ctxt, Ident, [Type])] -- A class must be implemented for the type.
  }

-- TODO: Rather than running all definitions in TC, be more fine grained and make it clear the meta context does not
-- persist between top-level definitions. (This has already lead to a bug once).

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
initCtxt = Ctxt [ioPure, ioBind, printInt, printStr, printChar, eqInt, eqBool, remainder] [] [("Int", Star 1), ("Bool", Star 1), ("Char", Star 1), ("String", Star 1), ("IO", KFunc (Star 1) (Star 2))] [("True", mono $ TCon "Bool"), ("False", mono $ TCon "Bool")] [] []
  where
    ioPure = ("ioPure", Forall [("a", Star 1)] [] (TArr (TVar "a") (TApp (TCon "IO") (TVar "a"))))
    ioBind = ("ioBind", Forall [("a", Star 1), ("b", Star 1)] [] (TArr (TApp (TCon "IO") (TVar "a")) (TArr (TArr (TVar "a") (TApp (TCon "IO") (TVar "b"))) (TApp (TCon "IO") (TVar "b")))))
    printInt = ("printInt", Forall [] [] (TArr (TCon "Int") (TApp (TCon "IO") (TCon "Unit"))))
    printStr = ("printStr", Forall [] [] (TArr (TCon "String") (TApp (TCon "IO") (TCon "Unit"))))
    printChar = ("printChar", Forall [] [] (TArr (TCon "Char") (TApp (TCon "IO") (TCon "Unit"))))
    eqInt = ("eqInt", Forall [] [] (TArr (TCon "Int") (TArr (TCon "Int") (TCon "Bool"))))
    eqBool = ("eqBool", Forall [] [] (TArr (TCon "Bool") (TArr (TCon "Bool") (TCon "Bool"))))
    remainder = ("remainder", Forall [] [] (TArr (TCon "Int") (TArr (TCon "Int") (TCon "Int"))))

initMCtxt :: MCtxt
initMCtxt = MCtxt nameSupply [] [] [] []
  where
    nameSupply = map singleton ['a' ..]

substCtxtMeta :: Subst -> Ctxt -> Ctxt
substCtxtMeta sub (Ctxt vars tvars tcons dcons cdefs ccs) = Ctxt (map (second (substSchemeMeta sub)) vars) tvars tcons dcons cdefs ccs

checkProg :: Prog -> TC ()
checkProg = mapM_ checkTop

checkTop :: Top -> TC ()
checkTop (TLet r x a t) = withError (("When checking the top level definiton " ++ x ++ "\n") ++) $ do
  checkPolyLet r x a t
  solveAllConstraints
checkTop (TData n r x s tvars cs) = do
  ctxt <- get
  lift $ put initMCtxt
  when (x `elem` map fst (typeCons ctxt)) $ throwError $ "Data type " ++ x ++ " is already defined"
  case s of
    RT -> do
      unless (null tvars) $ throwError "Runtime data types cannot be polymorphic" -- This is not quite true, as long as polymorphic recursion is disallowed.
      when (n == New) $ throwError "Newtypes must be removed at comptime (use ':=')"
      when (r == Rec) $ throwError "Runtime data types cannot be recursive"
      mapM_ (checkConstr [] x 1) cs
      extendTypeCon x $ Star 1
    CT -> do
      let kind res = foldr (KFunc . snd) res tvars
      if n == New
        then do
          when (r == Rec) $ throwError "Newtypes cannot be recursive"
          case cs of
            [Constr c [a]] -> do
              ctxt <- get
              when (c `elem` map fst (dataCons ctxt)) $ throwError $ "Data constructor " ++ c ++ " is already defined"
              i <- locally $ do
                mapM_ (uncurry extendType) tvars
                inferTypeStar a
              extendTypeCon x $ kind (Star i)
              extendDataCon c $ Forall tvars [] $ TArr a (TCon x)
            [_] -> throwError "Newtypes constructors must only have a single argument"
            _ -> throwError "Newtypes can only have a single constructor"
        else
          if r == Rec
            then do
              extendTypeCon x $ kind $ Star 3
              mapM_ (checkConstr tvars x 3) cs
            else do
              mapM_ (checkConstr tvars x 3) cs
              extendTypeCon x $ kind $ Star 3
checkTop (TClass x c@(Class tvars xs)) = do
  ctxt <- get
  when (isJust $ lookup x $ classDefs ctxt) $ throwError $ "Typeclass " ++ x ++ " is already defined"
  locally $ do
    mapM_ (uncurry extendType) tvars
    forM_ xs $ \(_, a) -> checkScheme a
  extendClassDef x c
  let names = map fst tvars
  forM_ xs $ \(y, Forall xs cs a) -> extend y $ Forall (tvars ++ xs) ((x, map TVar names) : cs) a
checkTop (TInst className as defs) = do
  ctxt <- get
  Class tvars sigs <- case lookup className $ classDefs ctxt of
    Just c -> pure c
    Nothing -> throwError $ "Cannot define instance for undefined class " ++ className
  let (names, kinds) = unzip tvars
  mapM_ (uncurry checkType) (zip as kinds)
  let sub = M.fromList $ zip names as
  let sigs' = map (substSigTVar sub) sigs
  unless (sort (map fst defs) == sort (map fst sigs)) $ throwError "Instance does not have same methods as class"
  locally $ do
    extendInstance className as
    forM_ sigs' $ \(x, Forall xs cs b) -> locally $ do
      let rhs = fromJust $ lookup x defs
      mapM_ (uncurry extendType) xs
      mapM_ (uncurry extendInstance) cs
      checkExpr rhs b
  extendInstance className as
  solveAllConstraints

-- Relies on the fact that the scheme does not capture variables in the substitution.
substSigTVar :: Subst -> (Ident, Scheme) -> (Ident, Scheme)
substSigTVar s (x, Forall xs cs a) = (x, Forall xs cs $ substVar s a)

solveAllConstraints :: TC ()
solveAllConstraints = do
  mctxt <- lift get
  sub <- solveConstraints (constraints mctxt)
  let mvars = metaVars mctxt
  let unsolved = S.fromList mvars `S.difference` M.keysSet sub
  unless (null unsolved) $ throwError $ "Unsolved meta variables: " ++ show unsolved
  let kindCons = map (\(ctxt, a, k) -> (substCtxtMeta sub ctxt, substMeta sub a, k)) $ kindConstraints mctxt
  forM_ kindCons $ \(ctxt, a, k) -> liftEither $ runExcept $ runStateT (evalStateT (checkType a k) ctxt) undefined
  let classCons = map (\(ctxt, x, as) -> (substCtxtMeta sub ctxt, x, map (substMeta sub) as)) $ mClassConstraints mctxt
  forM_ classCons $ \(ctxt, x, as) -> unless ((x, as) `elem` instances ctxt) $ throwError $ "Required instance for " ++ x ++ " " ++ show as ++ "\nContext:\n" ++ show (instances ctxt)
  lift $ put initMCtxt

checkConstr :: [(Ident, Kind)] -> Ident -> Int -> Constr -> TC ()
checkConstr tvars dataName i (Constr x as) = do
  ctxt <- get
  when (x `elem` map fst (dataCons ctxt)) $ throwError $ "Data constructor " ++ x ++ " is already defined"
  forM_ as $ \a -> locally $ do
    mapM_ (uncurry extendType) tvars
    checkType a (Star i)
  let a = foldr TArr (TCon dataName) as
  let a' = Forall tvars [] a
  extendDataCon x a'

checkExpr :: Expr -> Type -> TC ()
checkExpr t a = do
  b <- inferExpr t
  unify a b

instantiate :: Scheme -> TC Type
instantiate (Forall xs cs a) = do
  sub <-
    M.fromList
      <$> mapM
        ( \(y, k) -> do
            m <- meta
            hasKind m k
            pure (y, m)
        )
        xs
  let cs' = map (second (map (substVar sub))) cs
  mapM_ (uncurry hasClass) cs'
  pure $ substVar sub a

inferExpr :: Expr -> TC Type
inferExpr = \case
  EVar x -> do
    ctxt <- get
    case lookup x $ vars ctxt of
      Just a -> instantiate a
      Nothing -> throwError $ "Undefined variable " ++ x
  ECon x -> do
    ctxt <- get
    case lookup x $ dataCons ctxt of
      Just a -> instantiate a
      Nothing -> throwError $ "Undefined data constructor " ++ x
  ELam xs t -> locally $ do
    as <- mapM (\x -> do a <- meta; extend x $ Forall [] [] a; pure a) xs
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
      extend x (Forall [] [] a)
      inferExpr u
  ELet r x (Just a) t u -> locally $ do
    checkPolyLet r x a t
    inferExpr u
  EInt _ -> pure $ TCon "Int"
  EStr _ -> pure $ TCon "String"
  EChar _ -> pure $ TCon "Char"
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
  EBin BAnd x y -> checkLogic x y
  EBin BOr x y -> checkLogic x y
  EBin BEq x y -> do
    a <- meta
    hasClass "Eq" [a]
    checkExpr x a
    checkExpr y a
    pure $ TCon "Bool"
  EDo ts t -> do
    m <- meta
    hasClass "Monad" [m]
    locally $ do
      forM_ ts $ \(x, u) -> do
        a <- meta
        checkExpr u (TApp m a)
        extend x (Forall [] [] a)
      a <- meta
      checkExpr t (TApp m a)
      pure (TApp m a)
  EFold x cs -> do
    a <- inferExpr x
    b <- meta
    forM_ cs $ \(Pat c xs, rhs) -> locally $ do
      forM_ xs $ \v -> do
        c <- meta
        extend v (Forall [] [] c)
      let patExpr = foldl EApp (ECon c) (map EVar xs)
      checkExpr patExpr a
      checkExpr rhs b
    pure b

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

checkLogic :: Expr -> Expr -> TC Type
checkLogic x y = do
  checkExpr x $ TCon "Bool"
  checkExpr y $ TCon "Bool"
  pure $ TCon "Bool"

checkPolyLet :: Rec -> Ident -> Scheme -> Expr -> TC ()
checkPolyLet r x sch@(Forall xs cs a) t = do
  locally $ do
    mapM_ (uncurry extendType) xs
    mapM_ checkClassConstraint cs
    i <- inferTypeStar a
    when (r == Rec) $ do
      when (i == 3) $ throwError "Higher-order functions are not allowed to be recursive"
      extend x sch
    checkExpr t a
  extend x sch

checkClassConstraint :: (Ident, [Type]) -> TC ()
checkClassConstraint (x, vs) = do
  ctxt <- get
  Class tvars _ <- case lookup x $ classDefs ctxt of
    Nothing -> throwError $ "Undefined type class " ++ x
    Just c -> pure c
  let kinds = map snd tvars
  mapM_ (uncurry checkType) (zip vs kinds)
  -- checkType a k
  extendInstance x vs

checkScheme :: Scheme -> TC ()
checkScheme (Forall xs cs a) = locally $ do
  mapM_ (uncurry extendType) xs
  mapM_ checkClassConstraint cs
  _ <- inferTypeStar a
  pure ()

-- Only to be used once meta variables have been solved.
checkType :: Type -> Kind -> TC ()
checkType a k0 = do
  k1 <- inferType a
  unless (isSubKind k1 k0) $ throwError $ "Expected type " ++ show a ++ " to have kind " ++ show k0 ++ " but it has kind " ++ show k1

isSubKind :: Kind -> Kind -> Bool
isSubKind (Star i) (Star j) = i <= j
isSubKind (KFunc kd0 kr0) (KFunc kd1 kr1) = isSubKind kd1 kd0 && isSubKind kr0 kr1
isSubKind _ _ = False

inferTypeStar :: Type -> TC Int
inferTypeStar a = do
  k <- inferType a
  case k of
    Star i -> pure i
    _ -> throwError "Expected a type of kind *"

inferType :: Type -> TC Kind
inferType = \case
  TVar x -> do
    ctxt <- get
    case lookup x (typeVars ctxt) of
      Just k -> pure k
      Nothing -> throwError $ "Undefined type variable " ++ x
  TCon x -> do
    ctxt <- get
    case lookup x $ typeCons ctxt of
      Just k -> pure k
      Nothing -> throwError $ "Undefined type constructor " ++ x
  TArr a b -> do
    i <- inferTypeStar a
    j <- inferTypeStar b
    pure $ Star $ max (suc i) j
  TApp a b -> do
    k <- inferType a
    case k of
      KFunc k0 k1 -> k1 <$ checkType b k0
      _ -> throwError $ "Cannot type level apply the type " ++ show a ++ " of kind " ++ show k
  TMeta {} -> error "Meta variable encountered when trying to infer the kind for a type!"

suc :: Int -> Int
suc i = min (i + 1) 3

extend :: Ident -> Scheme -> TC ()
extend x a = modify $ \ctxt -> ctxt {vars = (x, a) : vars ctxt}

extendType :: Ident -> Kind -> TC ()
extendType x a = do
  ctxt <- get
  when (isJust $ lookup x $ typeVars ctxt) $ throwError $ "Shadowed type variable " ++ x
  put $ ctxt {typeVars = (x, a) : typeVars ctxt}

extendDataCon :: Ident -> Scheme -> TC ()
extendDataCon x a = modify $ \ctxt -> ctxt {dataCons = (x, a) : dataCons ctxt}

extendTypeCon :: Ident -> Kind -> TC ()
extendTypeCon x a = modify $ \ctxt -> ctxt {typeCons = (x, a) : typeCons ctxt}

extendClassDef :: Ident -> Class -> TC ()
extendClassDef x c = modify $ \ctxt -> ctxt {classDefs = (x, c) : classDefs ctxt}

extendInstance :: Ident -> [Type] -> TC ()
extendInstance x c = modify $ \ctxt -> ctxt {instances = (x, c) : instances ctxt}

hasKind :: Type -> Kind -> TC ()
hasKind a k = do
  ctxt <- get
  mctxt <- lift get
  lift $ put $ mctxt {kindConstraints = (ctxt, a, k) : kindConstraints mctxt}

hasClass :: Ident -> [Type] -> TC ()
hasClass x a = do
  ctxt <- get
  mctxt <- lift get
  lift $ put $ mctxt {mClassConstraints = (ctxt, x, a) : mClassConstraints mctxt}

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
mgu (TApp a0 b0) (TApp a1 b1) = do
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

mono :: Type -> Scheme
mono = Forall [] []

-- These should be in base now?
tryError :: (MonadError e m) => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

withError :: (MonadError e m) => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure