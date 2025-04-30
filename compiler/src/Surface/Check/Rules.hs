module Surface.Check.Rules (typecheckFile) where

import Control.Monad.Except
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List
import Data.Maybe
import Surface.Alpha
import Surface.Check.Ctxt
import Surface.Check.Monad
import Surface.Parse
import Surface.Syntax
import Util

-- TOODS:
--  Finish case implementation by making data types add their patterns to the context. Polymorphic patterns
--  must be handled too, which will require changes to the context and pattern checking.

-- Programs and Top Level

typecheckFile :: FilePath -> C ()
typecheckFile fp = evalStateT (checkFile fp) initCtxt

checkFile :: FilePath -> TC ()
checkFile fp = do
  prog <- lift $ parseFile fp
  checkProg prog

checkProg :: Prog -> TC ()
checkProg = mapM_ checkTop

checkTop :: Top -> TC ()
checkTop top = withError (("When checking " ++ topDesc top ++ "\n") ++) $ case top of
  TInclude fp -> checkFile fp
  TLet r x a t -> solve $ checkPolyLet r x a t
  TData d -> checkDataDef d
  TClass x c -> checkClassDef x c
  TInst i -> checkInstance i

checkDataDef :: DataDef -> TC ()
checkDataDef d = do
  scope <- getVarNames
  when (d.name `elem` scope) $ throwError "Duplicate data definition"
  case d.stage of
    RT -> checkRTDataDef d
    CT -> checkCTDataDef d

checkRTDataDef :: DataDef -> TC ()
checkRTDataDef d = do
  unless (d.recursive == NoRec) $
    throwError "Runtime data types cannot be recursive"
  mapM_ (eatRTConstructor d.name d.typeVars) d.constructors
  let dataKind = foldr (KFunc . snd) (Star 1) d.typeVars
  addTypeVar' d.name dataKind

checkCTDataDef :: DataDef -> TC ()
checkCTDataDef d
  | [Constr conName [a]] <- d.constructors, d.recursive == NoRec = checkNewtype d.name d.typeVars conName a
  | otherwise = checkChurchData d

checkChurchData :: DataDef -> TC ()
checkChurchData d = do
  let dataKind = foldr (KFunc . snd) (Star 3) d.typeVars
  addTypeVar' d.name dataKind
  mapM_ (eatCTConstructor d.name d.typeVars) d.constructors

checkNewtype :: Ident -> [(Ident, Kind)] -> Ident -> Type -> TC ()
checkNewtype dataName typeVars conName innerTy = do
  i <- locally' $ do
    mapM_ (uncurry addTypeVar') typeVars
    inferTypeStar innerTy
  let conTy = Forall typeVars [] (TArr innerTy $ TVar dataName)
  addVar' conName conTy
  let dataKind = foldr (KFunc . snd) (Star i) typeVars
  addTypeVar' dataName dataKind

eatRTConstructor :: Ident -> [(Ident, Kind)] -> Constr -> TC ()
eatRTConstructor dataName typeVars (Constr conName as) = do
  locally' $ do
    mapM_ (uncurry addTypeVar') typeVars
    mapM_ (`checkType` Star 1) as
  let conTy = foldr TArr (TVar dataName) as
  addVar' conName (Mono conTy)

eatCTConstructor :: Ident -> [(Ident, Kind)] -> Constr -> TC ()
eatCTConstructor dataName typeVars (Constr conName as) = do
  locally' $ do
    mapM_ (uncurry addTypeVar') typeVars
    mapM_ (`checkType` Star 3) as
  let conTy = foldr TArr (TVar dataName) as
  addVar' conName (Forall typeVars [] conTy)

checkClassDef :: Ident -> Class -> TC ()
checkClassDef x c@(Class tvars defs) = do
  ctxt <- get
  when (isJust $ lookup x $ classDefs ctxt) $ throwError $ "Typeclass " ++ x ++ " is already defined"
  locally' $ do
    mapM_ (uncurry addTypeVar') tvars
    forM_ defs $ \sig -> checkScheme sig.scheme
  addClassDef x c
  let names = map fst tvars
  forM_ defs $ \(Sig y (Forall xs cs a)) -> addVar' y $ Forall (tvars ++ xs) (InstanceSig x (map TVar names) : cs) a

checkInstance :: Instance -> TC ()
checkInstance (Instance isig defs) = do
  c <- checkInstanceSig isig
  ensureFreshInstance isig
  let sub = M.fromList $ zip (map fst c.tvars) isig.args
  let sigs = substTypeVars sub c.sigs
  let classDefNames = map (.var) c.sigs
  let instDefNames = map fst defs
  unless (sort classDefNames == sort instDefNames) $
    throwError "Instance does not have same methods as class"
  addInstance' isig
  forM_ sigs $ \sig -> do
    let rhs = fromJust $ lookup sig.var defs
    solve $ checkExprScheme rhs sig.scheme

-- Expressions

inferExpr :: Expr -> MTC Type
inferExpr = \case
  EVar x -> do
    ctxt <- getCtxt
    case lookup x $ vars ctxt of
      Just a -> instantiate a
      Nothing -> throwError $ "Undefined variable " ++ x
  ELam xs t -> locally $ do
    as <- mapM eatVar xs
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
      addMonoVar x a
      inferExpr u
  ELet r x (Just a) t u -> locally $ do
    checkPolyLet r x a t
    inferExpr u
  EInt _ -> pure TInt
  EStr _ -> pure TStr
  EChar _ -> pure TChar
  EIf x y z -> do
    checkExpr x TBool
    a <- meta
    checkExpr y a
    checkExpr z a
    pure a
  EBin BLT x y -> inferIntCmp x y
  EBin BLTE x y -> inferIntCmp x y
  EBin BGT x y -> inferIntCmp x y
  EBin BGTE x y -> inferIntCmp x y
  EBin BAdd x y -> inferArith x y
  EBin BSub x y -> inferArith x y
  EBin BMul x y -> inferArith x y
  EBin BAnd x y -> inferLogic x y
  EBin BOr x y -> inferLogic x y
  EBin BBitOr x y -> inferArith x y
  EBin BShiftR x y -> inferArith x y
  EBin BEq x y -> do
    a <- meta
    hasInstance "Eq" [a]
    checkExpr x a
    checkExpr y a
    pure $ TVar "Bool"
  EDo ts t -> do
    m <- meta
    hasInstance "Monad" [m]
    locally $ do
      forM_ ts $ \(x, u) -> do
        a <- meta
        checkExpr u (TApp m a)
        addMonoVar x a
      ret <- meta
      checkExpr t (TApp m ret)
      pure (TApp m ret)
  EFold {} -> undefined
  ECase scrut clauses -> do
    a <- inferExpr scrut
    bs <- mapM (inferClause a) clauses
    ret <- meta
    mapM_ (unify ret) bs
    pure ret

checkExpr :: Expr -> Type -> MTC ()
checkExpr t a = do
  b <- inferExpr t
  unify a b

checkExprScheme :: Expr -> Scheme -> MTC ()
checkExprScheme x (Forall xs cs a) = locally $ do
  mapM_ (uncurry addTypeVar) xs
  mapM_ addInstance cs
  checkExpr x a

-- Common expression forms

inferArith :: Expr -> Expr -> MTC Type
inferArith t u = do
  checkExpr t TInt
  checkExpr u TInt
  pure TInt

inferIntCmp :: Expr -> Expr -> MTC Type
inferIntCmp t u = do
  checkExpr t TInt
  checkExpr u TInt
  pure TBool

inferLogic :: Expr -> Expr -> MTC Type
inferLogic t u = do
  checkExpr t TBool
  checkExpr u TBool
  pure TBool

inferClause :: Type -> Clause -> MTC Type
inferClause scrutTy (Clause pat t) = locally $ do
  eatPat scrutTy pat
  inferExpr t

eatPat :: Type -> Pat -> MTC ()
eatPat scrutTy (Pat con xs) = do
  ctxt <- getCtxt
  sig <- case find (\p -> p.conName == con) ctxt.patterns of
    Just p -> pure p
    Nothing -> throwError $ "Invalid pattern constructor " ++ show con
  unless (length xs == length sig.params) $ throwError "Pattern has wrong number of arguments"
  unless (scrutTy == TVar sig.retType) $ throwError "Pattern returns the wrong type"
  mapM_ (uncurry addMonoVar) (zip xs sig.params)

checkPolyLet :: Rec -> Ident -> Scheme -> Expr -> MTC ()
checkPolyLet r x sch@(Forall xs cs a) t = do
  locally $ do
    mapM_ (uncurry addTypeVar) xs
    mapM_ (lift . eatInstanceSig) cs
    i <- lift $ inferTypeStar a
    when (r == Rec) $ do
      when (i == 3) $ throwError "Higher-order functions are not allowed to be recursive"
      addVar x sch
    checkExpr t a
  addVar x sch

eatInstanceSig :: InstanceSig -> TC ()
eatInstanceSig i = do
  checkInstanceSig i
  addInstance' i

checkInstanceSig :: InstanceSig -> TC Class
checkInstanceSig (InstanceSig x vs) = do
  ctxt <- get
  c <- case lookup x $ classDefs ctxt of
    Nothing -> throwError $ "Undefined type class " ++ x
    Just c -> pure c
  let kinds = map snd c.tvars
  mapM_ (uncurry checkType) (zip vs kinds)
  pure c

-- Types

checkScheme :: Scheme -> TC ()
checkScheme (Forall xs cs a) = locally' $ do
  mapM_ (uncurry addTypeVar') xs
  mapM_ eatInstanceSig cs
  _ <- inferTypeStar a
  pure ()

inferType :: Type -> TC Kind
inferType = \case
  TVar x -> do
    ctxt <- get
    case lookup x ctxt.typeVars of
      Just k -> pure k
      Nothing -> throwError $ "Undefined type variable " ++ x
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

checkType :: Type -> Kind -> TC ()
checkType a k0 = do
  k1 <- inferType a
  unless (isSubKind k1 k0) $ throwError $ "Expected type " ++ show a ++ " to have kind " ++ show k0 ++ " but it has kind " ++ show k1

inferTypeStar :: Type -> TC Int
inferTypeStar a = do
  k <- inferType a
  case k of
    Star i -> pure i
    _ -> throwError "Expected a type of kind *"

isSubKind :: Kind -> Kind -> Bool
isSubKind (Star i) (Star j) = i <= j
isSubKind (KFunc kd0 kr0) (KFunc kd1 kr1) = isSubKind kd1 kd0 && isSubKind kr0 kr1
isSubKind _ _ = False

suc :: Int -> Int
suc i = min (i + 1) 3

-- Constraint Solving

solve :: MTC () -> TC ()
solve m = do
  cons <- execStateT m initCons
  sub <- solveTypeEqualities cons.typeEqualities
  let unsolved = S.fromList cons.metas `S.difference` M.keysSet sub
  unless (null unsolved) $ throwError $ "Unsolved meta variables: " ++ show unsolved
  lift $ solveTypeKinds $ substMetas sub cons.typeKinds
  lift $ solveInstances $ substMetas sub cons.instances

solveTypeEqualities :: (MonadError String m) => [CTypeEquality] -> m Subst
solveTypeEqualities = \case
  [] -> pure M.empty
  CTypeEquality a0 a1 : cs -> do
    s0 <- solveTypeEqualities cs
    s1 <- mgu (substMeta s0 a0) (substMeta s0 a1)
    pure $ s1 @@ s0

solveTypeKinds :: [CTypeKind] -> C ()
solveTypeKinds = mapM_ $ \(CTypeKind ctxt a k) -> evalStateT (checkType a k) ctxt

solveInstances :: [CInstance] -> C ()
solveInstances = mapM_ $ \(CInstance ctxt sig) -> unless (sig `elem` ctxt.instances) $ throwError $ "Cannot find required instance " ++ show sig

mgu :: (MonadError String m) => Type -> Type -> m Subst
mgu (TMeta x) a = setMeta x a
mgu a (TMeta x) = setMeta x a
mgu (TVar x0) (TVar x1) | x0 == x1 = pure M.empty
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

-- Utilities

instantiate :: Scheme -> MTC Type
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
  mapM_ hasInstance' $ substTypeVars sub cs
  pure $ substTypeVars sub a

eatVar :: Ident -> MTC Type
eatVar x = do
  a <- meta
  addMonoVar x a
  pure a

topDesc :: Top -> String
topDesc = \case
  TInclude fp -> "the imported file " ++ show fp
  TLet _ x _ _ -> "the top-level let " ++ show x
  TData d -> "the data definition " ++ show d.name
  TClass x _ -> "the class definition " ++ show x
  TInst (Instance (InstanceSig x as) _) -> "The instance definition " ++ show x ++ show as

ensureFreshInstance :: InstanceSig -> TC ()
ensureFreshInstance sig = do
  insts <- getInstances
  when (sig `elem` insts) $ throwError $ "Dupliacte instance " ++ show sig