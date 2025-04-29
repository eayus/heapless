module Surface.Check.Monad where

import Control.Monad.Except
import Control.Monad.State
import Data.List
import Data.Maybe
import Surface.Check.Ctxt
import Surface.Syntax

type C = ExceptT String IO

type TC = StateT Ctxt C

type MTC = StateT Constraints TC

getCtxt :: MTC Ctxt
getCtxt = lift get

-- Modify Constraints

meta :: MTC Type
meta = do
  cons <- get
  let supply = map show [0 ..]
  let v = fromJust $ find (`notElem` cons.metas) supply
  put $ cons {metas = v : cons.metas}
  pure $ TMeta v

unify :: Type -> Type -> MTC ()
unify a b = modify (consAddTypeEquality $ CTypeEquality a b)

hasKind :: Type -> Kind -> MTC ()
hasKind a k = do
  ctxt <- getCtxt
  modify (consAddTypeKind $ CTypeKind ctxt a k)

hasInstance' :: InstanceSig -> MTC ()
hasInstance' i = do
  ctxt <- getCtxt
  modify (consAddInstance $ CInstance ctxt i)

hasInstance :: Ident -> [Type] -> MTC ()
hasInstance x as = do
  ctxt <- getCtxt
  let i = InstanceSig x as
  modify (consAddInstance $ CInstance ctxt i)

-- Query Ctxt

getInstances :: TC [InstanceSig]
getInstances = gets (.instances)

getVarNames :: TC [Ident]
getVarNames = gets (map fst . (.vars))

-- Modify Ctxt

addVar :: Ident -> Scheme -> MTC ()
addVar x a = lift $ addVar' x a

addVar' :: Ident -> Scheme -> TC ()
addVar' x a = modify $ ctxtAddVar x a

addTypeVar :: Ident -> Kind -> MTC ()
addTypeVar x k = lift $ addTypeVar' x k

addTypeVar' :: Ident -> Kind -> TC ()
addTypeVar' x k = modify $ ctxtAddTypeVar x k

addClassDef :: Ident -> Class -> TC ()
addClassDef x c = modify $ ctxtAddClassDef x c

addInstance' :: InstanceSig -> TC ()
addInstance' i = modify $ ctxtAddInstance i

addInstance :: InstanceSig -> MTC ()
addInstance i = lift $ addInstance' i

addMonoVar :: Ident -> Type -> MTC ()
addMonoVar x a = addVar x (Mono a)

locally' :: TC a -> TC a
locally' m = do
  ctxt <- get
  x <- m
  put ctxt
  pure x

locally :: MTC a -> MTC a
locally m = do
  ctxt <- lift get
  x <- m
  lift $ put ctxt
  pure x