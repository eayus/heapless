{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Surface.Check.Ctxt where

import Surface.Syntax

data Constraints = Constraints
  { metas :: [Ident],
    typeEqualities :: [CTypeEquality],
    typeKinds :: [CTypeKind],
    instances :: [CInstance]
  }

-- Modification

ctxtAddVar :: Ident -> Scheme -> Ctxt -> Ctxt
ctxtAddVar x a ctxt = ctxt {vars = (x, a) : ctxt.vars}

ctxtAddTypeVar :: Ident -> Kind -> Ctxt -> Ctxt
ctxtAddTypeVar x k ctxt = ctxt {typeVars = (x, k) : ctxt.typeVars}

ctxtAddClassDef :: Ident -> Class -> Ctxt -> Ctxt
ctxtAddClassDef x c ctxt = ctxt {classDefs = (x, c) : ctxt.classDefs}

ctxtAddInstance :: InstanceSig -> Ctxt -> Ctxt
ctxtAddInstance i ctxt = ctxt {instances = i : ctxt.instances}

consAddTypeEquality :: CTypeEquality -> Constraints -> Constraints
consAddTypeEquality c cons = cons {typeEqualities = c : cons.typeEqualities}

consAddTypeKind :: CTypeKind -> Constraints -> Constraints
consAddTypeKind c cons = cons {typeKinds = c : cons.typeKinds}

consAddInstance :: CInstance -> Constraints -> Constraints
consAddInstance c cons = cons {instances = c : cons.instances}

-- Initialisation

initCtxt :: Ctxt
initCtxt = Ctxt initVars initTypes [] [] []
  where
    initVars :: [(Ident, Scheme)]
    initVars = [ioPure, ioBind, printInt, printStr, printChar, readRAM, writeRAM, eqInt, eqBool, remainder]
      where
        ioPure = ("ioPure", Forall [("a", Star 1)] [] (TArr (TVar "a") (TApp (TVar "IO") (TVar "a"))))
        ioBind = ("ioBind", Forall [("a", Star 1), ("b", Star 1)] [] (TArr (TApp (TVar "IO") (TVar "a")) (TArr (TArr (TVar "a") (TApp (TVar "IO") (TVar "b"))) (TApp (TVar "IO") (TVar "b")))))
        printInt = ("printInt", Mono (TArr (TVar "Int") (TApp (TVar "IO") (TVar "Unit"))))
        printStr = ("printStr", Mono (TArr (TVar "String") (TApp (TVar "IO") (TVar "Unit"))))
        printChar = ("printChar", Mono (TArr (TVar "Char") (TApp (TVar "IO") (TVar "Unit"))))
        readRAM = ("readRAM", Mono (TArr (TVar "Int") (TApp (TVar "IO") (TVar "Int"))))
        writeRAM = ("writeRAM", Mono (TArr (TVar "Int") $ TArr (TVar "Int") $ TApp (TVar "IO") (TVar "Unit")))
        eqInt = ("eqInt", Mono (TArr (TVar "Int") (TArr (TVar "Int") (TVar "Bool"))))
        eqBool = ("eqBool", Mono (TArr (TVar "Bool") (TArr (TVar "Bool") (TVar "Bool"))))
        remainder = ("remainder", Mono (TArr (TVar "Int") (TArr (TVar "Int") (TVar "Int"))))

    initTypes :: [(Ident, Kind)]
    initTypes = [("Int", Star 1), ("Bool", Star 1), ("Char", Star 1), ("String", Star 1), ("IO", KFunc (Star 1) (Star 2))]

initCons :: Constraints
initCons = Constraints [] [] [] []