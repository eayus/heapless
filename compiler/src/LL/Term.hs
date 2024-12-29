module LL.Term where

import Core.Syntax (Mult)
import Core.Term (Prim)
import UC.Term (Type)

type Name = String

data Prog
  = Prog [Func] Expr -- Main has World bound in scope.

data Func
  = LetRec Name [(Name, Type)] Expr

data Expr
  = EVar Name
  | EPrim (Prim Expr) -- A primitive operation (arithmetic, constants, primitive IO, etc.)
  | EApp [Type] Type Expr [Expr] -- Function application.
  | ELetRec Name Type Expr Expr -- Recursive let binding.
  | ELetPair Name Name Mult Type Expr Expr -- Pair destructuring via let binding (non-recursive).
  | EPair Expr Expr -- Product constructor.
  | EIf Expr Expr Expr -- If expression.