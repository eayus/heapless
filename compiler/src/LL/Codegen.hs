module LL.Codegen where

import Core.Term (Prim (..), PrimType (..))
import Data.List
import LL.Term
import UC.Term (Type (..))

cgProg :: Prog -> String
cgProg (Prog fs t) = prelude ++ concatMap cgFunc fs ++ main
  where
    prelude = "fn print_int(n: usize) { println!(\"{}\", n); }"
    main = "fn main()" ++ braces ("let a = ();"++cg t)

cgFunc :: Func -> String -- TODO return type
cgFunc (LetRec x ps t) = "fn " ++ x ++ parens (intercalate "," $ map (\(y, a) -> y ++ ":" ++ cgType a) ps) ++ braces (cg t)

cg :: Expr -> String
cg = \case
  EVar x -> x
  EPrim p -> parens $ case fmap cg p of
    PAdd t u -> t ++ "+" ++ u
    PSub t u -> t ++ "-" ++ u
    PEql t u -> t ++ "==" ++ u
    PReadInt _ -> "read_int()"
    PPrintInt t _ -> "print_int(" ++ t ++ ")"
    PBool b -> if b then "true" else "false"
    PInt n -> show n
  EApp _ _ t us -> cg t ++ parens (intercalate "," $ map cg us)
  ELetPair x y _ _ _ t u -> "let " ++ parens (x ++ "," ++ y) ++ "=" ++ cg t ++ ";" ++ cg u
  EPair t u -> parens $ cg t ++ "," ++ cg u
  EIf t u v -> "if " ++ cg t ++ braces (cg u) ++ "else" ++ braces (cg v)

cgType :: Type -> String
cgType = \case
  TPrim p -> case p of
    TInt -> "usize"
    TBool -> "bool"
    TWorld -> "()"
  TFunc _ _ -> undefined
  TProd _ a _ b -> parens $ cgType a ++ "," ++ cgType b

parens :: String -> String
parens s = "(" ++ s ++ ")"

braces :: String -> String
braces s = "{" ++ s ++ "}"