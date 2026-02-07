module LL.Codegen where

import Core.Term (IntSize (..), Prim (..), PrimType (..))
import Data.List
import LL.Term
import UC.Term (Type (..))

cgProg :: Prog -> IO String
cgProg (Prog fs t) = do
  prelude <- readFile "data/prelude.rs"
  pure $ prelude ++ concatMap cgFunc fs ++ main
  where
    main = "#[no_mangle] pub fn rust_main()" ++ braces ("let a = ();" ++ cg t)

cgFunc :: Func -> String -- TODO return type
cgFunc (LetRec x ps b t) = "fn " ++ x ++ parens (intercalate "," $ map (\(y, a) -> y ++ ":" ++ cgType a) ps) ++ "->" ++ cgType b ++ braces (cg t)

cg :: Expr -> String
cg = \case
  EVar x -> x
  EPrim p -> parens $ case fmap cg p of
    PAdd t u -> t ++ "+" ++ u
    PSub t u -> t ++ "-" ++ u
    PEql t u -> t ++ "==" ++ u
    PReadInt w -> "read_int(" ++ w ++ ")"
    PPrintInt t w -> "print_int(" ++ t ++ ", " ++ w ++ ")"
    PBool b -> if b then "true" else "false"
    PInt n -> show n
    PUnit -> "()"
  EApp _ _ t us -> cg t ++ parens (intercalate "," $ map cg us)
  ELetPair x y _ _ _ t u -> braces $ "let " ++ parens (x ++ "," ++ y) ++ "=" ++ cg t ++ ";" ++ cg u
  EPair t u -> parens $ cg t ++ "," ++ cg u
  EIf t u v -> "if " ++ cg t ++ braces (cg u) ++ "else" ++ braces (cg v)

cgType :: Type -> String
cgType = \case
  TPrim p -> case p of
    TInt I64 -> "usize"
    TInt I32 -> "u32"
    TInt I16 -> "u16"
    TInt I8 -> "u8"
    TBool -> "bool"
    TWorld -> "()"
    TUnit -> "()"
    TOrd _ -> "()"
  TFunc _ _ -> undefined
  TProd _ a _ b -> parens $ cgType a ++ "," ++ cgType b

parens :: String -> String
parens s = "(" ++ s ++ ")"

braces :: String -> String
braces s = "{" ++ s ++ "}"
