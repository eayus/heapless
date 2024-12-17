-- Parser for the core language.
module Core.Parse (parseFile) where

import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.Except
import Core.Syntax
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

parseFile :: FilePath -> ExceptT String IO Expr
parseFile filepath = do
  contents <- lift $ readFile filepath
  case runParser (sc *> pExpr <* eof) filepath contents of
    Left err -> throwError $ errorBundlePretty err
    Right x -> pure x

pExpr :: Parser Expr
pExpr = makeExprParser pApps ops
  where
    ops =
      [ [ InfixL (EBin Add <$ symbol "+"),
          InfixL (EBin Sub <$ symbol "-")
        ]
      ]

    pApps = do
      t <- pAtom
      fs <- many (pRhs <|> pTyRhs)
      pure $ foldl (flip ($)) t fs

    pRhs = do
      rhs <- pAtom
      pure $ \lhs -> EApp lhs rhs

    pTyRhs = do
      symbol "@"
      rhs <- pType
      pure $ \lhs -> ETyApp lhs rhs

    pAtom =
      choice
        [ pLam,
          pTyLam,
          pLet,
          pLetRec,
          pLetPair,
          pPair,
          pIf,
          pInt,
          pBool,
          pPrim,
          EVar <$> pIdent,
          parens pExpr
        ]

    pLam = do
      symbol "λ"
      x <- pIdent
      symbol "."
      ELam x <$> pExpr

    pTyLam = do
      symbol "Λ"
      x <- pIdent
      symbol "."
      ETyLam x <$> pExpr

    pLet = try $ do
      symbol "let"
      q <- pMult
      x <- pIdent
      a <- optional pTypeAno
      symbol "="
      t <- pExpr
      symbol ";"
      ELet q x a t <$> pExpr

    pLetRec = try $ do
      symbol "let"
      symbol "rec"
      x <- pIdent
      a <- pTypeAno
      symbol "="
      t <- pExpr
      symbol ";"
      ELetRec x a t <$> pExpr

    pLetPair = do
      symbol "let"
      q <- pMult
      xs <- parens $ do
        x <- pIdent
        symbol ","
        y <- pIdent
        pure (x, y)
      a <- optional pTypeAno
      symbol "="
      t <- pExpr
      symbol ";"
      ELetPair q xs a t <$> pExpr

    pPair = try $ parens $ do
      x <- pExpr
      symbol ","
      EPair x <$> pExpr

    pPrim = do
      symbol "#"
      x <- pIdent
      symbol "["
      ts <- sepEndBy pExpr $ symbol ","
      symbol "]"
      pure $ EPrim x ts

    pIf = do
      symbol "if"
      t <- pExpr
      symbol "then"
      u <- pExpr
      symbol "else"
      EIf t u <$> pExpr

    pInt = EInt <$> lexeme L.decimal

    pBool = EBool <$> choice [True <$ symbol "True", False <$ symbol "False"]

pTypeAno :: Parser Type
pTypeAno = do
  symbol ":"
  pType

pType :: Parser Type
pType = makeExprParser pAtom ops
  where
    ops = [[InfixR (TFunc Many <$ symbol "->"), InfixR (TFunc One <$ symbol "-o")]]

    pAtom = choice [pForall, pProd, TName <$> pIdent, parens pType]

    pForall = do
      symbol "∀"
      x <- pIdent
      symbol "::"
      k <- pKind
      symbol "."
      TForall x k <$> pType

    pProd = parens $ do
      q <- pMult
      a <- pType
      symbol ","
      r <- pMult
      TProd q a r <$> pType

pKind :: Parser Kind
pKind = choice [KStar 1 <$ symbol "Type1", KStar 2 <$ symbol "Type2", KStar 3 <$ symbol "Type3"]

-- Not supplying a multiplicity is inferred to be Many (unrestricted).
pMult :: Parser Mult
pMult = fromMaybe Many <$> optional (One <$ symbol "1")

pIdent :: Parser Ident
pIdent = try $ lexeme $ do
  c <- letterChar
  cs <- many alphaNumChar
  let s = c : cs
  when (s `elem` reserved) $ fail $ "Reserved word " ++ show s ++ " used as identifier"
  pure s

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Utilities for lexing.

reserved :: [String]
reserved = ["let", "rec", "λ", "Λ", "if", "then", "else"]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol = void . L.symbol sc

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")
