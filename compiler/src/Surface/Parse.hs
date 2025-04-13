module Surface.Parse where

import Control.Monad.Combinators.Expr
import Control.Monad.Except
import Data.Void (Void)
import Surface.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

parseFile :: FilePath -> ExceptT String IO Prog
parseFile filepath = do
  contents <- lift $ readFile filepath
  case runParser (sc *> pProg <* eof) filepath contents of
    Left err -> throwError $ errorBundlePretty err
    Right x -> pure x

pProg :: Parser Prog
pProg = many pTop

pTop :: Parser Top
pTop = do
  symbol "let"
  r <- pRec
  x <- pIdent
  symbol ":"
  a <- pType
  symbol "="
  t <- pExpr
  symbol ";"
  pure $ TLet r x a t

pRec :: Parser Rec
pRec = optional (symbol "rec") >>= \case
  Nothing -> pure NoRec
  Just () -> pure Rec

pExpr :: Parser Expr
pExpr = makeExprParser pApps ops
  where
    ops =
      [ [
          InfixL (EBin BLTE <$ symbol "<="),
          InfixL (EBin BLT <$ symbol "<"),
          InfixL (EBin BGTE <$ symbol ">="),
          InfixL (EBin BGT <$ symbol ">")
        ],
        [ InfixL (EBin BAdd <$ symbol "+"),
          InfixL (EBin BAdd <$ symbol "-")
        ],
        [ InfixL (EBin BMul <$ symbol "*")
        ]
      ]

pApps :: Parser Expr
pApps = do
  xs <- some pExprAtom
  pure $ foldl1 EApp xs

pExprAtom :: Parser Expr
pExprAtom = choice [pEIf, pELam, pELet, pEVar, pEInt, parens pExpr]
  where
    pELam = do
      symbol "\\"
      xs <- some pLowerIdent
      symbol "=>"
      ELam xs <$> pExpr

    pELet = do
      symbol "let"
      r <- pRec
      x <- pIdent
      symbol "="
      t <- pExpr
      symbol ";"
      ELet r x Nothing t <$> pExpr

    pEVar = EVar <$> pLowerIdent

    pEInt = lexeme $ EInt <$> L.decimal

    pEIf = do
      symbol "if"
      x <- pExpr
      symbol "then"
      y <- pExpr
      symbol "else"
      EIf x y <$> pExpr

pType :: Parser Type
pType = makeExprParser pTypeAtom ops
  where
    ops = [[InfixR (TArr <$ symbol "->")]]

pTypeAtom :: Parser Type
pTypeAtom = choice [TVar <$> pLowerIdent, TCon <$> pUpperIdent, parens pType]

pLowerIdent :: Parser Ident
pLowerIdent = try $ lexeme $ do
  c <- lowerChar
  cs <- many alphaNumChar
  let s = c : cs
  when (s `elem` reserved) $ fail $ "Reserved word " ++ show s ++ " used as identifier"
  pure s

pUpperIdent :: Parser Ident
pUpperIdent = try $ lexeme $ do
  c <- upperChar
  cs <- many alphaNumChar
  let s = c : cs
  pure s

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
reserved = ["let", "rec", "\\", "Λ", "if", "then", "else", "type"]

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
