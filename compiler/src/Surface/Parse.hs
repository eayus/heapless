module Surface.Parse where

import Control.Monad.Combinators.Expr
import Control.Monad.Except
import Data.Maybe
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
pTop = pTLet <|> pTData <|> pTClass <|> pTInst <|> pInclude

pInclude :: Parser Top
pInclude = do
  symbol "include"
  s <- stringLiteral
  symbol ";"
  pure $ TInclude s

pTClass :: Parser Top
pTClass = do
  symbol "class"
  x <- pUpperIdent
  ts <- some $ do
    symbol "("
    v <- pLowerIdent
    symbol "::"
    k <- pKind
    symbol ")"
    pure (v, k)
  symbol "{"
  xs <- sepEndBy1 pSig (symbol ";")
  symbol "}"
  pure $ TClass x (Class ts xs)

pTInst :: Parser Top
pTInst = do
  symbol "inst"
  x <- pUpperIdent
  y <- some pTypeAtom
  symbol "{"
  xs <- sepEndBy1 pInstDef (symbol ";")
  symbol "}"
  pure $ TInst x y xs

pInstDef :: Parser (Ident, Expr)
pInstDef = do
  x <- pLowerIdent
  symbol "="
  t <- pExpr
  pure (x, t)

pSig :: Parser (Ident, Scheme)
pSig = do
  x <- pLowerIdent
  symbol ":"
  a <- pScheme
  pure (x, a)

pTData :: Parser Top
pTData = do
  symbol "data"
  n <- pNew
  r <- pRec
  x <- pUpperIdent
  xs <- many $ parens $ do
    v <- pLowerIdent
    symbol "::"
    k <- pKind
    pure (v, k)
  s <- pStage
  cs <- many pConstr
  symbol ";"
  pure $ TData n r x s xs cs

pTLet :: Parser Top
pTLet = do
  symbol "let"
  r <- pRec
  x <- pIdent
  symbol ":"
  a <- pScheme
  symbol "="
  t <- pExpr
  symbol ";"
  pure $ TLet r x a t

pRec :: Parser Rec
pRec =
  optional (keyword "rec") >>= \case
    Nothing -> pure NoRec
    Just () -> pure Rec

pNew :: Parser New
pNew =
  optional (symbol "new") >>= \case
    Nothing -> pure NoNew
    Just () -> pure New

pStage :: Parser Stage
pStage = choice [CT <$ symbol ":=", RT <$ symbol "="]

pPat :: Parser Pat
pPat = do
  c <- pUpperIdent
  xs <- many pLowerIdent
  pure $ Pat c xs

pExpr :: Parser Expr
pExpr = makeExprParser pApps ops
  where
    ops =
      [ [ InfixL (EBin BAnd <$ symbol "&&"),
          InfixL (EBin BOr <$ symbol "||"),
          InfixL (EBin BBitOr <$ symbol "|")
        ],
        [ InfixL (EBin BShiftR <$ symbol ">>")
        ],
        [ InfixL (EBin BLTE <$ symbol "<="),
          InfixL (EBin BLT <$ symbol "<"),
          InfixL (EBin BGTE <$ symbol ">="),
          InfixL (EBin BGT <$ symbol ">"),
          InfixL (EBin BEq <$ symbol "==")
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
pExprAtom = choice [pEDo, pEFold, pEStr, pEChar, pECon, pEIf, pELam, pELet, pEVar, pEInt, parens pExpr]
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
      a <- optional $ do
        symbol ":"
        pScheme
      symbol "="
      t <- pExpr
      symbol ";"
      ELet r x a t <$> pExpr

    pEVar = EVar <$> pLowerIdent

    pEInt = lexeme $ EInt <$> L.decimal

    pEStr = lexeme $ EStr <$> stringLiteral

    pEChar = lexeme $ EChar <$> charLiteral

    pECon = ECon <$> pUpperIdent

    pEIf = do
      symbol "if"
      x <- pExpr
      symbol "then"
      y <- pExpr
      symbol "else"
      EIf x y <$> pExpr

    pEDo = do
      symbol "do"
      symbol "{"
      xs <- many $ try $ do
        x <- pLowerIdent
        symbol "<-"
        t <- pExpr
        symbol ";"
        pure (x, t)
      t <- pExpr
      symbol "}"
      pure $ EDo xs t

    pEFold = do
      symbol "fold"
      t <- pExpr
      symbol "{"
      xs <-
        sepEndBy1
          ( do
              p <- pPat
              symbol "=>"
              u <- pExpr
              pure (p, u)
          )
          (symbol ";")
      symbol "}"
      pure $ EFold t xs

pType :: Parser Type
pType = makeExprParser pTypeApps ops
  where
    ops = [[InfixR (TArr <$ symbol "->")]]

pTypeApps :: Parser Type
pTypeApps = foldl1 TApp <$> some pTypeAtom

pTypeAtom :: Parser Type
pTypeAtom = choice [TVar <$> pLowerIdent, TCon <$> pUpperIdent, parens pType]

pScheme :: Parser Scheme
pScheme = do
  tvars <- optional $ do
    symbol "["
    xs <-
      sepEndBy1
        ( do
            a <- pLowerIdent
            symbol "::"
            k <- pKind
            pure (a, k)
        )
        (symbol ",")
    symbol "]"
    symbol "."
    pure xs
  clCons <- optional $ do
    symbol "{"
    xs <-
      sepEndBy1
        ( do
            c <- pUpperIdent
            x <- some pTypeAtom
            pure (c, x)
        )
        (symbol ",")
    symbol "}"
    symbol "=>"
    pure xs
  Forall (fromMaybe [] tvars) (fromMaybe [] clCons) <$> pType

pConstr :: Parser Constr
pConstr = do
  symbol "|"
  x <- pUpperIdent
  as <- many pTypeAtom
  pure $ Constr x as

pKind :: Parser Kind
pKind = makeExprParser pKindAtom [[InfixR (KFunc <$ symbol "->")]]

pKindAtom :: Parser Kind
pKindAtom = choice [Star 1 <$ symbol "*1", Star 2 <$ symbol "*2", Star 3 <$ symbol "*3"]

pLowerIdent :: Parser Ident
pLowerIdent = try $ lexeme $ do
  c <- lowerChar <|> char '_'
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

keyword :: String -> Parser ()
keyword s = try $ lexeme $ do
  string s
  notFollowedBy letterChar

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

charLiteral :: Parser Char
charLiteral = char '\'' *> L.charLiteral <* char '\''

reserved :: [String]
reserved = ["let", "rec", "data", "\\", "Λ", "if", "then", "else", "type", "do", "fold", "include"]

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
