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
pTop = pTLet <|> pTData <|> pTClass <|> pTInst

pTClass :: Parser Top
pTClass = do
  symbol "class"
  x <- pUpperIdent
  symbol "("
  v <- pLowerIdent
  symbol "::"
  k <- pKind
  symbol ")"
  symbol "{"
  xs <- sepEndBy1 pSig (symbol ";")
  symbol "}"
  pure $ TClass x (Class v k xs)

pTInst :: Parser Top
pTInst = do
  symbol "inst"
  x <- pUpperIdent
  y <- pTypeAtom
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
  x <- pUpperIdent
  s <- pStage
  cs <- many pConstr
  symbol ";"
  pure $ TData x s cs

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
  optional (symbol "rec") >>= \case
    Nothing -> pure NoRec
    Just () -> pure Rec

pStage :: Parser Stage
pStage = choice [CT <$ symbol ":=", RT <$ symbol "="]

pExpr :: Parser Expr
pExpr = makeExprParser pApps ops
  where
    ops =
      [ [ InfixL (EBin BLTE <$ symbol "<="),
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
pExprAtom = choice [pEStr, pECon, pEIf, pELam, pELet, pEVar, pEInt, parens pExpr]
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

    pECon = ECon <$> pUpperIdent

    pEIf = do
      symbol "if"
      x <- pExpr
      symbol "then"
      y <- pExpr
      symbol "else"
      EIf x y <$> pExpr

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
            x <- pTypeAtom
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

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

reserved :: [String]
reserved = ["let", "rec", "data", "\\", "Λ", "if", "then", "else", "type"]

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
