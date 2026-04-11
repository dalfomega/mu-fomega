{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Parser.Attoparsec
  ( ParseFailureCategory (..)
  , parseExpr
  , parseExprWithCategory
  ) where

import Control.Applicative (many, optional, some, (<|>))
import Control.Monad (void, when)
import Data.Attoparsec.Text
  ( IResult (Done, Fail, Partial)
  , Parser
  , char
  , endOfInput
  , endOfLine
  , feed
  , parse
  , peekChar
  , satisfy
  , string
  )
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified MuFomega.Parser.Megaparsec as Mega
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (Kind, Natural, NaturalFold, NaturalSubtract, Type), Var (Var))
import MuFomega.Syntax.Lazy (ExprLazy (EAnnot, EApp, EBinOp, EBuiltin, EForall, ELam, ELet, ENatural, EVar))

data ParseFailureCategory
  = UnexpectedEndOfInput
  | UnexpectedToken

instance Eq ParseFailureCategory where
  UnexpectedEndOfInput == UnexpectedEndOfInput = True
  UnexpectedToken == UnexpectedToken = True
  _ == _ = False

instance Show ParseFailureCategory where
  show UnexpectedEndOfInput = "UnexpectedEndOfInput"
  show UnexpectedToken = "UnexpectedToken"

parseExpr :: Text -> Either String ExprLazy
parseExpr = first diagnosticMessage . parseExprDetailed

parseExprWithCategory :: Text -> Either ParseFailureCategory ExprLazy
parseExprWithCategory input =
  case parseExpr input of
    Right expr -> Right expr
    Left _ ->
      case Mega.parseExprWithCategory input of
        Left Mega.UnexpectedEndOfInput -> Left UnexpectedEndOfInput
        Left Mega.UnexpectedToken -> Left UnexpectedToken
        Right _ -> Left UnexpectedToken

data ParseDiagnostic = ParseDiagnostic Text String

parseExprDetailed :: Text -> Either ParseDiagnostic ExprLazy
parseExprDetailed input =
  case finalize (parse fullExpression input) of
    Done rest expr ->
      if Text.null rest
        then Right expr
        else Left (ParseDiagnostic rest "unconsumed input")
    Fail rest _ctx msg -> Left (ParseDiagnostic rest msg)
    Partial _ -> Left (ParseDiagnostic "" "incomplete parse")
  where
    finalize result =
      case result of
        Partial k -> finalize (feed (Partial k) "")
        other -> other

fullExpression :: Parser ExprLazy
fullExpression = do
  whsp
  expr <- expression
  whsp
  endOfInput
  pure expr

expression :: Parser ExprLazy
expression =
  lambdaExpression
    <|> letExpression
    <|> forallExpression
    <|> arrowOrAnnotatedExpression

lambdaExpression :: Parser ExprLazy
lambdaExpression = do
  lambdaToken
  whsp
  _ <- char '('
  whsp
  name <- nonReservedLabel
  whsp
  _ <- char ':'
  whsp1
  tipe <- expression
  whsp
  _ <- char ')'
  whsp
  arrowToken
  whsp
  body <- expression
  pure (ELam name tipe body)

letExpression :: Parser ExprLazy
letExpression = do
  bindings <- some letBinding
  keyword "in"
  whsp1
  body <- expression
  pure (foldr (uncurry ELet) body bindings)

letBinding :: Parser (Text, ExprLazy)
letBinding = do
  keyword "let"
  whsp1
  name <- nonReservedLabel
  whsp
  _ <- char '='
  whsp
  value <- expression
  whsp1
  pure (name, value)

forallExpression :: Parser ExprLazy
forallExpression = do
  forallToken
  whsp
  _ <- char '('
  whsp
  name <- nonReservedLabel
  whsp
  _ <- char ':'
  whsp1
  tipe <- expression
  whsp
  _ <- char ')'
  whsp
  arrowToken
  whsp
  body <- expression
  pure (EForall name tipe body)

arrowOrAnnotatedExpression :: Parser ExprLazy
arrowOrAnnotatedExpression = do
  body <- operatorExpression
  mArrow <- optional $ do
    whsp
    arrowToken
    whsp
    expression
  case mArrow of
    Just codomain -> pure (EForall "_" body codomain)
    Nothing -> do
      mTipe <- optional $ do
        whsp
        _ <- char ':'
        whsp1
        expression
      pure (maybe body (EAnnot body) mTipe)

operatorExpression :: Parser ExprLazy
operatorExpression = plusExpression

plusExpression :: Parser ExprLazy
plusExpression = do
  firstTerm <- timesExpression
  rest <- many $ do
    whsp
    _ <- char '+'
    whsp
    timesExpression
  pure (foldl (EBinOp Plus) firstTerm rest)

timesExpression :: Parser ExprLazy
timesExpression = do
  firstTerm <- applicationExpression
  rest <- many $ do
    whsp
    _ <- char '*'
    whsp
    applicationExpression
  pure (foldl (EBinOp Times) firstTerm rest)

applicationExpression :: Parser ExprLazy
applicationExpression = do
  fn <- primitiveExpression
  args <- many (whsp1 *> primitiveExpression)
  pure (foldl EApp fn args)

primitiveExpression :: Parser ExprLazy
primitiveExpression =
  (ENatural <$> naturalLiteral)
    <|> identifierExpression
    <|> parenthesizedExpression

identifierExpression :: Parser ExprLazy
identifierExpression =
  (EBuiltin <$> builtin)
    <|> (EVar <$> variable)

variable :: Parser Var
variable = do
  name <- nonReservedLabel
  index <- optional $ do
    whsp
    _ <- char '@'
    whsp
    naturalIndex
  pure (Var name (maybe 0 id index))

parenthesizedExpression :: Parser ExprLazy
parenthesizedExpression = do
  _ <- char '('
  whsp
  inner <- expression
  whsp
  _ <- char ')'
  pure inner

naturalLiteral :: Parser Integer
naturalLiteral = zeroLiteral <|> nonZeroLiteral
  where
    zeroLiteral = do
      _ <- char '0'
      next <- peekChar
      case next of
        Just c | isDigit c -> fail "leading zero in natural literal"
        _ -> pure 0

    nonZeroLiteral = do
      firstDigit <- satisfy (`elem` ['1' .. '9'])
      restDigits <- many (satisfy isDigit)
      pure (read (firstDigit : restDigits))

naturalIndex :: Parser Word
naturalIndex = fromInteger <$> naturalLiteral

builtin :: Parser Builtin
builtin =
  builtinToken "Natural/subtract" *> pure NaturalSubtract
    <|> builtinToken "Natural/fold" *> pure NaturalFold
    <|> builtinToken "Natural" *> pure Natural
    <|> builtinToken "Type" *> pure Type
    <|> builtinToken "Kind" *> pure Kind

nonReservedLabel :: Parser Text
nonReservedLabel = do
  name <- label
  when (name `elem` builtinNames) $
    fail ("reserved builtin identifier: " <> Text.unpack name)
  pure name

label :: Parser Text
label = do
  firstChar <- satisfy isLabelFirstChar
  rest <- many (satisfy isLabelNextChar)
  let name = Text.pack (firstChar : rest)
  when (name `elem` keywordNames) $
    fail ("reserved keyword: " <> Text.unpack name)
  pure name

isLabelFirstChar :: Char -> Bool
isLabelFirstChar c = isAlpha c || c == '_'

isLabelNextChar :: Char -> Bool
isLabelNextChar c = isAlphaNum c || c == '-' || c == '/' || c == '_'

keyword :: Text -> Parser ()
keyword token = do
  _ <- string token
  ensureLabelBoundary

builtinToken :: Text -> Parser ()
builtinToken token = do
  _ <- string token
  ensureLabelBoundary

ensureLabelBoundary :: Parser ()
ensureLabelBoundary = do
  next <- peekChar
  case next of
    Just c | isLabelNextChar c -> fail "identifier boundary expected"
    _ -> pure ()

lambdaToken :: Parser ()
lambdaToken = void (char 'λ' <|> char '\\')

forallToken :: Parser ()
forallToken = void (char '∀') <|> keyword "forall"

arrowToken :: Parser ()
arrowToken = void (char '→') <|> void (string "->")

whsp :: Parser ()
whsp = void (many whitespaceChunk)

whsp1 :: Parser ()
whsp1 = void (some whitespaceChunk)

whitespaceChunk :: Parser ()
whitespaceChunk =
  void (char ' ')
    <|> void (char '\t')
    <|> void endOfLine
    <|> lineComment

lineComment :: Parser ()
lineComment = do
  _ <- string "--"
  _ <- many (satisfy (/= '\n'))
  _ <- optional (char '\n')
  pure ()

keywordNames :: [Text]
keywordNames = ["let", "in", "forall"]

builtinNames :: [Text]
builtinNames = ["Natural", "Natural/fold", "Natural/subtract", "Type", "Kind"]

diagnosticMessage :: ParseDiagnostic -> String
diagnosticMessage (ParseDiagnostic _ msg) = msg
