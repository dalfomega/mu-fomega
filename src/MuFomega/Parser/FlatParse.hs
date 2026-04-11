{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Parser.FlatParse
  ( ParseFailureCategory (..)
  , parseExpr
  , parseExprWithCategory
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified FlatParse.Basic as FP
import qualified MuFomega.Parser.Megaparsec as Mega
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (Kind, Natural, NaturalFold, NaturalSubtract, Type), Var (Var))
import MuFomega.Syntax.Lazy (ExprLazy (EAnnot, EApp, EBinOp, EBuiltin, EForall, ELam, ELet, ENatural, EVar))

data ParseFailureCategory
  = UnexpectedEndOfInput
  | UnexpectedToken
  deriving (Eq, Show)

parseExpr :: Text -> Either String ExprLazy
parseExpr input =
  case FP.runParser fullExpression (Text.encodeUtf8 input) of
    FP.OK expr rest
      | BS.null rest -> Right expr
      | otherwise -> Left "unconsumed input"
    FP.Fail -> Left "parse failure"
    FP.Err err -> Left err

parseExprWithCategory :: Text -> Either ParseFailureCategory ExprLazy
parseExprWithCategory input =
  case parseExpr input of
    Right expr -> Right expr
    Left _ ->
      case Mega.parseExprWithCategory input of
        Left Mega.UnexpectedEndOfInput -> Left UnexpectedEndOfInput
        Left Mega.UnexpectedToken -> Left UnexpectedToken
        Right _ -> Left UnexpectedToken

type Parser a = FP.Parser String a

fullExpression :: Parser ExprLazy
fullExpression = do
  whsp
  expr <- expression
  whsp
  FP.eof
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
  _ <- charP '('
  whsp
  name <- nonReservedLabel
  whsp
  _ <- charP ':'
  whsp1
  tipe <- expression
  whsp
  _ <- charP ')'
  whsp
  arrowToken
  whsp
  body <- expression
  pure (ELam name tipe body)

letExpression :: Parser ExprLazy
letExpression = do
  bindings <- FP.some letBinding
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
  _ <- charP '='
  whsp
  value <- expression
  whsp1
  pure (name, value)

forallExpression :: Parser ExprLazy
forallExpression = do
  forallToken
  whsp
  _ <- charP '('
  whsp
  name <- nonReservedLabel
  whsp
  _ <- charP ':'
  whsp1
  tipe <- expression
  whsp
  _ <- charP ')'
  whsp
  arrowToken
  whsp
  body <- expression
  pure (EForall name tipe body)

arrowOrAnnotatedExpression :: Parser ExprLazy
arrowOrAnnotatedExpression = do
  body <- operatorExpression
  mArrow <- FP.optional $ do
    whsp
    arrowToken
    whsp
    expression
  case mArrow of
    Just codomain -> pure (EForall "_" body codomain)
    Nothing -> do
      mTipe <- FP.optional $ do
        whsp
        _ <- charP ':'
        whsp1
        expression
      pure (maybe body (EAnnot body) mTipe)

operatorExpression :: Parser ExprLazy
operatorExpression = plusExpression

plusExpression :: Parser ExprLazy
plusExpression = do
  firstTerm <- timesExpression
  rest <- FP.many $ do
    whsp
    _ <- charP '+'
    whsp
    timesExpression
  pure (foldl (EBinOp Plus) firstTerm rest)

timesExpression :: Parser ExprLazy
timesExpression = do
  firstTerm <- applicationExpression
  rest <- FP.many $ do
    whsp
    _ <- charP '*'
    whsp
    applicationExpression
  pure (foldl (EBinOp Times) firstTerm rest)

applicationExpression :: Parser ExprLazy
applicationExpression = do
  fn <- primitiveExpression
  args <- FP.many (whsp1 *> primitiveExpression)
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
  index <- FP.optional $ do
    whsp
    _ <- charP '@'
    whsp
    naturalIndex
  pure (Var name (maybe 0 id index))

parenthesizedExpression :: Parser ExprLazy
parenthesizedExpression = do
  _ <- charP '('
  whsp
  inner <- expression
  whsp
  _ <- charP ')'
  pure inner

naturalLiteral :: Parser Integer
naturalLiteral = zeroLiteral <|> nonZeroLiteral
  where
    zeroLiteral = do
      _ <- charP '0'
      hasFollowingDigit <- isNextDigit
      when hasFollowingDigit FP.failed
      pure 0

    nonZeroLiteral = do
      firstDigit <- FP.satisfy (`elem` ['1' .. '9'])
      restDigits <- FP.many (FP.satisfy isDigit)
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
  when (name `elem` builtinNames) FP.failed
  pure name

label :: Parser Text
label = do
  firstChar <- FP.satisfy isLabelFirstChar
  rest <- FP.many (FP.satisfy isLabelNextChar)
  let name = Text.pack (firstChar : rest)
  when (name `elem` keywordNames) FP.failed
  pure name

isLabelFirstChar :: Char -> Bool
isLabelFirstChar c = isAlpha c || c == '_'

isLabelNextChar :: Char -> Bool
isLabelNextChar c = isAlphaNum c || c == '-' || c == '/' || c == '_'

keyword :: Text -> Parser ()
keyword token = do
  literal token
  ensureLabelBoundary

builtinToken :: Text -> Parser ()
builtinToken token = do
  literal token
  ensureLabelBoundary

ensureLabelBoundary :: Parser ()
ensureLabelBoundary = do
  next <- nextChar
  case next of
    Just c | isLabelNextChar c -> FP.failed
    _ -> pure ()

literal :: Text -> Parser ()
literal token = mapM_ charP (Text.unpack token)

charP :: Char -> Parser Char
charP = FP.satisfy . (==)

lambdaToken :: Parser ()
lambdaToken = void (charP 'λ' <|> charP '\\')

forallToken :: Parser ()
forallToken = void (charP '∀') <|> keyword "forall"

arrowToken :: Parser ()
arrowToken = void (charP '→') <|> void (literal "->")

whsp :: Parser ()
whsp = void (FP.many whitespaceChunk)

whsp1 :: Parser ()
whsp1 = void (FP.some whitespaceChunk)

whitespaceChunk :: Parser ()
whitespaceChunk =
  void (charP ' ')
    <|> void (charP '\t')
    <|> endOfLine
    <|> lineComment

endOfLine :: Parser ()
endOfLine =
  (void (charP '\r') *> void (charP '\n'))
    <|> void (charP '\n')

lineComment :: Parser ()
lineComment = do
  _ <- literal "--"
  _ <- FP.many (FP.satisfy (/= '\n'))
  _ <- (endOfLine *> pure True) <|> pure False
  pure ()

isNextDigit :: Parser Bool
isNextDigit =
  (FP.lookahead (FP.satisfy isDigit) *> pure True)
    <|> pure False

nextChar :: Parser (Maybe Char)
nextChar = FP.optional (FP.lookahead FP.anyChar)

keywordNames :: [Text]
keywordNames = ["let", "in", "forall"]

builtinNames :: [Text]
builtinNames = ["Natural", "Natural/fold", "Natural/subtract", "Type", "Kind"]
