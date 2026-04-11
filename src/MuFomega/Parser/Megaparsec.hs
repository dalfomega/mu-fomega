{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Parser.Megaparsec
  ( ParseFailureCategory (..)
  , parseExpr
  , parseExprWithCategory
  ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (Kind, Natural, NaturalFold, NaturalSubtract, Type), Var (Var))
import MuFomega.Syntax.Lazy (ExprLazy (EAnnot, EApp, EBinOp, EBuiltin, EForall, ELam, ELet, ENatural, EVar))
import Text.Megaparsec
  ( ParseError (TrivialError)
  , ParseErrorBundle (bundleErrors)
  , Parsec
  , anySingle
  , choice
  , eof
  , lookAhead
  , many
  , manyTill
  , notFollowedBy
  , optional
  , parse
  , satisfy
  , some
  , try
  , (<|>)
  )
import Text.Megaparsec.Char (alphaNumChar, char, eol, letterChar, string)
import Text.Megaparsec.Error (ErrorItem (EndOfInput))
import Data.Void (Void)

type Parser = Parsec Void Text

data ParseFailureCategory
  = UnexpectedEndOfInput
  | UnexpectedToken
  deriving (Eq, Show)

parseExpr :: Text -> Either (ParseErrorBundle Text Void) ExprLazy
parseExpr input = parse fullExpression (sourceName input) input

parseExprWithCategory :: Text -> Either ParseFailureCategory ExprLazy
parseExprWithCategory = first classifyFailure . parseExpr

fullExpression :: Parser ExprLazy
fullExpression = do
  whsp
  expr <- expression
  whsp
  eof
  pure expr

expression :: Parser ExprLazy
expression =
  choice
    [ try lambdaExpression
    , try letExpression
    , try forallExpression
    , try arrowExpression
    , annotatedExpression
    ]

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
  bindings <- some (try letBinding)
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

arrowExpression :: Parser ExprLazy
arrowExpression = do
  domain <- operatorExpression
  whsp
  arrowToken
  whsp
  codomain <- expression
  pure (EForall "_" domain codomain)

annotatedExpression :: Parser ExprLazy
annotatedExpression = do
  body <- operatorExpression
  mTipe <- optional $ try $ do
    whsp
    _ <- char ':'
    whsp1
    expression
  pure $ maybe body (EAnnot body) mTipe

operatorExpression :: Parser ExprLazy
operatorExpression = plusExpression

plusExpression :: Parser ExprLazy
plusExpression = do
  firstTerm <- timesExpression
  rest <- many $ try $ do
    whsp
    _ <- char '+'
    whsp
    timesExpression
  pure (foldl (EBinOp Plus) firstTerm rest)

timesExpression :: Parser ExprLazy
timesExpression = do
  firstTerm <- applicationExpression
  rest <- many $ try $ do
    whsp
    _ <- char '*'
    whsp
    applicationExpression
  pure (foldl (EBinOp Times) firstTerm rest)

applicationExpression :: Parser ExprLazy
applicationExpression = do
  fn <- primitiveExpression
  args <- many $ try (whsp1 *> primitiveExpression)
  pure (foldl EApp fn args)

primitiveExpression :: Parser ExprLazy
primitiveExpression =
  choice
    [ ENatural <$> naturalLiteral
    , try identifierExpression
    , parenthesizedExpression
    ]

identifierExpression :: Parser ExprLazy
identifierExpression =
  choice
    [ EVar <$> try variable
    , EBuiltin <$> builtin
    ]

variable :: Parser Var
variable = do
  name <- nonReservedLabel
  index <- optional $ try $ do
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
  pure $! inner

naturalLiteral :: Parser Integer
naturalLiteral = zeroLiteral <|> nonZeroLiteral
  where
    zeroLiteral = do
      _ <- char '0'
      notFollowedBy (satisfy isDigit)
      pure 0

    nonZeroLiteral = do
      firstDigit <- satisfy (`elem` ['1' .. '9'])
      restDigits <- many (satisfy isDigit)
      pure (read (firstDigit : restDigits))

naturalIndex :: Parser Word
naturalIndex = fromInteger <$> naturalLiteral

builtin :: Parser Builtin
builtin =
  choice
    [ builtinToken "Natural/subtract" *> pure NaturalSubtract
    , builtinToken "Natural/fold" *> pure NaturalFold
    , builtinToken "Natural" *> pure Natural
    , builtinToken "Type" *> pure Type
    , builtinToken "Kind" *> pure Kind
    ]

nonReservedLabel :: Parser Text
nonReservedLabel = do
  name <- label
  if name `elem` builtinNames then empty else pure name

label :: Parser Text
label = do
  firstChar <- labelFirstChar
  rest <- many labelNextChar
  let name = Text.pack (firstChar : rest)
  if name `elem` keywordNames then empty else pure name

labelFirstChar :: Parser Char
labelFirstChar = letterChar <|> char '_'

labelNextChar :: Parser Char
labelNextChar = alphaNumChar <|> satisfy (`elem` ['-', '/', '_'])

keyword :: Text -> Parser ()
keyword token = void (try (string token <* notFollowedBy labelNextChar))

builtinToken :: Text -> Parser ()
builtinToken token = void (try (string token <* notFollowedBy labelNextChar))

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
  choice
    [ void (char ' ')
    , void (char '\t')
    , void eol
    , lineComment
    ]

lineComment :: Parser ()
lineComment = do
  _ <- string "--"
  _ <- manyTill anySingle (lookAhead (void eol <|> eof))
  void eol <|> eof

keywordNames :: [Text]
keywordNames = ["let", "in", "forall"]

builtinNames :: [Text]
builtinNames = ["Natural", "Natural/fold", "Natural/subtract", "Type", "Kind"]

sourceName :: Text -> String
sourceName input
  | Text.null input = ""
  | otherwise = "<microfomega>"

classifyFailure :: ParseErrorBundle Text Void -> ParseFailureCategory
classifyFailure bundle =
  case bundleErrors bundle of
    TrivialError _ unexpected expected :| _
      | unexpected == Just EndOfInput -> UnexpectedEndOfInput
      | isNothing unexpected && null expected -> UnexpectedEndOfInput
    _ -> UnexpectedToken
