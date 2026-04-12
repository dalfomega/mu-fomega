{-# LANGUAGE OverloadedStrings #-}

module MuFomega.CLI
  ( ParserBackend (..)
  , EvaluatorBackend (..)
  , AstStrictness (..)
  , CliOptions (..)
  , defaultCliOptions
  , usageText
  , parseCliOptions
  , runPipeline
  ) where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified MuFomega.Eval.NbEDeBruijn as NbEDeBruijn
import qualified MuFomega.Eval.NbEHOAS as NbEHOAS
import qualified MuFomega.Eval.NbELocallyNameless as NbELocallyNameless
import qualified MuFomega.Eval.NbENamed as NbENamed
import qualified MuFomega.Eval.NbEParamHOAS as NbEParamHOAS
import qualified MuFomega.Normalize as Subst
import qualified MuFomega.Parser.Attoparsec as Atto
import qualified MuFomega.Parser.FlatParse as Flat
import qualified MuFomega.Parser.Megaparsec as Mega
import MuFomega.Pretty (prettyLazy, prettyStrict)
import MuFomega.Syntax.Common (Var (Var))
import MuFomega.Syntax.Convert (toStrict)
import MuFomega.Syntax.Lazy (ExprLazy)
import MuFomega.Syntax.Strict (ExprStrict)
import MuFomega.TypeCheck
  ( TypeError (KindHasNoType, InvalidTypeExpression, NotAFunction, TopLevelFreeVariable, TypeMismatch, UnboundVariable)
  , inferTypeLazy
  , inferTypeStrict
  )
import qualified Text.Megaparsec as MP

data ParserBackend
  = ParserMegaparsec
  | ParserAttoparsec
  | ParserFlatParse
  deriving (Eq, Show)

data EvaluatorBackend
  = EvalSubst
  | EvalNbEHOAS
  | EvalNbENamed
  | EvalNbEDeBruijn
  | EvalNbELocallyNameless
  | EvalNbEParamHOAS
  deriving (Eq, Show)

data AstStrictness
  = AstLazy
  | AstStrict
  deriving (Eq, Show)

data CliOptions = CliOptions
  { parserBackend :: ParserBackend
  , evaluatorBackend :: EvaluatorBackend
  , astStrictness :: AstStrictness
  , inputFile :: Maybe FilePath
  }
  deriving (Eq, Show)

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions
    { parserBackend = ParserMegaparsec
    , evaluatorBackend = EvalSubst
    , astStrictness = AstLazy
    , inputFile = Nothing
    }

usageText :: Text
usageText =
  Text.unlines
    [ "Usage: mu-fomega [--parser BACKEND] [--evaluator BACKEND] [--ast lazy|strict] [--input FILE]"
    , ""
    , "Parsers:    megaparsec | attoparsec | flatparse"
    , "Evaluators: subst | nbe-hoas | nbe-named | nbe-debruijn | nbe-locally-nameless | nbe-param-hoas"
    , "AST mode:   lazy | strict"
    ]

parseCliOptions :: [String] -> Either Text CliOptions
parseCliOptions = go defaultCliOptions
  where
    go opts args =
      case args of
        [] -> Right opts
        "--parser" : value : rest ->
          case parseParserBackend value of
            Just parsed -> go (opts {parserBackend = parsed}) rest
            Nothing -> Left ("invalid parser backend: " <> Text.pack value)
        "--evaluator" : value : rest ->
          case parseEvaluatorBackend value of
            Just parsed -> go (opts {evaluatorBackend = parsed}) rest
            Nothing -> Left ("invalid evaluator backend: " <> Text.pack value)
        "--ast" : value : rest ->
          case parseAstStrictness value of
            Just parsed -> go (opts {astStrictness = parsed}) rest
            Nothing -> Left ("invalid AST strictness: " <> Text.pack value)
        "--input" : value : rest ->
          go (opts {inputFile = Just value}) rest
        "-p" : value : rest -> go opts ("--parser" : value : rest)
        "-e" : value : rest -> go opts ("--evaluator" : value : rest)
        "-a" : value : rest -> go opts ("--ast" : value : rest)
        "-i" : value : rest -> go opts ("--input" : value : rest)
        flag : _
          | "-" `Text.isPrefixOf` Text.pack flag -> Left ("unknown flag: " <> Text.pack flag)
        path : rest ->
          case inputFile opts of
            Nothing -> go (opts {inputFile = Just path}) rest
            Just _ -> Left "multiple input files provided"

runPipeline :: CliOptions -> Text -> Either Text Text
runPipeline opts source = do
  parsed <- parseWithBackend (parserBackend opts) source
  checkTypeWithSpan (astStrictness opts) source parsed
  let rendered =
        case astStrictness opts of
          AstLazy ->
            let normalized = normalizeLazyBy (evaluatorBackend opts) parsed
             in lazyToStrictText (prettyLazy normalized)
          AstStrict ->
            let normalized = normalizeStrictBy (evaluatorBackend opts) (toStrict parsed)
             in lazyToStrictText (prettyStrict normalized)
  Right rendered

parseParserBackend :: String -> Maybe ParserBackend
parseParserBackend value =
  case value of
    "megaparsec" -> Just ParserMegaparsec
    "attoparsec" -> Just ParserAttoparsec
    "flatparse" -> Just ParserFlatParse
    _ -> Nothing

parseEvaluatorBackend :: String -> Maybe EvaluatorBackend
parseEvaluatorBackend value =
  case value of
    "subst" -> Just EvalSubst
    "nbe-hoas" -> Just EvalNbEHOAS
    "nbe-named" -> Just EvalNbENamed
    "nbe-debruijn" -> Just EvalNbEDeBruijn
    "nbe-locally-nameless" -> Just EvalNbELocallyNameless
    "nbe-param-hoas" -> Just EvalNbEParamHOAS
    _ -> Nothing

parseAstStrictness :: String -> Maybe AstStrictness
parseAstStrictness value =
  case value of
    "lazy" -> Just AstLazy
    "strict" -> Just AstStrict
    _ -> Nothing

parseWithBackend :: ParserBackend -> Text -> Either Text ExprLazy
parseWithBackend backend source =
  case backend of
    ParserMegaparsec ->
      first (Text.pack . MP.errorBundlePretty) (Mega.parseExpr source)
    ParserAttoparsec ->
      case Atto.parseExpr source of
        Right expr -> Right expr
        Left msg -> Left (recoverParseSpan source msg)
    ParserFlatParse ->
      case Flat.parseExpr source of
        Right expr -> Right expr
        Left msg -> Left (recoverParseSpan source msg)

recoverParseSpan :: Text -> String -> Text
recoverParseSpan source fallbackMessage =
  case Mega.parseExpr source of
    Left bundle -> Text.pack (MP.errorBundlePretty bundle)
    Right _ ->
      "<input>:1:1: parse error: " <> Text.pack fallbackMessage

checkTypeWithSpan :: AstStrictness -> Text -> ExprLazy -> Either Text ()
checkTypeWithSpan strictness source expr =
  case strictness of
    AstLazy ->
      case inferTypeLazy expr of
        Right _ -> Right ()
        Left err -> Left (renderTypeError source err)
    AstStrict ->
      case inferTypeStrict (toStrict expr) of
        Right _ -> Right ()
        Left err -> Left (renderTypeError source err)

normalizeLazyBy :: EvaluatorBackend -> ExprLazy -> ExprLazy
normalizeLazyBy backend expr =
  case backend of
    EvalSubst -> Subst.normalizeLazy expr
    EvalNbEHOAS -> NbEHOAS.normalizeLazy expr
    EvalNbENamed -> NbENamed.normalizeLazy expr
    EvalNbEDeBruijn -> NbEDeBruijn.normalizeLazy expr
    EvalNbELocallyNameless -> NbELocallyNameless.normalizeLazy expr
    EvalNbEParamHOAS -> NbEParamHOAS.normalizeLazy expr

normalizeStrictBy :: EvaluatorBackend -> ExprStrict -> ExprStrict
normalizeStrictBy backend expr =
  case backend of
    EvalSubst -> Subst.normalizeStrict expr
    EvalNbEHOAS -> NbEHOAS.normalizeStrict expr
    EvalNbENamed -> NbENamed.normalizeStrict expr
    EvalNbEDeBruijn -> NbEDeBruijn.normalizeStrict expr
    EvalNbELocallyNameless -> NbELocallyNameless.normalizeStrict expr
    EvalNbEParamHOAS -> NbEParamHOAS.normalizeStrict expr

renderTypeError :: Text -> TypeError -> Text
renderTypeError source err =
  let (line, col) = typeErrorLocation source err
   in "<input>:" <> tshow line <> ":" <> tshow col <> ": type error: " <> typeErrorMessage err

typeErrorLocation :: Text -> TypeError -> (Int, Int)
typeErrorLocation source err =
  case err of
    UnboundVariable var -> locationForVar source var
    TopLevelFreeVariable var -> locationForVar source var
    _ -> (1, 1)

locationForVar :: Text -> Var -> (Int, Int)
locationForVar source var =
  case locateVar source var of
    Just offset -> offsetToLineCol source offset
    Nothing -> (1, 1)

locateVar :: Text -> Var -> Maybe Int
locateVar source (Var name idx)
  | idx == 0 =
      firstOffset source name
  | otherwise =
      firstOffset source (name <> "@" <> tshow idx)

firstOffset :: Text -> Text -> Maybe Int
firstOffset source needle
  | Text.null needle = Nothing
  | otherwise =
      let (prefix, suffix) = Text.breakOn needle source
       in if Text.null suffix then Nothing else Just (Text.length prefix)

offsetToLineCol :: Text -> Int -> (Int, Int)
offsetToLineCol source offset =
  go 1 1 0 (Text.unpack source)
  where
    target = max 0 offset

    go line col consumed chars
      | consumed >= target = (line, col)
      | otherwise =
          case chars of
            [] -> (line, col)
            c : rest
              | c == '\n' -> go (line + 1) 1 (consumed + 1) rest
              | otherwise -> go line (col + 1) (consumed + 1) rest

typeErrorMessage :: TypeError -> Text
typeErrorMessage err =
  case err of
    TypeMismatch expected actual ->
      "expected " <> exprSummary expected <> " but inferred " <> exprSummary actual
    NotAFunction actual ->
      "non-function in application: " <> exprSummary actual
    UnboundVariable var ->
      "unbound variable " <> varSummary var
    InvalidTypeExpression expr ->
      "invalid type expression: " <> exprSummary expr
    KindHasNoType ->
      "Kind has no type"
    TopLevelFreeVariable var ->
      "top-level free variable " <> varSummary var

exprSummary :: Show a => a -> Text
exprSummary = Text.pack . show

varSummary :: Var -> Text
varSummary (Var name idx)
  | idx == 0 = name
  | otherwise = name <> "@" <> tshow idx

tshow :: Show a => a -> Text
tshow = Text.pack . show

lazyToStrictText :: Lazy.Text -> Text
lazyToStrictText = Text.pack . Lazy.unpack
