{-# LANGUAGE OverloadedStrings #-}

module MuFomega.CLISpec
  ( spec
  ) where

import qualified Data.Text as Text
import MuFomega.CLI
  ( AstStrictness (AstLazy, AstStrict)
  , CliOptions (CliOptions)
  , EvaluatorBackend (EvalNbEParamHOAS, EvalSubst)
  , ParserBackend (ParserAttoparsec, ParserFlatParse, ParserMegaparsec)
  , astStrictness
  , defaultCliOptions
  , evaluatorBackend
  , inputFile
  , parseCliOptions
  , parserBackend
  , runPipeline
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "CLI option parsing" $ do
    it "parses explicit parser/evaluator/ast flags" $ do
      parseCliOptions ["--parser", "flatparse", "--evaluator", "nbe-param-hoas", "--ast", "strict", "--input", "prog.mf"]
        `shouldBe` Right (mkOpts ParserFlatParse EvalNbEParamHOAS AstStrict (Just "prog.mf"))

    it "accepts a positional input file" $ do
      parseCliOptions ["program.dhall"]
        `shouldBe` Right (mkOpts ParserMegaparsec EvalSubst AstLazy (Just "program.dhall"))

    it "rejects unknown parser backend" $ do
      parseCliOptions ["--parser", "unknown"]
        `shouldBe` Left "invalid parser backend: unknown"

  describe "CLI integration pipeline" $ do
    it "runs parse -> type-check -> normalize -> pretty with defaults" $ do
      runPipeline defaultCliOptions readmeProgram
        `shouldBe` Right "1144"

    it "supports strict AST with non-default backend selections" $ do
      let opts = mkOpts ParserAttoparsec EvalNbEParamHOAS AstStrict Nothing
      runPipeline opts readmeProgram `shouldBe` Right "1144"

    it "reports parse errors with source spans" $ do
      let result = runPipeline defaultCliOptions "let x ="
      result `shouldSatisfy` isSpannedParseError

    it "reports type errors with source spans" $ do
      let result = runPipeline (mkOpts ParserMegaparsec EvalSubst AstLazy Nothing) "x"
      result `shouldBe` Left "<input>:1:1: type error: top-level free variable x"

    it "supports flatparse + substitution + lazy for valid program" $ do
      let opts = mkOpts ParserFlatParse EvalSubst AstLazy Nothing
      runPipeline opts readmeProgram `shouldBe` Right "1144"

mkOpts :: ParserBackend -> EvaluatorBackend -> AstStrictness -> Maybe FilePath -> CliOptions
mkOpts parser evaluator strictness filePath =
  CliOptions
    { parserBackend = parser
    , evaluatorBackend = evaluator
    , astStrictness = strictness
    , inputFile = filePath
    }

isSpannedParseError :: Either Text.Text Text.Text -> Bool
isSpannedParseError result =
  case result of
    Left err -> "<microfomega>:" `Text.isPrefixOf` err || "<input>:" `Text.isPrefixOf` err
    Right _ -> False

readmeProgram :: Text.Text
readmeProgram =
  Text.unlines
    [ "let f = λ(x : Natural) → λ(x : Natural) → (123 + x) * x@1"
    , "let id = λ(a : Type) → λ(x : a) → x"
    , "let type_of_id = ∀(a : Type) → a → a"
    , "let _ = id : type_of_id"
    , "let _ = type_of_id : Type"
    , "let _ = Type : Kind"
    , "let Void = ∀(r : Type) → r"
    , "let Unit = ∀(r : Type) → r → r"
    , "let Pair = λ(a : Type) → λ(b : Type) → ∀(r : Type) → (a → b → r) → r"
    , "in f (Natural/subtract 2 10) (id Natural 20)"
    ]
