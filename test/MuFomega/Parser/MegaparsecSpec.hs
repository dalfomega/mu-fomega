{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Parser.MegaparsecSpec
  ( spec
  ) where

import qualified MuFomega.Parser.CommonGrammarSpec as Common
import qualified MuFomega.Parser.Megaparsec as Mega
import Test.Hspec (Spec)

spec :: Spec
spec =
  Common.grammarSpec
    Common.GrammarBackend
      { Common.backendName = "megaparsec"
      , Common.backendParseExpr = either (Left . show) Right . Mega.parseExpr
      , Common.backendParseExprWithCategory = mapLeft convertCategory . Mega.parseExprWithCategory
      }
  where
    convertCategory category =
      case category of
        Mega.UnexpectedEndOfInput -> Common.UnexpectedEndOfInput
        Mega.UnexpectedToken -> Common.UnexpectedToken

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f value =
  case value of
    Left err -> Left (f err)
    Right ok -> Right ok
