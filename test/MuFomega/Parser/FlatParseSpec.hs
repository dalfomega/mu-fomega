{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Parser.FlatParseSpec
  ( spec
  ) where

import qualified MuFomega.Parser.CommonGrammarSpec as Common
import qualified MuFomega.Parser.FlatParse as Flat
import Test.Hspec (Spec)

spec :: Spec
spec =
  Common.grammarSpec
    Common.GrammarBackend
      { Common.backendName = "flatparse"
      , Common.backendParseExpr = Flat.parseExpr
      , Common.backendParseExprWithCategory = mapLeft convertCategory . Flat.parseExprWithCategory
      }
  where
    convertCategory category =
      case category of
        Flat.UnexpectedEndOfInput -> Common.UnexpectedEndOfInput
        Flat.UnexpectedToken -> Common.UnexpectedToken

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f value =
  case value of
    Left err -> Left (f err)
    Right ok -> Right ok
