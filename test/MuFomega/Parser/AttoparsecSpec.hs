{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Parser.AttoparsecSpec (
    spec,
) where

import qualified MuFomega.Parser.Attoparsec as Atto
import qualified MuFomega.Parser.CommonGrammarSpec as Common
import Test.Hspec (Spec)

spec :: Spec
spec =
    Common.grammarSpec
        Common.GrammarBackend
            { Common.backendName = "attoparsec"
            , Common.backendParseExpr = Atto.parseExpr
            , Common.backendParseExprWithCategory = mapLeft convertCategory . Atto.parseExprWithCategory
            }
  where
    convertCategory category =
        case category of
            Atto.UnexpectedEndOfInput -> Common.UnexpectedEndOfInput
            Atto.UnexpectedToken -> Common.UnexpectedToken

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f value =
    case value of
        Left err -> Left (f err)
        Right ok -> Right ok
