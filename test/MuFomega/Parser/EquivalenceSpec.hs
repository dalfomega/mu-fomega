{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Parser.EquivalenceSpec
  ( spec
  ) where

import Data.Text (Text)
import qualified MuFomega.Parser.Attoparsec as Atto
import qualified MuFomega.Parser.FlatParse as Flat
import qualified MuFomega.Parser.Megaparsec as Mega
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec =
  describe "parser equivalence" $ do
    it "all parser backends agree on representative corpus" $
      map parserAgreement corpus `shouldSatisfy` and

parserAgreement :: Text -> Bool
parserAgreement input =
  case (Mega.parseExpr input, Atto.parseExpr input, Flat.parseExpr input) of
    (Right m, Right a, Right f) -> m == a && a == f
    (Left _, Left _, Left _) -> True
    _ -> False

corpus :: [Text]
corpus =
  [ "x"
  , "x@1"
  , "Natural"
  , "Natural/subtract 2 10"
  , "f x * y + z"
  , "\\(x : Natural) -> x"
  , "forall (a : Type) -> a"
  , "let x = 1 let y = x in y"
  , "01"
  , "()"
  , "x@-1"
  ]
