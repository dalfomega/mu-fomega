{-# LANGUAGE OverloadedStrings #-}

module MuFomega.ChurchSpec
  ( spec
  ) where

import MuFomega.Church
  ( churchAddAppText
  , churchSubAppText
  , churchToNaturalText
  , naturalToChurchText
  , parseChurchStrict
  )
import MuFomega.Normalize (normalizeStrict)
import MuFomega.Syntax.Strict (ExprStrict (SENatural))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "church encoding arithmetic" $ do
    it "converts and adds 123 + 456 correctly" $ do
      let exprText =
            churchToNaturalText (churchAddAppText (naturalToChurchText 123) (naturalToChurchText 456))
          result =
            case parseChurchStrict exprText of
              Right expr -> normalizeStrict expr
              Left err -> error ("church parse failed in add test: " <> err)
      result `shouldBe` SENatural 579

    it "converts and subtracts 1234 - 567 correctly" $ do
      let exprText =
            churchToNaturalText (churchSubAppText (naturalToChurchText 1234) (naturalToChurchText 567))
          result =
            case parseChurchStrict exprText of
              Right expr -> normalizeStrict expr
              Left err -> error ("church parse failed in sub test: " <> err)
      result `shouldBe` SENatural 667
