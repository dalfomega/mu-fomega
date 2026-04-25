module Main (main) where

import qualified Small
import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
    describe "Small.hello" $ do
        it "returns a greeting" $ do
            Small.hello `shouldBe` "Hello from small"
