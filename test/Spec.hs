module Main (main) where

import qualified MuFomega.SyntaxSpec as SyntaxSpec
import qualified MuFomega.ShiftSpec as ShiftSpec
import qualified MuFomega.SubstituteSpec as SubstituteSpec
import qualified MuFomega.TraversalSpec as TraversalSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  SyntaxSpec.spec
  TraversalSpec.spec
  ShiftSpec.spec
  SubstituteSpec.spec
