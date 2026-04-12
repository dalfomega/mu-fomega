module Main (main) where

import qualified MuFomega.CLISpec as CLISpec
import qualified MuFomega.ChurchSpec as ChurchSpec
import qualified MuFomega.Eval.NbEParitySpec as NbEParitySpec
import qualified MuFomega.NormalizeSpec as NormalizeSpec
import qualified MuFomega.Parser.AttoparsecSpec as AttoparsecSpec
import qualified MuFomega.Parser.EquivalenceSpec as EquivalenceSpec
import qualified MuFomega.Parser.FlatParseSpec as FlatParseSpec
import qualified MuFomega.Parser.MegaparsecSpec as MegaparsecSpec
import qualified MuFomega.PrettySpec as PrettySpec
import qualified MuFomega.ShiftSpec as ShiftSpec
import qualified MuFomega.SubstituteSpec as SubstituteSpec
import qualified MuFomega.SyntaxSpec as SyntaxSpec
import qualified MuFomega.TraversalSpec as TraversalSpec
import qualified MuFomega.TypeCheckSpec as TypeCheckSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    SyntaxSpec.spec
    TraversalSpec.spec
    ShiftSpec.spec
    SubstituteSpec.spec
    ChurchSpec.spec
    NormalizeSpec.spec
    NbEParitySpec.spec
    MegaparsecSpec.spec
    AttoparsecSpec.spec
    FlatParseSpec.spec
    EquivalenceSpec.spec
    PrettySpec.spec
    TypeCheckSpec.spec
    CLISpec.spec
