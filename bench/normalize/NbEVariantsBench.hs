module Main (main) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf)
import qualified MuFomega.Eval.NbEDeBruijn as NbEDeBruijn
import qualified MuFomega.Eval.NbEHOAS as NbEHOAS
import qualified MuFomega.Eval.NbELocallyNameless as NbELocallyNameless
import qualified MuFomega.Eval.NbENamed as NbENamed
import qualified MuFomega.Eval.NbEParamHOAS as NbEParamHOAS
import qualified MuFomega.Normalize as Subst
import MuFomega.Syntax.Convert (toLazy, toStrict)
import MuFomega.Syntax.Lazy (ExprLazy)
import NormalizeWorkloads
  ( betaRedexChainWorkload
  , builtInOpWorkload
  , churchArithmeticWorkloadLazy
  , readmeWorkloadLazy
  )

main :: IO ()
main = do
  verifyCorrectness
  defaultMain
    [ benchReadme
    , benchFamily "builtin-ops" [500, 1000, 2000] builtInOpWorkload
    , benchFamily "beta-redex-chain" [500, 1000, 2000] betaRedexChainWorkload
    , benchFamily "church-arithmetic" [3, 6, 9] churchArithmeticWorkloadLazy
    ]

verifyCorrectness :: IO ()
verifyCorrectness =
  mapM_ assertParity verificationInputs

verificationInputs :: [ExprLazy]
verificationInputs =
  [ readmeWorkloadLazy
  , builtInOpWorkload 30
  , betaRedexChainWorkload 40
  , churchArithmeticWorkloadLazy 4
  ]

assertParity :: ExprLazy -> IO ()
assertParity expr =
  let expectedLazy = Subst.normalizeLazy expr
      expectedStrict = Subst.normalizeStrict (toStrict expr)
   in do
        assertEq "nbe-hoas lazy" (NbEHOAS.normalizeLazy expr) expectedLazy
        assertEq "nbe-named lazy" (NbENamed.normalizeLazy expr) expectedLazy
        assertEq "nbe-debruijn lazy" (NbEDeBruijn.normalizeLazy expr) expectedLazy
        assertEq "nbe-locally-nameless lazy" (NbELocallyNameless.normalizeLazy expr) expectedLazy
        assertEq "nbe-param-hoas lazy" (NbEParamHOAS.normalizeLazy expr) expectedLazy
        assertEq "nbe-hoas strict" (toLazy (NbEHOAS.normalizeStrict (toStrict expr))) (toLazy expectedStrict)
        assertEq "nbe-named strict" (toLazy (NbENamed.normalizeStrict (toStrict expr))) (toLazy expectedStrict)
        assertEq "nbe-debruijn strict" (toLazy (NbEDeBruijn.normalizeStrict (toStrict expr))) (toLazy expectedStrict)
        assertEq "nbe-locally-nameless strict" (toLazy (NbELocallyNameless.normalizeStrict (toStrict expr))) (toLazy expectedStrict)
        assertEq "nbe-param-hoas strict" (toLazy (NbEParamHOAS.normalizeStrict (toStrict expr))) (toLazy expectedStrict)

assertEq :: String -> ExprLazy -> ExprLazy -> IO ()
assertEq label actual expected =
  if actual == expected
    then pure ()
    else error (label <> " diverged from substitution normalizer")

benchReadme :: Benchmark
benchReadme =
  env (pure readmeWorkloadLazy) $ \expr ->
    bgroup
      "readme"
      [ bench "subst-lazy" (nf Subst.normalizeLazy expr)
      , bench "subst-strict" (nf (Subst.normalizeStrict . toStrict) expr)
      , bench "nbe-hoas-lazy" (nf NbEHOAS.normalizeLazy expr)
      , bench "nbe-named-lazy" (nf NbENamed.normalizeLazy expr)
      , bench "nbe-debruijn-lazy" (nf NbEDeBruijn.normalizeLazy expr)
      , bench "nbe-locally-nameless-lazy" (nf NbELocallyNameless.normalizeLazy expr)
      , bench "nbe-param-hoas-lazy" (nf NbEParamHOAS.normalizeLazy expr)
      ]

benchFamily :: String -> [Int] -> (Int -> ExprLazy) -> Benchmark
benchFamily workloadName sizes mkExpr =
  bgroup workloadName (map benchSize sizes)
  where
    benchSize n =
      env (pure (mkExpr n)) $ \expr ->
        bgroup
          ("n=" <> show n)
          [ bench "subst-lazy" (nf Subst.normalizeLazy expr)
          , bench "subst-strict" (nf (Subst.normalizeStrict . toStrict) expr)
          , bench "nbe-hoas-lazy" (nf NbEHOAS.normalizeLazy expr)
          , bench "nbe-named-lazy" (nf NbENamed.normalizeLazy expr)
          , bench "nbe-debruijn-lazy" (nf NbEDeBruijn.normalizeLazy expr)
          , bench "nbe-locally-nameless-lazy" (nf NbELocallyNameless.normalizeLazy expr)
          , bench "nbe-param-hoas-lazy" (nf NbEParamHOAS.normalizeLazy expr)
          ]
