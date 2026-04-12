module Main (main) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf)
import MuFomega.Normalize (normalizeLazy, normalizeStrict)
import MuFomega.Syntax.Convert (toLazy, toStrict)
import MuFomega.Syntax.Lazy (ExprLazy)
import MuFomega.Syntax.Strict (ExprStrict)
import NormalizeWorkloads (builtInOpWorkload, churchArithmeticWorkloadStrict)

main :: IO ()
main =
  defaultMain
    [ strictWorkloadGroup "church-arithmetic" [20, 30, 40] churchArithmeticWorkloadStrict
    , lazyWorkloadGroup "builtin-ops" [1000, 2000, 3000] builtInOpWorkload
    ]

strictWorkloadGroup :: String -> [Int] -> (Int -> ExprStrict) -> Benchmark
strictWorkloadGroup workloadName sizes mkExpr =
  bgroup workloadName (map sizeCase sizes)
  where
    sizeCase n =
      env (pure (mkExpr n)) $ \exprStrict ->
        bgroup
          ("n=" <> show n)
          [ bench "strict" (nf normalizeStrict exprStrict)
          , bench "lazy" (nf normalizeLazy (toLazy exprStrict))
          ]

lazyWorkloadGroup :: String -> [Int] -> (Int -> ExprLazy) -> Benchmark
lazyWorkloadGroup workloadName sizes mkExpr =
  bgroup workloadName (map sizeCase sizes)
  where
    sizeCase n =
      env (pure (mkExpr n)) $ \exprLazy ->
        bgroup
          ("n=" <> show n)
          [ bench "lazy" (nf normalizeLazy exprLazy)
          , bench "strict" (nf normalizeStrict (toStrict exprLazy))
          ]
