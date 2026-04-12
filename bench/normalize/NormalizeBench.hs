module Main (main) where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf)
import MuFomega.Normalize (normalizeLazy, normalizeStrict)
import MuFomega.Syntax.Lazy
  ( ExprLazy
      ( EAnnot
      , EApp
      , EBinOp
      , EBuiltin
      , EForall
      , ELam
      , ELet
      , ENatural
      , EVar
      )
  )
import MuFomega.Syntax.Strict
  ( ExprStrict
      ( SEAnnot
      , SEApp
      , SEBinOp
      , SEBuiltin
      , SEForall
      , SELam
      , SELet
      , SENatural
      , SEVar
      )
  )
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
          , bench "lazy" (nf normalizeLazy (toLazyExpr exprStrict))
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
          , bench "strict" (nf normalizeStrict (toStrictExpr exprLazy))
          ]

toStrictExpr :: ExprLazy -> ExprStrict
toStrictExpr expr =
  case expr of
    ENatural n -> SENatural n
    EBuiltin b -> SEBuiltin b
    EVar v -> SEVar v
    EAnnot body tipe -> SEAnnot (toStrictExpr body) (toStrictExpr tipe)
    ELam name tipe body -> SELam name (toStrictExpr tipe) (toStrictExpr body)
    EForall name tipe body -> SEForall name (toStrictExpr tipe) (toStrictExpr body)
    ELet name value body -> SELet name (toStrictExpr value) (toStrictExpr body)
    EApp f x -> SEApp (toStrictExpr f) (toStrictExpr x)
    EBinOp op l r -> SEBinOp op (toStrictExpr l) (toStrictExpr r)

toLazyExpr :: ExprStrict -> ExprLazy
toLazyExpr expr =
  case expr of
    SENatural n -> ENatural n
    SEBuiltin b -> EBuiltin b
    SEVar v -> EVar v
    SEAnnot body tipe -> EAnnot (toLazyExpr body) (toLazyExpr tipe)
    SELam name tipe body -> ELam name (toLazyExpr tipe) (toLazyExpr body)
    SEForall name tipe body -> EForall name (toLazyExpr tipe) (toLazyExpr body)
    SELet name value body -> ELet name (toLazyExpr value) (toLazyExpr body)
    SEApp f x -> EApp (toLazyExpr f) (toLazyExpr x)
    SEBinOp op l r -> EBinOp op (toLazyExpr l) (toLazyExpr r)
