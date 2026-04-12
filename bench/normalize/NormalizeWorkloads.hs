{-# LANGUAGE OverloadedStrings #-}

module NormalizeWorkloads
  ( builtInOpWorkload
  , churchArithmeticWorkloadStrict
  ) where

import MuFomega.Church (churchAlternatingText, parseChurchStrict)
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (NaturalSubtract))
import MuFomega.Syntax.Lazy
  ( ExprLazy
      ( EApp
      , EBinOp
      , EBuiltin
      , ENatural
      )
  )
import MuFomega.Syntax.Strict (ExprStrict)

builtInOpWorkload :: Int -> ExprLazy
builtInOpWorkload n =
  foldl step (ENatural 0) [1 .. max 1 n]
  where
    step acc i =
      let addPart = EBinOp Plus acc (ENatural (toInteger (i `mod` 11)))
          mulPart = EBinOp Times addPart (ENatural 1)
          subBy = ENatural (toInteger (i `mod` 3))
       in EApp (EApp (EBuiltin NaturalSubtract) subBy) mulPart

churchArithmeticWorkloadStrict :: Int -> ExprStrict
churchArithmeticWorkloadStrict n =
  case parseChurchStrict (churchAlternatingText n) of
    Right expr -> expr
    Left err -> error ("failed to parse church benchmark workload: " <> err)
