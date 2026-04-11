{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import MuFomega.Syntax.Common (BinOp (Plus), Builtin (Natural), Var (Var))
import MuFomega.Syntax.Convert (toLazy, toStrict)
import MuFomega.Syntax.Lazy
  ( ExprLazy
      ( EApp
      , EBinOp
      , EBuiltin
      , ELam
      , ENatural
      , EVar
      )
  )

main :: IO ()
main =
  defaultMain
    [ bgroup
        "strict-vs-lazy"
        [ bench "toStrict" (nf toStrict sampleLazy)
        , bench "toLazy" (nf toLazy (toStrict sampleLazy))
        ]
    ]

sampleLazy :: ExprLazy
sampleLazy =
  EApp
    ( ELam
        "x"
        (EBuiltin Natural)
        (EBinOp Plus (EVar (Var "x" 0)) (ENatural 1))
    )
    (EBinOp Plus (ENatural 20) (ENatural 22))
