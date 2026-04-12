{-# LANGUAGE OverloadedStrings #-}

module NormalizeWorkloads (
    builtInOpWorkload,
    builtInOpWorkloadRightAssociated,
    betaRedexChainWorkload,
    readmeWorkloadLazy,
    churchArithmeticWorkloadLazy,
    churchArithmeticWorkloadStrict,
) where

import qualified Data.Text as Text
import MuFomega.Church (churchAlternatingText, parseChurchStrict)
import qualified MuFomega.Parser.Attoparsec as Atto
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (Natural, NaturalSubtract), Var (Var))
import MuFomega.Syntax.Convert (toLazy)
import MuFomega.Syntax.Lazy (
    ExprLazy (
        EApp,
        EBinOp,
        EBuiltin,
        ELam,
        ENatural,
        EVar
    ),
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

builtInOpWorkloadRightAssociated :: Int -> ExprLazy
builtInOpWorkloadRightAssociated n =
    foldr step (ENatural 0) [1 .. max 1 n]
  where
    step i acc =
        let addPart = EBinOp Plus (ENatural (toInteger (i `mod` 11))) acc
            mulPart = EBinOp Times addPart (ENatural 1)
            subBy = ENatural (toInteger (i `mod` 3))
         in EApp (EApp (EBuiltin NaturalSubtract) subBy) mulPart

betaRedexChainWorkload :: Int -> ExprLazy
betaRedexChainWorkload n =
    foldr step (ENatural 0) [1 .. max 1 n]
  where
    step i acc =
        EApp
            (ELam "x" (EBuiltin Natural) (EBinOp Plus (EVar (Var "x" 0)) (ENatural (toInteger (i `mod` 7)))))
            acc

readmeWorkloadLazy :: ExprLazy
readmeWorkloadLazy =
    case Atto.parseExpr readmeProgram of
        Right expr -> expr
        Left err -> error ("failed to parse README workload: " <> err)

churchArithmeticWorkloadLazy :: Int -> ExprLazy
churchArithmeticWorkloadLazy = toLazy . churchArithmeticWorkloadStrict

churchArithmeticWorkloadStrict :: Int -> ExprStrict
churchArithmeticWorkloadStrict n =
    case parseChurchStrict (churchAlternatingText n) of
        Right expr -> expr
        Left err -> error ("failed to parse church benchmark workload: " <> err)

readmeProgram :: Text.Text
readmeProgram =
    Text.unlines
        [ "let f = λ(x : Natural) → λ(x : Natural) → (123 + x) * x@1"
        , "let id = λ(a : Type) → λ(x : a) → x"
        , "let type_of_id = ∀(a : Type) → a → a"
        , "let _ = id : type_of_id"
        , "let _ = type_of_id : Type"
        , "let _ = Type : Kind"
        , "let Void = ∀(r : Type) → r"
        , "let Unit = ∀(r : Type) → r → r"
        , "let Pair = λ(a : Type) → λ(b : Type) → ∀(r : Type) → (a → b → r) → r"
        , "in f (Natural/subtract 2 10) (id Natural 20)"
        ]
