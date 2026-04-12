{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Eval.NbEParitySpec
  ( spec
  ) where

import qualified Data.Text as Text
import MuFomega.Church (churchAlternatingText, parseChurchStrict)
import qualified MuFomega.Eval.NbEDeBruijn as NbEDeBruijn
import qualified MuFomega.Eval.NbEHOAS as NbEHOAS
import qualified MuFomega.Eval.NbELocallyNameless as NbELocallyNameless
import qualified MuFomega.Eval.NbENamed as NbENamed
import qualified MuFomega.Eval.NbEParamHOAS as NbEParamHOAS
import qualified MuFomega.Normalize as Subst
import MuFomega.Parser.Megaparsec (parseExpr)
import MuFomega.Syntax.Common
  ( BinOp (Plus, Times)
  , Builtin (Kind, Natural, NaturalFold, NaturalSubtract, Type)
  , Var (Var)
  )
import MuFomega.Syntax.Convert (toLazy, toStrict)
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
import MuFomega.TypeCheck (inferTypeLazy)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "NbE conformance" $ do
    it "matches substitution normalization for the README sample" $ do
      case parseExpr readmeProgram of
        Left bundle -> show bundle `shouldBe` "README sample program must parse"
        Right expr -> assertAllEvaluatorsAgreeLazy expr

    it "matches substitution normalization on church arithmetic workloads" $ do
      mapM_ assertChurchParity [1, 2, 3, 5, 8]

    it "matches substitution normalization on conformance suite" $
      mapM_ assertAllEvaluatorsAgreeLazy conformanceCases

  describe "NbE evaluator agreement on generated terms" $ do
    it "all evaluators agree on generated well-typed terms" $ property $
      allAgreeOnGeneratedWellTyped

allAgreeOnGeneratedWellTyped :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
allAgreeOnGeneratedWellTyped a b c d e =
  let expr = generatedWellTypedTerm a b c d e
   in inferTypeLazy expr == Right (EBuiltin Natural)
        && allAgreeLazy expr
        && allAgreeStrict expr

generatedWellTypedTerm :: Integer -> Integer -> Integer -> Integer -> Integer -> ExprLazy
generatedWellTypedTerm a b c d e =
  ELet
    "x"
    (ENatural (toNatural a))
    ( ELet
        "y"
        (ENatural (toNatural b))
        ( EApp
            ( EApp
                (EBuiltin NaturalSubtract)
                (ENatural (toNatural c))
            )
            ( EBinOp
                Plus
                (EBinOp Times (EVar (Var "x" 0)) (ENatural (toNatural d + 1)))
                ( EApp
                    ( EApp
                        ( EApp
                            ( EApp
                                (EBuiltin NaturalFold)
                                (ENatural (toNatural e `mod` 4))
                            )
                            (EBuiltin Natural)
                        )
                        (ELam "n" (EBuiltin Natural) (EBinOp Plus (EVar (Var "n" 0)) (ENatural 1)))
                    )
                    (EVar (Var "y" 0))
                )
            )
        )
    )

toNatural :: Integer -> Integer
toNatural n =
  abs n `mod` 20

assertChurchParity :: Int -> IO ()
assertChurchParity n =
  case parseChurchStrict (churchAlternatingText n) of
    Left err ->
      err `shouldBe` "church workload must parse"
    Right exprStrict -> do
      let expectedStrict = Subst.normalizeStrict exprStrict
      NbEHOAS.normalizeStrict exprStrict `shouldBe` expectedStrict
      NbENamed.normalizeStrict exprStrict `shouldBe` expectedStrict
      NbEDeBruijn.normalizeStrict exprStrict `shouldBe` expectedStrict
      NbELocallyNameless.normalizeStrict exprStrict `shouldBe` expectedStrict
      NbEParamHOAS.normalizeStrict exprStrict `shouldBe` expectedStrict

assertAllEvaluatorsAgreeLazy :: ExprLazy -> IO ()
assertAllEvaluatorsAgreeLazy expr = do
  let expected = Subst.normalizeLazy expr
      strictExpr = toStrict expr
      expectedStrict = Subst.normalizeStrict strictExpr
  NbEHOAS.normalizeLazy expr `shouldBe` expected
  NbENamed.normalizeLazy expr `shouldBe` expected
  NbEDeBruijn.normalizeLazy expr `shouldBe` expected
  NbELocallyNameless.normalizeLazy expr `shouldBe` expected
  NbEParamHOAS.normalizeLazy expr `shouldBe` expected

  toLazy (NbEHOAS.normalizeStrict strictExpr) `shouldBe` toLazy expectedStrict
  toLazy (NbENamed.normalizeStrict strictExpr) `shouldBe` toLazy expectedStrict
  toLazy (NbEDeBruijn.normalizeStrict strictExpr) `shouldBe` toLazy expectedStrict
  toLazy (NbELocallyNameless.normalizeStrict strictExpr) `shouldBe` toLazy expectedStrict
  toLazy (NbEParamHOAS.normalizeStrict strictExpr) `shouldBe` toLazy expectedStrict

allAgreeLazy :: ExprLazy -> Bool
allAgreeLazy expr =
  let expected = Subst.normalizeLazy expr
   in and
        [ NbEHOAS.normalizeLazy expr == expected
        , NbENamed.normalizeLazy expr == expected
        , NbEDeBruijn.normalizeLazy expr == expected
        , NbELocallyNameless.normalizeLazy expr == expected
        , NbEParamHOAS.normalizeLazy expr == expected
        ]

allAgreeStrict :: ExprLazy -> Bool
allAgreeStrict expr =
  let strictExpr = toStrict expr
      expected = toLazy (Subst.normalizeStrict strictExpr)
   in and
        [ toLazy (NbEHOAS.normalizeStrict strictExpr) == expected
        , toLazy (NbENamed.normalizeStrict strictExpr) == expected
        , toLazy (NbEDeBruijn.normalizeStrict strictExpr) == expected
        , toLazy (NbELocallyNameless.normalizeStrict strictExpr) == expected
        , toLazy (NbEParamHOAS.normalizeStrict strictExpr) == expected
        ]

conformanceCases :: [ExprLazy]
conformanceCases =
  [ ENatural 42
  , EApp (ENatural 1) (ENatural 2)
  , EApp (EBuiltin NaturalSubtract) (ENatural 2)
  , ELam "x" (EBuiltin Natural) (EVar (Var "x" 0))
  , ELam "x" (EBuiltin Natural) (ELam "x" (EBuiltin Natural) (EVar (Var "x" 1)))
  , EForall "x" (EBuiltin Type) (EVar (Var "x" 0))
  , EForall "x" (EBuiltin Type) (EVar (Var "x" 1))
  , EApp (ELam "x" (EBuiltin Natural) (EVar (Var "x" 0))) (EAnnot (ENatural 7) (EBuiltin Natural))
  , EApp (EApp (EBuiltin NaturalSubtract) (ENatural 2)) (ENatural 10)
  , EApp (EApp (EApp (EBuiltin NaturalSubtract) (ENatural 2)) (ENatural 10)) (ENatural 9)
  , EApp (EApp (EApp (EApp (EBuiltin NaturalFold) (ENatural 3)) (EBuiltin Natural)) (ELam "n" (EBuiltin Natural) (EBinOp Plus (EVar (Var "n" 0)) (ENatural 1)))) (ENatural 0)
  , EApp (EApp (EApp (EApp (EBuiltin NaturalFold) (EVar (Var "n" 0))) (EBuiltin Natural)) (ELam "n" (EBuiltin Natural) (EVar (Var "n" 0)))) (ENatural 0)
  , EApp (EApp (EApp (EApp (EBuiltin NaturalFold) (ENatural (-1))) (EBuiltin Natural)) (ELam "n" (EBuiltin Natural) (EVar (Var "n" 0)))) (ENatural 0)
  , EApp (EApp (EApp (EApp (EBuiltin NaturalFold) (ENatural 2)) (EBuiltin Natural)) (ELam "n" (EBuiltin Natural) (EBinOp Plus (EVar (Var "n" 0)) (ENatural 1)))) (ENatural 0)
  , EBinOp Plus (ENatural 0) (EVar (Var "x" 0))
  , EBinOp Plus (EVar (Var "x" 0)) (ENatural 0)
  , EBinOp Times (EVar (Var "x" 0)) (ENatural 2)
  , ELet "x" (ENatural 10) (EVar (Var "x" 0))
  , ELet "x" (ENatural 1) (ELet "x" (ENatural 2) (EVar (Var "x" 1)))
  , ELet "f" (ELam "x" (EBuiltin Natural) (EBinOp Plus (EVar (Var "x" 0)) (ENatural 1))) (EApp (EVar (Var "f" 0)) (ENatural 10))
  , ELam "x" (EBuiltin Natural) (EApp (EVar (Var "x" 1)) (EVar (Var "x" 0)))
  , EForall "x" (EBuiltin Type) (EForall "x" (EBuiltin Type) (EVar (Var "x" 1)))
  , EApp (EApp (EBuiltin NaturalSubtract) (EVar (Var "x" 0))) (ENatural 10)
  , EApp (EApp (EApp (EApp (EBuiltin NaturalFold) (ENatural 1)) (EBuiltin Kind)) (ELam "n" (EBuiltin Natural) (EVar (Var "n" 0)))) (ENatural 0)
  ]

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
