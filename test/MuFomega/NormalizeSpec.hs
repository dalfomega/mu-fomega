{-# LANGUAGE OverloadedStrings #-}

module MuFomega.NormalizeSpec (
    spec,
) where

import qualified Data.Text as Text
import MuFomega.Gen (AnyExprLazy (..))
import MuFomega.Normalize (
    alphaNormalizeLazy,
    alphaNormalizeStrict,
    betaNormalizeLazy,
    betaNormalizeStrict,
    normalizeLazy,
    normalizeStrict,
 )
import MuFomega.Parser.Megaparsec (parseExpr)
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (Natural, NaturalFold, NaturalSubtract, Type), Var (Var))
import MuFomega.Syntax.Convert (toLazy, toStrict)
import MuFomega.Syntax.Lazy (
    ExprLazy (
        EAnnot,
        EApp,
        EBinOp,
        EBuiltin,
        EForall,
        ELam,
        ELet,
        ENatural,
        EVar
    ),
 )
import MuFomega.Syntax.Strict (
    ExprStrict (
        SEAnnot,
        SEApp,
        SEBinOp,
        SEBuiltin,
        SEForall,
        SELam,
        SELet,
        SENatural,
        SEVar
    ),
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
    describe "normalization examples" $ do
        it "normalizes the README sample program to 1144" $ do
            let expected = ENatural 1144
            case parseExpr readmeProgram of
                Left bundle ->
                    show bundle `shouldBe` "README sample program must parse"
                Right expr -> do
                    normalizeLazy expr `shouldBe` expected
                    toLazy (normalizeStrict (toStrict expr)) `shouldBe` expected

        it "normalizes symbolically under lambda" $
            normalizeLazy symbolicUnderLambdaInput `shouldBe` symbolicUnderLambdaExpected

        it "alpha-normalizes nested binders to underscore with indices" $
            alphaNormalizeLazy nestedBinderInput `shouldBe` nestedBinderExpected

        it "alpha-normalization shifts free underscore indices under binders" $
            alphaNormalizeLazy freeUnderscoreInput `shouldBe` freeUnderscoreExpected

        it "beta-normalizes under forall and let" $
            betaNormalizeLazy forallLetInput `shouldBe` forallLetExpected

        it "beta-normalizes Natural/subtract according to max 0 (b - a)" $ do
            normalizeLazy (EApp (EApp (EBuiltin NaturalSubtract) (ENatural 2)) (ENatural 10))
                `shouldBe` ENatural 8
            normalizeLazy (EApp (EApp (EBuiltin NaturalSubtract) (ENatural 10)) (ENatural 2))
                `shouldBe` ENatural 0

        it "beta-normalizes Natural/fold n T succ zero as succ^n zero" $
            normalizeLazy foldInput `shouldBe` ENatural 3

        it "keeps partial builtin applications unchanged" $ do
            betaNormalizeLazy (EApp (EBuiltin NaturalSubtract) (ENatural 2))
                `shouldBe` EApp (EBuiltin NaturalSubtract) (ENatural 2)
            betaNormalizeLazy (EApp (EApp (EBuiltin NaturalFold) (ENatural 2)) (EBuiltin Natural))
                `shouldBe` EApp (EApp (EBuiltin NaturalFold) (ENatural 2)) (EBuiltin Natural)

        it "preserves builtin application shape for symbolic Natural/subtract" $
            betaNormalizeLazy symbolicSubtractWithRestInput `shouldBe` symbolicSubtractWithRestExpected

        it "preserves builtin application shape for symbolic Natural/fold" $
            betaNormalizeLazy symbolicFoldWithRestInput `shouldBe` symbolicFoldWithRestExpected

        it "applies extra arguments after successful Natural/fold reduction" $
            betaNormalizeLazy foldWithRestInput `shouldBe` foldWithRestExpected

        it "keeps non-reducible arithmetic symbolic" $
            betaNormalizeLazy symbolicTimesInput `shouldBe` symbolicTimesExpected

        it "erases annotations during beta-normalization" $
            betaNormalizeLazy (EAnnot (ENatural 7) (EBuiltin Natural)) `shouldBe` ENatural 7

    describe "strict normalization examples" $ do
        it "matches alpha-normalization behavior for free underscore" $
            alphaNormalizeStrict freeUnderscoreStrictInput `shouldBe` freeUnderscoreStrictExpected

        it "matches beta-normalization behavior for symbolic builtins and lets" $ do
            betaNormalizeStrict symbolicSubtractWithRestStrictInput `shouldBe` symbolicSubtractWithRestStrictExpected
            betaNormalizeStrict forallLetStrictInput `shouldBe` forallLetStrictExpected
            betaNormalizeStrict symbolicTimesStrictInput `shouldBe` symbolicTimesStrictExpected
            betaNormalizeStrict foldWithRestStrictInput `shouldBe` foldWithRestStrictExpected

    describe "normalization properties" $ do
        it "is idempotent for lazy normalization" $ property $ \(AnyExprLazy expr) ->
            normalizeLazy (normalizeLazy expr) == normalizeLazy expr

        it "is idempotent for strict normalization" $ property $ \(AnyExprLazy expr) ->
            let strict = toStrict expr
             in normalizeStrict (normalizeStrict strict) == normalizeStrict strict

        it "has lazy/strict parity via conversion" $ property $ \(AnyExprLazy expr) ->
            toLazy (normalizeStrict (toStrict expr)) == normalizeLazy expr

symbolicUnderLambdaInput :: ExprLazy
symbolicUnderLambdaInput =
    ELam
        "x"
        (EBuiltin Natural)
        (EApp (ELam "y" (EBuiltin Natural) (EBinOp Plus (EVar (Var "x" 0)) (EVar (Var "y" 0)))) (ENatural 123))

symbolicUnderLambdaExpected :: ExprLazy
symbolicUnderLambdaExpected =
    ELam "_" (EBuiltin Natural) (EBinOp Plus (EVar (Var "_" 0)) (ENatural 123))

nestedBinderInput :: ExprLazy
nestedBinderInput = ELam "a" (EBuiltin Type) (ELam "b" (EBuiltin Type) (EVar (Var "a" 0)))

nestedBinderExpected :: ExprLazy
nestedBinderExpected = ELam "_" (EBuiltin Type) (ELam "_" (EBuiltin Type) (EVar (Var "_" 1)))

freeUnderscoreInput :: ExprLazy
freeUnderscoreInput = ELam "x" (EBuiltin Type) (EVar (Var "_" 0))

freeUnderscoreExpected :: ExprLazy
freeUnderscoreExpected = ELam "_" (EBuiltin Type) (EVar (Var "_" 1))

forallLetInput :: ExprLazy
forallLetInput =
    EForall
        "a"
        (EAnnot (ENatural 1) (EBuiltin Natural))
        (ELet "x" (ENatural 2) (EApp (ELam "y" (EBuiltin Natural) (EVar (Var "x" 1))) (ENatural 5)))

forallLetExpected :: ExprLazy
forallLetExpected = EForall "a" (ENatural 1) (EVar (Var "x" 0))

foldInput :: ExprLazy
foldInput =
    EApp
        ( EApp
            ( EApp
                (EApp (EBuiltin NaturalFold) (ENatural 3))
                (EBuiltin Natural)
            )
            (ELam "n" (EBuiltin Natural) (EBinOp Plus (EVar (Var "n" 0)) (ENatural 1)))
        )
        (ENatural 0)

foldWithRestInput :: ExprLazy
foldWithRestInput = EApp foldInput (ENatural 99)

foldWithRestExpected :: ExprLazy
foldWithRestExpected = EApp (ENatural 3) (ENatural 99)

symbolicSubtractWithRestInput :: ExprLazy
symbolicSubtractWithRestInput =
    EApp
        (EApp (EApp (EBuiltin NaturalSubtract) (EVar (Var "x" 0))) (ENatural 10))
        (ENatural 9)

symbolicSubtractWithRestExpected :: ExprLazy
symbolicSubtractWithRestExpected = symbolicSubtractWithRestInput

symbolicFoldWithRestInput :: ExprLazy
symbolicFoldWithRestInput =
    EApp
        ( EApp
            ( EApp
                ( EApp
                    (EBuiltin NaturalFold)
                    (EVar (Var "n" 0))
                )
                (EBuiltin Natural)
            )
            (ELam "k" (EBuiltin Natural) (EVar (Var "k" 0)))
        )
        (ENatural 0)

symbolicFoldWithRestExpected :: ExprLazy
symbolicFoldWithRestExpected = symbolicFoldWithRestInput

symbolicTimesInput :: ExprLazy
symbolicTimesInput = EBinOp Times (EVar (Var "x" 0)) (ENatural 2)

symbolicTimesExpected :: ExprLazy
symbolicTimesExpected = symbolicTimesInput

freeUnderscoreStrictInput :: ExprStrict
freeUnderscoreStrictInput = SELam "x" (SEBuiltin Type) (SEVar (Var "_" 0))

freeUnderscoreStrictExpected :: ExprStrict
freeUnderscoreStrictExpected = SELam "_" (SEBuiltin Type) (SEVar (Var "_" 1))

forallLetStrictInput :: ExprStrict
forallLetStrictInput =
    SEForall
        "a"
        (SEAnnot (SENatural 1) (SEBuiltin Natural))
        (SELet "x" (SENatural 2) (SEApp (SELam "y" (SEBuiltin Natural) (SEVar (Var "x" 1))) (SENatural 5)))

forallLetStrictExpected :: ExprStrict
forallLetStrictExpected = SEForall "a" (SENatural 1) (SEVar (Var "x" 0))

symbolicSubtractWithRestStrictInput :: ExprStrict
symbolicSubtractWithRestStrictInput =
    SEApp
        (SEApp (SEApp (SEBuiltin NaturalSubtract) (SEVar (Var "x" 0))) (SENatural 10))
        (SENatural 9)

symbolicSubtractWithRestStrictExpected :: ExprStrict
symbolicSubtractWithRestStrictExpected = symbolicSubtractWithRestStrictInput

symbolicTimesStrictInput :: ExprStrict
symbolicTimesStrictInput = SEBinOp Times (SEVar (Var "x" 0)) (SENatural 2)

symbolicTimesStrictExpected :: ExprStrict
symbolicTimesStrictExpected = symbolicTimesStrictInput

foldWithRestStrictInput :: ExprStrict
foldWithRestStrictInput = toStrict foldWithRestInput

foldWithRestStrictExpected :: ExprStrict
foldWithRestStrictExpected = toStrict foldWithRestExpected

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
