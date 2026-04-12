{-# LANGUAGE OverloadedStrings #-}

module MuFomega.ShiftSpec (
    spec,
) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Text (Text)
import MuFomega.Gen (AnyExprLazy (..), AnyExprStrict (..))
import MuFomega.Shift (shiftLazy, shiftStrict)
import MuFomega.Syntax.Common (BinOp (Plus), Var (Var))
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
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow)
import Test.QuickCheck (property, (==>))

spec :: Spec
spec = do
    describe "README shift examples (lazy)" $ do
        it "increments threshold only in matching binder bodies" $ do
            force (shiftLazy 1 "x" 0 lambdaShadowSampleLazy) `shouldBe` force lambdaShadowShiftedLazy

        it "does not increment threshold for non-matching binder names" $ do
            force (shiftLazy 1 "x" 0 lambdaDifferentBinderLazy) `shouldBe` force lambdaDifferentBinderShiftedLazy

        it "let shifts value at same threshold and body at raised threshold for same symbol" $ do
            force (shiftLazy 1 "x" 0 letShadowSampleLazy) `shouldBe` force letShadowShiftedLazy

        it "shifts inside annotation type branch" $ do
            force (shiftLazy 1 "x" 0 (EAnnot (ENatural 1) (EVar (Var "x" 0))))
                `shouldBe` force (EAnnot (ENatural 1) (EVar (Var "x" 1)))

        it "shifts inside lambda annotation branch" $ do
            force (shiftLazy 1 "x" 0 (ELam "y" (EVar (Var "x" 0)) (EVar (Var "y" 0))))
                `shouldBe` force (ELam "y" (EVar (Var "x" 1)) (EVar (Var "y" 0)))

        it "shifts inside forall annotation branch" $ do
            force (shiftLazy 1 "x" 0 (EForall "y" (EVar (Var "x" 0)) (EVar (Var "y" 0))))
                `shouldBe` force (EForall "y" (EVar (Var "x" 1)) (EVar (Var "y" 0)))

        it "shifts application argument branch" $ do
            force (shiftLazy 1 "x" 0 (EApp (ENatural 0) (EVar (Var "x" 0))))
                `shouldBe` force (EApp (ENatural 0) (EVar (Var "x" 1)))

    describe "README shift examples (strict)" $ do
        it "increments threshold only in matching binder bodies" $ do
            force (shiftStrict 1 "x" 0 lambdaShadowSampleStrict) `shouldBe` force lambdaShadowShiftedStrict

        it "does not increment threshold for non-matching binder names" $ do
            force (shiftStrict 1 "x" 0 lambdaDifferentBinderStrict) `shouldBe` force lambdaDifferentBinderShiftedStrict

        it "let shifts value at same threshold and body at raised threshold for same symbol" $ do
            force (shiftStrict 1 "x" 0 letShadowSampleStrict) `shouldBe` force letShadowShiftedStrict

        it "shifts inside strict annotation type branch" $ do
            force (shiftStrict 1 "x" 0 (SEAnnot (SENatural 1) (SEVar (Var "x" 0))))
                `shouldBe` force (SEAnnot (SENatural 1) (SEVar (Var "x" 1)))

        it "shifts inside strict lambda annotation branch" $ do
            force (shiftStrict 1 "x" 0 (SELam "y" (SEVar (Var "x" 0)) (SEVar (Var "y" 0))))
                `shouldBe` force (SELam "y" (SEVar (Var "x" 1)) (SEVar (Var "y" 0)))

        it "shifts inside strict forall annotation branch" $ do
            force (shiftStrict 1 "x" 0 (SEForall "y" (SEVar (Var "x" 0)) (SEVar (Var "y" 0))))
                `shouldBe` force (SEForall "y" (SEVar (Var "x" 1)) (SEVar (Var "y" 0)))

        it "shifts strict application argument branch" $ do
            force (shiftStrict 1 "x" 0 (SEApp (SENatural 0) (SEVar (Var "x" 0))))
                `shouldBe` force (SEApp (SENatural 0) (SEVar (Var "x" 1)))

    describe "shift properties" $ do
        it "shift 0 x m e == e for lazy" $ property $ \(AnyExprLazy e) ->
            shiftLazy 0 "x" 0 e == e

        it "shift 0 x m e == e for strict" $ property $ \(AnyExprStrict e) ->
            shiftStrict 0 "x" 0 e == e

        it "guarded inverse for lazy when no underflow can occur" $ property $ \(AnyExprLazy e) ->
            let x = "x"
                d = 1
                m = 0
             in minIndexLazy x m e > 0 ==>
                    (shiftLazy (-d) x m . shiftLazy d x m) e == e

        it "guarded inverse for strict when no underflow can occur" $ property $ \(AnyExprStrict e) ->
            let x = "x"
                d = 1
                m = 0
             in minIndexStrict x m e > 0 ==>
                    (shiftStrict (-d) x m . shiftStrict d x m) e == e

        it "leaves non-matching symbols unchanged" $ do
            shiftLazy 1 "x" 0 (EVar (Var "y" 4)) `shouldBe` EVar (Var "y" 4)

        it "throws on underflow for lazy negative shift" $ do
            evaluate (force (shiftLazy (-1) "x" 0 (EVar (Var "x" 0)))) `shouldThrow` anyErrorCall

        it "throws on underflow for strict negative shift" $ do
            evaluate (force (shiftStrict (-1) "x" 0 (SEVar (Var "x" 0)))) `shouldThrow` anyErrorCall

        it "covers negative shift non-underflow else branch (lazy)" $ do
            force (shiftLazy (-1) "x" 0 (EVar (Var "x" 3))) `shouldBe` EVar (Var "x" 2)

        it "covers negative shift non-underflow else branch (strict)" $ do
            force (shiftStrict (-1) "x" 0 (SEVar (Var "x" 3))) `shouldBe` SEVar (Var "x" 2)

    describe "lazy/strict parity" $ do
        it "shift results agree via conversion" $ property $ \(AnyExprLazy e) ->
            let outLazy = shiftLazy 2 "x" 1 e
                outStrict = shiftStrict 2 "x" 1 (toStrict e)
             in toLazy outStrict == outLazy

minIndexLazy :: Text -> Word -> ExprLazy -> Word
minIndexLazy target m = go maxBound
  where
    go acc expr =
        case expr of
            ENatural _ -> acc
            EBuiltin _ -> acc
            EVar (Var name ix)
                | name == target && ix >= m -> min acc ix
                | otherwise -> acc
            EAnnot a b -> go (go acc a) b
            ELam _ a b -> go (go acc a) b
            EForall _ a b -> go (go acc a) b
            ELet _ a b -> go (go acc a) b
            EApp a b -> go (go acc a) b
            EBinOp _ a b -> go (go acc a) b

minIndexStrict :: Text -> Word -> ExprStrict -> Word
minIndexStrict target m = go maxBound
  where
    go acc expr =
        case expr of
            SENatural _ -> acc
            SEBuiltin _ -> acc
            SEVar (Var name ix)
                | name == target && ix >= m -> min acc ix
                | otherwise -> acc
            SEAnnot a b -> go (go acc a) b
            SELam _ a b -> go (go acc a) b
            SEForall _ a b -> go (go acc a) b
            SELet _ a b -> go (go acc a) b
            SEApp a b -> go (go acc a) b
            SEBinOp _ a b -> go (go acc a) b

lambdaShadowSampleLazy :: ExprLazy
lambdaShadowSampleLazy =
    ELam
        "x"
        (ENatural 0)
        (EBinOp Plus (EVar (Var "x" 0)) (EVar (Var "x" 1)))

lambdaShadowShiftedLazy :: ExprLazy
lambdaShadowShiftedLazy =
    ELam
        "x"
        (ENatural 0)
        (EBinOp Plus (EVar (Var "x" 0)) (EVar (Var "x" 2)))

lambdaDifferentBinderLazy :: ExprLazy
lambdaDifferentBinderLazy =
    ELam
        "y"
        (ENatural 0)
        (EBinOp Plus (EVar (Var "x" 0)) (EVar (Var "x" 1)))

lambdaDifferentBinderShiftedLazy :: ExprLazy
lambdaDifferentBinderShiftedLazy =
    ELam
        "y"
        (ENatural 0)
        (EBinOp Plus (EVar (Var "x" 1)) (EVar (Var "x" 2)))

letShadowSampleLazy :: ExprLazy
letShadowSampleLazy =
    ELet
        "x"
        (EAnnot (EVar (Var "x" 0)) (ENatural 0))
        (EForall "x" (ENatural 0) (EVar (Var "x" 1)))

letShadowShiftedLazy :: ExprLazy
letShadowShiftedLazy =
    ELet
        "x"
        (EAnnot (EVar (Var "x" 1)) (ENatural 0))
        (EForall "x" (ENatural 0) (EVar (Var "x" 1)))

lambdaShadowSampleStrict :: ExprStrict
lambdaShadowSampleStrict =
    SELam
        "x"
        (SENatural 0)
        (SEBinOp Plus (SEVar (Var "x" 0)) (SEVar (Var "x" 1)))

lambdaShadowShiftedStrict :: ExprStrict
lambdaShadowShiftedStrict =
    SELam
        "x"
        (SENatural 0)
        (SEBinOp Plus (SEVar (Var "x" 0)) (SEVar (Var "x" 2)))

lambdaDifferentBinderStrict :: ExprStrict
lambdaDifferentBinderStrict =
    SELam
        "y"
        (SENatural 0)
        (SEBinOp Plus (SEVar (Var "x" 0)) (SEVar (Var "x" 1)))

lambdaDifferentBinderShiftedStrict :: ExprStrict
lambdaDifferentBinderShiftedStrict =
    SELam
        "y"
        (SENatural 0)
        (SEBinOp Plus (SEVar (Var "x" 1)) (SEVar (Var "x" 2)))

letShadowSampleStrict :: ExprStrict
letShadowSampleStrict =
    SELet
        "x"
        (SEAnnot (SEVar (Var "x" 0)) (SENatural 0))
        (SEForall "x" (SENatural 0) (SEVar (Var "x" 1)))

letShadowShiftedStrict :: ExprStrict
letShadowShiftedStrict =
    SELet
        "x"
        (SEAnnot (SEVar (Var "x" 1)) (SENatural 0))
        (SEForall "x" (SENatural 0) (SEVar (Var "x" 1)))
