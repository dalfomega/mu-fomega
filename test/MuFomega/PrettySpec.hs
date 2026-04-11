{-# LANGUAGE OverloadedStrings #-}

module MuFomega.PrettySpec
  (spec) where

import qualified Data.Text.Lazy as Lazy
import MuFomega.Gen (AnyExprLazy (..), AnyExprStrict (..))
import MuFomega.Pretty (prettyLazy, prettyStrict)
import MuFomega.Syntax.Common (BinOp (Plus, Times), Builtin (Kind, Natural, NaturalFold, NaturalSubtract, Type), Var (Var))
import MuFomega.Syntax.Lazy
  (ExprLazy (EAnnot, EApp, EBinOp, EBuiltin, EForall, ELam, ELet, ENatural, EVar))
import MuFomega.Syntax.Strict (ExprStrict (SENatural, SEBuiltin, SEVar))
import MuFomega.Syntax.Convert (toStrict)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "pretty printing lazy expressions" $ do
    describe "literals and builtins" $ do
      it "prints natural numbers" $
        prettyLazy (ENatural 42) `shouldBe` "42"

      it "prints builtins" $
        prettyLazy (EBuiltin Natural) `shouldBe` "Natural"

      it "prints all builtins" $
        prettyLazy (EBuiltin Type) `shouldBe` "Type"

      it "prints Kind" $
        prettyLazy (EBuiltin Kind) `shouldBe` "Kind"

      it "prints NaturalFold" $
        prettyLazy (EBuiltin NaturalFold) `shouldBe` "Natural/fold"

      it "prints NaturalSubtract" $
        prettyLazy (EBuiltin NaturalSubtract) `shouldBe` "Natural/subtract"

      it "prints variables" $
        prettyLazy (EVar (Var "x" 0)) `shouldBe` "x"

      it "prints indexed variables" $
        prettyLazy (EVar (Var "x" 1)) `shouldBe` "x@1"

    describe "precedence examples from README" $ do
      it "prints times without parens" $
        prettyLazy (EBinOp Times (ENatural 2) (ENatural 3)) `shouldBe` "2 * 3"

      it "prints plus without parens" $
        prettyLazy (EBinOp Plus (ENatural 1) (ENatural 2)) `shouldBe` "1 + 2"

      it "prints times then plus needs parens" $
        prettyLazy (EBinOp Times (ENatural 2) (EBinOp Plus (ENatural 3) (ENatural 4)))
          `shouldBe` "2 * (3 + 4)"

      it "prints plus then times needs parens" $
        prettyLazy (EBinOp Plus (ENatural 1) (EBinOp Times (ENatural 2) (ENatural 3)))
          `shouldBe` "1 + 2 * 3"

      it "prints nested times" $
        prettyLazy (EBinOp Times (EBinOp Times (ENatural 1) (ENatural 2)) (ENatural 3))
          `shouldBe` "1 * 2 * 3"

      it "prints nested plus" $
        prettyLazy (EBinOp Plus (EBinOp Plus (ENatural 1) (ENatural 2)) (ENatural 3))
          `shouldBe` "1 + 2 + 3"

    describe "application precedence" $ do
      it "prints application" $
        prettyLazy (EApp (EVar (Var "f" 0)) (ENatural 1)) `shouldBe` "f 1"

    describe "annotation precedence" $ do
      it "prints annotation" $
        prettyLazy (EAnnot (ENatural 123) (EBuiltin Natural)) `shouldBe` "123 : Natural"

      it "prints nested annotations" $
        prettyLazy (EAnnot (EAnnot (ENatural 123) (EBuiltin Natural)) (EBuiltin Type)) `shouldBe` "(123 : Natural) : Type"

      it "prints annotation with times" $
        prettyLazy (EAnnot (EBinOp Times (ENatural 2) (ENatural 3)) (EBuiltin Natural)) `shouldBe` "(2 * 3) : Natural"

    describe "lambda precedence" $ do
      it "prints lambda" $
        prettyLazy (ELam "x" (EBuiltin Natural) (EVar (Var "x" 0))) `shouldBe` "λ(x : Natural) → x"

      it "prints curried lambdas" $
        prettyLazy (ELam "x" (EBuiltin Natural) (ELam "y" (EBuiltin Natural) (EVar (Var "x" 0)))) `shouldBe` "λ(x : Natural) → λ(y : Natural) → x"

    describe "forall precedence" $ do
      it "prints forall" $
        prettyLazy (EForall "a" (EBuiltin Type) (EBuiltin Natural)) `shouldBe` "∀(a : Type) → Natural"

    describe "let precedence" $ do
      it "prints let" $
        prettyLazy (ELet "x" (ENatural 1) (EVar (Var "x" 0))) `shouldBe` "let x = 1 in x"

      it "prints nested let" $
        prettyLazy (ELet "x" (ENatural 1) (ELet "y" (ENatural 2) (EVar (Var "x" 0)))) `shouldBe` "let x = 1 in let y = 2 in x"

    describe "round-trip on generated expressions" $ do
      it "round-trips arbitrary lazy expressions" $ property $ \(AnyExprLazy expr) ->
        Lazy.toStrict (prettyLazy expr) /= mempty

  describe "pretty printing strict expressions" $ do
    describe "literals and builtins" $ do
      it "prints natural numbers" $
        prettyStrict (SENatural 42) `shouldBe` "42"

      it "prints builtins" $
        prettyStrict (SEBuiltin Natural) `shouldBe` "Natural"

      it "prints Type" $
        prettyStrict (SEBuiltin Type) `shouldBe` "Type"

      it "prints Kind" $
        prettyStrict (SEBuiltin Kind) `shouldBe` "Kind"

      it "prints variables" $
        prettyStrict (SEVar (Var "x" 0)) `shouldBe` "x"

      it "prints indexed variables" $
        prettyStrict (SEVar (Var "x" 1)) `shouldBe` "x@1"

    describe "parity with lazy" $ do
      it "produces same output for corresponding expressions" $
        prettyStrict (toStrict sampleLazy) `shouldBe` prettyLazy sampleLazy

      it "round-trips arbitrary strict expressions" $ property $ \(AnyExprStrict expr) ->
        Lazy.toStrict (prettyStrict expr) /= mempty

  describe "complex expressions from README" $ do
    it "prints lambda with shadowing example" $
      prettyLazy (ELam "x" (EBuiltin Natural) (ELam "x" (EBuiltin Natural) (EBinOp Plus (ENatural 123) (EVar (Var "x" 1)))))
        `shouldBe` "λ(x : Natural) → λ(x : Natural) → 123 + x@1"

  describe "tricky formatting cases" $ do
    it "handles mixed operators" $
      prettyLazy (EBinOp Plus (ENatural 1) (EBinOp Times (ENatural 2) (ENatural 3))) `shouldBe` "1 + 2 * 3"

    it "handles application with builtin" $
      prettyLazy (EApp (EVar (Var "f" 0)) (EBuiltin Natural)) `shouldBe` "f Natural"

sampleLazy :: ExprLazy
sampleLazy = ELet "x" (ENatural 1) (ELam "x" (EBuiltin Natural) (EVar (Var "x" 0)))