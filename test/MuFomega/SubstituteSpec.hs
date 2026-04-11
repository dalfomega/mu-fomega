{-# LANGUAGE OverloadedStrings #-}

module MuFomega.SubstituteSpec
  ( spec
  ) where

import MuFomega.Gen (AnyExprLazy (..))
import Control.DeepSeq (force)
import MuFomega.Substitute (substituteLazy, substituteStrict)
import MuFomega.Syntax.Common (BinOp (Plus), Builtin (Natural), Var (Var))
import MuFomega.Syntax.Convert (toLazy, toStrict)
import MuFomega.Syntax.Lazy
  ( ExprLazy
      ( EAnnot
      , EApp
      , EBinOp
      , EBuiltin
      , EForall
      , ELet
      , ELam
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
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "substitution basics (lazy)" $ do
    it "performs direct replacement" $ do
      substituteLazy "x" 0 (ENatural 5) (EBinOp Plus (EVar (Var "x" 0)) (ENatural 1))
        `shouldBe` EBinOp Plus (ENatural 5) (ENatural 1)

    it "is a no-op when symbol/index does not match" $ do
      substituteLazy "x" 0 (ENatural 5) (EBinOp Plus (EVar (Var "y" 0)) (ENatural 1))
        `shouldBe` EBinOp Plus (EVar (Var "y" 0)) (ENatural 1)

    it "respects shadowing for same binder symbol" $ do
      substituteLazy "x" 0 (EVar (Var "y" 0)) shadowingSampleLazy
        `shouldBe` shadowingExpectedLazy

    it "matches README nested shadowed substitution scenario" $ do
      force (substituteLazy "x" 0 (EVar (Var "y" 0)) readmeBodyLazy)
        `shouldBe` force readmeExpectedLazy

    it "covers annotate/app/forall/let branches" $ do
      force (substituteLazy "x" 0 (EVar (Var "y" 0)) complexSampleLazy)
        `shouldBe` force complexExpectedLazy

    it "forces replacement shifting in lazy lambda/forall/let branches" $ do
      let replacement = EVar (Var "x" 0)
      force (substituteLazy "z" 0 replacement lazyShiftTriggerSample)
        `shouldBe` force lazyShiftTriggerExpected

    it "forces lazy let branch shift constant" $ do
      let replacement = EVar (Var "z" 0)
      force (substituteLazy "z" 0 replacement lazyLetShiftSample)
        `shouldBe` force lazyLetShiftExpected

  describe "substitution parity" $ do
    it "strict and lazy implementations agree via conversion" $ property $ \(AnyExprLazy expr) ->
      let replacement = EVar (Var "y" 0)
          outLazy = substituteLazy "x" 0 replacement expr
          outStrict = substituteStrict "x" 0 (toStrict replacement) (toStrict expr)
       in toLazy outStrict == outLazy

  describe "substitution basics (strict)" $ do
    it "performs direct replacement" $ do
      substituteStrict "x" 0 (SENatural 5) (SEBinOp Plus (SEVar (Var "x" 0)) (SENatural 1))
        `shouldBe` SEBinOp Plus (SENatural 5) (SENatural 1)

    it "is a no-op when symbol/index does not match" $ do
      substituteStrict "x" 0 (SENatural 5) (SEBinOp Plus (SEVar (Var "y" 0)) (SENatural 1))
        `shouldBe` SEBinOp Plus (SEVar (Var "y" 0)) (SENatural 1)

    it "respects shadowing for same binder symbol" $ do
      force (substituteStrict "x" 0 (SEVar (Var "y" 0)) shadowingSampleStrict)
        `shouldBe` force shadowingExpectedStrict

    it "covers annotate/app/forall/let branches" $ do
      force (substituteStrict "x" 0 (SEVar (Var "y" 0)) complexSampleStrict)
        `shouldBe` force complexExpectedStrict

    it "forces replacement shifting in strict lambda/forall/let branches" $ do
      let replacement = SEVar (Var "x" 0)
      force (substituteStrict "z" 0 replacement strictShiftTriggerSample)
        `shouldBe` force strictShiftTriggerExpected

    it "forces strict let branch one binding" $ do
      let replacement = SEVar (Var "x" 0)
      force (substituteStrict "z" 0 replacement strictLetShiftSample)
        `shouldBe` force strictLetShiftExpected

    it "forces strict let branch shift constant" $ do
      let replacement = SEVar (Var "z" 0)
      force (substituteStrict "z" 0 replacement strictLetShiftDemandSample)
        `shouldBe` force strictLetShiftDemandExpected

shadowingSampleLazy :: ExprLazy
shadowingSampleLazy =
  ELam
    "x"
    (EBuiltin Natural)
    (EBinOp Plus (EVar (Var "x" 0)) (EVar (Var "x" 1)))

shadowingExpectedLazy :: ExprLazy
shadowingExpectedLazy =
  ELam
    "x"
    (EBuiltin Natural)
    (EBinOp Plus (EVar (Var "x" 0)) (EVar (Var "y" 0)))

readmeBodyLazy :: ExprLazy
readmeBodyLazy =
  ELam
    "y"
    (EBuiltin Natural)
    ( ELam
        "x"
        (EBuiltin Natural)
        ( EBinOp
            Plus
            (EBinOp Plus (EVar (Var "x" 0)) (EVar (Var "x" 1)))
            (EVar (Var "x" 2))
        )
    )

readmeExpectedLazy :: ExprLazy
readmeExpectedLazy =
  ELam
    "y"
    (EBuiltin Natural)
    ( ELam
        "x"
        (EBuiltin Natural)
        ( EBinOp
            Plus
            (EBinOp Plus (EVar (Var "x" 0)) (EVar (Var "y" 1)))
            (EVar (Var "x" 1))
        )
    )

shadowingSampleStrict :: ExprStrict
shadowingSampleStrict =
  SELam
    "x"
    (SEBuiltin Natural)
    (SEBinOp Plus (SEVar (Var "x" 0)) (SEVar (Var "x" 1)))

shadowingExpectedStrict :: ExprStrict
shadowingExpectedStrict =
  SELam
    "x"
    (SEBuiltin Natural)
    (SEBinOp Plus (SEVar (Var "x" 0)) (SEVar (Var "y" 0)))

complexSampleStrict :: ExprStrict
complexSampleStrict =
  SEAnnot
    ( SELet
        "x"
        (SEVar (Var "x" 0))
        ( SEApp
            (SEForall "x" (SEVar (Var "x" 0)) (SEVar (Var "x" 1)))
            (SEVar (Var "x" 2))
        )
    )
    (SEBuiltin Natural)

complexExpectedStrict :: ExprStrict
complexExpectedStrict =
  SEAnnot
    ( SELet
        "x"
        (SEVar (Var "y" 0))
        ( SEApp
            (SEForall "x" (SEVar (Var "x" 0)) (SEVar (Var "x" 1)))
            (SEVar (Var "x" 1))
        )
    )
    (SEBuiltin Natural)

complexSampleLazy :: ExprLazy
complexSampleLazy =
  EAnnot
    ( ELet
        "x"
        (EVar (Var "x" 0))
        ( EApp
            (EForall "x" (EVar (Var "x" 0)) (EVar (Var "x" 1)))
            (EVar (Var "x" 2))
        )
    )
    (EBuiltin Natural)

complexExpectedLazy :: ExprLazy
complexExpectedLazy =
  EAnnot
    ( ELet
        "x"
        (EVar (Var "y" 0))
        ( EApp
            (EForall "x" (EVar (Var "x" 0)) (EVar (Var "x" 1)))
            (EVar (Var "x" 1))
        )
    )
    (EBuiltin Natural)

lazyShiftTriggerSample :: ExprLazy
lazyShiftTriggerSample =
  ELam
    "x"
    (EBuiltin Natural)
    ( EForall
        "x"
        (EBuiltin Natural)
        (ELet "x" (EVar (Var "z" 0)) (EVar (Var "z" 1)))
    )

lazyShiftTriggerExpected :: ExprLazy
lazyShiftTriggerExpected =
  ELam
    "x"
    (EBuiltin Natural)
    ( EForall
        "x"
        (EBuiltin Natural)
        (ELet "x" (EVar (Var "x" 2)) (EVar (Var "z" 0)))
    )

strictShiftTriggerSample :: ExprStrict
strictShiftTriggerSample =
  SELam
    "x"
    (SEBuiltin Natural)
    ( SEForall
        "x"
        (SEBuiltin Natural)
        (SELet "x" (SEVar (Var "z" 0)) (SEVar (Var "z" 1)))
    )

strictShiftTriggerExpected :: ExprStrict
strictShiftTriggerExpected =
  SELam
    "x"
    (SEBuiltin Natural)
    ( SEForall
        "x"
        (SEBuiltin Natural)
        (SELet "x" (SEVar (Var "x" 2)) (SEVar (Var "z" 0)))
    )

strictLetShiftSample :: ExprStrict
strictLetShiftSample =
  SELet
    "x"
    (SEVar (Var "z" 0))
    (SEVar (Var "z" 1))

strictLetShiftExpected :: ExprStrict
strictLetShiftExpected =
  SELet
    "x"
    (SEVar (Var "x" 0))
    (SEVar (Var "z" 0))

lazyLetShiftSample :: ExprLazy
lazyLetShiftSample =
  ELet
    "z"
    (ENatural 0)
    (EVar (Var "z" 1))

lazyLetShiftExpected :: ExprLazy
lazyLetShiftExpected =
  ELet
    "z"
    (ENatural 0)
    (EVar (Var "z" 1))

strictLetShiftDemandSample :: ExprStrict
strictLetShiftDemandSample =
  SELet
    "z"
    (SENatural 0)
    (SEVar (Var "z" 1))

strictLetShiftDemandExpected :: ExprStrict
strictLetShiftDemandExpected =
  SELet
    "z"
    (SENatural 0)
    (SEVar (Var "z" 1))
