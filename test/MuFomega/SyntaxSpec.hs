{-# LANGUAGE OverloadedStrings #-}

module MuFomega.SyntaxSpec
  ( spec
  ) where

import Control.DeepSeq (force)
import MuFomega.Gen (AnyExprLazy (..), AnyExprStrict (..))
import MuFomega.Syntax.Common
  ( BinOp (Plus, Times)
  , Builtin
  , Builtin (Kind, Natural, NaturalFold, NaturalSubtract, Type)
  , Var (Var, varIndex, varName)
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
  describe "constructor coverage" $ do
    it "touches every lazy constructor" $ do
      map lazyTag lazySamples `shouldBe` [1 .. 9]

    it "touches every strict constructor" $ do
      map strictTag strictSamples `shouldBe` [1 .. 9]

  describe "conversion laws" $ do
    it "toLazy . toStrict = id" $ property $ \(AnyExprLazy expr) ->
      (toLazy . toStrict) expr == expr

    it "toStrict . toLazy = id" $ property $ \(AnyExprStrict expr) ->
      (toStrict . toLazy) expr == expr

  describe "NFData forcing" $ do
    it "force reaches normal form for lazy AST" $ property $ \(AnyExprLazy expr) ->
      force expr `seq` True

    it "force reaches normal form for strict AST" $ property $ \(AnyExprStrict expr) ->
      force expr `seq` True

  describe "instance and selector coverage" $ do
    it "uses Var selectors" $ do
      let v = Var "x" 3
      varName v `shouldBe` "x"
      varIndex v `shouldBe` 3

    it "covers Eq/Show for Common constructors" $ do
      Var "x" 0 == Var "x" 0 `shouldBe` True
      Var "x" 0 == Var "x" 1 `shouldBe` False
      Var "x" 0 /= Var "x" 1 `shouldBe` True
      show (Var "x" 0) `shouldBe` "Var {varName = \"x\", varIndex = 0}"
      showList [Var "x" 0, Var "y" 1] "" `shouldBe` "[Var {varName = \"x\", varIndex = 0},Var {varName = \"y\", varIndex = 1}]"

      map show allBuiltins
        `shouldBe` ["Natural", "NaturalFold", "NaturalSubtract", "Type", "Kind"]
      NaturalFold == NaturalFold `shouldBe` True
      NaturalFold == NaturalSubtract `shouldBe` False
      NaturalFold /= NaturalSubtract `shouldBe` True
      showList [Natural, Kind] "" `shouldBe` "[Natural,Kind]"

      map show allBinOps `shouldBe` ["Plus", "Times"]
      Plus == Plus `shouldBe` True
      Plus == Times `shouldBe` False
      Plus /= Times `shouldBe` True
      showList [Plus, Times] "" `shouldBe` "[Plus,Times]"

    it "covers Eq/Show for all lazy constructors" $ do
      map show lazySamples `shouldBe` map show lazySamples
      and (zipWith (==) lazySamples lazySamples) `shouldBe` True
      ENatural 1 == EBuiltin Natural `shouldBe` False
      ENatural 1 /= EBuiltin Natural `shouldBe` True
      showList lazySamples "" `shouldBe` show lazySamples

    it "covers Eq/Show for all strict constructors" $ do
      map show strictSamples `shouldBe` map show strictSamples
      and (zipWith (==) strictSamples strictSamples) `shouldBe` True
      SENatural 1 == SEBuiltin Natural `shouldBe` False
      SENatural 1 /= SEBuiltin Natural `shouldBe` True
      showList strictSamples "" `shouldBe` show strictSamples

lazyTag :: ExprLazy -> Int
lazyTag expr =
  case expr of
    ENatural _ -> 1
    EBuiltin _ -> 2
    EVar _ -> 3
    EAnnot _ _ -> 4
    ELam _ _ _ -> 5
    EForall _ _ _ -> 6
    ELet _ _ _ -> 7
    EApp _ _ -> 8
    EBinOp _ _ _ -> 9

strictTag :: ExprStrict -> Int
strictTag expr =
  case expr of
    SENatural _ -> 1
    SEBuiltin _ -> 2
    SEVar _ -> 3
    SEAnnot _ _ -> 4
    SELam _ _ _ -> 5
    SEForall _ _ _ -> 6
    SELet _ _ _ -> 7
    SEApp _ _ -> 8
    SEBinOp _ _ _ -> 9

lazySamples :: [ExprLazy]
lazySamples =
  [ ENatural 0
  , EBuiltin Natural
  , EVar (Var "x" 0)
  , EAnnot (ENatural 1) (EBuiltin Natural)
  , ELam "x" (EBuiltin Natural) (EVar (Var "x" 0))
  , EForall "x" (EBuiltin Natural) (EBuiltin Natural)
  , ELet "x" (ENatural 1) (EVar (Var "x" 0))
  , EApp (EVar (Var "f" 0)) (ENatural 1)
  , EBinOp Plus (ENatural 1) (ENatural 2)
  ]

strictSamples :: [ExprStrict]
strictSamples = toStrict <$> lazySamples

allBuiltins :: [Builtin]
allBuiltins = [Natural, NaturalFold, NaturalSubtract, Type, Kind]

allBinOps :: [BinOp]
allBinOps = [Plus, Times]
