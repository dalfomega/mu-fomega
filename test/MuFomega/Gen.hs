{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Gen
  ( AnyExprLazy (..)
  , AnyExprStrict (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import MuFomega.Syntax.Common (BinOp (..), Builtin (..), Var (..))
import MuFomega.Syntax.Convert (toStrict)
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
import MuFomega.Syntax.Strict (ExprStrict)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , chooseInt
  , elements
  , frequency
  , listOf
  , oneof
  , scale
  , sized
  )

newtype AnyExprLazy = AnyExprLazy
  { getAnyExprLazy :: ExprLazy
  }
  deriving (Eq, Show)

newtype AnyExprStrict = AnyExprStrict
  { getAnyExprStrict :: ExprStrict
  }
  deriving (Eq, Show)

instance Arbitrary AnyExprLazy where
  arbitrary = AnyExprLazy <$> sized genExprLazy

instance Arbitrary AnyExprStrict where
  arbitrary = AnyExprStrict . toStrict . getAnyExprLazy <$> (arbitrary :: Gen AnyExprLazy)

genExprLazy :: Int -> Gen ExprLazy
genExprLazy n
  | n <= 0 = genLeaf
  | otherwise =
      frequency
        [ (5, genLeaf)
        , (2, EAnnot <$> genSub <*> genSub)
        , (2, ELam <$> genLabel <*> genSub <*> genSub)
        , (2, EForall <$> genLabel <*> genSub <*> genSub)
        , (2, ELet <$> genLabel <*> genSub <*> genSub)
        , (3, EApp <$> genSub <*> genSub)
        , (2, EBinOp <$> genBinOp <*> genSub <*> genSub)
        ]
  where
    genSub = genExprLazy (n `div` 2)

genLeaf :: Gen ExprLazy
genLeaf =
  oneof
    [ ENatural . toInteger <$> chooseInt (0, 200)
    , EBuiltin <$> genBuiltin
    , EVar <$> genVar
    ]

genBuiltin :: Gen Builtin
genBuiltin = elements [Natural, NaturalFold, NaturalSubtract, Type, Kind]

genBinOp :: Gen BinOp
genBinOp = elements [Plus, Times]

genVar :: Gen Var
genVar = Var <$> genLabel <*> genIndex

genIndex :: Gen Word
genIndex = fromIntegral <$> chooseInt (0, 5)

genLabel :: Gen Text
genLabel = do
  first <- elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_")
  rest <- scale (`min` 6) (listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-/_")))
  pure (Text.pack (first : rest))
