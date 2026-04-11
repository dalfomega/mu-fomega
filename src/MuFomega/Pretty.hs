{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Pretty
  ( Doc
  , prettyLazy
  , prettyStrict
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder (Builder, toLazyText)
import MuFomega.Syntax.Common (BinOp (..), Builtin (..), Var (Var))
import MuFomega.Syntax.Lazy (ExprLazy (..))
import MuFomega.Syntax.Strict (ExprStrict (..))

newtype Doc = Doc Builder

runDoc :: Doc -> Builder
runDoc (Doc b) = b

class ToDoc a where
  toDoc :: Int -> a -> Doc

instance ToDoc Var where
  toDoc _ (Var name idx)
    | idx == 0 = Doc (Builder.fromText name)
    | otherwise = Doc (Builder.fromText name <> "@" <> Builder.fromString (show idx))

instance ToDoc Builtin where
  toDoc _ b = Doc $ case b of
    Natural -> "Natural"
    NaturalFold -> "Natural/fold"
    NaturalSubtract -> "Natural/subtract"
    Type -> "Type"
    Kind -> "Kind"

instance ToDoc ExprLazy where
  toDoc outer expr = case expr of
    ENatural n -> Doc (Builder.fromString (show n))
    EBuiltin b -> toDoc outer b
    EVar v -> toDoc outer v
    EAnnot body tipe ->
      parenIf (8 > outer) $
        Doc (runDoc (toDoc 7 body) <> " : " <> runDoc (toDoc 8 tipe))
    ELam name tipe body ->
      parenIf (50 > outer) $
        Doc ("λ(" <> runDoc (fromText name) <> " : " <> runDoc (toDoc 50 tipe) <> ") → " <> runDoc (toDoc 50 body))
    EForall name tipe body ->
      parenIf (50 > outer) $
        Doc ("∀(" <> runDoc (fromText name) <> " : " <> runDoc (toDoc 50 tipe) <> ") → " <> runDoc (toDoc 50 body))
    ELet name value body ->
      parenIf (50 > outer) $
        Doc ("let " <> runDoc (fromText name) <> " = " <> runDoc (toDoc 50 value) <> " in " <> runDoc (toDoc 50 body))
    EApp fn arg ->
      parenIf (5 > outer) $
        Doc (runDoc (toDoc 5 fn) <> " " <> runDoc (toDoc 4 arg))
    EBinOp op lhs rhs ->
      case op of
        Times ->
          parenIf (10 > outer) $
            Doc (runDoc (toDoc 10 lhs) <> " * " <> runDoc (toDoc 10 rhs))
        Plus ->
          parenIf (20 > outer) $
            Doc (runDoc (toDoc 20 lhs) <> " + " <> runDoc (toDoc 20 rhs))

instance ToDoc ExprStrict where
  toDoc outer expr = case expr of
    SENatural n -> Doc (Builder.fromString (show n))
    SEBuiltin b -> toDoc outer b
    SEVar v -> toDoc outer v
    SEAnnot body tipe ->
      parenIf (8 > outer) $
        Doc (runDoc (toDoc 7 body) <> " : " <> runDoc (toDoc 8 tipe))
    SELam name tipe body ->
      parenIf (50 > outer) $
        Doc ("λ(" <> runDoc (fromText name) <> " : " <> runDoc (toDoc 50 tipe) <> ") → " <> runDoc (toDoc 50 body))
    SEForall name tipe body ->
      parenIf (50 > outer) $
        Doc ("∀(" <> runDoc (fromText name) <> " : " <> runDoc (toDoc 50 tipe) <> ") → " <> runDoc (toDoc 50 body))
    SELet name value body ->
      parenIf (50 > outer) $
        Doc ("let " <> runDoc (fromText name) <> " = " <> runDoc (toDoc 50 value) <> " in " <> runDoc (toDoc 50 body))
    SEApp fn arg ->
      parenIf (5 > outer) $
        Doc (runDoc (toDoc 5 fn) <> " " <> runDoc (toDoc 4 arg))
    SEBinOp op lhs rhs ->
      case op of
        Times ->
          parenIf (10 > outer) $
            Doc (runDoc (toDoc 10 lhs) <> " * " <> runDoc (toDoc 10 rhs))
        Plus ->
          parenIf (20 > outer) $
            Doc (runDoc (toDoc 20 lhs) <> " + " <> runDoc (toDoc 20 rhs))

parenIf :: Bool -> Doc -> Doc
parenIf True doc = Doc ("(" <> runDoc doc <> ")")
parenIf False doc = doc

fromText :: Text -> Doc
fromText = Doc . Builder.fromText

prettyLazy :: ExprLazy -> Lazy.Text
prettyLazy e = toLazyText (runDoc (toDoc 100 e))

prettyStrict :: ExprStrict -> Lazy.Text
prettyStrict e = toLazyText (runDoc (toDoc 100 e))
