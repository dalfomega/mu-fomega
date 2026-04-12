{-# LANGUAGE OverloadedStrings #-}

module MuFomega.Church
  ( naturalToChurchText
  , churchToNaturalText
  , churchAddText
  , churchSubText
  , churchAddAppText
  , churchSubAppText
  , churchAlternatingText
  , parseChurchStrict
  ) where

import Data.Text (Text)
import qualified MuFomega.Parser.Attoparsec as Atto
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

naturalToChurchText :: Integer -> Text
naturalToChurchText n =
  "\\(f : Natural) -> \\(z : Natural) -> " <> iter (max 0 n) "f" "z"

churchToNaturalText :: Text -> Text
churchToNaturalText churchExpr =
  "(" <> churchExpr <> ") (\\(n : Natural) -> n + 1) 0"

churchAddText :: Text
churchAddText =
  "\\(m : Natural) -> \\(n : Natural) -> \\(f : Natural) -> \\(z : Natural) -> m f (n f z)"

churchSubText :: Text
churchSubText =
  "\\(m : Natural) -> \\(n : Natural) -> \\(f : Natural) -> \\(z : Natural) -> Natural/fold (Natural/subtract (n (\\(k : Natural) -> k + 1) 0) (m (\\(k : Natural) -> k + 1) 0)) Natural f z"

churchAddAppText :: Text -> Text -> Text
churchAddAppText l r = "(" <> churchAddText <> ") (" <> l <> ") (" <> r <> ")"

churchSubAppText :: Text -> Text -> Text
churchSubAppText l r = "(" <> churchSubText <> ") (" <> l <> ") (" <> r <> ")"

churchAlternatingText :: Int -> Text
churchAlternatingText n = churchToNaturalText finalChurch
  where
    steps = max 1 n
    delta = naturalToChurchText 100
    start = naturalToChurchText 1000
    finalChurch = foldl step start [1 .. steps]
    step acc i
      | odd i = churchAddAppText acc delta
      | otherwise = churchSubAppText acc delta

parseChurchStrict :: Text -> Either String ExprStrict
parseChurchStrict = fmap toStrictExpr . Atto.parseExpr

iter :: Integer -> Text -> Text -> Text
iter k fn base
  | k <= 0 = base
  | otherwise = fn <> " (" <> iter (k - 1) fn base <> ")"

toStrictExpr :: ExprLazy -> ExprStrict
toStrictExpr expr =
  case expr of
    ENatural n -> SENatural n
    EBuiltin b -> SEBuiltin b
    EVar v -> SEVar v
    EAnnot body tipe -> SEAnnot (toStrictExpr body) (toStrictExpr tipe)
    ELam name tipe body -> SELam name (toStrictExpr tipe) (toStrictExpr body)
    EForall name tipe body -> SEForall name (toStrictExpr tipe) (toStrictExpr body)
    ELet name value body -> SELet name (toStrictExpr value) (toStrictExpr body)
    EApp f x -> SEApp (toStrictExpr f) (toStrictExpr x)
    EBinOp op l r -> SEBinOp op (toStrictExpr l) (toStrictExpr r)
