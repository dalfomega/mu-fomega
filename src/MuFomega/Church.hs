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
import MuFomega.Syntax.Convert (toStrict)
import MuFomega.Syntax.Strict (ExprStrict)

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
parseChurchStrict = fmap toStrict . Atto.parseExpr

iter :: Integer -> Text -> Text -> Text
iter k fn base
  | k <= 0 = base
  | otherwise = fn <> " (" <> iter (k - 1) fn base <> ")"
