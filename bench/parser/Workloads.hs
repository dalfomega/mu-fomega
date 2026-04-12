{-# LANGUAGE OverloadedStrings #-}

module Workloads (
    Workload (..),
    workloads,
    renderWorkload,
) where

import Data.Text (Text)
import qualified Data.Text as Text

data Workload
    = NestedParens
    | NestedOps
    | NestedLambdas
    | NestedLets
    deriving (Eq, Show)

workloads :: [Workload]
workloads = [NestedParens, NestedOps, NestedLambdas, NestedLets]

renderWorkload :: Workload -> Int -> Text
renderWorkload workload n =
    case workload of
        NestedParens -> nestedParens n
        NestedOps -> nestedOps n
        NestedLambdas -> nestedLambdas n
        NestedLets -> nestedLets n

nestedParens :: Int -> Text
nestedParens n =
    Text.replicate n "(" <> "0" <> Text.replicate n ")"

nestedOps :: Int -> Text
nestedOps n =
    foldr1 combine (replicate (max 1 (n + 1)) "x")
  where
    combine left right = "(" <> left <> " + " <> right <> ")"

nestedLambdas :: Int -> Text
nestedLambdas n =
    Text.concat (replicate n "λ(x : Natural) → ") <> "x"

nestedLets :: Int -> Text
nestedLets n =
    let depth = max 1 n
        binding i
            | i == 0 = "let x0 = 0 "
            | otherwise = "let x" <> tshow i <> " = x" <> tshow (i - 1) <> " "
        bindings = Text.concat [binding i | i <- [0 .. depth - 1]]
        body = "in x" <> tshow (depth - 1)
     in bindings <> body

tshow :: Int -> Text
tshow = Text.pack . show
