{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, replicateM_, when)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import Data.List (intercalate)
import Data.Text (Text)
import qualified GHC.Clock as Clock
import qualified MuFomega.Parser.Attoparsec as Atto
import qualified MuFomega.Parser.FlatParse as Flat
import qualified MuFomega.Parser.Megaparsec as Mega
import MuFomega.Syntax.Lazy (ExprLazy)
import Workloads (renderWorkload, workloads)

main :: IO ()
main = do
  verifyParity
  runScalingStudy
  defaultMain
    [mkParserGroup "megaparsec" parseMegaOrDie
     ,mkParserGroup "attoparsec" parseAttoOrDie
    , mkParserGroup "flatparse" parseFlatOrDie
    ]

verifyParity :: IO ()
verifyParity = do
  let depths = [4, 8, 16]
  forM_ workloads $ \workload ->
    forM_ depths $ \n -> do
      let input = renderWorkload workload n
      let megaExpr = parseMegaOrDie input
      let attoExpr = parseAttoOrDie input
      let flatExpr = parseFlatOrDie input
      when (megaExpr /= attoExpr || attoExpr /= flatExpr) $ do
        error ("Parser mismatch on workload=" <> show workload <> " depth=" <> show n)

mkParserGroup :: String -> (Text -> ExprLazy) -> Benchmark
mkParserGroup parserName parserFn =
  bgroup parserName [bgroup (show workload) (map (mkCase workload) benchDepths) | workload <- workloads]
  where
    mkCase workload n =
      let input = renderWorkload workload n
       in bench ("n=" <> show n) (nf parserFn input)

runScalingStudy :: IO ()
runScalingStudy = do
  putStrLn "Parser scaling study (log-log exponent by workload/parser):"
  putStrLn ""
  runForParser "megaparsec" parseMegaOrDie -- too slow
  runForParser "attoparsec" parseAttoOrDie
  runForParser "flatparse" parseFlatOrDie
  putStrLn ""

runForParser :: String -> (Text -> ExprLazy) -> IO ()
runForParser parserName parserFn = do
  putStrLn ("Parser: " <> parserName)
  forM_ workloads $ \workload -> do
    rows <- forM scalingDepths $ \n -> do
      let input = renderWorkload workload n
      micros <- measureMicros 10 parserFn input
      pure (n, micros)
    let fittedExponent = logLogExponent rows
    putStrLn
      ( "  "
          <> show workload
          <> ": exponent="
          <> showFF fittedExponent
          <> " ("
          <> complexityClass fittedExponent
          <> ")"
      )
    putStrLn ("    " <> renderRows rows)
  putStrLn ""

measureMicros :: Int -> (Text -> ExprLazy) -> Text -> IO Double
measureMicros repeats parserFn input = do
  start <- Clock.getMonotonicTimeNSec
  replicateM_ repeats $ do
    _ <- evaluate (force (parserFn input))
    pure ()
  end <- Clock.getMonotonicTimeNSec
  let totalMicros = fromIntegral (end - start) / 1.0e3 :: Double
  pure (totalMicros / fromIntegral repeats)

logLogExponent :: [(Int, Double)] -> Double
logLogExponent points =
  let pairs =
        [ (log (fromIntegral n :: Double), log t)
        | (n, t) <- points
        , n > 0
        , t > 0
        ]
      k = fromIntegral (length pairs)
      sx = sum (map fst pairs)
      sy = sum (map snd pairs)
      sxx = sum [x * x | (x, _) <- pairs]
      sxy = sum [x * y | (x, y) <- pairs]
      denom = k * sxx - sx * sx
   in if denom == 0 then 0 else (k * sxy - sx * sy) / denom

complexityClass :: Double -> String
complexityClass fittedExponent
  | fittedExponent < 1.3 = "linear-ish"
  | fittedExponent < 1.8 = "sub-quadratic"
  | fittedExponent < 2.3 = "quadratic-ish"
  | otherwise = "super-quadratic"

renderRows :: [(Int, Double)] -> String
renderRows rows =
  intercalate ", " ["n=" <> show n <> ":" <> showFF t <> "µs" | (n, t) <- rows]

showFF :: Double -> String
showFF value =
  let scaled = fromIntegral (round (value * 10000) :: Integer) / 10000.0 :: Double
   in show scaled

parseMegaOrDie :: Text -> ExprLazy
parseMegaOrDie input =
  case Mega.parseExpr input of
    Right expr -> expr
    Left _ -> error "Megaparsec parser failed on generated workload"

parseAttoOrDie :: Text -> ExprLazy
parseAttoOrDie input =
  case Atto.parseExpr input of
    Right expr -> expr
    Left _ -> error "Attoparsec parser failed on generated workload"

parseFlatOrDie :: Text -> ExprLazy
parseFlatOrDie input =
  case Flat.parseExpr input of
    Right expr -> expr
    Left _ -> error "FlatParse parser failed on generated workload"

scalingDepths :: [Int]
scalingDepths = [1000, 2000, 3000]

benchDepths :: [Int]
benchDepths = [1000, 2000, 3000]
