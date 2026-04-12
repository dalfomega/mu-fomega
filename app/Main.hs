{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import MuFomega.CLI (CliOptions (inputFile), parseCliOptions, runPipeline, usageText)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> Text.putStr usageText
    ["-h"] -> Text.putStr usageText
    _ ->
      case parseCliOptions args of
        Left err -> dieWithUsage err
        Right options -> do
          input <- loadInput options
          case runPipeline options input of
            Left err -> die (Text.unpack err)
            Right output -> Text.putStrLn output

loadInput :: CliOptions -> IO Text.Text
loadInput options =
  case inputFile options of
    Just path -> Text.readFile path
    Nothing -> Text.getContents

dieWithUsage :: Text.Text -> IO a
dieWithUsage err =
  die (Text.unpack (err <> "\n\n" <> usageText))
