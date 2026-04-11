#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal file..."
hpack

githash=$(git rev-parse HEAD)

echo "Running benchmarks with cabal..."
cabal bench parser-bench \
  --benchmark-option="--time-limit" \
  --benchmark-option=4 \
  --benchmark-option="--csv" \
  --benchmark-option="parser-bench-$githash.csv"

echo "Benchmarks complete!"
