#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal file..."
hpack

echo "Running benchmarks with cabal..."
cabal bench strict-vs-lazy-bench

echo "Benchmarks complete!"
