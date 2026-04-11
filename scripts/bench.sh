#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal files..."
hpack

echo "Running benchmarks with cabal..."
cabal bench all

echo "Benchmarks complete!"