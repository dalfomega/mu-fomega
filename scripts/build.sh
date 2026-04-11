#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal files..."
hpack

echo "Building all targets with cabal..."
cabal build --enable-benchmarks all

echo "Build successful!"
