#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal file..."
hpack

echo "Building all targets with cabal..."
cabal build all --enable-tests --enable-benchmarks

echo "Build successful!"
