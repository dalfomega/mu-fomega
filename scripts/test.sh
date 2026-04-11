#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal file..."
hpack

echo "Running tests with cabal..."
cabal test mu-fomega-test

echo "All tests passed!"
