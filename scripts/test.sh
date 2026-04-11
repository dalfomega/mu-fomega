#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal files..."
hpack

echo "Running tests with cabal..."
cabal test all

echo "All tests passed!"
