#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal files..."
hpack

echo "Running tests with coverage..."
cabal test all --coverage

echo "Coverage report generated!"
echo "Open tix/*.html in a browser to view coverage."