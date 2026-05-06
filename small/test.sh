#!/bin/bash

# Test the "small" Haskell project

# Change to the directory
cd "$(git rev-parse --show-toplevel)/small"

set -e

hpack

echo "Running tests for small project..."
cabal test all
echo "Tests completed successfully."
