#!/bin/bash

# Build the "small" Haskell project independently

# Change to the directory

cd "$(git rev-parse --show-toplevel)/small"

set -e

hpack

echo "Building small project..."
cabal build all
echo "Build completed successfully."
