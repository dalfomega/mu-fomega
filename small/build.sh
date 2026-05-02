#!/bin/bash

# Build the small Haskell project independently

set -e

hpack

echo "Building small project..."
cabal build all
echo "Build completed successfully."
