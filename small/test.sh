#!/bin/bash

# Test the small Haskell project independently

set -e

echo "Testing small project..."
cabal test all
echo "Tests completed successfully."
