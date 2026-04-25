#!/usr/bin/env bash
set -euo pipefail

#hpack

cabal run exe:mu-fomega -- "$@"
