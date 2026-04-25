#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

MODE="write"
if [[ "${1:-}" == "--check" ]]; then
  MODE="check"
fi

require_tool() {
  local tool="$1"
  local hint="$2"
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "Missing required tool: $tool" >&2
    echo "Install hint: $hint" >&2
    exit 1
  fi
}

pick_haskell_formatter() {
  if command -v fourmolu >/dev/null 2>&1; then
    echo "fourmolu"
    return
  fi
  if command -v ormolu >/dev/null 2>&1; then
    echo "ormolu"
    return
  fi

  echo "Missing required tool: fourmolu or ormolu" >&2
  echo "Install hint: cabal install fourmolu  (or: cabal install ormolu)" >&2
  exit 1
}

collect_files() {
  local pattern="$1"
  git ls-files -z -- "$pattern"
}

HASKELL_FORMATTER="$(pick_haskell_formatter)"
require_tool shfmt "brew install shfmt  (or package manager of choice)"
require_tool black "python3 -m pip install black"
require_tool dhall "brew install dhall"

mapfile -d '' HASKELL_FILES < <(collect_files '*.hs')
mapfile -d '' BASH_FILES < <(collect_files '*.sh')
mapfile -d '' PYTHON_FILES < <(collect_files '*.py')
mapfile -d '' MF_FILES < <(collect_files '*.mf')

echo "Formatting Haskell with $HASKELL_FORMATTER"
if [[ ${#HASKELL_FILES[@]} -gt 0 ]]; then
  if [[ "$MODE" == "check" ]]; then
    if [[ "$HASKELL_FORMATTER" == "fourmolu" ]]; then
      "$HASKELL_FORMATTER" --mode check "${HASKELL_FILES[@]}"
    else
      "$HASKELL_FORMATTER" --mode check "${HASKELL_FILES[@]}"
    fi
  else
    if [[ "$HASKELL_FORMATTER" == "fourmolu" ]]; then
      "$HASKELL_FORMATTER" --mode inplace "${HASKELL_FILES[@]}"
    else
      "$HASKELL_FORMATTER" --mode inplace "${HASKELL_FILES[@]}"
    fi
  fi
fi

echo "Formatting Bash with shfmt"
if [[ ${#BASH_FILES[@]} -gt 0 ]]; then
  if [[ "$MODE" == "check" ]]; then
    shfmt -d -i 2 -ci "${BASH_FILES[@]}"
  else
    shfmt -w -i 2 -ci "${BASH_FILES[@]}"
  fi
fi

echo "Formatting Python with black"
if [[ ${#PYTHON_FILES[@]} -gt 0 ]]; then
  if [[ "$MODE" == "check" ]]; then
    black --check "${PYTHON_FILES[@]}"
  else
    black "${PYTHON_FILES[@]}"
  fi
fi

echo "Formatting micro-Fω (.mf) with dhall format"
if [[ ${#MF_FILES[@]} -gt 0 ]]; then
  if [[ "$MODE" == "check" ]]; then
    dhall format --check "${MF_FILES[@]}"
  else
    dhall format --inplace "${MF_FILES[@]}"
  fi
fi

echo "Reformatting complete ($MODE mode)."
