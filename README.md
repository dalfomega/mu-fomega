# Implementation techniques for System Fω

This repository contains exploratory code testing various implementation techniques for System Fω.

The language is implemented in the style of Dhall.

See [micro-Fω-spec.md](./micro-Fω-spec.md) for a complete formal specification of the language.

## CLI usage

The `mu-fomega` executable reads an expression from `--input FILE` (or stdin), parses it,
type-checks it, normalizes it, and prints the normal form. Parse/type errors are reported
with source spans.

Example:

```bash
$ cabal run exe:mu-fomega -- --parser flatparse --evaluator nbe-hoas --ast strict --input examples/readme.mf
1144
```

The program `examples/readme.mf` evaluates to the number 1144.

You can also pipe input directly:

```bash
$ echo 'let id = λ(a : Type) → λ(x : a) → x in id Natural 20' | cabal run exe:mu-fomega --
20
```

The script `scripts/run-cli.sh` runs the executable:

```bash
$ ./scripts/run-cli.sh --input examples/readme.mf
1144
```

Example programs are available in `examples/`:

- `examples/readme.mf`
- `examples/polymorphic-functions.mf`
- `examples/church-naturals.mf`
- `examples/church-lists.mf`

## Formatting

Run all formatters via:

```bash
./scripts/reformat.sh
```

Check formatting without modifying files:

```bash
./scripts/reformat.sh --check
```

The script formats tracked files (`*.hs`, `*.sh`, `*.py`) using:

- `fourmolu` (or `ormolu`) for Haskell
- `shfmt` for Bash
- `black` for Python
