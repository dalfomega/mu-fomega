#!/usr/bin/env bash
set -e

echo "Running hpack to generate .cabal file..."
hpack

echo "Running tests with coverage instrumentation..."
cabal test mu-fomega-test --enable-coverage

shopt -s nullglob

tix_candidates=(dist-newstyle/build/*/ghc-*/mu-fomega-*/t/mu-fomega-test/hpc/vanilla/tix/mu-fomega-test.tix)

if [ ${#tix_candidates[@]} -eq 0 ]; then
  echo "No coverage .tix file found for mu-fomega-test" >&2
  exit 1
fi

tix_path="${tix_candidates[0]}"

mix_dirs=(
  dist-newstyle/build/*/ghc-*/mu-fomega-*/build/extra-compilation-artifacts/hpc/vanilla/mix
  dist-newstyle/build/*/ghc-*/mu-fomega-*/t/mu-fomega-test/build/mu-fomega-test/mu-fomega-test-tmp/extra-compilation-artifacts/hpc/vanilla/mix
)

hpcdir_args=()
for mix_dir in "${mix_dirs[@]}"; do
  hpcdir_args+=("--hpcdir=${mix_dir}")
done

report_excludes=("--exclude=mu-fomega-0.1.0.0-inplace-mu-fomega-test:")

echo "Generating coverage report..."
hpc markup "${tix_path}" "${hpcdir_args[@]}" "${report_excludes[@]}" --destdir="coverage-html"

echo "Collecting XML coverage report..."
hpc report "${tix_path}" "${hpcdir_args[@]}" "${report_excludes[@]}" --per-module --xml-output >coverage.xml

exceptions_file="docs/coverage-exceptions.txt"
if [ ! -f "${exceptions_file}" ]; then
  exceptions_file="/dev/null"
fi

echo "Validating 100% module coverage for library metrics..."

awk -v EXCEPTIONS="${exceptions_file}" '
  function short_module(name,    n) {
    split(name, n, "/");
    return n[2];
  }

  function attr_num(line, key,    r, s, p) {
    r = key "=\"[0-9]+\"";
    if (match(line, r)) {
      s = substr(line, RSTART, RLENGTH);
      split(s, p, "\"");
      return p[2] + 0;
    }
    return 0;
  }

  function check_metric(metric, line,    boxes, count, cov, shortName) {
    if (currentModule == "") {
      return;
    }
    if (currentModule !~ /-inplace\/MuFomega/) {
      return;
    }

    shortName = short_module(currentModule);
    boxes = attr_num(line, "boxes");
    count = attr_num(line, "count");
    cov = (boxes == 0) ? 100.0 : (100.0 * count / boxes);

    if (cov < 100.0 && !(shortName in exempt)) {
      printf("%s below 100%%: %s => %.2f%% (%d/%d)\n", metric, shortName, cov, count, boxes);
      bad = 1;
    }
  }

  BEGIN {
    while ((getline line < EXCEPTIONS) > 0) {
      if (line ~ /^#/ || line ~ /^[[:space:]]*$/) {
        continue;
      }
      split(line, parts, "|");
      exempt[parts[1]] = 1;
    }
    close(EXCEPTIONS);
  }
  /<module[[:space:]]+name[[:space:]]*=/ {
    currentModule = "";
    if (match($0, /name[[:space:]]*=[[:space:]]*"[^"]+"/)) {
      key = substr($0, RSTART, RLENGTH);
      split(key, a, "\"");
      currentModule = a[2];
    }
    next;
  }
  /<\/module>/ {
    currentModule = "";
    next;
  }
  /<exprs[[:space:]]+boxes=/ {
    check_metric("Expressions", $0);
    }
  /<alts[[:space:]]+boxes=/ {
    check_metric("Alternatives", $0);
    next;
  }
  /<local[[:space:]]+boxes=/ {
    check_metric("Local", $0);
    next;
  }
  /<toplevel[[:space:]]+boxes=/ {
    check_metric("TopLevel", $0);
    next;
  }
  END {
    if (bad) {
      exit 1;
    }
  }
' coverage.xml

echo "Coverage threshold satisfied. HTML report at coverage-html/hpc_index.html"
