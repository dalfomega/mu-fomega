#!/usr/bin/env python3
"""Generate static benchmark history report with commit-based plots.

Features:
- Reads one or more Criterion CSV files.
- Detects commit hash suffix in filenames like: parser-bench-<hash>.csv
- Always plots Mean values with Stddev error bars.
- Groups plots by first factor segment (e.g. "attoparsec", "builtin-ops").
- Uses second factor segment as the line series in each plot.
- If multiple n=... values exist for the same series, keeps only the largest n.
- Lets users enable/disable each input file in the HTML UI.
- Opens the generated report in a browser after writing it.
"""

from __future__ import annotations

import argparse
import csv
import html
import json
import os
import re
import subprocess
import sys
from collections import OrderedDict, defaultdict
from dataclasses import dataclass
from statistics import fmean
from typing import Dict, Iterable, List, Mapping, Sequence, Tuple

HASHED_FILENAME_RE = re.compile(r"^(?P<dataset>.+)-(?P<commit>[0-9a-f]{7,40})$")
HASH_RE = re.compile(r"^[0-9a-f]{7,40}$")


@dataclass(frozen=True)
class ParsedFactor:
    group: str
    series: str
    n_value: int | None
    options: Tuple[Tuple[str, str], ...]
    flags: Tuple[str, ...]


@dataclass(frozen=True)
class RawPoint:
    file_id: str
    commit: str
    group: str
    series: str
    n_value: int | None
    mean: float
    stddev: float
    options: Tuple[Tuple[str, str], ...]
    flags: Tuple[str, ...]


@dataclass(frozen=True)
class ReducedPoint:
    file_id: str
    commit: str
    group: str
    series: str
    n_value: int | None
    mean: float
    stddev: float
    options_text: str


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate static benchmark history HTML report."
    )
    parser.add_argument("csv_files", nargs="+", help="Input CSV files.")
    parser.add_argument(
        "-o",
        "--output",
        default="bench-report.html",
        help="Output HTML path (default: bench-report.html).",
    )
    parser.add_argument(
        "--no-open",
        action="store_true",
        help="Do not open the generated report in a browser.",
    )
    return parser.parse_args(argv)


def infer_dataset_and_commit(path: str) -> Tuple[str, str]:
    base = os.path.basename(path)
    stem, _ext = os.path.splitext(base)
    match = HASHED_FILENAME_RE.match(stem)
    if match:
        return match.group("dataset"), match.group("commit")
    return stem, f"unversioned:{stem}"


def parse_factor_name(name: str) -> ParsedFactor:
    parts = [part for part in name.split("/") if part]

    group = parts[0] if parts else "unknown"
    tail = parts[1:] if len(parts) > 1 else []

    series = "default"

    options: List[Tuple[str, str]] = []
    flags: List[str] = []
    n_value: int | None = None

    for token in tail:
        if "=" in token:
            key, value = token.split("=", 1)
            key = key.strip()
            value = value.strip()
            options.append((key, value))
            if key == "n":
                try:
                    n_value = int(value)
                except ValueError:
                    pass
        else:
            cleaned = token.strip()
            if series == "default":
                series = cleaned
            else:
                flags.append(cleaned)

    return ParsedFactor(
        group=group,
        series=series,
        n_value=n_value,
        options=tuple(options),
        flags=tuple(flags),
    )


def iter_criterion_rows(path: str) -> Iterable[Mapping[str, str]]:
    """Yield rows while tolerating repeated Criterion header blocks."""
    header: List[str] | None = None
    with open(path, newline="", encoding="utf-8") as handle:
        reader = csv.reader(handle)
        for row in reader:
            if not row:
                continue
            if all(not cell.strip() for cell in row):
                continue

            if row[0].strip() == "Name":
                header = [cell.strip() for cell in row]
                continue

            if header is None or len(row) < len(header):
                continue

            values = [cell.strip() for cell in row[: len(header)]]
            yield dict(zip(header, values))


def safe_float(value: str | None) -> float | None:
    if value is None:
        return None
    try:
        return float(value)
    except ValueError:
        return None


def collect_points(
    paths: Sequence[str],
) -> Tuple[List[RawPoint], Dict[str, Dict[str, str]], List[str]]:
    raw_points: List[RawPoint] = []
    files_meta: Dict[str, Dict[str, str]] = {}
    commit_seen_order: "OrderedDict[str, None]" = OrderedDict()

    for path in paths:
        abs_path = os.path.abspath(path)
        if not os.path.exists(abs_path):
            continue

        dataset, commit = infer_dataset_and_commit(abs_path)
        file_id = os.path.basename(abs_path)
        files_meta[file_id] = {
            "id": file_id,
            "path": abs_path,
            "label": os.path.basename(abs_path),
            "dataset": dataset,
            "commit": commit,
        }
        commit_seen_order.setdefault(commit, None)

        for row in iter_criterion_rows(abs_path):
            factor_name = row.get("Name")
            if not factor_name:
                continue

            mean = safe_float(row.get("Mean"))
            stddev = safe_float(row.get("Stddev"))
            if mean is None:
                continue
            if stddev is None:
                stddev = 0.0

            parsed = parse_factor_name(factor_name)
            raw_points.append(
                RawPoint(
                    file_id=file_id,
                    commit=commit,
                    group=parsed.group,
                    series=parsed.series,
                    n_value=parsed.n_value,
                    mean=mean,
                    stddev=stddev,
                    options=parsed.options,
                    flags=parsed.flags,
                )
            )

    return raw_points, files_meta, list(commit_seen_order.keys())


def reduce_points(points: Sequence[RawPoint]) -> List[ReducedPoint]:
    """Keep largest n per (file,commit,group,series), then average duplicates."""
    grouped: Dict[Tuple[str, str, str, str], List[RawPoint]] = defaultdict(list)
    for point in points:
        key = (point.file_id, point.commit, point.group, point.series)
        grouped[key].append(point)

    reduced: List[ReducedPoint] = []
    for key, bucket in grouped.items():
        n_values = [p.n_value for p in bucket if p.n_value is not None]
        max_n = max(n_values) if n_values else None

        if max_n is None:
            selected = bucket
        else:
            selected = [p for p in bucket if p.n_value == max_n]

        mean = fmean(p.mean for p in selected)
        stddev = fmean(p.stddev for p in selected)

        option_tokens: List[str] = []
        seen = set()
        for point in selected:
            for key_opt, value_opt in point.options:
                token = f"{key_opt}={value_opt}"
                if token not in seen:
                    seen.add(token)
                    option_tokens.append(token)
            for flag in point.flags:
                if flag and flag not in seen:
                    seen.add(flag)
                    option_tokens.append(flag)
        options_text = ", ".join(option_tokens)

        file_id, commit, group, series = key
        reduced.append(
            ReducedPoint(
                file_id=file_id,
                commit=commit,
                group=group,
                series=series,
                n_value=max_n,
                mean=mean,
                stddev=stddev,
                options_text=options_text,
            )
        )

    return reduced


def order_commits(commits: Sequence[str]) -> List[str]:
    hash_like = [commit for commit in commits if HASH_RE.fullmatch(commit)]
    non_hash = [commit for commit in commits if commit not in hash_like]

    if not hash_like:
        return list(commits)

    timed: List[Tuple[str, int]] = []
    unknown_hashes: List[str] = []

    for commit in hash_like:
        try:
            output = subprocess.check_output(
                ["git", "show", "-s", "--format=%ct", commit],
                stderr=subprocess.DEVNULL,
                text=True,
            ).strip()
            timed.append((commit, int(output)))
        except Exception:
            unknown_hashes.append(commit)

    if not timed:
        return list(commits)

    timed_sorted = [commit for commit, _ts in sorted(timed, key=lambda item: item[1])]
    first_seen = {commit: index for index, commit in enumerate(commits)}
    unknown_hashes.sort(key=lambda commit: first_seen[commit])
    non_hash.sort(key=lambda commit: first_seen[commit])
    return timed_sorted + unknown_hashes + non_hash


def short_commit_label(commit: str) -> str:
    if HASH_RE.fullmatch(commit):
        return commit[:7]
    if commit.startswith("unversioned:"):
        stem = commit.split(":", 1)[1]
        return stem if len(stem) <= 18 else f"{stem[:18]}..."
    return commit if len(commit) <= 18 else f"{commit[:18]}..."


def build_payload(
    reduced: Sequence[ReducedPoint],
    files_meta: Mapping[str, Mapping[str, str]],
    commit_order: Sequence[str],
) -> Dict[str, object]:
    index: Dict[str, Dict[str, Dict[str, Dict[str, Dict[str, object]]]]] = defaultdict(
        lambda: defaultdict(lambda: defaultdict(dict))
    )

    groups_set = set()
    series_by_group: Dict[str, set[str]] = defaultdict(set)

    for point in reduced:
        groups_set.add(point.group)
        series_by_group[point.group].add(point.series)
        index[point.group][point.series][point.commit][point.file_id] = {
            "mean": point.mean,
            "stddev": point.stddev,
            "n": point.n_value,
            "options": point.options_text,
        }

    files = [files_meta[file_id] for file_id in sorted(files_meta.keys())]

    return {
        "files": files,
        "commits": list(commit_order),
        "commit_labels": {
            commit: short_commit_label(commit) for commit in commit_order
        },
        "groups": sorted(groups_set),
        "series_by_group": {
            group: sorted(series) for group, series in series_by_group.items()
        },
        "index": index,
    }


def build_html(payload: Mapping[str, object], input_paths: Sequence[str]) -> str:
    input_paths_html = "\n".join(html.escape(path) for path in input_paths)
    payload_json = json.dumps(payload)

    return f"""<!doctype html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\" />
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
  <title>Benchmark Report</title>
  <script src=\"https://cdn.plot.ly/plotly-2.35.2.min.js\"></script>
  <style>
    body {{ font-family: -apple-system, BlinkMacSystemFont, Segoe UI, sans-serif; margin: 1rem 1.25rem; }}
    h1 {{ margin: 0 0 0.75rem 0; }}
    .controls {{ margin: 0.75rem 0 1rem 0; padding: 0.75rem; border: 1px solid #ddd; border-radius: 8px; }}
    .file-list {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(340px, 1fr)); gap: 0.4rem 1rem; margin-top: 0.65rem; }}
    .file-item {{ display: flex; gap: 0.5rem; align-items: center; }}
    .button-row {{ display: flex; gap: 0.5rem; }}
    button {{ cursor: pointer; border: 1px solid #bdbdbd; background: #fff; border-radius: 6px; padding: 0.35rem 0.6rem; }}
    .chart {{ width: 100%; height: 560px; margin: 1.5rem 0; border: 1px solid #ddd; border-radius: 8px; }}
    .muted {{ color: #555; font-size: 0.92rem; }}
    details {{ margin: 0.65rem 0 0.8rem 0; }}
    pre {{ background: #f7f7f7; padding: 0.75rem; border: 1px solid #eee; overflow: auto; }}
  </style>
</head>
<body>
  <h1>Benchmark History Report</h1>

  <div class=\"controls\">
    <div class=\"button-row\">
      <button id=\"enable-all\" type=\"button\">Enable all input files</button>
      <button id=\"disable-all\" type=\"button\">Disable all input files</button>
    </div>
    <div class=\"file-list\" id=\"file-list\"></div>
    <div class=\"muted\">Mean is plotted with Stddev error bars. For each series, only the largest n is used.</div>
  </div>

  <details>
    <summary>Raw input files</summary>
    <pre>{input_paths_html}</pre>
  </details>

  <div id=\"charts\"></div>

  <script>
    const payload = {payload_json};
    const state = {{
      enabledFiles: new Set(payload.files.map((file) => file.id)),
    }};

    function mean(values) {{
      if (values.length === 0) return null;
      return values.reduce((acc, x) => acc + x, 0) / values.length;
    }}

    function fileById(fileId) {{
      return payload.files.find((f) => f.id === fileId);
    }}

    function buildFileList() {{
      const container = document.getElementById("file-list");
      container.innerHTML = "";

      for (const file of payload.files) {{
        const id = `file-toggle-${{CSS.escape(file.id)}}`;

        const row = document.createElement("div");
        row.className = "file-item";

        const checkbox = document.createElement("input");
        checkbox.type = "checkbox";
        checkbox.id = id;
        checkbox.checked = state.enabledFiles.has(file.id);
        checkbox.addEventListener("change", () => {{
          if (checkbox.checked) state.enabledFiles.add(file.id);
          else state.enabledFiles.delete(file.id);
          renderCharts();
        }});

        const label = document.createElement("label");
        label.setAttribute("for", id);
        const shortCommit = payload.commit_labels[file.commit] || file.commit;
        label.textContent = `${{file.label}}  [${{shortCommit}}]`;

        row.appendChild(checkbox);
        row.appendChild(label);
        container.appendChild(row);
      }}
    }}

    function aggregatePoint(group, series, commit) {{
      const perFile = ((((payload.index[group] || {{}})[series] || {{}})[commit]) || {{}});
      const means = [];
      const stddevs = [];
      const detailRows = [];

      for (const [fileId, point] of Object.entries(perFile)) {{
        if (!state.enabledFiles.has(fileId)) continue;
        means.push(point.mean);
        stddevs.push(point.stddev);

        const file = fileById(fileId);
        const fileName = file ? file.label : fileId;
        const nInfo = point.n === null ? "n=?" : `n=${{point.n}}`;
        const opts = point.options ? `, opts: ${{point.options}}` : "";
        detailRows.push(`${{fileName}}: mean=${{point.mean}}, stddev=${{point.stddev}}, ${{nInfo}}${{opts}}`);
      }}

      if (means.length === 0) {{
        return {{ y: null, err: null, detail: "No enabled file data" }};
      }}

      return {{
        y: mean(means),
        err: mean(stddevs),
        detail: detailRows.join("<br>"),
      }};
    }}

    function tracesForGroup(group) {{
      const seriesList = (payload.series_by_group[group] || []).slice().sort();
      const commits = payload.commits;
      const traces = [];

      for (const series of seriesList) {{
        const y = [];
        const errors = [];
        const details = [];

        for (const commit of commits) {{
          const aggregated = aggregatePoint(group, series, commit);
          y.push(aggregated.y);
          errors.push(aggregated.err);
          details.push(aggregated.detail);
        }}

        if (y.every((v) => v === null)) continue;

        traces.push({{
          x: commits,
          y,
          name: series,
          type: "scatter",
          mode: "lines+markers",
          connectgaps: false,
          error_y: {{
            type: "data",
            array: errors,
            visible: true,
            thickness: 1,
            width: 4,
          }},
          customdata: details,
          hovertemplate:
            "series=%{{fullData.name}}<br>commit=%{{x}}<br>mean=%{{y}}<br>%{{customdata}}<extra></extra>",
        }});
      }}

      return traces;
    }}

    function renderCharts() {{
      const container = document.getElementById("charts");
      container.innerHTML = "";

      for (const group of payload.groups) {{
        const chart = document.createElement("div");
        chart.className = "chart";
        container.appendChild(chart);

        const traces = tracesForGroup(group);
        const commitLabels = payload.commits.map((c) => payload.commit_labels[c] || c);

        Plotly.newPlot(
          chart,
          traces,
          {{
            title: group,
            xaxis: {{
              type: "category",
              tickvals: payload.commits,
              ticktext: commitLabels,
              tickangle: -35,
            }},
            yaxis: {{ title: "Mean" }},
            margin: {{ l: 72, r: 20, t: 50, b: 105 }},
            hovermode: "x unified",
          }},
          {{ responsive: true, displaylogo: false }}
        );
      }}
    }}

    document.getElementById("enable-all").addEventListener("click", () => {{
      state.enabledFiles = new Set(payload.files.map((file) => file.id));
      buildFileList();
      renderCharts();
    }});

    document.getElementById("disable-all").addEventListener("click", () => {{
      state.enabledFiles = new Set();
      buildFileList();
      renderCharts();
    }});

    buildFileList();
    renderCharts();
  </script>
</body>
</html>
"""


def open_report_in_browser(path: str) -> None:
    abs_path = os.path.abspath(path)

    command: List[str] | None
    if sys.platform == "darwin":
        command = ["open", abs_path]
    elif sys.platform.startswith("linux"):
        command = ["xdg-open", abs_path]
    elif os.name == "nt":
        command = ["cmd", "/c", "start", "", abs_path]
    else:
        command = None

    if command is None:
        return

    try:
        subprocess.Popen(command, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except Exception:
        pass


def main(argv: Sequence[str]) -> int:
    args = parse_args(argv)

    missing = [path for path in args.csv_files if not os.path.exists(path)]
    if missing:
        for path in missing:
            print(f"error: file not found: {path}", file=sys.stderr)
        return 2

    raw_points, files_meta, commit_first_seen = collect_points(args.csv_files)
    if not raw_points:
        print("error: no benchmark points found in inputs", file=sys.stderr)
        return 2

    reduced = reduce_points(raw_points)
    ordered_commits = order_commits(commit_first_seen)
    payload = build_payload(reduced, files_meta, ordered_commits)
    html_doc = build_html(payload, args.csv_files)

    with open(args.output, "w", encoding="utf-8") as handle:
        handle.write(html_doc)

    print(f"Wrote report: {args.output}")
    print(f"Input files: {len(files_meta)}")
    print(f"Points (raw): {len(raw_points)}")
    print(f"Points (reduced): {len(reduced)}")

    if not args.no_open:
        open_report_in_browser(args.output)
        print("Opened report in browser.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
