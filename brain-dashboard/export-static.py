#!/usr/bin/env python3
"""
Export brain dashboard data for online deployment.
Keeps last 5 runs only to save space.
"""

import json
import sys
import re
import shutil
from pathlib import Path
from datetime import datetime

# Source directories
MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
TRACES_DIR = MCTS_ROOT / "arena-traces"
CHECKPOINTS = MCTS_ROOT / "checkpoints"

# Export directory
EXPORT_DIR = Path("/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/export")

def get_run_from_file(trace_file):
    """Extract run number from filename or file timestamp."""
    filename = trace_file.name

    # Try parsing filename first
    # Format: arena-R40-iter1-... or selfplay_R25_01_GC_first.txt
    match = re.search(r'[-_]R(\d+)[-_]', filename)
    if match:
        return int(match.group(1))

    # Old format: selfplay_025_001_GC_first.txt
    if "_0" in filename:
        match = re.search(r'_(\d{3})_', filename)
        if match:
            return int(match.group(1))

    # Fallback: timestamp-based run assignment (for very old files)
    file_time = trace_file.stat().st_mtime
    if file_time >= 1786770000:  # Aug 15, 2026
        return 25
    elif file_time >= 1786424400:  # Aug 11, 2026
        return 24
    else:
        return 23

def collect_traces_by_run():
    """Collect all traces grouped by run number."""
    runs = {}

    if not TRACES_DIR.exists():
        return runs

    for trace_file in TRACES_DIR.glob("*.txt"):
        run_num = get_run_from_file(trace_file)

        if run_num not in runs:
            runs[run_num] = {"selfplay": [], "arena": []}

        # Determine type from filename
        if trace_file.name.startswith("selfplay-"):
            runs[run_num]["selfplay"].append(trace_file)
        elif trace_file.name.startswith("arena-"):
            runs[run_num]["arena"].append(trace_file)

    return runs

def export_for_online():
    """Export last 5 runs to online deployment directory."""

    # Collect traces by run
    runs_data = collect_traces_by_run()

    # Get last 5 runs
    sorted_runs = sorted(runs_data.keys(), reverse=True)[:5]

    if not sorted_runs:
        print("No runs found to export", file=sys.stderr)
        return

    print(f"Exporting runs: {sorted_runs}")

    # Create export directory
    EXPORT_DIR.mkdir(parents=True, exist_ok=True)

    # Clear old data
    export_traces = EXPORT_DIR / "arena-traces"
    if export_traces.exists():
        shutil.rmtree(export_traces)
    export_traces.mkdir(parents=True, exist_ok=True)

    # Copy traces for selected runs
    total_files = 0
    total_size = 0

    for run_num in sorted_runs:
        run_traces = runs_data[run_num]

        # Copy selfplay traces
        for trace_file in run_traces["selfplay"]:
            dest = export_traces / trace_file.name
            shutil.copy2(trace_file, dest)
            total_files += 1
            total_size += trace_file.stat().st_size

        # Copy arena traces
        for trace_file in run_traces["arena"]:
            dest = export_traces / trace_file.name
            shutil.copy2(trace_file, dest)
            total_files += 1
            total_size += trace_file.stat().st_size

    # Copy checkpoint metadata (best.meta and latest checkpoints for these runs)
    checkpoints_dir = EXPORT_DIR / "checkpoints"
    checkpoints_dir.mkdir(exist_ok=True)

    if CHECKPOINTS.exists():
        # Copy best.meta
        best_meta = CHECKPOINTS / "best.meta"
        if best_meta.exists():
            shutil.copy2(best_meta, checkpoints_dir / "best.meta")

        # Copy checkpoint metadata for selected runs
        for run_num in sorted_runs:
            for ckpt_file in CHECKPOINTS.glob(f"*R{run_num:02d}*.meta"):
                shutil.copy2(ckpt_file, checkpoints_dir / ckpt_file.name)

    # Copy run logs
    for run_num in sorted_runs:
        log_file = Path(f"/tmp/sp_R{run_num}.log")
        if log_file.exists():
            shutil.copy2(log_file, EXPORT_DIR / f"sp_R{run_num}.log")

    # Generate summary
    summary = {
        "generated": datetime.now().isoformat(),
        "runs_included": sorted_runs,
        "total_files": total_files,
        "total_size_mb": round(total_size / 1024 / 1024, 2),
        "note": "Last 5 runs only"
    }

    (EXPORT_DIR / "export-summary.json").write_text(json.dumps(summary, indent=2))

    print(f"\nExport complete:")
    print(f"  Runs: {sorted_runs}")
    print(f"  Files: {total_files}")
    print(f"  Size: {summary['total_size_mb']} MB")
    print(f"  Export dir: {EXPORT_DIR}")

    return EXPORT_DIR

if __name__ == "__main__":
    export_for_online()
