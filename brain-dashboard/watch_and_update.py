#!/usr/bin/env python3
"""Watch for new trace files and rebuild canonical store automatically."""

import time
import subprocess
from pathlib import Path

TRACES_DIR = Path("/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces")
CANONICAL_STORE = Path("/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json")
BUILD_SCRIPT = Path("/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/build_canonical_store.py")
RESCORE_SCRIPT = Path("/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/rescore_games.py")

def get_latest_trace_time():
    """Get timestamp of most recent trace file."""
    traces = list(TRACES_DIR.glob("*.txt"))
    if not traces:
        return 0
    return max(t.stat().st_mtime for t in traces)

def rebuild_canonical_store():
    """Rebuild canonical store and fix any corrupted scores."""
    print(f"\n[{time.strftime('%H:%M:%S')}] Rebuilding canonical store...")

    try:
        # Build canonical store
        result = subprocess.run(
            ["python3", str(BUILD_SCRIPT)],
            capture_output=True,
            text=True,
            timeout=600
        )

        if result.returncode == 0:
            print(f"[{time.strftime('%H:%M:%S')}] ✓ Canonical store rebuilt")

            # Run rescore to fix any scores >1.0
            rescore_result = subprocess.run(
                ["python3", str(RESCORE_SCRIPT)],
                capture_output=True,
                text=True,
                timeout=300
            )

            if rescore_result.returncode == 0:
                # Check if there were any bad scores
                if "Re-scored 0 games" not in rescore_result.stdout:
                    print(f"[{time.strftime('%H:%M:%S')}] ✓ Re-scored corrupted games")
            else:
                print(f"[{time.strftime('%H:%M:%S')}] ⚠ Rescore failed")

        else:
            print(f"[{time.strftime('%H:%M:%S')}] ✗ Rebuild failed: {result.stderr[:200]}")

    except subprocess.TimeoutExpired:
        print(f"[{time.strftime('%H:%M:%S')}] ✗ Rebuild timed out")
    except Exception as e:
        print(f"[{time.strftime('%H:%M:%S')}] ✗ Error: {e}")

def main():
    """Watch for new trace files and rebuild canonical store."""
    print("=" * 60)
    print("CANONICAL STORE AUTO-UPDATER")
    print("=" * 60)
    print(f"Watching: {TRACES_DIR}")
    print(f"Canonical store: {CANONICAL_STORE}")
    print(f"Check interval: 60 seconds")
    print("=" * 60)
    print()

    last_trace_time = get_latest_trace_time()
    last_rebuild_time = time.time()

    # Initial rebuild
    rebuild_canonical_store()

    while True:
        time.sleep(60)  # Check every minute

        current_trace_time = get_latest_trace_time()

        # Rebuild if:
        # 1. New traces appeared (trace time changed)
        # 2. It's been more than 5 minutes since last rebuild (periodic update)
        should_rebuild = False
        reason = ""

        if current_trace_time > last_trace_time:
            should_rebuild = True
            reason = "new traces detected"
            last_trace_time = current_trace_time
        elif (time.time() - last_rebuild_time) > 300:  # 5 minutes
            should_rebuild = True
            reason = "periodic update"

        if should_rebuild:
            print(f"\n[{time.strftime('%H:%M:%S')}] Trigger: {reason}")
            rebuild_canonical_store()
            last_rebuild_time = time.time()

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nStopped by user")
