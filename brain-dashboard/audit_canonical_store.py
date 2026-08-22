#!/usr/bin/env python3
"""Audit canonical store against actual trace files - find discrepancies."""

import json
import re
from pathlib import Path
from collections import defaultdict

MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
TRACES_DIR = MCTS_ROOT / "arena-traces"
CANONICAL_STORE = MCTS_ROOT / "brain-dashboard" / "canonical_games.json"

def audit_canonical_store():
    """Compare canonical store against actual trace files."""
    print("=== AUDITING CANONICAL STORE AGAINST ACTUAL FILES ===\n")

    # Load canonical store
    if not CANONICAL_STORE.exists():
        print(f"❌ Canonical store not found: {CANONICAL_STORE}")
        return

    store = json.loads(CANONICAL_STORE.read_text())
    canonical_games = {g["filename"]: g for g in store["games"]}

    # Scan actual trace files
    actual_files = set()
    for trace_file in TRACES_DIR.glob("*.txt"):
        stem = trace_file.stem
        if stem.startswith("arena-") or stem.startswith("selfplay-"):
            actual_files.add(stem)

    # Compare
    canonical_files = set(canonical_games.keys())

    # Files in canonical but not on disk
    missing_files = canonical_files - actual_files
    if missing_files:
        print(f"⚠ {len(missing_files)} files in canonical store BUT NOT ON DISK:")
        for f in sorted(missing_files)[:10]:
            print(f"  - {f}")
        if len(missing_files) > 10:
            print(f"  ... and {len(missing_files) - 10} more")
        print()

    # Files on disk but not in canonical
    extra_files = actual_files - canonical_files
    if extra_files:
        print(f"⚠ {len(extra_files)} files ON DISK BUT NOT IN CANONICAL STORE:")
        for f in sorted(extra_files)[:10]:
            print(f"  - {f}")
        if len(extra_files) > 10:
            print(f"  ... and {len(extra_files) - 10} more")
        print()

    if not missing_files and not extra_files:
        print(f"✓ All {len(actual_files)} trace files are in canonical store")
        print()

    # Sample audit: verify doom values for R26 arena games
    print("Sampling R26 arena games to verify doom values...")
    r26_arena_canonical = [g for g in store["games"] if g["run"] == "R26" and g["type"] == "arena"]

    if r26_arena_canonical:
        print(f"  Found {len(r26_arena_canonical)} R26 arena games in canonical store")

        # Pick 5 random games and verify their doom values
        import random
        sample = random.sample(r26_arena_canonical, min(5, len(r26_arena_canonical)))

        for game in sample:
            filename = game["filename"]
            trace_file = TRACES_DIR / f"{filename}.txt"

            if not trace_file.exists():
                print(f"  ❌ {filename}: file not found")
                continue

            # Parse doom from file
            content = trace_file.read_text()
            doom_match = re.search(r'ALL_DOOM=(.+)', content)
            if doom_match:
                doom_str = doom_match.group(1).strip()
                actual_doom = 0
                for pair in doom_str.split():
                    if '=' in pair:
                        faction, d = pair.split('=')
                        actual_doom = int(d)
                        break

                canonical_doom = game["doom"]
                if actual_doom == canonical_doom:
                    print(f"  ✓ {filename}: doom {canonical_doom} MATCHES")
                else:
                    print(f"  ❌ {filename}: doom MISMATCH - canonical={canonical_doom}, actual={actual_doom}")
            else:
                print(f"  ⚠ {filename}: could not parse doom from file")
        print()

    # Check for data quality issues
    print("Data quality checks:")
    issues = []

    # Score >1.0
    high_scores = [g for g in store["games"] if g["score"] > 1.0]
    if high_scores:
        max_score = max(g["score"] for g in high_scores)
        issues.append(f"{len(high_scores)} games with score >1.0 (max {max_score:.3f})")

    # Doom=0 and no win
    zero_doom_no_win = [g for g in store["games"] if g["doom"] == 0 and not g["won"]]
    if zero_doom_no_win:
        issues.append(f"{len(zero_doom_no_win)} games with doom=0 and no win")

    # Negative doom
    negative_doom = [g for g in store["games"] if g["doom"] < 0]
    if negative_doom:
        issues.append(f"{len(negative_doom)} games with negative doom")

    # Negative score
    negative_score = [g for g in store["games"] if g["score"] < 0]
    if negative_score:
        issues.append(f"{len(negative_score)} games with negative score")

    if issues:
        print("  ⚠ ISSUES FOUND:")
        for issue in issues:
            print(f"    - {issue}")
    else:
        print("  ✓ No data quality issues found")

    print()

    # Summary by run
    print("Summary by run:")
    by_run = defaultdict(lambda: {"total": 0, "arena": 0, "selfplay": 0})
    for game in store["games"]:
        by_run[game["run"]]["total"] += 1
        by_run[game["run"]][game["type"]] += 1

    for run in sorted(by_run.keys()):
        d = by_run[run]
        print(f"  {run}: {d['total']} total ({d['arena']} arena, {d['selfplay']} selfplay)")

    print()
    print("✓ Audit complete")

if __name__ == "__main__":
    audit_canonical_store()
