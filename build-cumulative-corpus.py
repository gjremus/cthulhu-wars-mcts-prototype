#!/usr/bin/env python3
"""
Build cumulative corpus of best games across ALL iterations.
Re-filters the entire corpus each run using 75% threshold per faction.
"""

import os
import re
import shutil
from pathlib import Path

TRACE_DIR = Path('/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces')
BACKUP_DIR = Path('/Users/gremus/cthulhu-wars-mcts-prototype/checkpoint_backups/cumulative_corpus')
OUTPUT_LIST = Path('/tmp/cumulative_filtered_games.txt')

def main():
    # Find all arena trace files
    traces = list(TRACE_DIR.glob('arena-R39-iter*-*.txt'))
    print(f"Found {len(traces)} total arena traces")

    # Group by faction and extract scores
    by_faction = {'gc': [], 'bg': [], 'ys': [], 'cc': []}

    for trace_path in traces:
        trace = trace_path.name
        # Parse: arena-R39-iter12-gc-game135-d25.txt
        match = re.match(r'arena-R39-iter(\d+)-(\w+)-game\d+-d\d+\.txt', trace)
        if not match:
            continue

        iter_num = int(match.group(1))
        faction = match.group(2)

        # Read score from file
        try:
            with open(trace_path, 'r') as f:
                for line in f:
                    if line.startswith('Score (0-1):'):
                        score = float(line.split(':')[1].strip())
                        by_faction[faction].append((trace, score, iter_num))
                        break
        except Exception as e:
            print(f"Warning: failed to read {trace}: {e}")

    # Filter: keep games >= 75% of best per faction ACROSS ALL ITERATIONS
    filtered = []
    print(f"\n=== CUMULATIVE CORPUS (75% threshold per faction) ===")
    for faction, games in by_faction.items():
        if games:
            scores = [g[1] for g in games]
            best = max(scores)
            threshold = best * 0.75
            kept = [g for g in games if g[1] >= threshold]

            # Stats by iteration
            iter_counts = {}
            for trace, score, iter_num in kept:
                iter_counts[iter_num] = iter_counts.get(iter_num, 0) + 1

            print(f"{faction.upper()}: {len(kept)}/{len(games)} games (best={best:.3f}, threshold={threshold:.3f})")
            print(f"  By iter: {dict(sorted(iter_counts.items()))}")
            filtered.extend(kept)

    print(f"\nTotal corpus: {len(filtered)} games")

    # Save list
    with open(OUTPUT_LIST, 'w') as f:
        for trace, score, iter_num in sorted(filtered, key=lambda x: (x[2], x[0])):
            f.write(f"{trace}\n")

    print(f"Saved list to {OUTPUT_LIST}")

    # Backup traces
    BACKUP_DIR.mkdir(parents=True, exist_ok=True)
    print(f"\nBacking up {len(filtered)} traces to {BACKUP_DIR}")

    for trace, score, iter_num in filtered:
        src = TRACE_DIR / trace
        dst = BACKUP_DIR / trace
        shutil.copy2(src, dst)

    print("Backup complete!")

    # Summary by iteration
    print("\n=== SUMMARY BY ITERATION ===")
    by_iter = {}
    for trace, score, iter_num in filtered:
        by_iter[iter_num] = by_iter.get(iter_num, 0) + 1

    for iter_num in sorted(by_iter.keys()):
        print(f"Iter {iter_num}: {by_iter[iter_num]} games")

if __name__ == '__main__':
    main()
