#!/usr/bin/env python3
"""Deduplicate and renumber iter 12 games to sequential per faction."""

import os
import re
from pathlib import Path
from collections import defaultdict

trace_dir = Path('/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces')

for faction in ['gc', 'bg', 'ys', 'cc']:
    print(f'\n{faction.upper()}:')

    # Get all files for this faction
    files = list(trace_dir.glob(f'arena-R39-iter12-{faction}-game*-d*.txt'))
    print(f'  Found {len(files)} total files')

    # Group by game number, keep newest
    by_game = defaultdict(list)
    for f in files:
        match = re.search(r'-game(\d+)-d(\d+)\.txt$', f.name)
        if match:
            game_num = int(match.group(1))
            doom = int(match.group(2))
            mtime = f.stat().st_mtime
            by_game[game_num].append((mtime, doom, f))

    # Deduplicate: keep newest per game number
    deduped = []
    for game_num, versions in sorted(by_game.items()):
        versions.sort(key=lambda x: -x[0])  # Sort by mtime descending
        newest = versions[0]
        deduped.append((game_num, newest[1], newest[2]))  # (old_game_num, doom, path)

        # Delete older duplicates
        for old_version in versions[1:]:
            old_version[2].unlink()

    print(f'  After dedup: {len(deduped)} unique games')

    # Renumber sequentially
    for new_num, (old_num, doom, old_path) in enumerate(deduped, 1):
        new_name = f'arena-R39-iter12-{faction}-game{new_num}-d{doom}.txt'
        new_path = trace_dir / new_name

        if old_path.name != new_name:
            old_path.rename(new_path)

    print(f'  Renumbered to 1-{len(deduped)}')

print('\nDone.')
