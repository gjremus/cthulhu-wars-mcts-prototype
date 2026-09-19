#!/usr/bin/env python3
"""Renumber iter 12 games to sequential per faction."""

import os
import re
from pathlib import Path

trace_dir = Path('/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces')

for faction in ['bg', 'ys', 'cc']:
    # Get all files for this faction
    files = sorted(trace_dir.glob(f'arena-R39-iter12-{faction}-game*-d*.txt'))

    # Extract game numbers and sort
    game_files = []
    for f in files:
        match = re.search(r'-game(\d+)-d(\d+)\.txt$', f.name)
        if match:
            game_num = int(match.group(1))
            doom = int(match.group(2))
            game_files.append((game_num, doom, f))

    # Sort by game number
    game_files.sort(key=lambda x: x[0])

    print(f'\n{faction.upper()}: {len(game_files)} games')

    # Renumber sequentially
    for new_num, (old_num, doom, old_path) in enumerate(game_files, 1):
        new_name = f'arena-R39-iter12-{faction}-game{new_num}-d{doom}.txt'
        new_path = trace_dir / new_name

        if old_path != new_path:
            # Rename
            old_path.rename(new_path)
            if new_num <= 5 or new_num > len(game_files) - 5:
                print(f'  Renamed game {old_num} -> {new_num} (doom={doom})')

    print(f'  Renumbered to 1-{len(game_files)}')

print('\nDone. GC unchanged (already sequential 1-400).')
