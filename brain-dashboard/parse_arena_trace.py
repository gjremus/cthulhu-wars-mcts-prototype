#!/usr/bin/env python3
"""Parse /tmp/arena_trace.log to extract REAL game data."""
import re
import json
from datetime import datetime

def parse_arena_trace_log(trace_path='/tmp/arena_trace.log'):
    """Parse arena trace log and return list of game dicts."""
    games = []

    with open(trace_path, 'r') as f:
        content = f.read()

    # Split into individual games by "rolloutCapped start" and "rolloutCapped ended"
    game_blocks = []
    current_block = []

    for line in content.split('\n'):
        if 'rolloutCapped start' in line:
            if current_block:
                game_blocks.append('\n'.join(current_block))
            current_block = [line]
        elif current_block:
            current_block.append(line)

    if current_block:
        game_blocks.append('\n'.join(current_block))

    print(f"Found {len(game_blocks)} game blocks")

    for idx, block in enumerate(game_blocks, 1):
        # Extract timestamp from first line
        timestamp_match = re.search(r'^(\d{2}:\d{2}:\d{2})', block)
        timestamp_str = timestamp_match.group(1) if timestamp_match else "00:00:00"

        # Extract winner(s)
        winner_match = re.search(r"winners?=(.+)$", block, re.MULTILINE)
        if not winner_match:
            continue  # Game still in progress

        winners_str = winner_match.group(1)
        # Parse HTML spans: <span class='gc inline-block'>Great Cthulhu</span>
        winner_factions = []
        for match in re.finditer(r"class='(\w+) inline-block'>([^<]+)</span>", winners_str):
            faction_code = match.group(1).upper()
            winner_factions.append(faction_code)

        if not winner_factions:
            continue

        # For now, we don't have doom data from trace log
        # We'll need to match this with >>> overall data or mark as incomplete
        games.append({
            'game_num': idx,
            'timestamp': timestamp_str,
            'winners': winner_factions,
            'complete': True
        })

    print(f"Parsed {len(games)} completed games")
    return games

if __name__ == '__main__':
    games = parse_arena_trace_log()

    # Group by iteration (20 games per iter)
    iters = {}
    for i, game in enumerate(games):
        iter_num = i // 20
        if iter_num not in iters:
            iters[iter_num] = []
        iters[iter_num].append(game)

    print(f"\nGames by iteration:")
    for iter_num in sorted(iters.keys())[:20]:
        games_in_iter = iters[iter_num]
        print(f"  Iter {iter_num}: {len(games_in_iter)} games")

    # Save to JSON
    output = {
        'generated': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
        'total_games': len(games),
        'games': games
    }

    with open('arena_trace_games.json', 'w') as f:
        json.dump(output, f, indent=2)

    print(f"\nSaved {len(games)} games to arena_trace_games.json")
