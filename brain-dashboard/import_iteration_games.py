#!/usr/bin/env python3
"""
CORRECT import: ADD iteration games to canonical store without touching baseline games.
"""
import json
import os
import re
from pathlib import Path
from datetime import datetime

def parse_all_games_file(filepath):
    """Parse an all-games.txt file and return list of game dicts."""
    filename = os.path.basename(filepath)
    match = re.match(r'arena-([^-]+)-iter(\d+)-(\w+)-all-games\.txt', filename)
    if not match:
        return []

    run_tag = match.group(1)
    iteration = int(match.group(2))
    faction = match.group(3).upper()

    games = []
    with open(filepath, 'r') as f:
        for line in f:
            if not line.startswith('game='):
                continue

            game_match = re.search(r'game=(\d+)', line)
            doom_match = re.search(r'doom=(\d+)', line)
            score_match = re.search(r'score=([0-9.]+)', line)
            won_match = re.search(r'won=(true|false)', line)
            leader_match = re.search(r'leader=(\d+)', line)

            if not all([game_match, doom_match, score_match, won_match]):
                continue

            game_num = int(game_match.group(1))
            doom = int(doom_match.group(1))
            score = float(score_match.group(1))
            won = won_match.group(1) == 'true'
            leader = int(leader_match.group(1)) if leader_match else 0

            games.append({
                'run': run_tag,
                'iteration': iteration,
                'faction': faction,
                'game_num': game_num,
                'doom': doom,
                'score': score,
                'won': won,
                'leader_doom': leader,
                'type': 'arena',
                'filename': filename,
                'filepath': str(filepath)
            })

    return games

if __name__ == '__main__':
    trace_dir = '/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces'
    canonical_path = '/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json'

    # Load existing canonical store
    with open(canonical_path, 'r') as f:
        store = json.load(f)

    baseline_games = [g for g in store['games'] if g.get('iteration', -1) < 0]
    print(f"Baseline games: {len(baseline_games)}")

    # Find all all-games.txt files
    all_games_files = sorted(Path(trace_dir).glob("*-all-games.txt"))
    print(f"Found {len(all_games_files)} all-games.txt files")

    # Parse all files
    new_games = []
    for filepath in all_games_files:
        games = parse_all_games_file(filepath)
        new_games.extend(games)

    print(f"Parsed {len(new_games)} iteration games from all-games.txt files")

    # Build final store: baseline + iteration games
    final_games = baseline_games[:]

    for g in new_games:
        try:
            timestamp = int(Path(g['filepath']).stat().st_mtime)
        except:
            timestamp = 0

        canonical_game = {
            'run': g['run'],
            'iteration': g['iteration'],
            'type': 'arena',
            'doom': g['doom'],
            'score': g['score'],
            'won': g['won'],
            'game_num': g['game_num'],
            'filename': g['filename'].replace('-all-games.txt', f"-game{g['game_num']}"),
            'filepath': g['filepath'],
            'timestamp': timestamp,
            'game_length': 0,
            'era': 'new'
        }
        final_games.append(canonical_game)

    # Update store
    store['games'] = final_games
    store['total_games'] = len(final_games)
    store['generated'] = datetime.now().strftime('%Y-%m-%d %H:%M')

    print(f"\nFinal store: {len(final_games)} games")
    print(f"  Baseline (iter<0): {len([g for g in final_games if g.get('iteration', -1) < 0])}")
    print(f"  Iteration (iter>=0): {len([g for g in final_games if g.get('iteration', -1) >= 0])}")

    # Save
    with open(canonical_path, 'w') as f:
        json.dump(store, f, indent=2)

    print(f"\nSaved to {canonical_path}")
