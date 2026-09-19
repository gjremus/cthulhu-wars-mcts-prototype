#!/usr/bin/env python3
"""
Import ALL games from all-games.txt files into canonical store.
Includes full data: doom, score, won, breakdown for every game.
"""
import json
import os
import re
from pathlib import Path
from datetime import datetime

def parse_all_games_file(filepath):
    """Parse an all-games.txt file and return list of game dicts."""
    # Extract metadata from filename: arena-R34-iter1-gc-all-games.txt
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

            # Parse game line
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

def test_import(trace_dir, limit=2):
    """Test import on first N all-games.txt files."""
    print(f"=== TESTING IMPORT (limit={limit}) ===\n")

    all_games_files = sorted(Path(trace_dir).glob("*-all-games.txt"))
    test_files = all_games_files[:limit]

    for filepath in test_files:
        games = parse_all_games_file(filepath)
        print(f"File: {filepath.name}")
        print(f"  Parsed {len(games)} games")
        if games:
            g = games[0]
            print(f"  Sample: run={g['run']} iter={g['iteration']} faction={g['faction']}")
            print(f"          doom={g['doom']} score={g['score']:.4f} won={g['won']}")
        print()

    return True

def full_import(trace_dir, canonical_path, dry_run=False):
    """Import all games from all-games.txt files."""
    print(f"=== FULL IMPORT (dry_run={dry_run}) ===\n")

    # Load existing canonical store
    with open(canonical_path, 'r') as f:
        store = json.load(f)

    existing_count = len(store['games'])
    print(f"Existing games in canonical store: {existing_count}")

    # Find all all-games.txt files
    all_games_files = sorted(Path(trace_dir).glob("*-all-games.txt"))
    print(f"Found {len(all_games_files)} all-games.txt files")

    # Parse all files
    new_games = []
    for filepath in all_games_files:
        games = parse_all_games_file(filepath)
        new_games.extend(games)

    print(f"Parsed {len(new_games)} total games from all-games.txt files\n")

    # Remove duplicates: keep only games with iteration >= 0 from all-games files
    # (iter -1 games are already in canonical store from old import)
    filtered_games = [g for g in new_games if g['iteration'] >= 0]
    print(f"Filtered to {len(filtered_games)} iteration games (iter >= 0)")

    # Remove any existing iteration games from canonical store
    # Note: old games use 'iteration', new games use 'iter'
    store['games'] = [g for g in store['games'] if g.get('iteration', g.get('iter', -1)) < 0]
    print(f"Removed old iteration games, keeping {len(store['games'])} baseline games")

    # Convert to canonical format and add to store
    for g in filtered_games:
        # Get file timestamp
        try:
            timestamp = int(Path(g['filepath']).stat().st_mtime)
        except:
            timestamp = 0

        canonical_game = {
            'run': g['run'],
            'iteration': g['iteration'],  # Use 'iteration' to match old schema
            'type': 'arena',
            'doom': g['doom'],
            'score': g['score'],
            'won': g['won'],
            'game_num': g['game_num'],
            'filename': g['filename'].replace('-all-games.txt', f"-game{g['game_num']}"),
            'filepath': g['filepath'],
            'timestamp': timestamp,
            'game_length': 0,  # Unknown from all-games.txt
            'era': 'new'
        }
        store['games'].append(canonical_game)

    # Update metadata
    store['total_games'] = len(store['games'])
    store['generated'] = datetime.now().strftime('%Y-%m-%d %H:%M')

    print(f"\nFinal canonical store: {store['total_games']} total games")
    print(f"  Baseline games (iter <0): {len([g for g in store['games'] if g.get('iteration', g.get('iter', -1)) < 0])}")
    print(f"  Iteration games (iter >=0): {len([g for g in store['games'] if g.get('iteration', g.get('iter', -1)) >= 0])}")

    if not dry_run:
        # Save canonical store
        with open(canonical_path, 'w') as f:
            json.dump(store, f, indent=2)
        print(f"\nSaved to {canonical_path}")
    else:
        print("\nDRY RUN - not saving")

    return True

if __name__ == '__main__':
    trace_dir = '/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces'
    canonical_path = '/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json'

    # Test on 2 files first
    print("STEP 1: Testing import on 2 files\n")
    test_import(trace_dir, limit=2)

    input("Press Enter to continue with full import (dry run)...")

    # Dry run
    print("\nSTEP 2: Full import (dry run)\n")
    full_import(trace_dir, canonical_path, dry_run=True)

    input("\nPress Enter to run REAL import...")

    # Real import
    print("\nSTEP 3: REAL import\n")
    full_import(trace_dir, canonical_path, dry_run=False)

    print("\n=== IMPORT COMPLETE ===")
