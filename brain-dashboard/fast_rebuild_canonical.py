#!/usr/bin/env python3
"""
FAST rebuild of canonical store - ONLY uses all-games.txt summary files.
Does NOT parse individual trace files (6368 files would take 5+ minutes).
Use this for completed iterations only. In-progress iterations can be added later.
"""
import json
import re
from pathlib import Path
from datetime import datetime
from collections import Counter

def parse_all_games_file(filepath):
    """Parse an all-games.txt file and return list of game dicts."""
    filename = filepath.name
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
            score_match = re.search(r'score=([0-9.Ee+-]+)', line)
            won_match = re.search(r'won=(true|false)', line)
            leader_match = re.search(r'leader=(\d+)', line)
            all_doom_match = re.search(r'all_doom=\[([^\]]+)\]', line)
            breakdown_match = re.search(r'breakdown=\[([^\]]+)\]', line)

            if not all([game_match, doom_match, score_match, won_match]):
                continue

            game_num = int(game_match.group(1))
            doom = int(doom_match.group(1))
            score = float(score_match.group(1))
            won = won_match.group(1) == 'true'
            leader = int(leader_match.group(1)) if leader_match else 0
            breakdown = breakdown_match.group(1) if breakdown_match else ''

            # Parse all_doom and calculate placement
            placement = "?"
            if all_doom_match:
                all_doom_str = all_doom_match.group(1)
                doom_dict = {}
                for entry in all_doom_str.split():
                    if '=' in entry:
                        f, d = entry.split('=')
                        doom_dict[f.strip()] = int(d)

                ranked = sorted(doom_dict.items(), key=lambda x: -x[1])
                for rank, (f, d) in enumerate(ranked, 1):
                    if f == faction:
                        placement = {1: "1st", 2: "2nd", 3: "3rd", 4: "4th"}.get(rank, "?")
                        break

            try:
                timestamp = int(filepath.stat().st_mtime)
            except:
                timestamp = 0

            # Build filename for this game (no file extension)
            game_filename = f"arena-{run_tag}-iter{iteration}-{faction.lower()}-game{game_num}"

            games.append({
                'run_tag': run_tag,
                'iteration': iteration,
                'type': 'arena',
                'doom': doom,
                'score': score,
                'won': won,
                'game_num': game_num,
                'filename': game_filename,
                'filepath': str(filepath.parent / f"{game_filename}-d{doom}.txt"),
                'timestamp': timestamp,
                'game_length': 0,  # Would need to read individual file for this
                'era': 'new',
                'placement': placement,
                'breakdown': breakdown,
                'result': 'unknown'  # Will calculate after loading all games
            })

    return games

def calculate_result_field(all_games):
    """Calculate 'result' field (best/worst/included/excluded) for each game."""
    # Group games by (run, faction, iteration)
    by_run_faction_iter = {}
    for g in all_games:
        # Extract faction from filename
        faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
        faction = faction_match.group(1).upper() if faction_match else "UNK"

        key = (g['run_tag'], faction, g['iteration'])
        if key not in by_run_faction_iter:
            by_run_faction_iter[key] = []
        by_run_faction_iter[key].append(g)

    # Calculate 75th percentile per faction/iteration
    percentile_75 = {}
    for key, games in by_run_faction_iter.items():
        scores = sorted([g['score'] for g in games])
        if scores:
            p75_idx = int(len(scores) * 0.75)
            p75 = scores[min(p75_idx, len(scores) - 1)]
            percentile_75[key] = p75

    # Assign result field to each game
    for g in all_games:
        faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
        faction = faction_match.group(1).upper() if faction_match else "UNK"

        key = (g['run_tag'], faction, g['iteration'])
        games_in_group = by_run_faction_iter.get(key, [])

        if not games_in_group:
            g['result'] = 'unknown'
            continue

        scores = [game['score'] for game in games_in_group]
        best_score = max(scores)
        worst_score = min(scores)

        if g['score'] == best_score:
            g['result'] = 'best'
        elif g['score'] == worst_score:
            g['result'] = 'worst'
        else:
            # Check against previous iteration's 75th percentile
            prev_key = (g['run_tag'], faction, g['iteration'] - 1)
            prev_p75 = percentile_75.get(prev_key, 0.0)

            if g['iteration'] == 0 or prev_p75 == 0.0:
                # Iter 0 or no previous: use 75% of current best
                threshold = best_score * 0.75
                g['result'] = 'included' if g['score'] >= threshold else 'excluded'
            else:
                # Iter 1+: use previous 75th percentile
                g['result'] = 'included' if g['score'] > prev_p75 else 'excluded'

if __name__ == '__main__':
    trace_dir = Path('/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces')
    canonical_path = Path('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json')

    # Find all all-games.txt files
    all_games_files = sorted(trace_dir.glob("*-all-games.txt"))
    print(f"Found {len(all_games_files)} all-games.txt summary files")

    # Parse all files
    all_games = []
    for filepath in all_games_files:
        games = parse_all_games_file(filepath)
        all_games.extend(games)
        if len(all_games) % 400 == 0:
            print(f"  Parsed {len(all_games)} games...")

    print(f"Parsed {len(all_games)} total games")

    # Calculate result field
    calculate_result_field(all_games)

    # Stats by run
    by_run = Counter(g['run_tag'] for g in all_games)
    print(f"\nGames by run:")
    for run in sorted(by_run.keys()):
        count = by_run[run]
        iters = sorted(set(g['iteration'] for g in all_games if g['run_tag'] == run))
        print(f"  {run}: {count} games, iterations {min(iters)}-{max(iters)}")

    # Build canonical store
    store = {
        'generated': datetime.now().strftime('%Y-%m-%d %H:%M'),
        'total_games': len(all_games),
        'games': all_games
    }

    # Save
    with open(canonical_path, 'w') as f:
        json.dump(store, f, indent=2)

    print(f"\nSaved {len(all_games)} games to {canonical_path}")
