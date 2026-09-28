#!/usr/bin/env python3
"""Build performanceHistory array from games in canonical store."""
import json
from collections import defaultdict
import re

with open('canonical_games.json', 'r') as f:
    data = json.load(f)

games = data['games']

# Group by (run, iteration)
by_run_iter = defaultdict(list)
for game in games:
    run = game.get('run')
    iteration = game.get('iteration') or game.get('iter', 0)
    if run:
        by_run_iter[(run, iteration)].append(game)

# Build performance history
perf_hist = []
for (run, iteration), games_group in sorted(by_run_iter.items()):
    # Skip killed games
    completed = [g for g in games_group if g.get('result') not in ['killed', 'KILLED']]
    if not completed:
        continue
    
    wins = [g for g in completed if g.get('won') == True]
    
    # Calculate averages
    avg_doom = sum(g.get('doom', 0) for g in completed) / len(completed) if completed else 0
    avg_sbs = sum(g.get('spellbooks', 0) for g in completed) / len(completed) if completed else 0
    avg_aps = sum(g.get('game_length', 0) for g in completed) / len(completed) if completed else 0
    avg_score = sum(g.get('score', 0) for g in completed) / len(completed) if completed else 0
    
    # Per-faction breakdown
    factions_data = {}
    for game in completed:
        # Extract faction from filename
        faction_match = re.search(r'-(gc|bg|ys|cc)-', game.get('filename', ''))
        if faction_match:
            faction = faction_match.group(1).upper()
            if faction not in factions_data:
                factions_data[faction] = {'games': 0, 'wins': 0, 'doom': [], 'sbs': []}
            factions_data[faction]['games'] += 1
            if game.get('won'):
                factions_data[faction]['wins'] += 1
            factions_data[faction]['doom'].append(game.get('doom', 0))
            factions_data[faction]['sbs'].append(game.get('spellbooks', 0))
    
    # Calculate per-faction averages
    factions = {}
    for faction, stats in factions_data.items():
        factions[faction] = {
            'games': stats['games'],
            'wins': stats['wins'],
            'win_rate': stats['wins'] / stats['games'] if stats['games'] > 0 else 0,
            'avg_doom': sum(stats['doom']) / len(stats['doom']) if stats['doom'] else 0,
            'avg_sbs': sum(stats['sbs']) / len(stats['sbs']) if stats['sbs'] else 0,
        }
    
    perf_hist.append({
        'run': run,
        'iter': iteration,
        'type': games_group[0].get('type', 'arena'),
        'total_games': len(completed),
        'wins': len(wins),
        'avg_doom': avg_doom,
        'avg_spellbooks': avg_sbs,
        'avg_length': avg_aps,
        'avg_score': avg_score,
        'factions': factions
    })

data['performanceHistory'] = perf_hist

with open('canonical_games.json', 'w') as f:
    json.dump(data, f, indent=2)

print(f'Built performance history: {len(perf_hist)} entries')
for entry in perf_hist:
    print(f"  {entry['run']} I{entry['iter']:02d}: {entry['total_games']} games, doom={entry['avg_doom']:.1f}, sbs={entry['avg_spellbooks']:.1f}")
