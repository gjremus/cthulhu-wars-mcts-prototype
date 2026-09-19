#!/usr/bin/env python3
"""
Fix R39 iteration 11 corpus size.

Iteration 11 was misconfigured to generate 1600 games (400 per faction)
instead of 400 TOTAL (100 per faction).

This script filters the canonical store to keep only the top 100 games
per faction for R39 iter 11, matching the intended corpus size.
"""
import json

canonical_path = '/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json'

# Load canonical store
with open(canonical_path, 'r') as f:
    store = json.load(f)

games = store['games']

# Separate R39 iter11 games from others
r39_iter11_games = [g for g in games if g['run'] == 'R39' and g['iteration'] == 11]
other_games = [g for g in games if not (g['run'] == 'R39' and g['iteration'] == 11)]

print(f"Found {len(r39_iter11_games)} R39 iter11 games")
print(f"Found {len(other_games)} other games")

# Group R39 iter11 by faction
import re
by_faction = {}
for g in r39_iter11_games:
    # Extract faction from filename
    match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
    if match:
        faction = match.group(1).upper()
        if faction not in by_faction:
            by_faction[faction] = []
        by_faction[faction].append(g)

print("\nR39 iter11 games by faction:")
for faction in sorted(by_faction.keys()):
    print(f"  {faction}: {len(by_faction[faction])} games")

# Filter to top 100 per faction (by score)
filtered_iter11_games = []
for faction in sorted(by_faction.keys()):
    faction_games = by_faction[faction]
    # Sort by score descending
    sorted_games = sorted(faction_games, key=lambda x: -x['score'])
    # Take top 100
    top_100 = sorted_games[:100]
    filtered_iter11_games.extend(top_100)
    print(f"  {faction}: kept top 100 games (score {top_100[0]['score']:.3f} to {top_100[-1]['score']:.3f})")

print(f"\nFiltered R39 iter11: {len(filtered_iter11_games)} games")

# Rebuild canonical store with filtered iter11 games
all_games = other_games + filtered_iter11_games

# Recalculate result field for R39 iter11
print("\nRecalculating result field for filtered R39 iter11 games...")

# Group by faction
by_faction_filtered = {}
for g in filtered_iter11_games:
    match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
    if match:
        faction = match.group(1).upper()
        if faction not in by_faction_filtered:
            by_faction_filtered[faction] = []
        by_faction_filtered[faction].append(g)

# Recalculate median for iter 10 (to determine iter 11 inclusion threshold)
iter10_games = [g for g in other_games if g['run'] == 'R39' and g['iteration'] == 10]
iter10_by_faction = {}
for g in iter10_games:
    match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
    if match:
        faction = match.group(1).upper()
        if faction not in iter10_by_faction:
            iter10_by_faction[faction] = []
        iter10_by_faction[faction].append(g)

iter10_medians = {}
for faction, games_list in iter10_by_faction.items():
    scores = sorted([g['score'] for g in games_list])
    if scores:
        median = scores[len(scores) // 2]
        iter10_medians[faction] = median

print(f"Iter 10 medians: {iter10_medians}")

# Assign result field to filtered iter11 games
for g in filtered_iter11_games:
    match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
    faction = match.group(1).upper() if match else "UNK"

    faction_games = by_faction_filtered.get(faction, [])
    if not faction_games:
        g['result'] = 'unknown'
        continue

    # Find best and worst in this faction
    scores = [game['score'] for game in faction_games]
    best_score = max(scores)
    worst_score = min(scores)

    if g['score'] == best_score:
        g['result'] = 'best'
    elif g['score'] == worst_score:
        g['result'] = 'worst'
    else:
        # Check against iter 10 median
        prev_median = iter10_medians.get(faction, 0.0)

        if prev_median == 0.0:
            # No previous median: use 75% threshold
            threshold = best_score * 0.75
            g['result'] = 'included' if g['score'] >= threshold else 'excluded'
        else:
            # Compare to previous median
            g['result'] = 'included' if g['score'] > prev_median else 'excluded'

# Count result types
result_counts = {}
for g in filtered_iter11_games:
    result = g['result']
    result_counts[result] = result_counts.get(result, 0) + 1

print(f"Result distribution: {result_counts}")

# Save updated canonical store
from datetime import datetime
store['games'] = all_games
store['total_games'] = len(all_games)
store['generated'] = datetime.now().strftime('%Y-%m-%d %H:%M')

# Backup original
import shutil
backup_path = canonical_path + '.backup_before_iter11_fix'
shutil.copy2(canonical_path, backup_path)
print(f"\nBackup saved to {backup_path}")

with open(canonical_path, 'w') as f:
    json.dump(store, f, indent=2)

print(f"\nUpdated canonical store saved: {len(all_games)} total games")
print(f"  R39 iter11: {len(filtered_iter11_games)} games (was {len(r39_iter11_games)})")
print(f"  Other: {len(other_games)} games")
