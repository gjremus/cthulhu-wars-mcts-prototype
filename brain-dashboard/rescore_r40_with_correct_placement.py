#!/usr/bin/env python3
"""Rescore R40 games with correct placement (spellbooks before doom)."""

import json
import re
from pathlib import Path

MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
TRACES_DIR = MCTS_ROOT / "arena-traces"
CANONICAL_STORE = MCTS_ROOT / "brain-dashboard" / "canonical_games.json"

def parse_all_factions_from_trace(trace_file):
    """Parse ALL 4 factions' doom and spellbook counts from trace."""
    content = trace_file.read_text()
    lines = content.split('\n')

    # Count spellbooks per faction from SpellbookAction lines
    spellbooks = {}
    for line in lines:
        if 'SpellbookAction' in line:
            match = re.match(r'SpellbookAction\((\w+),', line)
            if match:
                faction = match.group(1)
                spellbooks[faction] = spellbooks.get(faction, 0) + 1

    # Get doom values from ALL_DOOM line
    doom_values = {}
    for line in reversed(lines[-20:]):
        if line.startswith('ALL_DOOM='):
            # Format: ALL_DOOM=CC=36 YS=23 BG=11 GC=6
            doom_str = line.replace('ALL_DOOM=', '')
            for pair in doom_str.split():
                faction, doom = pair.split('=')
                doom_values[faction] = int(doom)
            break

    return spellbooks, doom_values

def calculate_correct_placement(spellbooks, doom_values):
    """Calculate placement: 6-SB factions by doom desc, then <6-SB by doom desc."""
    # Build list of (faction, sbs, doom)
    factions_data = []
    for faction in doom_values.keys():
        sbs = spellbooks.get(faction, 0)
        doom = doom_values[faction]
        factions_data.append((faction, sbs, doom))

    # Separate into completed (6 SBs) and incomplete (<6 SBs)
    completed = [(f, s, d) for f, s, d in factions_data if s >= 6]
    incomplete = [(f, s, d) for f, s, d in factions_data if s < 6]

    # Sort each group by doom descending
    completed.sort(key=lambda x: x[2], reverse=True)
    incomplete.sort(key=lambda x: x[2], reverse=True)

    # Combine: completed first, then incomplete
    ranked = completed + incomplete

    # Build placement map
    placements = {}
    for place, (faction, sbs, doom) in enumerate(ranked, 1):
        placements[faction] = place

    return placements

def rescore_r40_games():
    """Rescore all R40 games with correct placement."""
    print("Loading canonical store...")
    with open(CANONICAL_STORE, 'r') as f:
        store = json.load(f)

    games = store.get('games', [])
    r40_games = [g for g in games if g.get('run') == 'R40' and g.get('iter') == 1]

    print(f"Found {len(r40_games)} R40 iter1 games")
    print("Rescoring with correct placement...")

    rescored = 0
    errors = 0

    for game in r40_games:
        trace_filename = game.get('trace_file')
        if not trace_filename:
            errors += 1
            continue

        trace_path = TRACES_DIR / trace_filename
        if not trace_path.exists():
            errors += 1
            continue

        try:
            # Parse all factions' data
            spellbooks, doom_values = parse_all_factions_from_trace(trace_path)

            # Calculate correct placements
            placements = calculate_correct_placement(spellbooks, doom_values)

            # Update game's placement
            brain_faction = game.get('faction')
            if brain_faction in placements:
                old_place = game.get('place')
                new_place = placements[brain_faction]
                game['place'] = new_place

                # Recalculate score based on new placement
                # Curriculum Stage 1: (SB/1 * 0.5) + (min(doom,10)/10 * 0.5)
                sbs = game.get('spellbooks', 0)
                doom = game.get('doom', 0)
                score = (sbs / 1.0 * 0.5) + (min(doom, 10) / 10.0 * 0.5)
                game['score'] = score

                if old_place != new_place:
                    rescored += 1

        except Exception as e:
            print(f"Error processing {trace_filename}: {e}")
            errors += 1

    # Save updated store
    with open(CANONICAL_STORE, 'w') as f:
        json.dump(store, f, indent=2)

    print(f"\n✓ Rescored {rescored} games with placement changes")
    print(f"✗ {errors} errors")
    print(f"Updated canonical store saved")

if __name__ == '__main__':
    rescore_r40_games()
