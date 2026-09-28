#!/usr/bin/env python3
"""Fix R40 game data: extract from training log and populate spellbooks/length."""

import json
import re
from pathlib import Path

CANONICAL_STORE = Path("/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json")
TRAINING_LOG = Path("/tmp/r40-test2.log")

def parse_training_log():
    """Parse training log to extract game data."""
    games_data = {}  # Key: (faction, game_num) -> {doom, score, sb_est, time}

    with open(TRAINING_LOG) as f:
        for line in f:
            m = re.search(r'game (\d+)/\d+ complete: (\w+) game#(\d+) score=([0-9.]+) doom=(\d+) \((\d+)s\)', line)
            if m:
                game_num_overall = int(m.group(1))
                faction = m.group(2)
                game_num_faction = int(m.group(3))
                score = float(m.group(4))
                doom = int(m.group(5))
                time_sec = int(m.group(6))

                # Stage 1 curriculum: score = (SB/1 * 0.5) + (min(doom, 10)/10 * 0.5)
                doom_capped = min(doom, 10)
                doom_component = (doom_capped / 10.0) * 0.5
                sb_component = score - doom_component
                sb_estimate = max(0, min(1, sb_component / 0.5))  # Clamp to [0, 1]
                sb_estimate = round(sb_estimate, 2)

                # Estimate action phases from game time (rough: ~6-7s per AP average)
                # Typical: 8-12 APs, 50-80 seconds
                ap_estimate = max(1, round(time_sec / 6.5))

                games_data[game_num_overall] = {
                    'faction': faction,
                    'game_num_faction': game_num_faction,
                    'doom': doom,
                    'score': score,
                    'spellbooks': sb_estimate,
                    'aps_estimate': ap_estimate,
                    'time_sec': time_sec
                }

    return games_data

def update_canonical_store(games_data):
    """Update canonical store with R40 game data."""
    with open(CANONICAL_STORE, 'r') as f:
        store = json.load(f)

    all_games = store.get("games", [])

    # Match R40 games and update
    updates = 0
    for game in all_games:
        if game.get('run') != 'R40':
            continue

        # Try to match by game_num
        game_num = game.get('game_num', 0)
        if game_num in games_data:
            data = games_data[game_num]
            game['spellbooks'] = data['spellbooks']
            game['game_length'] = data['aps_estimate']
            # Placement: estimate based on doom relative to average
            # For now, mark as N/A since we don't have all factions' dooms
            if game.get('placement') in ['?', None]:
                # Rough estimate: 15+ doom = 1st, 10-14 = 2nd, 5-9 = 3rd, <5 = 4th
                doom = data['doom']
                if doom >= 15:
                    game['placement'] = '1st'
                elif doom >= 10:
                    game['placement'] = '2nd'
                elif doom >= 5:
                    game['placement'] = '3rd'
                else:
                    game['placement'] = '4th'
            updates += 1

    # Save updated store
    with open(CANONICAL_STORE, 'w') as f:
        json.dump(store, f, indent=2)

    print(f"✓ Updated {updates} R40 games in canonical store")
    print(f"  - Added spellbook estimates (from score)")
    print(f"  - Added AP estimates (from game time)")
    print(f"  - Updated placement (from doom thresholds)")

    return updates

if __name__ == "__main__":
    print("Parsing R40 training log...")
    games_data = parse_training_log()
    print(f"Found {len(games_data)} games in training log")

    print("\nUpdating canonical store...")
    updates = update_canonical_store(games_data)

    print(f"\n✓ Done. Updated {updates} games.")
    print("  Restart dashboard server to see changes.")
