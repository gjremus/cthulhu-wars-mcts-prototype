#!/usr/bin/env python3
"""Re-score games with corrupted scores >1.0 using current Outcome.scala model."""

import json
import re
from pathlib import Path

TRACES_DIR = Path("/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces")
CANONICAL_STORE = Path("/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json")

# Current Outcome.scala constants
NON_WINNER_CEILING = 1.0  # Default, can be overridden by CW_NONWINNER_CEILING
STALEMATE_PENALTY = 0.25  # Default

def parse_trace_file(trace_path):
    """Parse trace file to extract game state for re-scoring."""
    content = trace_path.read_text()

    # Find brain faction from filename (for arena games)
    stem = trace_path.stem
    is_arena = stem.startswith("arena-")
    brain_faction = None

    if is_arena:
        parts = stem.split('-')
        for part in parts:
            if part in ['bg', 'cc', 'gc', 'ys']:
                brain_faction = part.upper()
                break

    # Parse ALL_DOOM line to get doom values
    all_doom_match = re.search(r'ALL_DOOM=(.+)', content)
    if not all_doom_match:
        return None

    doom_str = all_doom_match.group(1).strip()
    doom_pairs = {}
    for pair in doom_str.split():
        if '=' in pair:
            faction, d = pair.split('=')
            doom_pairs[faction] = int(d)

    # Determine brain faction for selfplay (first faction)
    if not brain_faction and doom_pairs:
        brain_faction = list(doom_pairs.keys())[0]

    if not brain_faction:
        return None

    brain_doom = doom_pairs.get(brain_faction, 0)

    # Find winner
    won_match = re.search(r'(.+) won</div>', content)
    won = False
    winner_faction = None
    if won_match:
        won_text = won_match.group(1)
        faction_names = {
            'BG': 'Black Goat', 'YS': 'Yellow Sign',
            'CC': 'Crawling Chaos', 'GC': 'Great Cthulhu'
        }
        for fac, name in faction_names.items():
            if name in won_text:
                winner_faction = fac
                if fac == brain_faction:
                    won = True
                break

    # Parse shaping score (stored in trace as "Score:")
    # The shaping score in the trace is the raw shaping value before terminal label
    shaping_match = re.search(r'Score:\s*([0-9.]+)', content)
    shaping = float(shaping_match.group(1)) if shaping_match else 0.0

    return {
        'brain_faction': brain_faction,
        'brain_doom': brain_doom,
        'won': won,
        'shaping': shaping,
        'all_doom': doom_pairs,
        'winner_faction': winner_faction,
        'num_factions': len(doom_pairs)
    }

def calculate_current_score(game_state):
    """Calculate score using current Outcome.scala model (with placement bonus fix)."""

    # If brain won, score is always 1.0
    if game_state['won']:
        return 1.0

    # For non-winners, use the placement bonus model
    shaping = game_state['shaping']

    # Current model: 0.7 ceiling to make room for placement bonus
    capped_with_room = 0.7 * NON_WINNER_CEILING * max(0.0, min(1.0, shaping))

    # Check if stalemate (no winner)
    if game_state['winner_faction'] is None:
        # Stalemate: no placement ranking possible, no bonus
        return STALEMATE_PENALTY * capped_with_room

    # Calculate placement by doom ranking
    all_factions = list(game_state['all_doom'].keys())
    doom_ranking = sorted(all_factions, key=lambda f: -game_state['all_doom'][f])
    my_rank = doom_ranking.index(game_state['brain_faction'])
    places_above_last = game_state['num_factions'] - 1 - my_rank  # 0 for last, 1 for 3rd in 4p, etc
    bonus = places_above_last * 0.1

    final_score = capped_with_room + bonus
    return min(1.0, max(0.0, final_score))  # Clamp to [0,1]

def rescore_games():
    """Re-score all games with score >1.0."""

    # Load canonical store
    store = json.load(CANONICAL_STORE.open())

    # Find games with score >1.0
    bad_games = [g for g in store['games'] if g.get('score', 0) > 1.0]

    print(f"Re-scoring {len(bad_games)} games with score >1.0...")
    print()

    rescored_count = 0
    failed_count = 0

    for game in bad_games:
        trace_path = Path(game['filepath'])

        if not trace_path.exists():
            print(f"⚠ Trace file not found: {trace_path.name}")
            failed_count += 1
            continue

        # Parse trace file
        game_state = parse_trace_file(trace_path)
        if not game_state:
            print(f"⚠ Failed to parse: {trace_path.name}")
            failed_count += 1
            continue

        # Calculate new score
        old_score = game['score']
        new_score = calculate_current_score(game_state)

        # Update game in store
        game['score'] = new_score
        rescored_count += 1

        if rescored_count <= 10:  # Show first 10
            print(f"{trace_path.name}")
            print(f"  Old score: {old_score:.3f} → New score: {new_score:.3f}")
            print(f"  Won: {game_state['won']}, Doom: {game_state['brain_doom']}, Shaping: {game_state['shaping']:.3f}")
            print()

    # Save updated canonical store
    print(f"Saving updated canonical store to {CANONICAL_STORE}...")
    store['generated'] = "2026-08-23 (re-scored)"

    with CANONICAL_STORE.open('w') as f:
        json.dump(store, f, indent=2)
    print(f"✓ Saved")

    print(f"✓ Re-scored {rescored_count} games")
    print(f"✗ Failed: {failed_count} games")
    print()

    # Verify no more >1.0 scores
    bad_remaining = [g for g in store['games'] if g.get('score', 0) > 1.0]
    if bad_remaining:
        print(f"⚠ WARNING: {len(bad_remaining)} games still have score >1.0!")
        for g in bad_remaining[:5]:
            print(f"  {g['filename']}: score={g['score']:.3f}")
    else:
        print(f"✓ All scores now in [0,1] range")

    return rescored_count, failed_count

if __name__ == "__main__":
    rescore_games()
