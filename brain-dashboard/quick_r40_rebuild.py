#!/usr/bin/env python3
"""Quick rebuild - R40 only."""

import json
import re
from pathlib import Path

MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
TRACES_DIR = MCTS_ROOT / "arena-traces"
CANONICAL_STORE = MCTS_ROOT / "brain-dashboard" / "canonical_games.json"

def parse_r40_trace(trace_file):
    """Parse R40 trace file."""
    try:
        stem = trace_file.stem
        content = trace_file.read_text()
        lines = content.strip().split('\n')

        # Extract metadata from end of file
        brain_seat = None
        result = None
        doom = 0
        spellbooks = 0
        aps = 0
        score = 0.0

        for line in reversed(lines[-20:]):
            if line.startswith("Brain seat:"):
                brain_seat = line.split(":")[1].strip()
            elif line.startswith("Result:"):
                result = line.split(":")[1].strip()
            elif line.startswith("Doom:"):
                doom = int(line.split(":")[1].strip())
            elif line.startswith("Spellbooks:"):
                spellbooks = int(line.split(":")[1].strip())
            elif line.startswith("Action Phases:"):
                aps = int(line.split(":")[1].strip())
            elif line.startswith("Score (0-1):"):
                score = float(line.split(":")[1].strip())

        # Parse placement from ALL_DOOM line
        place = "?"
        for line in reversed(lines[-20:]):
            if line.startswith("ALL_DOOM="):
                doom_str = line.replace("ALL_DOOM=", "")
                doom_pairs = doom_str.split()
                doom_vals = sorted([int(p.split("=")[1]) for p in doom_pairs], reverse=True)
                if doom in doom_vals:
                    place = doom_vals.index(doom) + 1
                break

        return {
            "run": "R40",
            "iter": 1,
            "type": "arena",
            "faction": brain_seat,
            "result": result.lower(),
            "doom": doom,
            "spellbooks": spellbooks,
            "length": aps,
            "place": place,
            "score": score,
            "trace_file": trace_file.name,
            "timestamp": trace_file.stat().st_mtime
        }
    except Exception as e:
        print(f"Error parsing {trace_file.name}: {e}")
        return None

def build_performance_history(games):
    """Build performance history with faction breakdowns."""
    if not games:
        return []

    # Group by run+iter
    by_run_iter = {}
    for g in games:
        key = f"{g['run']}_iter{g['iter']}"
        if key not in by_run_iter:
            by_run_iter[key] = []
        by_run_iter[key].append(g)

    history = []
    for key, run_games in sorted(by_run_iter.items()):
        # Overall stats
        total = len(run_games)
        wins = sum(1 for g in run_games if g['result'] == 'win')
        avg_doom = sum(g['doom'] for g in run_games) / total
        avg_sbs = sum(g['spellbooks'] for g in run_games) / total
        avg_aps = sum(g['length'] for g in run_games) / total
        avg_score = sum(g['score'] for g in run_games) / total

        # Faction breakdown
        factions = {}
        for g in run_games:
            faction = g['faction']
            if faction not in factions:
                factions[faction] = {'games': 0, 'wins': 0, 'doom': 0, 'sbs': 0}
            factions[faction]['games'] += 1
            if g['result'] == 'win':
                factions[faction]['wins'] += 1
            factions[faction]['doom'] += g['doom']
            factions[faction]['sbs'] += g['spellbooks']

        # Average faction stats
        for faction in factions:
            n = factions[faction]['games']
            factions[faction]['avg_doom'] = factions[faction]['doom'] / n
            factions[faction]['avg_sbs'] = factions[faction]['sbs'] / n
            factions[faction]['win_rate'] = factions[faction]['wins'] / n

        run_parts = key.split('_')
        entry = {
            'run': run_parts[0],
            'iter': int(run_parts[1].replace('iter', '')),
            'type': 'arena',
            'total_games': total,
            'wins': wins,
            'win_rate': wins / total,
            'avg_doom': avg_doom,
            'avg_spellbooks': avg_sbs,
            'avg_length': avg_aps,
            'avg_score': avg_score,
            'factions': factions
        }
        history.append(entry)

    return history

def main():
    print("Quick R40 rebuild...")

    # Find R40 iter1 traces
    r40_traces = sorted(TRACES_DIR.glob("arena-R40-iter1-*.txt"))
    print(f"Found {len(r40_traces)} R40 traces")

    games = []
    for trace in r40_traces:
        game = parse_r40_trace(trace)
        if game:
            games.append(game)

    games.sort(key=lambda g: g["timestamp"])

    # Load existing canonical store to preserve non-R40 data
    try:
        with open(CANONICAL_STORE, 'r') as f:
            existing_store = json.load(f)
        all_games = existing_store.get("games", [])
        existing_perf_history = existing_store.get("performanceHistory", [])
    except:
        all_games = []
        existing_perf_history = []

    # Remove old R40 games, add new R40 games
    all_games = [g for g in all_games if g.get("run") != "R40"]
    all_games.extend(games)
    all_games.sort(key=lambda g: g["timestamp"])

    # Build NEW performance history with updated R40 data
    perf_history = build_performance_history(games)

    # Replace R40 entry in existing performance history
    final_perf_history = [e for e in existing_perf_history if e.get("run") != "R40"]
    final_perf_history.extend(perf_history)
    final_perf_history.sort(key=lambda e: (e['run'], e['iter']), reverse=True)

    store = {
        "generated": "2026-09-27 20:00",
        "total_games": len(all_games),
        "errors": [],
        "games": all_games,
        "performanceHistory": final_perf_history
    }

    CANONICAL_STORE.write_text(json.dumps(store, indent=2))

    print(f"✓ Built canonical store: {len(games)} R40 games")
    print(f"  Avg doom: {sum(g['doom'] for g in games) / len(games):.1f}")
    print(f"  Avg SBs: {sum(g['spellbooks'] for g in games) / len(games):.1f}")
    print(f"  Avg APs: {sum(g['length'] for g in games) / len(games):.1f}")
    print(f"  Performance history: {len(perf_history)} entries")

if __name__ == "__main__":
    main()
