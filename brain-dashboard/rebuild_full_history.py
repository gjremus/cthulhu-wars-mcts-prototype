#!/usr/bin/env python3
"""Full rebuild - ALL runs, ALL iters."""

import json
import re
from pathlib import Path

MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
TRACES_DIR = MCTS_ROOT / "arena-traces"
CANONICAL_STORE = MCTS_ROOT / "brain-dashboard" / "canonical_games.json"

def parse_trace(trace_file):
    """Parse any arena trace file."""
    try:
        stem = trace_file.stem
        content = trace_file.read_text()
        lines = content.strip().split('\n')

        # Parse filename: arena-R##-iter##-faction-game##-d##.txt
        match = re.match(r'arena-(R\d+)-iter(\d+)-(\w+)-game\d+-d\d+', stem)
        if not match:
            return None

        run, iter_num, faction = match.groups()
        iter_num = int(iter_num)

        # Extract metadata from end of file
        brain_seat = faction.upper()
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
            "run": run,
            "iter": iter_num,
            "type": "arena",
            "faction": brain_seat,
            "result": result.lower() if result else "loss",
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
        if total == 0:
            continue

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
            factions[faction]['avg_doom'] = round(factions[faction]['doom'] / n, 2)
            factions[faction]['avg_sbs'] = round(factions[faction]['sbs'] / n, 2)
            factions[faction]['win_rate'] = round(factions[faction]['wins'] / n, 3)

        run_parts = key.split('_')
        entry = {
            'run': run_parts[0],
            'iter': int(run_parts[1].replace('iter', '')),
            'type': 'arena',
            'total_games': total,
            'wins': wins,
            'win_rate': round(wins / total, 3),
            'avg_doom': round(avg_doom, 2),
            'avg_spellbooks': round(avg_sbs, 2),
            'avg_length': round(avg_aps, 2),
            'avg_score': round(avg_score, 3),
            'factions': factions
        }
        history.append(entry)

    return history

def main():
    print("Full rebuild - ALL runs ALL iters...")

    # Find ALL arena traces
    all_traces = sorted(TRACES_DIR.glob("arena-*.txt"))
    print(f"Found {len(all_traces)} total arena traces")

    games = []
    for trace in all_traces:
        game = parse_trace(trace)
        if game:
            games.append(game)

    games.sort(key=lambda g: g["timestamp"])

    print(f"Parsed {len(games)} games")

    # Build performance history
    perf_history = build_performance_history(games)
    perf_history.sort(key=lambda x: (x['run'], x['iter']), reverse=True)

    store = {
        "generated": "2026-09-27 19:45",
        "total_games": len(games),
        "errors": [],
        "games": games,
        "performanceHistory": perf_history
    }

    CANONICAL_STORE.write_text(json.dumps(store, indent=2))

    print(f"\n✓ Built canonical store:")
    print(f"  Total games: {len(games)}")
    print(f"  Performance history entries: {len(perf_history)}")
    for entry in perf_history:
        print(f"    {entry['run']} iter{entry['iter']}: {entry['total_games']} games, avg_doom={entry['avg_doom']}, avg_sbs={entry['avg_spellbooks']}")

if __name__ == "__main__":
    main()
