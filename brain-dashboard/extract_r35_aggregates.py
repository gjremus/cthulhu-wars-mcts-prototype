#!/usr/bin/env python3
"""
Extract R35 aggregate data from sp_R35_trace.log >>> overall lines.
Generates entries for Performance History showing doom averages.
Individual game data is LOST - this only recovers aggregates.
"""
import re
from datetime import datetime

def parse_r35_log():
    """Parse /tmp/sp_R35_trace.log and extract aggregate data per iteration."""
    log_path = '/tmp/sp_R35_trace.log'

    with open(log_path, 'r') as f:
        content = f.read()

    lines = content.split('\n')

    # Extract iteration data
    iters = []
    current_iter = None

    for line in lines:
        # Match: iter N | arena 20 games
        iter_match = re.match(r'^iter (\d+) \| arena', line)
        if iter_match:
            current_iter = int(iter_match.group(1))
            continue

        # Match: >>> overall 0/20 = 0% | GC:0/5(d16) BG:0/5(d14) YS:0/5(d7) CC:0/5(d8)
        if '>>> overall' in line and current_iter is not None:
            parts = line.split('|')
            if len(parts) < 2:
                continue

            # Parse overall win rate
            overall_match = re.search(r'>>> overall (\d+)/(\d+)', parts[0])
            if not overall_match:
                continue

            total_wins = int(overall_match.group(1))
            total_games = int(overall_match.group(2))

            # Parse per-faction stats: GC:0/5(d16)
            faction_stats = {}
            faction_parts = parts[1].strip().split()
            for faction_str in faction_parts:
                match = re.match(r'([A-Z]+):(\d+)/(\d+)\(d(\d+)\)', faction_str)
                if match:
                    faction = match.group(1)
                    wins = int(match.group(2))
                    games = int(match.group(3))
                    avg_doom = int(match.group(4))
                    faction_stats[faction] = {
                        'wins': wins,
                        'games': games,
                        'doom': avg_doom
                    }

            if faction_stats:
                # Calculate aggregate doom across all factions
                total_doom = sum(s['doom'] * s['games'] for s in faction_stats.values())
                aggregate_doom = total_doom / total_games if total_games > 0 else 0

                iters.append({
                    'iteration': current_iter,
                    'total_wins': total_wins,
                    'total_games': total_games,
                    'aggregate_doom': aggregate_doom,
                    'factions': faction_stats
                })

            current_iter = None

    return iters

def generate_performance_history_rows():
    """Generate Performance History table rows for R35."""
    iters = parse_r35_log()

    print(f"Found {len(iters)} R35 iterations with aggregate data")
    print("\nPerformance History rows:")
    print("Run | Iter | Avg Doom | Avg Score | Arena WR | Best Game")
    print("-" * 60)

    for it in iters:
        # Score is UNKNOWN (lost data)
        avg_score = "?"
        arena_wr = f"{it['total_wins']}/{it['total_games']}"
        best_game = "?"

        print(f"R35 | {it['iteration']:3d} | {it['aggregate_doom']:6.1f} | {avg_score:>9s} | {arena_wr:>8s} | {best_game:>9s}")

    return iters

if __name__ == '__main__':
    iters = generate_performance_history_rows()

    # Print JSON for dashboard integration
    print("\n\nJSON for dashboard:")
    import json
    print(json.dumps(iters, indent=2))
