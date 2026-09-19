#!/usr/bin/env python3
"""Test performance history parsing."""
import sys
import os

# Add server.py directory to path
sys.path.insert(0, '/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard')

# Import from server
import json
import re
from pathlib import Path
import time

# Copied from server.py
_live_log_cache = {}
_live_log_cache_timestamp = 0

def get_performance_history():
    """Get performance history from canonical store."""
    # Load games from canonical store
    try:
        with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
            store = json.load(f)
        all_games = store.get("games", [])
    except Exception as e:
        print(f"ERROR loading canonical store: {e}")
        return []

    # Aggregate by (run, iter, type)
    iter_stats = {}
    for game in all_games:
        run = game.get("run", "?")
        # Try both field names - "iteration" (new) and "iter" (old)
        iter_num = game.get("iteration") if "iteration" in game else game.get("iter", -1)
        is_arena = game.get("type") == "arena"
        game_type = "Arena" if is_arena else "Self-play"
        doom = game.get("doom", 0)
        score = game.get("score", 0.0) if game.get("score") is not None else 0.0
        is_win = game.get("won", False)

        key = (run, iter_num, game_type)
        if key not in iter_stats:
            iter_stats[key] = {"dooms": [], "scores": [], "wins": 0, "total": 0}

        iter_stats[key]["dooms"].append(doom)
        iter_stats[key]["scores"].append(score)
        iter_stats[key]["total"] += 1
        if is_win:
            iter_stats[key]["wins"] += 1

    # ALSO parse live PolicyRun logs for in-progress runs (with caching)
    global _live_log_cache, _live_log_cache_timestamp
    current_time = time.time()

    # Only re-parse if cache is older than 10 seconds
    if current_time - _live_log_cache_timestamp > 10:
        _live_log_cache = {}
        try:
            tmp_dir = Path('/tmp')
            log_files = list(tmp_dir.glob('sp_R*.log'))

            for log_file in log_files:
                try:
                    content = log_file.read_text()

                    # Extract run tag - try checkpoint tag first, then filename
                    run_tag = None

                    # Try checkpoint tag (most reliable): "Loaded checkpoint [tag=R35_boot_val6"
                    checkpoint_match = re.search(r'Loaded checkpoint \[tag=(\w+)', content)
                    if checkpoint_match:
                        run_tag = checkpoint_match.group(1).split('_')[0]  # R35_boot_val6 -> R35

                    # Fall back to filename: sp_R35_trace.log -> R35
                    if not run_tag:
                        filename_match = re.match(r'sp_(\w+)', log_file.name)
                        if filename_match:
                            run_tag = filename_match.group(1).split('_')[0]  # R35_trace -> R35

                    if not run_tag:
                        continue

                    print(f"\nParsing log file: {log_file.name}, run_tag={run_tag}")

                    lines = content.split('\n')

                    # Parse each iteration's results
                    current_iter = None
                    for line in lines:
                        # Match: iter 1 | arena 20 games
                        iter_match = re.match(r'^iter (\d+) \| arena', line)
                        if iter_match:
                            current_iter = int(iter_match.group(1))

                        # Match: >>> overall 0/20 = 0% | GC:0/5(d14) BG:0/5(d9) YS:0/5(d4) CC:0/5(d5)
                        if '>>> overall' in line and current_iter is not None:
                            print(f"  Found >>> overall for iter {current_iter}: {line.strip()}")
                            # Parse overall wins/total
                            overall_match = re.search(r'overall (\d+)/(\d+)', line)
                            if overall_match:
                                total_wins = int(overall_match.group(1))
                                total_games = int(overall_match.group(2))
                                print(f"    Wins: {total_wins}/{total_games}")

                                # Parse per-faction doom
                                faction_stats = {}
                                parts = line.split('|')
                                if len(parts) >= 2:
                                    faction_parts = parts[1].strip().split()
                                    for faction_str in faction_parts:
                                        match = re.match(r'([A-Z]+):(\d+)/(\d+)\(d(\d+)\)', faction_str)
                                        if match:
                                            faction = match.group(1)
                                            doom_avg = int(match.group(4))
                                            faction_stats[faction] = doom_avg

                                    if faction_stats:
                                        # Calculate aggregate doom
                                        total_doom = sum(faction_stats.values())
                                        aggregate_doom = total_doom / len(faction_stats)

                                        key = (run_tag, current_iter, "Arena")
                                        _live_log_cache[key] = {
                                            "run": run_tag,
                                            "iter": current_iter,
                                            "avg_doom": round(aggregate_doom, 1),
                                            "avg_score": 0.0,
                                            "type": "Arena",
                                            "wins": total_wins,
                                            "total": total_games
                                        }
                                        print(f"    Cached: {key} -> doom={aggregate_doom:.1f}")

                            current_iter = None

                except Exception as e:
                    print(f"  Error parsing {log_file}: {e}")
                    continue

        except Exception as e:
            print(f"Error during live log parsing: {e}")

        _live_log_cache_timestamp = current_time

    # Merge canonical store + live cache
    history = []
    for (run, iter_num, game_type), stats in iter_stats.items():
        if stats["total"] > 0:
            avg_doom = sum(stats["dooms"]) / len(stats["dooms"])
            avg_score = sum(stats["scores"]) / len(stats["scores"])
            history.append({
                "run": run,
                "iter": iter_num,
                "avg_doom": round(avg_doom, 1),
                "avg_score": round(avg_score, 3),
                "type": game_type,
                "wins": stats["wins"],
                "total": stats["total"]
            })

    # Add live cache entries
    history.extend(_live_log_cache.values())

    # Sort by run (descending) then iter (descending)
    history.sort(key=lambda x: (x["run"], x["iter"]), reverse=True)

    return history

if __name__ == '__main__':
    history = get_performance_history()
    print(f"\n\nTotal entries: {len(history)}")
    print("\nR35 entries:")
    r35 = [h for h in history if h['run'] == 'R35']
    for entry in r35:
        print(f"  Iter {entry['iter']:3d}: doom={entry['avg_doom']:6.1f}, score={entry['avg_score']:.3f}, wins={entry['wins']}/{entry['total']}")
