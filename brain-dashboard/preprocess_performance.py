#!/usr/bin/env python3
"""Pre-process performance data as iterations complete."""

import json
import re
from pathlib import Path
from collections import defaultdict

MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
TRACES_DIR = MCTS_ROOT / "arena-traces"
CACHE_FILE = MCTS_ROOT / "brain-dashboard" / "performance_cache.json"
PROGRESS_CACHE = MCTS_ROOT / "brain-dashboard" / "progress_cache.json"

def compute_performance_data():
    """Compute performance data from all trace files."""
    # Aggregate by (run, iter, type)
    iter_stats = defaultdict(lambda: {"dooms": [], "scores": [], "wins": 0, "total": 0})

    # R26 cutoff for timestamp-based detection
    r26_cutoff = 1787234280

    for trace_file in TRACES_DIR.glob("*.txt"):
        try:
            stem = trace_file.stem
            is_arena = stem.startswith("arena-")
            is_selfplay = stem.startswith("selfplay-")

            if not is_arena and not is_selfplay:
                continue

            # Parse iteration
            parts = stem.split('-')
            iter_num = 0
            for part in parts:
                if part.startswith('iter'):
                    try:
                        iter_num = int(part[4:])
                    except:
                        pass

            # Assign run based on timestamp
            file_time = trace_file.stat().st_mtime
            if file_time >= r26_cutoff:
                run_str = "R26"
            elif file_time >= 1786770000:
                run_str = "R25"
            elif file_time >= 1786424400:
                run_str = "R24"
            else:
                run_str = "R23"

            # Quick parse for doom and score
            content = trace_file.read_text()

            # Get doom
            doom = 0
            all_doom_match = re.search(r'ALL_DOOM=(.+)', content)
            if all_doom_match:
                doom_str = all_doom_match.group(1).strip()
                doom_values = []
                for pair in doom_str.split():
                    if '=' in pair:
                        faction, d = pair.split('=')
                        doom_values.append(int(d))
                if doom_values:
                    doom = doom_values[0]

            # Get score
            score_match = re.search(r'FINAL_SCORE=([0-9.]+)', content)
            score = float(score_match.group(1)) if score_match else 0.0

            # Check for win
            won = False
            if 'won' in content and all_doom_match:
                doom_str = all_doom_match.group(1).strip()
                first_faction = None
                for pair in doom_str.split():
                    if '=' in pair:
                        first_faction = pair.split('=')[0]
                        break

                if first_faction:
                    won_line_match = re.search(r'(.+) won</div>', content)
                    if won_line_match:
                        won_text = won_line_match.group(1)
                        faction_names = {
                            'BG': 'Black Goat', 'YS': 'Yellow Sign',
                            'CC': 'Crawling Chaos', 'GC': 'Great Cthulhu'
                        }
                        if first_faction in faction_names:
                            if faction_names[first_faction] in won_text:
                                won = True

            game_type = "Arena" if is_arena else "Self-play"
            key = (run_str, iter_num, game_type)

            iter_stats[key]["dooms"].append(doom)
            iter_stats[key]["scores"].append(score)
            iter_stats[key]["total"] += 1
            if won:
                iter_stats[key]["wins"] += 1

        except Exception as e:
            pass

    # Convert to list
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

    # Sort by run (descending) then iter (descending)
    history.sort(key=lambda x: (x["run"], x["iter"]), reverse=True)

    return history

def compute_progress_data():
    """Compute progress data (per-game data for charts)."""
    data = {"selfplay": [], "arena": []}

    r26_cutoff = 1787234280

    for trace_file in sorted(TRACES_DIR.glob("*.txt")):
        try:
            stem = trace_file.stem
            is_arena = stem.startswith("arena-")
            is_selfplay = stem.startswith("selfplay-")

            if not is_arena and not is_selfplay:
                continue

            # Parse iteration
            parts = stem.split('-')
            iter_num = 0
            for part in parts:
                if part.startswith('iter'):
                    try:
                        iter_num = int(part[4:])
                    except:
                        pass

            # Assign run
            file_time = trace_file.stat().st_mtime
            if file_time >= r26_cutoff:
                run_str = "R26"
            elif file_time >= 1786770000:
                run_str = "R25"
            elif file_time >= 1786424400:
                run_str = "R24"
            else:
                run_str = "R23"

            run_num = int(run_str[1:])

            # Parse doom and score
            content = trace_file.read_text()

            doom = 0
            all_doom_match = re.search(r'ALL_DOOM=(.+)', content)
            if all_doom_match:
                doom_str = all_doom_match.group(1).strip()
                doom_values = []
                for pair in doom_str.split():
                    if '=' in pair:
                        faction, d = pair.split('=')
                        doom_values.append(int(d))
                if doom_values:
                    doom = doom_values[0]

            score_match = re.search(r'FINAL_SCORE=([0-9.]+)', content)
            score = float(score_match.group(1)) if score_match else 0.0

            if doom > 0 or score > 0:
                target = "arena" if is_arena else "selfplay"

                existing_games = [g for g in data[target] if g["run"] == run_str and g["iter"] == iter_num]
                game_num = len(existing_games) + 1

                data[target].append({
                    "run": run_str,
                    "iter": iter_num,
                    "game": game_num,
                    "run_iter_game": f"{run_str}_I{iter_num:02d}_G{game_num:02d}",
                    "run_iter_game_num": run_num * 1000000 + iter_num * 1000 + game_num,
                    "doom": doom,
                    "score": score,
                    "type": target
                })

        except Exception as e:
            pass

    return data

if __name__ == "__main__":
    print("Pre-processing performance data...")

    # Compute performance history
    print("  Computing performance history...")
    perf_data = compute_performance_data()
    CACHE_FILE.write_text(json.dumps(perf_data, indent=2))
    print(f"  Saved {len(perf_data)} entries to {CACHE_FILE}")

    # Compute progress data
    print("  Computing progress data...")
    progress_data = compute_progress_data()
    PROGRESS_CACHE.write_text(json.dumps(progress_data, indent=2))
    sp_count = len(progress_data.get("selfplay", []))
    ar_count = len(progress_data.get("arena", []))
    print(f"  Saved {sp_count} selfplay, {ar_count} arena entries to {PROGRESS_CACHE}")

    print("Done!")
