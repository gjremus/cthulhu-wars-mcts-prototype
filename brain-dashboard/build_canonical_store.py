#!/usr/bin/env python3
"""Build canonical game store - single source of truth for ALL dashboard data."""

import json
import re
from pathlib import Path
from collections import defaultdict

MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
TRACES_DIR = MCTS_ROOT / "arena-traces"
CANONICAL_STORE = MCTS_ROOT / "brain-dashboard" / "canonical_games.json"

def parse_trace_file(trace_file):
    """Parse a single trace file into canonical game record."""
    try:
        stem = trace_file.stem
        content = trace_file.read_text()

        # Skip metadata files (all-games summaries)
        if "-all-games" in stem:
            return None

        # Determine game type from filename
        is_arena = stem.startswith("arena-")
        is_selfplay = stem.startswith("selfplay-")

        if not is_arena and not is_selfplay:
            return None

        # Parse iteration number from filename
        iter_num = 0
        parts = stem.split('-')
        for part in parts:
            if part.startswith('iter'):
                try:
                    iter_num = int(part[4:])
                except:
                    pass

        # Assign run based on file modification timestamp
        file_time = trace_file.stat().st_mtime

        # Check for tagged runs by filename pattern (R28, R27, Run27, etc.)
        run_match = re.search(r'[-_](R\d+)[-_]', stem)
        if run_match:
            run = run_match.group(1)
            era = "new"
        elif "Run" in stem and any(f"Run{i}" in stem for i in range(1, 100)):
            # Extract run number from filename
            for i in range(1, 100):
                if f"Run{i}" in stem:
                    run = f"Run{i}"
                    era = "new"
                    break
        # R27 starts Aug 22 19:51 (first iter 1 file)
        elif file_time >= 1787446290:  # Aug 22 19:51:30 2026
            run = "R27"
            era = "new"
        elif file_time >= 1787234280:  # Aug 20, 2026 08:58 (R26 start)
            run = "R26"
            era = "old"
        elif file_time >= 1786770000:  # Aug 15, 2026 (R25 start)
            run = "R25"
            era = "old"
        elif file_time >= 1786424400:  # Aug 11, 2026 (R24 start)
            run = "R24"
            era = "old"
        else:
            run = "R23"
            era = "old"

        # R27 iterations keep their original numbers from filenames

        # For ARENA games: parse brain's faction from filename
        # arena-iter##-{faction}-{best|first}-d#.txt
        brain_faction = None
        if is_arena:
            parts = stem.split('-')
            for i, part in enumerate(parts):
                if part in ['bg', 'cc', 'gc', 'ys']:
                    brain_faction = part.upper()
                    break

        # Parse doom from ALL_DOOM= line
        doom = 0
        all_doom_match = re.search(r'ALL_DOOM=(.+)', content)
        if all_doom_match:
            doom_str = all_doom_match.group(1).strip()

            # For ARENA: find brain's faction doom
            if is_arena and brain_faction:
                for pair in doom_str.split():
                    if '=' in pair:
                        faction, d = pair.split('=')
                        if faction == brain_faction:
                            doom = int(d)
                            break
            else:
                # For SELFPLAY: first doom value is brain's doom
                for pair in doom_str.split():
                    if '=' in pair:
                        faction, d = pair.split('=')
                        doom = int(d)
                        break

        # Parse score from FINAL_SCORE= line
        score = 0.0
        score_match = re.search(r'FINAL_SCORE=([0-9.]+)', content)
        if score_match:
            score = float(score_match.group(1))

        # Check if brain won
        won = False
        won_line_match = re.search(r'(.+) won</div>', content)
        if won_line_match:
            won_text = won_line_match.group(1)
            faction_names = {
                'BG': 'Black Goat', 'YS': 'Yellow Sign',
                'CC': 'Crawling Chaos', 'GC': 'Great Cthulhu'
            }

            # For ARENA: check if brain's faction won
            if is_arena and brain_faction and brain_faction in faction_names:
                if faction_names[brain_faction] in won_text:
                    won = True
            # For SELFPLAY: check if first faction won
            elif all_doom_match:
                doom_str = all_doom_match.group(1).strip()
                for pair in doom_str.split():
                    if '=' in pair:
                        first_faction = pair.split('=')[0]
                        if first_faction in faction_names:
                            if faction_names[first_faction] in won_text:
                                won = True
                        break

        # Game type determined by filename prefix only
        game_type = "arena" if is_arena else "selfplay"

        # Parse game length (count ActionPhaseAction occurrences)
        game_length = content.count('ActionPhaseAction')

        # Check if game was killed (early termination)
        is_killed = 'KILLED' in stem
        result = "killed" if is_killed else ("won" if won else "lost")
        kill_ap = None
        if is_killed:
            # Parse kill reason to get AP number: "Doom too low at AP~15"
            kill_reason_match = re.search(r'at AP~(\d+)', content)
            if kill_reason_match:
                kill_ap = int(kill_reason_match.group(1))

        return {
            "filename": stem,
            "filepath": str(trace_file),
            "run": run,
            "era": era,
            "iter": iter_num,
            "type": game_type,
            "doom": doom,
            "score": score,
            "won": won,
            "result": result,
            "game_length": game_length,
            "kill_ap": kill_ap,
            "timestamp": int(file_time)
        }

    except Exception as e:
        print(f"ERROR parsing {trace_file.name}: {e}")
        return None

def build_canonical_store():
    """Build canonical game store from all trace files."""
    print("Building canonical game store from trace files...")

    games = []
    errors = []

    for trace_file in sorted(TRACES_DIR.glob("*.txt")):
        game = parse_trace_file(trace_file)
        if game:
            games.append(game)
        else:
            errors.append(trace_file.name)

    # Sort by timestamp (chronological order)
    games.sort(key=lambda g: g["timestamp"])

    # Add sequential game numbers per (run, iter, type)
    game_nums = defaultdict(int)
    for game in games:
        key = (game["run"], game["iter"], game["type"])
        game_nums[key] += 1
        game["game_num"] = game_nums[key]

    store = {
        "generated": "2026-08-22 11:45",
        "total_games": len(games),
        "errors": errors,
        "games": games
    }

    CANONICAL_STORE.write_text(json.dumps(store, indent=2))

    print(f"✓ Canonical store built: {len(games)} games")
    print(f"  Errors: {len(errors)}")

    # Summary stats
    by_run = defaultdict(lambda: {"total": 0, "arena": 0, "selfplay": 0})
    for game in games:
        by_run[game["run"]]["total"] += 1
        by_run[game["run"]][game["type"]] += 1

    print("\nSummary by run:")
    for run in sorted(by_run.keys()):
        d = by_run[run]
        print(f"  {run}: {d['total']} total ({d['arena']} arena, {d['selfplay']} selfplay)")

    return store

def audit_canonical_store(store):
    """Audit canonical store for data quality issues."""
    print("\n=== AUDITING CANONICAL STORE ===")

    games = store["games"]
    issues = []

    # Check R26 arena games
    r26_arena = [g for g in games if g["run"] == "R26" and g["type"] == "arena"]
    r26_selfplay = [g for g in games if g["run"] == "R26" and g["type"] == "selfplay"]

    print(f"\nR26 Games:")
    print(f"  Arena: {len(r26_arena)} games")
    print(f"  Selfplay: {len(r26_selfplay)} games")

    if r26_arena:
        r26_arena_iters = sorted(set(g["iter"] for g in r26_arena))
        r26_arena_dooms = [g["doom"] for g in r26_arena]
        print(f"  Arena iterations: {r26_arena_iters}")
        print(f"  Arena doom range: {min(r26_arena_dooms)}-{max(r26_arena_dooms)}")
        print(f"  Arena avg doom: {sum(r26_arena_dooms)/len(r26_arena_dooms):.1f}")

    if r26_selfplay:
        r26_selfplay_iters = sorted(set(g["iter"] for g in r26_selfplay))
        r26_selfplay_dooms = [g["doom"] for g in r26_selfplay]
        print(f"  Selfplay iterations: {r26_selfplay_iters[:20]}{'...' if len(r26_selfplay_iters) > 20 else ''}")
        print(f"  Selfplay doom range: {min(r26_selfplay_dooms)}-{max(r26_selfplay_dooms)}")
        print(f"  Selfplay avg doom: {sum(r26_selfplay_dooms)/len(r26_selfplay_dooms):.1f}")

    # Check latest iteration by run
    print(f"\nLatest iterations:")
    for run in ["R24", "R25", "R26"]:
        run_games = [g for g in games if g["run"] == run]
        if run_games:
            latest = max(g["iter"] for g in run_games)
            latest_games = [g for g in run_games if g["iter"] == latest]
            types = set(g["type"] for g in latest_games)
            print(f"  {run} iter {latest}: {len(latest_games)} games, types: {types}")

    # Check for score >1.0 corruption
    high_scores = [g for g in games if g["score"] > 1.0]
    if high_scores:
        issues.append(f"{len(high_scores)} games with score >1.0 (max {max(g['score'] for g in high_scores):.3f})")

    # Check for doom=0 non-wins
    zero_doom = [g for g in games if g["doom"] == 0 and not g["won"]]
    if zero_doom:
        issues.append(f"{len(zero_doom)} games with doom=0 and no win")

    if issues:
        print(f"\n⚠ ISSUES FOUND:")
        for issue in issues:
            print(f"  - {issue}")
    else:
        print(f"\n✓ No data quality issues found")

    return issues

if __name__ == "__main__":
    store = build_canonical_store()
    audit_canonical_store(store)
    print(f"\n✓ Canonical store saved to: {CANONICAL_STORE}")
