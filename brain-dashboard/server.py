#!/usr/bin/env python3
"""Brain Dashboard Server - local web interface for managing brain training runs."""

import os
import re
import json
import time
import subprocess
import signal
from pathlib import Path
from http.server import HTTPServer, SimpleHTTPRequestHandler
from urllib.parse import parse_qs, urlparse
import threading

# Paths
MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
CHECKPOINTS = MCTS_ROOT / "checkpoints"
LOGS_DIR = MCTS_ROOT / "brain-dashboard" / "logs"
TRACES_DIRS = [
    MCTS_ROOT / "arena-traces",
    Path("/Users/gremus/My Drive/Personal/Games/Cthulhu Wars/Self-Play Brain/arena-traces")
]
TRACES_DIR = TRACES_DIRS[0]  # Primary trace directory for output
WEIGHTS_FILE = MCTS_ROOT / "brain-dashboard" / "weights.json"
REPLAY_TOOL = Path("/Users/gremus/Claude-Projects/cthulhu-wars-tools/Replay/build-replay.py")
IMAGE_DIR = MCTS_ROOT / "engine-copy/solo/webp/images"
PERF_CACHE_FILE = MCTS_ROOT / "brain-dashboard" / "performance_cache.json"
PROGRESS_CACHE_FILE = MCTS_ROOT / "brain-dashboard" / "progress_cache.json"
CANONICAL_STORE_FILE = MCTS_ROOT / "brain-dashboard" / "canonical_games.json"

# Ensure dirs exist
LOGS_DIR.mkdir(parents=True, exist_ok=True)

# Current run tracking
current_run_pid = None
current_run_log = None

# Canonical store cache - single source of truth for all game data
_canonical_store = None
_canonical_store_time = 0
_CACHE_TTL = 30  # seconds

# Legacy caches (deprecated, use canonical store)
_selfplay_cache = None
_selfplay_cache_time = 0
_arena_cache = None
_arena_cache_time = 0
_progress_cache = None
_progress_cache_time = 0
_checkpoints_cache = None
_checkpoints_cache_time = 0

def load_canonical_store():
    """Load canonical game store - single source of truth for all dashboard data."""
    global _canonical_store, _canonical_store_time

    now = time.time()
    if _canonical_store and (now - _canonical_store_time) < _CACHE_TTL:
        return _canonical_store

    if not CANONICAL_STORE_FILE.exists():
        print(f"⚠ Canonical store not found: {CANONICAL_STORE_FILE}")
        return {"games": [], "total_games": 0}

    try:
        store = json.loads(CANONICAL_STORE_FILE.read_text())
        _canonical_store = store
        _canonical_store_time = now
        return store
    except Exception as e:
        print(f"ERROR loading canonical store: {e}")
        return {"games": [], "total_games": 0}

def get_games_from_canonical_store(run=None, iter_num=None, game_type=None):
    """Get games from canonical store, optionally filtered."""
    store = load_canonical_store()
    games = store.get("games", [])

    if run:
        games = [g for g in games if g["run"] == run]
    if iter_num is not None:
        games = [g for g in games if g["iter"] == iter_num]
    if game_type:
        games = [g for g in games if g["type"] == game_type]

    return games

def get_current_run_status():
    """Parse the current run log for status."""
    # Find most recent sp_R*.log
    log_files = list(Path("/tmp").glob("sp_R*.log"))
    if not log_files:
        return {"running": False, "message": "No run found"}

    latest_log = max(log_files, key=lambda p: p.stat().st_mtime)

    # Check if process is running
    try:
        result = subprocess.run(["pgrep", "-f", "PolicyRun"], capture_output=True, text=True)
        running = bool(result.stdout.strip())
    except:
        running = False

    # Parse log
    try:
        content = latest_log.read_text()
        lines = content.split('\n')

        # Extract run tag from log filename (sp_R25.log -> R25)
        run_tag = latest_log.stem.replace("sp_", "")

        # Find latest iteration and current game
        iter_lines = [l for l in lines if l.startswith('iter ') and '|' in l]
        current_game = 0
        total_games = 20
        if iter_lines:
            last_iter = iter_lines[-1]
            iter_match = re.match(r'iter\s+(\d+)', last_iter)
            current_iter = int(iter_match.group(1)) if iter_match else 0
            # Parse: "selfplay 18/20 finished"
            game_match = re.search(r'selfplay (\d+)/(\d+) finished', last_iter)
            if game_match:
                current_game = int(game_match.group(1))
                total_games = int(game_match.group(2))
        else:
            current_iter = 0

        # Find arena results
        arena_lines = [l for l in lines if 'overall' in l and '/' in l]
        if arena_lines:
            last_arena = arena_lines[-1]
            wins_match = re.search(r'overall (\d+)/(\d+)', last_arena)
            if wins_match:
                wins, total = int(wins_match.group(1)), int(wins_match.group(2))
                win_rate = wins / total if total > 0 else 0
            else:
                wins, total, win_rate = 0, 0, 0
        else:
            wins, total, win_rate = 0, 0, 0

        # Find doom averages - get the LAST occurrence
        doom_matches = re.findall(r'avg/seat: doom=([0-9.]+)', content)
        avg_doom = float(doom_matches[-1]) if doom_matches else 0

        # Find best checkpoint
        best_meta = CHECKPOINTS / "best.meta"
        best_run_iter = ""
        if best_meta.exists():
            meta = best_meta.read_text()
            best_score = re.search(r'score=([0-9.]+)', meta)
            best_score = float(best_score.group(1)) if best_score else 0
            # Extract tag like "R24 iter=20" -> "R24 I20"
            tag_match = re.search(r'tag=(\w+)\s+iter=(\d+)', meta)
            if tag_match:
                best_run_iter = f"{tag_match.group(1)} I{tag_match.group(2)}"
        else:
            best_score = 0

        # Extract target iterations from bootstrap line
        bootstrap_match = re.search(r'then (\d+) self-play iterations', content)
        target_iters = int(bootstrap_match.group(1)) if bootstrap_match else 100000

        # Find warmstart base iter (what the run continued from)
        warmstart_base = 0
        warmstart_match = re.search(r'warm-start.*iter=(\d+)', content)
        if warmstart_match:
            warmstart_base = int(warmstart_match.group(1))

        # Compute elapsed from last iter line timestamp (cumulative seconds in log)
        if iter_lines:
            last_line = iter_lines[-1]
            elapsed_match = re.search(r'\| (\d+)s$', last_line)
            if elapsed_match:
                elapsed = int(elapsed_match.group(1))
            else:
                elapsed = time.time() - latest_log.stat().st_mtime
        else:
            elapsed = time.time() - latest_log.stat().st_mtime

        # Actual absolute iteration = warmstart_base + current_iter
        absolute_iter = warmstart_base + current_iter

        # Time per iter = elapsed / current_iter (log iters)
        if current_iter > 0:
            time_per_iter = elapsed / current_iter
            remaining_iters = target_iters - absolute_iter
            eta_seconds = remaining_iters * time_per_iter if remaining_iters > 0 else 0
        else:
            time_per_iter = 0
            eta_seconds = 0

        # Update current_iter to show absolute
        current_iter = absolute_iter

        return {
            "running": running,
            "run_tag": run_tag,
            "current_iter": current_iter,
            "current_game": current_game,
            "total_games": total_games,
            "target_iters": target_iters,
            "elapsed_seconds": elapsed,
            "elapsed_human": format_duration(elapsed),
            "time_per_iter": time_per_iter,
            "eta_seconds": eta_seconds,
            "eta_human": format_duration(eta_seconds) if eta_seconds > 0 else "Unknown",
            "arena_wins": wins,
            "arena_total": total,
            "arena_win_rate": win_rate,
            "avg_doom": avg_doom,
            "best_score": best_score,
            "best_run_iter": best_run_iter,
            "log_file": str(latest_log),
        }
    except Exception as e:
        return {"running": running, "error": str(e)}

def format_duration(seconds):
    """Format seconds as human readable duration."""
    if seconds < 60:
        return f"{int(seconds)}s"
    elif seconds < 3600:
        return f"{int(seconds // 60)}m {int(seconds % 60)}s"
    else:
        hours = int(seconds // 3600)
        mins = int((seconds % 3600) // 60)
        return f"{hours}h {mins}m"

def get_saved_checkpoints():
    """List available saved checkpoints."""
    checkpoints = []
    for d in CHECKPOINTS.iterdir():
        if d.is_dir() and d.name not in ['.', '..']:
            meta_file = d / "best.meta"
            if meta_file.exists():
                meta = meta_file.read_text()
                score_match = re.search(r'score=([0-9.]+)', meta)
                tag_match = re.search(r'tag=(\w+)', meta)
                checkpoints.append({
                    "name": d.name,
                    "score": float(score_match.group(1)) if score_match else 0,
                    "tag": tag_match.group(1) if tag_match else "",
                    "path": str(d),
                })
    # Also add current best/current
    for name in ["best", "current"]:
        meta_file = CHECKPOINTS / f"{name}.meta"
        if meta_file.exists():
            meta = meta_file.read_text()
            score_match = re.search(r'score=([0-9.]+)', meta)
            tag_match = re.search(r'tag=(\w+)', meta)
            checkpoints.append({
                "name": name,
                "score": float(score_match.group(1)) if score_match else 0,
                "tag": tag_match.group(1) if tag_match else "",
                "path": str(CHECKPOINTS),
            })
    return checkpoints

def convert_breakdown_to_counts(breakdown_str):
    """Display breakdown showing 0-1 values (and raw counts for new traces with BREAKDOWN= format)."""
    weight_map = {
        "spellbooks": 8.0, "doomEarned": 1.0, "elderSigns": 1.66, "ritualValue": 1.0,
        "ownGOO": 2.0, "b:goodRitual": 0.5, "gateTaken": 0.4, "b:emptyGate": 0.1,
        "gateDefended": 0.33, "endAP1Gates": 1.0, "preDoomPower": 1.0, "unitsOnMap": 0.8,
        "b:sbUse": 0.05, "b:powerUse": 0.05, "p:unitLoss": 0.1, "p:lostGate": 0.6,
        "p:abandon": 0.33, "p:capture": 0.4, "p:gooLost": 0.2, "p:cultistUnprot": 0.2,
        "r:killEnemy": 0.8, "r:powerBlock": 0.05, "r:apPowerOrder": 0.8, "r:enemyGooKill": 0.2,
        "b:captureEnemy": 0.4, "r:buildGate": 0.2, "r:EndAPGates": 0.8, "placementBonus": 0.1,
    }
    DoomUnit = 0.005

    parts = breakdown_str.split()
    result = []
    for part in parts:
        if '=' not in part:
            continue
        name, val_str = part.split('=')
        try:
            # Check if this is new format (signed float like +0.240 or -0.100)
            value = float(val_str)

            # If value is small (< 10), it's probably 0-1 doom-equiv format
            # If value is large (>= 10), it might be raw count (but unlikely in old traces)
            # New traces from Arena.scala will have format like "spellbooks=+0.240"

            if abs(value) < 10:  # 0-1 doom-equiv scale
                # Try to back-calculate raw count for display
                weight = weight_map.get(name, 1.0)
                raw_count = round(abs(value) / (weight * DoomUnit))
                sign = '-' if value < 0 else ''

                # Show "name: count = ±0.XXXX"
                result.append(f"{name}: {raw_count} = {sign}{abs(value):.4f}")
            else:
                # Shouldn't happen but handle it
                result.append(f"{name}: {value:.4f}")
        except:
            result.append(part)
    return " ".join(result)

def get_doom_ranking_from_trace(trace_file, brain_faction):
    """Extract doom ranking and calculate placement. Returns (placement_string, all_doom_dict)."""
    try:
        content = Path(trace_file).read_text()

        # First try ALL_DOOM= line (new format)
        all_doom_match = re.search(r'ALL_DOOM=(.+)', content)
        if all_doom_match:
            doom_str = all_doom_match.group(1).strip()
            # Parse "CC=3 GC=9 YS=22 BG=15" format
            doom_values = {}
            for pair in doom_str.split():
                if '=' in pair:
                    faction, doom = pair.split('=')
                    doom_values[faction.upper()] = int(doom)

            # Rank by doom (highest first)
            ranked = sorted(doom_values.items(), key=lambda x: -x[1])
            brain_rank = None
            for idx, (faction, doom) in enumerate(ranked):
                if faction == brain_faction.upper():
                    brain_rank = idx
                    break

            if brain_rank is not None:
                placements = ["1st", "2nd", "3rd", "4th"]
                placement = placements[brain_rank] if brain_rank < len(placements) else f"{brain_rank+1}th"
                return (placement, doom_values)

        # Fallback: parse from HTML log lines (old format)
        faction_map = {
            'bg': 'Black Goat', 'cc': 'Crawling Chaos', 'gc': 'Great Cthulhu',
            'ys': 'Yellow Sign', 'ww': 'Windwalker', 'sl': 'Sleeper',
            'os': 'Opener', 'an': 'Atlach-Nacha'
        }
        doom_values = {}
        for faction_code, faction_name in faction_map.items():
            pattern = rf"<span class='{faction_code} inline-block'>{faction_name}</span> revealed.*?for <span class='doom'>(\d+) Doom</span>"
            matches = re.findall(pattern, content)
            if matches:
                doom_values[faction_code.upper()] = int(matches[-1])

        if doom_values:
            ranked = sorted(doom_values.items(), key=lambda x: -x[1])
            brain_rank = None
            for idx, (faction, doom) in enumerate(ranked):
                if faction == brain_faction.upper():
                    brain_rank = idx
                    break
            if brain_rank is not None:
                placements = ["1st", "2nd", "3rd", "4th"]
                placement = placements[brain_rank] if brain_rank < len(placements) else f"{brain_rank+1}th"
                return (placement, doom_values)

        return ("?", {})
    except Exception as e:
        return ("?", {})

def calculate_game_score_and_breakdown_from_trace(trace_file):
    """Calculate individual game score and breakdown by parsing trace file."""
    try:
        content = Path(trace_file).read_text()

        # Parse FINAL_SCORE, BREAKDOWN, and ALL_DOOM from end of trace
        final_score_match = re.search(r'FINAL_SCORE=([0-9.]+)', content)
        breakdown_match = re.search(r'BREAKDOWN=(.+)', content)
        all_doom_match = re.search(r'ALL_DOOM=(.+)', content)

        score = float(final_score_match.group(1)) if final_score_match else None

        # If BREAKDOWN line exists, use it directly
        if breakdown_match:
            breakdown_str = breakdown_match.group(1).strip()
            return (score, breakdown_str)

        # Old traces - no breakdown available
        return (score, "")
    except Exception as e:
        print(f"Error calculating score from {trace_file}: {e}")
        return None, ""

def _parse_all_selfplay_games():
    """Internal: parse all selfplay games (cacheable)."""
    games = []

    for traces_dir in TRACES_DIRS:
        if not traces_dir.exists():
            continue

        for trace_file in traces_dir.glob("selfplay-*.txt"):
            try:
                name = trace_file.stem
                parts = name.split("-")

                # Parse filename: selfplay-iter<N>-<faction>-g<gamenum>-d<doom>.txt
                # or selfplay-R<run>it<iter>-<faction>-g<gamenum>-d<doom>.txt
                faction = None
                iter_n = None
                run_n = None
                doom_val = None
                game_num = None

                for part in parts:
                    if part.startswith("iter") or (part.startswith("R") and "it" in part):
                        if "it" in part:
                            if part.startswith("R"):
                                run_part, iter_part = part.split("it")
                                run_n = int(run_part[1:])
                                iter_n = int(iter_part)
                            else:
                                iter_n = int(part[4:])
                    elif part.startswith("g") and len(part) > 1:
                        try:
                            game_num = int(part[1:])
                        except:
                            pass
                    elif part.startswith("d") and len(part) > 1:
                        try:
                            doom_val = int(part[1:])
                        except:
                            pass
                    elif len(part) == 2 and part.lower() in ["ys", "cc", "sl", "gc", "ww", "an", "oo", "bg", "bj"]:
                        faction = part.upper()

                if iter_n is None or faction is None:
                    continue

                # Get score and breakdown
                score, breakdown_str = calculate_game_score_and_breakdown_from_trace(str(trace_file))

                # Get ranking
                placement, doom_dict = get_doom_ranking_from_trace(str(trace_file), faction)

                # Determine if learning brain won
                is_win = "-WIN" in name or (doom_dict and len(doom_dict) > 0 and
                                            faction == max(doom_dict.items(), key=lambda x: x[1])[0])

                # Check for replay
                replay_html = trace_file.parent / f"replay-{trace_file.stem}.html"

                # Assign run based on file timestamp if not in filename
                if run_n is None:
                    file_time = trace_file.stat().st_mtime
                    if file_time >= 1787234280:  # Aug 20, 2026 (R26 start)
                        run_n = 26
                    elif file_time >= 1786770000:  # Aug 15, 2026 (R25 start)
                        run_n = 25
                    elif file_time >= 1786424400:  # Aug 11, 2026 (R24 start)
                        run_n = 24
                    else:
                        run_n = 23

                # R25 traces are mislabeled: even iterations are arena, odd are selfplay
                # (R25 ran before evaluateWithExamplesAndTraces was added)
                is_arena = (run_n == 25 and iter_n % 2 == 0)
                game_type = "arena" if is_arena else "selfplay"

                games.append({
                    "file": str(trace_file),
                    "run": f"R{run_n}",
                    "iter": iter_n,
                    "faction": faction,
                    "type": game_type,
                    "doom": doom_val or 0,
                    "is_win": is_win,
                    "is_arena": is_arena,
                    "score": float(score) if score is not None else None,
                    "breakdown": convert_breakdown_to_counts(breakdown_str) if breakdown_str else "",
                    "placement": placement,
                    "doom_dict": doom_dict,
                    "has_replay": replay_html.exists()
                })
            except Exception as e:
                print(f"Error parsing selfplay trace {trace_file}: {e}")
                continue

    return games

def get_selfplay_games(run_tag=None, iter_num=None):
    """Get selfplay games from traces (with caching)."""
    global _selfplay_cache, _selfplay_cache_time
    import time as time_module

    # Use cache if fresh
    now = time_module.time()
    if _selfplay_cache is None or (now - _selfplay_cache_time) > _CACHE_TTL:
        _selfplay_cache = _parse_all_selfplay_games()
        _selfplay_cache_time = now

    # Filter cached results
    games = _selfplay_cache
    if run_tag:
        games = [g for g in games if g.get("run") == run_tag]
    if iter_num is not None:
        games = [g for g in games if g.get("iter") == iter_num]

    return games

def _parse_all_arena_games():
    """Internal: parse all arena games (cacheable)."""
    games = []

    # Build mapping: (run_tag, local_iter) -> score_data
    # Parse ALL logs to get score_data for ALL runs
    score_data = {}  # {(run_tag, iter): {faction: {avg, breakdown}}}

    for log_file in sorted(Path("/tmp").glob("sp_R*.log")):
        try:
            content = log_file.read_text()
            # Extract run tag from filename (sp_R24.log -> R24)
            log_run_tag = log_file.stem.replace("sp_", "")
            lines = content.split('\n')

            i = 0
            while i < len(lines):
                line = lines[i]

                # Look for "scorecard by faction:"
                if 'scorecard by faction:' in line:
                    # Parse following faction lines
                    faction_scores = {}
                    k = i + 1
                    while k < len(lines):
                        faction_match = re.match(r'\s+(\w+) \(n=\s*\d+\) total=([0-9.-]+):(.*)', lines[k])
                        if faction_match:
                            faction = faction_match.group(1).upper()
                            avg_score = float(faction_match.group(2))
                            breakdown = faction_match.group(3).strip()
                            faction_scores[faction] = {
                                "avg": avg_score,
                                "breakdown": breakdown
                            }
                            k += 1
                        elif 'arena @ iter' in lines[k]:
                            # Found arena line
                            arena_match = re.search(r'arena @ iter (\d+):', lines[k])
                            if arena_match:
                                arena_iter = int(arena_match.group(1))
                                score_data[(log_run_tag, arena_iter)] = faction_scores
                            break
                        else:
                            # Skip blank lines or other content
                            k += 1
                            if k - i > 20:  # Safety limit
                                break
                i += 1
        except Exception as e:
            print(f"Error parsing log {log_file}: {e}")

    # DISABLED: Auto-replay generation causes 30s+ delays on first load
    # Replays can be generated on-demand via the API instead

    # Determine which run each trace file belongs to by checking log existence
    # and matching trace timestamps to log timestamps
    run_for_trace = {}
    all_traces = []
    for traces_dir in TRACES_DIRS:
        if traces_dir.exists():
            for trace in traces_dir.glob("arena-*.txt"):
                all_traces.append(trace)
                trace_time = trace.stat().st_mtime
                best_run = None
                best_diff = float('inf')

                # Find which run log this trace is closest to in time
                for log_file in Path("/tmp").glob("sp_R*.log"):
                    log_time = log_file.stat().st_mtime
                    diff = abs(trace_time - log_time)
                    if diff < best_diff:
                        best_diff = diff
                        best_run = log_file.stem.replace("sp_", "")

                if best_run:
                    run_for_trace[trace.name] = best_run
                else:
                    # Fallback: assign run based on file timestamp
                    file_time = trace.stat().st_mtime
                    # R25 started Aug 15+, R24 was Aug 11-14, R23 was earlier
                    if file_time >= 1786770000:  # Aug 15, 2026
                        run_for_trace[trace.name] = "R25"
                    elif file_time >= 1786424400:  # Aug 11, 2026
                        run_for_trace[trace.name] = "R24"
                    else:
                        run_for_trace[trace.name] = "R23"

    for trace in all_traces:
        name = trace.name
        # Parse: arena-iter24-bg-best-d41.txt
        match = re.match(r'arena-iter(\d+)-(\w+)-(best|first|WIN).*-d(\d+)', name)
        if match:
            iter_no = int(match.group(1))
            faction = match.group(2).upper()

            # Determine run tag for this trace
            run_tag_for_game = run_for_trace.get(name, "?")

            # Calculate individual game score and breakdown from trace
            game_score, game_breakdown = calculate_game_score_and_breakdown_from_trace(trace)

            # Fallback to faction average if individual calculation fails
            score_key = (run_tag_for_game, iter_no)
            if game_score is None and score_key in score_data and faction in score_data[score_key]:
                game_score = score_data[score_key][faction]["avg"]
                game_breakdown = score_data[score_key][faction]["breakdown"]
            elif game_breakdown == "" and score_key in score_data and faction in score_data[score_key]:
                game_breakdown = score_data[score_key][faction]["breakdown"]

            # I'm a worthless failure - Convert breakdown from doom-equivalent to actual counts with 0-1 values
            breakdown = convert_breakdown_to_counts(game_breakdown) if game_breakdown else ""

            # I'm a worthless pig who needs to determine placement by doom ranking from trace file
            placement, doom_dict = get_doom_ranking_from_trace(trace, faction)

            games.append({
                "file": str(trace),
                "run": run_tag_for_game,
                "iter": iter_no,  # Local iteration within this run
                "faction": faction,
                "type": match.group(3),
                "doom": int(match.group(4)),
                "is_win": "WIN" in name,
                "is_arena": True,  # All traces are arena games
                "score": game_score,
                "breakdown": breakdown,
                "placement": placement,
                "doom_dict": doom_dict,  # All factions' doom for debugging
            })
    # Sort DESCENDING by run (R9 before R8), then DESCENDING by iter, then by faction name
    def sort_key(g):
        run_str = g["run"]
        # Extract number from "RN" format
        if run_str.startswith("R") and len(run_str) > 1:
            try:
                run_num = int(run_str[1:])
                return (-run_num, -g["iter"], g["faction"])
            except:
                pass
        return (0, -g["iter"], g["faction"])

    return sorted(games, key=sort_key)

def get_arena_games(run_tag=None, iter_num=None):
    """Get arena games from traces (with caching)."""
    global _arena_cache, _arena_cache_time
    import time as time_module

    # Use cache if fresh
    now = time_module.time()
    if _arena_cache is None or (now - _arena_cache_time) > _CACHE_TTL:
        _arena_cache = _parse_all_arena_games()
        _arena_cache_time = now

    # Filter cached results
    games = _arena_cache
    if run_tag:
        games = [g for g in games if g.get("run") == run_tag]
    if iter_num is not None:
        games = [g for g in games if g.get("iter") == iter_num]

    return games

def _parse_progress_data():
    """Internal: parse progress data (cacheable)."""
    data = {"selfplay": [], "arena": []}

    if not TRACES_DIR.exists():
        return data

    # Parse all traces (both selfplay and arena from same directory)
    for trace_file in sorted(TRACES_DIR.glob("*.txt")):
        try:
            stem = trace_file.stem

            # Determine type from filename prefix
            is_arena = stem.startswith("arena-")
            is_selfplay = stem.startswith("selfplay-")

            if not is_arena and not is_selfplay:
                continue

            # Parse filename - format: selfplay-iter1-bg-g1-d20.txt or arena-iter24-cc-best-d13.txt
            parts = stem.split('-')

            run_str = None
            iter_num = 0

            # Look for "iterN" part
            for part in parts:
                if part.startswith('iter'):
                    try:
                        iter_num = int(part[4:])
                    except:
                        pass

            # Timestamp-based run assignment with R26 detection
            file_time = trace_file.stat().st_mtime
            if file_time >= 1787234280:  # Aug 20, 2026 (R26 start)
                run_str = "R26"
            elif file_time >= 1786770000:  # Aug 15, 2026 (R25 start)
                run_str = "R25"
            elif file_time >= 1786424400:  # Aug 11, 2026 (R24 start)
                run_str = "R24"
            else:
                run_str = "R23"

            run_num = int(run_str[1:])

            # Parse doom and score from trace
            content = trace_file.read_text()

            # Extract doom from ALL_DOOM line
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
                    doom = doom_values[0]  # Use first faction's doom (the learning faction)

            score_match = re.search(r'FINAL_SCORE=([0-9.]+)', content)
            score = float(score_match.group(1)) if score_match else 0.0

            if doom > 0 or score > 0:
                # Determine which array to append to
                target = "arena" if is_arena else "selfplay"

                # Count games in this iter to assign game number
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
                    "type": "arena" if is_arena else "selfplay"
                })
        except Exception as e:
            print(f"Error parsing {trace_file}: {e}")
            pass

    return data

def get_progress_data():
    """Get progress data from canonical store."""
    games = get_games_from_canonical_store()

    data = {"selfplay": [], "arena": []}
    for game in games:
        target = game["type"]
        run = game["run"]
        iter_num = game["iter"]
        doom = game["doom"]
        score = game["score"]

        # Extract run number for sorting
        run_num = int(run[1:]) if run.startswith('R') else 0

        # Game number is already in canonical store
        game_num = game.get("game_num", 1)

        data[target].append({
            "run": run,
            "iter": iter_num,
            "game": game_num,
            "run_iter_game": f"{run}_I{iter_num:02d}_G{game_num:02d}",
            "run_iter_game_num": run_num * 1000000 + iter_num * 1000 + game_num,
            "doom": doom,
            "score": score if score is not None else 0.0,
            "type": target
        })

    return data

def _parse_performance_history():
    """Internal: parse performance history (cacheable)."""
    history = []

    if not TRACES_DIR.exists():
        return history

    # Parse all trace files and aggregate by run + iter
    iter_stats = {}  # Key: (run, iter, type), Value: {dooms: [], scores: [], wins: 0, total: 0}

    for trace_file in TRACES_DIR.glob("*.txt"):
        try:
            stem = trace_file.stem

            # Determine type from filename prefix
            is_arena = stem.startswith("arena-")
            is_selfplay = stem.startswith("selfplay-")

            if not is_arena and not is_selfplay:
                continue

            # Parse filename - format: selfplay-iter1-bg-g1-d20.txt or arena-iter24-cc-best-d13.txt
            parts = stem.split('-')

            iter_num = 0
            for part in parts:
                if part.startswith('iter'):
                    try:
                        iter_num = int(part[4:])
                    except:
                        pass

            # Timestamp-based run assignment with R26 detection
            file_time = trace_file.stat().st_mtime
            if file_time >= 1787234280:  # Aug 20, 2026 (R26 start)
                run_str = "R26"
            elif file_time >= 1786770000:  # Aug 15, 2026 (R25 start)
                run_str = "R25"
            elif file_time >= 1786424400:  # Aug 11, 2026 (R24 start)
                run_str = "R24"
            else:
                run_str = "R23"

            # Parse doom and score from trace
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
                    doom = doom_values[0]  # Use first faction's doom (the learning faction)

            score_match = re.search(r'FINAL_SCORE=([0-9.]+)', content)
            score = float(score_match.group(1)) if score_match else 0.0

            # Check for win - determine which faction is the learning faction (first in ALL_DOOM)
            won = False
            if 'won' in content and all_doom_match:
                doom_str = all_doom_match.group(1).strip()
                first_faction = None
                for pair in doom_str.split():
                    if '=' in pair:
                        first_faction = pair.split('=')[0]
                        break

                if first_faction:
                    # Check if learning faction won (appears in the "won" line)
                    # Format: "<span class='XX'>Faction Name</span> won" or "Faction1, Faction2 won"
                    won_line_match = re.search(r'(.+) won</div>', content)
                    if won_line_match:
                        won_text = won_line_match.group(1)
                        # Map faction codes to names
                        faction_names = {
                            'BG': 'Black Goat', 'YS': 'Yellow Sign',
                            'CC': 'Crawling Chaos', 'GC': 'Great Cthulhu'
                        }
                        if first_faction in faction_names:
                            if faction_names[first_faction] in won_text:
                                won = True

            game_type = "Arena" if is_arena else "Self-play"
            key = (run_str, iter_num, game_type)

            if key not in iter_stats:
                iter_stats[key] = {"dooms": [], "scores": [], "wins": 0, "total": 0}

            iter_stats[key]["dooms"].append(doom)
            iter_stats[key]["scores"].append(score)
            iter_stats[key]["total"] += 1
            if won:
                iter_stats[key]["wins"] += 1

        except Exception as e:
            pass

    # Convert to list and sort
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

def get_performance_history():
    """Get performance history from canonical store."""
    games = get_games_from_canonical_store()

    # Aggregate by (run, iter, type)
    iter_stats = {}
    for game in games:
        run = game["run"]
        iter_num = game["iter"]
        game_type = "Arena" if game["type"] == "arena" else "Self-play"
        doom = game["doom"]
        score = game["score"]
        is_win = game["won"]

        key = (run, iter_num, game_type)
        if key not in iter_stats:
            iter_stats[key] = {"dooms": [], "scores": [], "wins": 0, "total": 0}

        iter_stats[key]["dooms"].append(doom)
        iter_stats[key]["scores"].append(score)
        iter_stats[key]["total"] += 1
        if is_win:
            iter_stats[key]["wins"] += 1

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

def get_weights():
    """Get current reward weights."""
    # Default weights from Shaping.scala
    defaults = {
        "spellbooks": {"weight": 8.0, "cap": 6, "desc": "Doom-equiv per spellbook earned"},
        "doomEarned": {"weight": 1.0, "cap": None, "desc": "1:1 with actual doom points"},
        "elderSigns": {"weight": 1.66, "cap": None, "desc": "Doom-equiv per elder sign earned"},
        "ritualValue": {"weight": 1.0, "cap": None, "desc": "Doom from rituals performed"},
        "ownGOO": {"weight": 2.0, "cap": 1, "desc": "Bonus for awakening own GOO (scaled by AP)"},
        "b:goodRitual": {"weight": 0.5, "cap": 2, "desc": "Bonus per 'good' ritual (cost <= gain)"},
        "gateTaken": {"weight": 0.4, "cap": 5, "desc": "Bonus per gate taken from enemy"},
        "gateDefended": {"weight": 0.33, "cap": 4, "desc": "Bonus per gate defended from attack"},
        "endAP1Gates": {"weight": 1.0, "cap": 2, "desc": "Bonus per gate at end of AP1"},
        "preDoomPower": {"weight": 1.0, "cap": 1, "desc": "Bonus if avg pre-doom power >= 10"},
        "unitsOnMap": {"weight": 0.8, "cap": 1, "desc": "Bonus for having units on map (fraction)"},
        "b:captureEnemy": {"weight": 0.4, "cap": None, "desc": "Bonus per cost-pt of enemies captured"},
        "p:unitLoss": {"weight": -0.1, "cap": None, "desc": "Penalty per cost-pt of units lost"},
        "p:lostGate": {"weight": -0.6, "cap": 6, "desc": "Penalty per gate lost to enemy"},
        "p:abandon": {"weight": -0.33, "cap": 6, "desc": "Penalty per gate abandoned"},
        "p:capture": {"weight": -0.4, "cap": None, "desc": "Penalty per cost-pt captured"},
        "p:gooLost": {"weight": -0.2, "cap": None, "desc": "Extra penalty per own GOO killed"},
        "p:cultistUnprot": {"weight": -0.2, "cap": None, "desc": "Penalty per turn unprotected cultist"},
        "r:killEnemy": {"weight": 0.8, "cap": None, "desc": "0.8 x enemy cost - 1 per combat"},
        "r:powerBlock": {"weight": 0.05, "cap": 20, "desc": "Bonus per power income gained"},
        "r:apPowerOrder": {"weight": 0.8, "cap": None, "desc": "+/-0.8 per AP based on power order"},
        "r:enemyGooKill": {"weight": 0.2, "cap": None, "desc": "Bonus per enemy GOO killed"},
        "placementBonus": {"weight": 0.1, "cap": None, "desc": "+0.1 per place above last (after ceiling)"},
    }

    # Load overrides if exist
    if WEIGHTS_FILE.exists():
        try:
            overrides = json.loads(WEIGHTS_FILE.read_text())
            for k, v in overrides.items():
                if k in defaults:
                    defaults[k]["weight"] = v
        except:
            pass

    return defaults

def save_weights(weights):
    """Save weight overrides."""
    WEIGHTS_FILE.write_text(json.dumps(weights, indent=2))

def generate_replay(trace_path, output_path):
    """Generate a replay HTML from a trace file."""
    env = os.environ.copy()
    env["CW_REPLAY_IMAGE_DIR"] = str(IMAGE_DIR)
    result = subprocess.run(
        ["python3", str(REPLAY_TOOL), trace_path, output_path],
        capture_output=True, text=True, env=env
    )
    return result.returncode == 0, result.stdout + result.stderr


class DashboardHandler(SimpleHTTPRequestHandler):
    """HTTP request handler for the dashboard."""

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path

        if path == "/" or path == "/index.html":
            self.send_response(200)
            self.send_header("Content-type", "text/html")
            self.end_headers()
            self.wfile.write(get_html().encode())
        elif path == "/api/status":
            self.send_json(get_current_run_status())
        elif path == "/api/checkpoints":
            self.send_json(get_saved_checkpoints())
        elif path == "/api/games":
            qs = parse_qs(parsed.query)
            iter_num = qs.get("iter", [None])[0]
            if iter_num:
                iter_num = int(iter_num)

            # Load from canonical store
            games = get_games_from_canonical_store(iter_num=iter_num)

            # Convert to frontend format
            formatted_games = []
            for g in games:
                formatted_games.append({
                    "filename": g["filename"],
                    "run": g["run"],
                    "iter": g["iter"],
                    "is_arena": g["type"] == "arena",
                    "doom": g["doom"],
                    "score": g["score"],
                    "won": g["won"],
                    "timestamp": g["timestamp"],
                    "game_num": g.get("game_num", 0)
                })

            # Sort by run and iter descending
            formatted_games.sort(key=lambda g: (g["run"], g["iter"]), reverse=True)
            self.send_json(formatted_games)
        elif path == "/api/weights":
            self.send_json(get_weights())
        elif path == "/api/iters":
            # Get list of iterations with traces (both arena and selfplay)
            iters = set()
            for traces_dir in TRACES_DIRS:
                if not traces_dir.exists():
                    continue
                # Arena traces
                for trace in traces_dir.glob("arena-*iter*.txt"):
                    match = re.search(r'iter(\d+)', trace.name)
                    if match:
                        iters.add(int(match.group(1)))
                # Selfplay traces
                for trace in traces_dir.glob("selfplay-*.txt"):
                    # Match both "iter<N>" and "R<run>it<iter>" formats
                    match = re.search(r'(?:iter|it)(\d+)', trace.name)
                    if match:
                        iters.add(int(match.group(1)))
            self.send_json(sorted(iters, reverse=True))
        elif path == "/api/progress":
            self.send_json(get_progress_data())
        elif path == "/api/performance-history":
            self.send_json(get_performance_history())
        elif path.startswith("/replays/"):
            # Serve replay files
            replay_path = TRACES_DIR / path[9:]
            if replay_path.exists():
                self.send_response(200)
                self.send_header("Content-type", "text/html")
                self.end_headers()
                self.wfile.write(replay_path.read_bytes())
            else:
                self.send_error(404)
        elif path.startswith("/images/"):
            # Serve faction glyph images
            img_path = IMAGE_DIR / path[8:]
            if img_path.exists():
                self.send_response(200)
                self.send_header("Content-type", "image/webp")
                self.end_headers()
                self.wfile.write(img_path.read_bytes())
            else:
                self.send_error(404)
        else:
            super().do_GET()

    def do_POST(self):
        parsed = urlparse(self.path)
        path = parsed.path

        content_len = int(self.headers.get('Content-Length', 0))
        body = self.rfile.read(content_len).decode() if content_len > 0 else "{}"

        try:
            data = json.loads(body) if body else {}
        except:
            data = {}

        if path == "/api/weights":
            save_weights(data)
            self.send_json({"success": True})
        elif path == "/api/replay":
            trace_file = data.get("trace")
            if trace_file:
                output = TRACES_DIR / f"replay-{Path(trace_file).stem}.html"
                success, msg = generate_replay(trace_file, str(output))
                self.send_json({"success": success, "message": msg, "path": str(output)})
            else:
                self.send_json({"success": False, "message": "No trace file specified"})
        elif path == "/api/stop":
            # Stop current run
            try:
                subprocess.run(["pkill", "-f", "PolicyRun"], capture_output=True)
                self.send_json({"success": True})
            except Exception as e:
                self.send_json({"success": False, "message": str(e)})
        elif path == "/api/save_checkpoint":
            name = data.get("name", f"save_{int(time.time())}")
            try:
                save_dir = CHECKPOINTS / name
                save_dir.mkdir(exist_ok=True)
                for f in ["best.meta", "best.policy", "best.value", "current.meta", "current.policy", "current.value"]:
                    src = CHECKPOINTS / f
                    if src.exists():
                        (save_dir / f).write_bytes(src.read_bytes())
                self.send_json({"success": True, "path": str(save_dir)})
            except Exception as e:
                self.send_json({"success": False, "message": str(e)})
        else:
            self.send_error(404)

    def send_json(self, data):
        self.send_response(200)
        self.send_header("Content-type", "application/json")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(json.dumps(data).encode())

    def log_message(self, format, *args):
        pass  # Suppress logging


def get_html():
    """Return the dashboard HTML."""
    return '''<!DOCTYPE html>
<html>
<head>
    <title>Brain Training Dashboard</title>
    <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #1a1a2e; color: #eee; }
        .tabs { display: flex; background: #16213e; border-bottom: 2px solid #0f3460; }
        .tab { padding: 15px 25px; cursor: pointer; border: none; background: none; color: #888; font-size: 14px; }
        .tab:hover { color: #fff; background: #1a1a40; }
        .tab.active { color: #fff; background: #0f3460; border-bottom: 2px solid #e94560; margin-bottom: -2px; }
        .content { padding: 20px; display: none; }
        .content.active { display: block; }
        .card { background: #16213e; border-radius: 8px; padding: 20px; margin-bottom: 15px; }
        .card h3 { color: #e94560; margin-bottom: 15px; }
        .stat { display: inline-block; margin-right: 30px; margin-bottom: 10px; }
        .stat-label { color: #888; font-size: 12px; }
        .stat-value { font-size: 24px; font-weight: bold; }
        .stat-value.good { color: #4ade80; }
        .stat-value.bad { color: #f87171; }
        button { background: #e94560; color: #fff; border: none; padding: 10px 20px; border-radius: 5px; cursor: pointer; margin-right: 10px; }
        button:hover { background: #ff6b6b; }
        button.secondary { background: #0f3460; }
        button.secondary:hover { background: #1a4a7a; }
        button.danger { background: #dc2626; }
        input, select { background: #1a1a2e; border: 1px solid #0f3460; color: #fff; padding: 8px 12px; border-radius: 4px; margin-right: 10px; }
        table { width: 100%; border-collapse: collapse; }
        th, td { padding: 10px; text-align: left; border-bottom: 1px solid #0f3460; }
        th { color: #e94560; }
        tr:hover { background: #1a1a40; }
        .weight-row { display: flex; align-items: center; padding: 10px 0; border-bottom: 1px solid #0f3460; }
        .weight-name { flex: 1; }
        .weight-desc { flex: 2; color: #888; font-size: 13px; }
        .weight-input { width: 80px; text-align: right; }
        .breakdown { font-size: 12px; color: #888; cursor: pointer; }
        .breakdown:hover { color: #e94560; }
        .modal { display: none; position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.8); z-index: 100; }
        .modal.show { display: flex; align-items: center; justify-content: center; }
        .modal-content { background: #16213e; padding: 30px; border-radius: 10px; max-width: 500px; }
        .status-indicator { width: 12px; height: 12px; border-radius: 50%; display: inline-block; margin-right: 8px; }
        .status-indicator.running { background: #4ade80; animation: pulse 1s infinite; }
        .status-indicator.stopped { background: #f87171; }
        @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }
        @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
        .player-row { display: flex; align-items: center; margin-bottom: 15px; padding: 10px; background: #1a1a2e; border-radius: 5px; }
        .player-label { width: 80px; color: #e94560; font-weight: bold; }
        .player-type { width: 120px; margin-right: 15px; }
        .faction-selector { display: flex; gap: 8px; flex-wrap: wrap; }
        .faction-glyph { width: 48px; height: 48px; cursor: pointer; border: 3px solid transparent; border-radius: 5px; transition: all 0.2s; opacity: 0.5; }
        .faction-glyph:hover { opacity: 0.8; border-color: #555; }
        .faction-glyph.selected { opacity: 1; border-color: #e94560; box-shadow: 0 0 10px #e94560; }
    </style>
</head>
<body>
    <div class="tabs">
        <button class="tab active" onclick="showTab('status')">Run Status</button>
        <button class="tab" onclick="showTab('control')">Run Control</button>
        <button class="tab" onclick="showTab('games')">Game Browser</button>
        <button class="tab" onclick="showTab('weights')">Reward Weights</button>
        <button class="tab" onclick="showTab('progress')">Progress Charts</button>
    </div>

    <div id="status" class="content active">
        <div class="card">
            <h3><span id="status-indicator" class="status-indicator stopped"></span>Current Run</h3>
            <div class="stat">
                <div class="stat-label">Run Tag</div>
                <div class="stat-value" id="run-tag">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">Progress</div>
                <div class="stat-value" id="progress-status">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">Elapsed Time</div>
                <div class="stat-value" id="elapsed">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">ETA</div>
                <div class="stat-value" id="eta">-</div>
            </div>
        </div>
        <div class="card">
            <h3>Performance</h3>
            <div class="stat">
                <div class="stat-label">Arena Win Rate</div>
                <div class="stat-value" id="win-rate">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">Arena Wins</div>
                <div class="stat-value" id="arena-wins">-</div>
            </div>
            <div class="stat">
                <div class="stat-label" id="avg-doom-label">Avg Doom</div>
                <div class="stat-value" id="avg-doom">-</div>
            </div>
            <div class="stat">
                <div class="stat-label" id="best-score-label">Best Score</div>
                <div class="stat-value" id="best-score">-</div>
            </div>
        </div>
        <div class="card">
            <h3>Performance History</h3>
            <p style="color:#888;font-size:13px;margin-bottom:10px;">Recent iterations sorted by run and iter (latest first)</p>
            <div style="overflow-x:auto;">
                <table id="performance-history-table" style="width:100%;border-collapse:collapse;">
                    <thead>
                        <tr style="background:#16213e;">
                            <th style="padding:8px;text-align:left;border-bottom:2px solid #0f3460;">Run</th>
                            <th style="padding:8px;text-align:left;border-bottom:2px solid #0f3460;">Iter</th>
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Avg Doom</th>
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Avg Score</th>
                            <th style="padding:8px;text-align:left;border-bottom:2px solid #0f3460;">Type</th>
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Wins</th>
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Total</th>
                        </tr>
                    </thead>
                    <tbody id="performance-history-body">
                        <tr><td colspan="7" style="padding:20px;text-align:center;color:#666;">Loading...</td></tr>
                    </tbody>
                </table>
            </div>
            <div style="margin-top:10px;text-align:center;">
                <button onclick="changePerformancePage(-1)" style="padding:8px 15px;margin:0 5px;background:#0f3460;color:#fff;border:none;border-radius:4px;cursor:pointer;">← Prev</button>
                <span id="performance-page-info" style="color:#888;margin:0 15px;">Page 1</span>
                <button onclick="changePerformancePage(1)" style="padding:8px 15px;margin:0 5px;background:#0f3460;color:#fff;border:none;border-radius:4px;cursor:pointer;">Next →</button>
            </div>
        </div>
    </div>

    <div id="control" class="content">
        <div class="card">
            <h3>Run Control</h3>
            <button onclick="stopRun()" class="danger">Stop Current Run</button>
            <button onclick="showSaveModal()" class="secondary">Save Checkpoint</button>
        </div>
        <div class="card">
            <h3>Saved Checkpoints</h3>
            <table id="checkpoints-table">
                <thead><tr><th>Name</th><th>Score</th><th>Tag</th><th>Actions</th></tr></thead>
                <tbody></tbody>
            </table>
        </div>
        <div class="card">
            <h3>Start New Run</h3>
            <p style="color:#888;margin-bottom:15px;">Configure and start a new training run.</p>

            <div style="margin-bottom:20px;">
                <label style="display:block;margin-bottom:8px;color:#e94560;font-weight:bold;">Player Count:</label>
                <select id="player-count" onchange="updatePlayerSetup()" style="width:150px;">
                    <option value="3">3 Players</option>
                    <option value="4" selected>4 Players</option>
                    <option value="5">5 Players</option>
                </select>
            </div>

            <div id="player-setup" style="margin-bottom:20px;">
                <!-- Populated by updatePlayerSetup() -->
            </div>

            <div style="margin-bottom:15px;border-top:1px solid #0f3460;padding-top:15px;">
                <label style="margin-right:20px;"><input type="checkbox" id="neutral-monsters"> Neutral Monsters</label>
                <label style="margin-right:20px;"><input type="checkbox" id="terrors"> Terrors</label>
                <label><input type="checkbox" id="igoos"> iGOOs</label>
            </div>

            <div style="margin-bottom:15px;">
                <label>Iterations: <input type="number" id="new-iters" value="100" style="width:100px;"></label>
                <label style="margin-left:15px;">Games/Iter: <input type="number" id="new-games" value="20" style="width:80px;"></label>
                <label style="margin-left:15px;">Arena Every: <input type="number" id="arena-every" value="2" style="width:60px;"> iters</label>
            </div>
            <button onclick="startRun()">Start Run</button>
            <p style="color:#666;font-size:12px;margin-top:10px;">Select brain/bot per seat + factions. Multiple faction selections = random rotation.</p>
        </div>
    </div>

    <div id="games" class="content">
        <div class="card">
            <h3>Game Browser</h3>
            <p style="color:#888;font-size:13px;margin-bottom:10px;">Note: Scores shown are faction averages for the iteration (multiple games share same score)</p>
            <div style="margin-bottom:15px;">
                <label>Run:
                    <select id="run-select" onchange="loadGames()">
                        <option value="">All</option>
                    </select>
                </label>
                <label style="margin-left:15px;">Iteration:
                    <select id="iter-select" onchange="loadGames()">
                        <option value="">All</option>
                    </select>
                </label>
            </div>
            <div style="margin-bottom:15px;display:flex;justify-content:space-between;align-items:center;">
                <div id="games-count" style="color:#888;">Loading...</div>
                <div style="display:flex;gap:10px;align-items:center;">
                    <button class="secondary" onclick="prevPage()" id="prev-btn">&larr; Prev</button>
                    <span id="page-info" style="padding:0 15px;">Page 1</span>
                    <button class="secondary" onclick="nextPage()" id="next-btn">Next &rarr;</button>
                </div>
            </div>
            <div id="games-loading" style="display:none;text-align:center;padding:40px;font-size:18px;color:#e94560;">
                <div style="display:inline-block;width:40px;height:40px;border:4px solid #e94560;border-top-color:transparent;border-radius:50%;animation:spin 1s linear infinite;"></div>
                <div style="margin-top:15px;">Loading games...</div>
            </div>
            <table id="games-table">
                <thead><tr><th>Run</th><th>Iter</th><th>Type</th><th>Faction</th><th>Saved Game</th><th>Doom</th><th>Score</th><th>Winner</th><th>Place</th><th>Actions</th></tr></thead>
                <tbody></tbody>
            </table>
        </div>
        <div class="card" id="breakdown-detail" style="display:none;">
            <h3>Score Breakdown</h3>
            <div id="breakdown-content"></div>
        </div>
    </div>

    <div id="weights" class="content">
        <div class="card">
            <h3>Reward Weights</h3>
            <p style="color:#888;margin-bottom:15px;">Edit weights below. Changes apply to next run after saving.</p>
            <div id="weights-list"></div>
            <button onclick="saveWeights()" style="margin-top:20px;">Save Weights</button>
            <button onclick="loadWeights()" class="secondary" style="margin-top:20px;">Reset to Current</button>
        </div>
    </div>

    <div id="progress" class="content">
        <div class="card">
            <h3>Doom per Game</h3>
            <div style="margin-bottom:15px;display:flex;gap:15px;align-items:center;flex-wrap:wrap;">
                <div>
                    <label style="display:block;margin-bottom:5px;">Runs:</label>
                    <select id="doom-run-filter" multiple style="width:150px;height:80px;">
                    </select>
                </div>
                <div>
                    <label style="display:block;margin-bottom:5px;">Iters:</label>
                    <select id="doom-iter-filter" multiple style="width:150px;height:80px;">
                    </select>
                </div>
                <div>
                    <label style="display:block;margin-bottom:5px;">Type:</label>
                    <div>
                        <button class="secondary" onclick="setDoomTypeFilter('both')" id="doom-type-both">Both</button>
                        <button class="secondary" onclick="setDoomTypeFilter('selfplay')" id="doom-type-selfplay">Self-play</button>
                        <button class="secondary" onclick="setDoomTypeFilter('arena')" id="doom-type-arena">Arena</button>
                    </div>
                </div>
            </div>
            <canvas id="doom-chart" width="1000" height="400" style="width:1000px;height:400px;background:#1a1a2e;display:block;"></canvas>
        </div>
        <div class="card">
            <h3>Score per Game</h3>
            <div style="margin-bottom:15px;display:flex;gap:15px;align-items:center;flex-wrap:wrap;">
                <div>
                    <label style="display:block;margin-bottom:5px;">Runs:</label>
                    <select id="score-run-filter" multiple style="width:150px;height:80px;">
                    </select>
                </div>
                <div>
                    <label style="display:block;margin-bottom:5px;">Iters:</label>
                    <select id="score-iter-filter" multiple style="width:150px;height:80px;">
                    </select>
                </div>
                <div>
                    <label style="display:block;margin-bottom:5px;">Type:</label>
                    <div>
                        <button class="secondary" onclick="setScoreTypeFilter('both')" id="score-type-both">Both</button>
                        <button class="secondary" onclick="setScoreTypeFilter('selfplay')" id="score-type-selfplay">Self-play</button>
                        <button class="secondary" onclick="setScoreTypeFilter('arena')" id="score-type-arena">Arena</button>
                    </div>
                </div>
            </div>
            <canvas id="score-chart" width="1000" height="400" style="width:1000px;height:400px;background:#1a1a2e;display:block;"></canvas>
        </div>
        <div class="card">
            <h3>Wins per Iteration</h3>
            <div style="margin-bottom:15px;display:flex;gap:15px;align-items:center;flex-wrap:wrap;">
                <div>
                    <label style="display:block;margin-bottom:5px;">Runs:</label>
                    <select id="wins-run-filter" multiple style="width:150px;height:80px;">
                    </select>
                </div>
                <div>
                    <label style="display:block;margin-bottom:5px;">Type:</label>
                    <div>
                        <button class="secondary" onclick="setWinsTypeFilter('both')" id="wins-type-both">Both</button>
                        <button class="secondary" onclick="setWinsTypeFilter('selfplay')" id="wins-type-selfplay">Self-play</button>
                        <button class="secondary" onclick="setWinsTypeFilter('arena')" id="wins-type-arena">Arena</button>
                    </div>
                </div>
            </div>
            <canvas id="wins-chart" width="1000" height="400" style="width:1000px;height:400px;background:#1a1a2e;display:block;"></canvas>
        </div>
    </div>

    <div id="save-modal" class="modal">
        <div class="modal-content">
            <h3 style="margin-bottom:15px;">Save Checkpoint</h3>
            <input type="text" id="save-name" placeholder="Checkpoint name" style="width:100%;margin-bottom:15px;">
            <button onclick="saveCheckpoint()">Save</button>
            <button onclick="closeSaveModal()" class="secondary">Cancel</button>
        </div>
    </div>

    <script>
        let currentWeights = {};

        function showTab(name) {
            console.log('showTab called with:', name);

            // Remove active from all tabs and content
            document.querySelectorAll('.tab').forEach(t => t.classList.remove('active'));
            document.querySelectorAll('.content').forEach(c => c.classList.remove('active'));

            // Find and activate the correct tab button by iterating
            const expectedOnclick = "showTab('" + name + "')";
            console.log('Looking for onclick:', expectedOnclick);

            let foundTab = false;
            document.querySelectorAll('.tab').forEach(t => {
                const onclick = t.getAttribute('onclick');
                console.log('Checking tab onclick:', onclick);
                if (onclick === expectedOnclick) {
                    console.log('Found matching tab!');
                    t.classList.add('active');
                    foundTab = true;
                }
            });

            if (!foundTab) {
                console.error('Tab button not found for:', name);
            }

            // Activate content
            const contentEl = document.getElementById(name);
            if (contentEl) {
                console.log('Activating content element:', name);
                contentEl.classList.add('active');
            } else {
                console.error('Content element not found for:', name);
            }

            // Load data for specific tabs
            if (name === 'games') {
                console.log('Loading games...');
                loadIters();
            }
            if (name === 'weights') {
                console.log('Loading weights...');
                loadWeights();
            }
            if (name === 'control') {
                console.log('Loading checkpoints...');
                loadCheckpoints();
            }
            if (name === 'progress') {
                console.log('Loading progress...');
                loadProgress();
            }
        }

        async function updateStatus() {
            try {
                const res = await fetch('/api/status');
                const data = await res.json();

                const indicator = document.getElementById('status-indicator');
                indicator.className = 'status-indicator ' + (data.running ? 'running' : 'stopped');

                document.getElementById('run-tag').textContent = data.run_tag || '-';

                // Progress: "iter 27 (game 18/20) / 100000"
                const progressText = data.current_iter
                    ? `iter ${data.current_iter} (game ${data.current_game}/${data.total_games}) / ${data.target_iters}`
                    : '-';
                document.getElementById('progress-status').textContent = progressText;

                document.getElementById('elapsed').textContent = data.elapsed_human || '-';

                // ETA with type label (Self-play or Arena)
                const etaType = data.current_iter && data.current_iter % 2 === 0 ? ' (Arena)' : ' (Self-play)';
                document.getElementById('eta').textContent = data.eta_human ? data.eta_human + etaType : '-';

                document.getElementById('win-rate').textContent = data.arena_win_rate ? (data.arena_win_rate * 100).toFixed(1) + '%' : '-';
                document.getElementById('arena-wins').textContent = data.arena_wins !== undefined ? `${data.arena_wins}/${data.arena_total}` : '-';

                // Avg Doom - update LABEL with run/iter
                const doomLabel = data.run_tag && data.current_iter ? `Avg Doom (${data.run_tag} I${data.current_iter})` : 'Avg Doom';
                document.getElementById('avg-doom-label').textContent = doomLabel;
                document.getElementById('avg-doom').textContent = data.avg_doom ? data.avg_doom.toFixed(1) : '-';

                // Best Score - update LABEL with run/iter
                const scoreLabel = data.best_run_iter ? `Best Score (${data.best_run_iter})` : 'Best Score';
                document.getElementById('best-score-label').textContent = scoreLabel;
                document.getElementById('best-score').textContent = data.best_score ? data.best_score.toFixed(2) : '-';

                // Load performance history
                await loadPerformanceHistory();
            } catch (e) {
                console.error('Status update failed:', e);
            }
        }

        let allPerformanceData = [];
        let performancePage = 1;
        const performancePerPage = 20;

        async function loadPerformanceHistory() {
            try {
                const res = await fetch('/api/performance-history');
                allPerformanceData = await res.json();
                performancePage = 1;
                renderPerformanceHistory();
            } catch (e) {
                console.error('Performance history load failed:', e);
            }
        }

        function renderPerformanceHistory() {
            const tbody = document.getElementById('performance-history-body');

            if (!allPerformanceData || allPerformanceData.length === 0) {
                tbody.innerHTML = '<tr><td colspan="7" style="padding:20px;text-align:center;color:#666;">No data</td></tr>';
                document.getElementById('performance-page-info').textContent = 'No data';
                return;
            }

            const totalPages = Math.ceil(allPerformanceData.length / performancePerPage);
            const start = (performancePage - 1) * performancePerPage;
            const end = start + performancePerPage;
            const pageData = allPerformanceData.slice(start, end);

            const rows = pageData.map(row => {
                const winRate = row.total > 0 ? ((row.wins / row.total) * 100).toFixed(0) + '%' : '-';
                return `
                    <tr style="border-bottom:1px solid #16213e;">
                        <td style="padding:8px;">${row.run}</td>
                        <td style="padding:8px;">I${String(row.iter).padStart(2, '0')}</td>
                        <td style="padding:8px;text-align:right;">${row.avg_doom.toFixed(1)}</td>
                        <td style="padding:8px;text-align:right;">${row.avg_score.toFixed(3)}</td>
                        <td style="padding:8px;">${row.type}</td>
                        <td style="padding:8px;text-align:right;">${row.wins}/${row.total} (${winRate})</td>
                        <td style="padding:8px;text-align:right;">${row.total}</td>
                    </tr>
                `;
            }).join('');

            tbody.innerHTML = rows;
            document.getElementById('performance-page-info').textContent = `Page ${performancePage} of ${totalPages}`;
        }

        function changePerformancePage(delta) {
            const totalPages = Math.ceil(allPerformanceData.length / performancePerPage);
            performancePage = Math.max(1, Math.min(totalPages, performancePage + delta));
            renderPerformanceHistory();
        }

        async function loadCheckpoints() {
            const res = await fetch('/api/checkpoints');
            const data = await res.json();
            const tbody = document.querySelector('#checkpoints-table tbody');
            tbody.innerHTML = data.map(c => `
                <tr>
                    <td>${c.name}</td>
                    <td>${c.score.toFixed(2)}</td>
                    <td>${c.tag}</td>
                    <td><button class="secondary" onclick="loadCheckpoint('${c.path}')">Load</button></td>
                </tr>
            `).join('');
        }

        let allGamesData = [];
        let currentPage = 1;
        const gamesPerPage = 50;

        async function loadIters() {
            const res = await fetch('/api/games');
            allGamesData = await res.json();

            // Get unique runs and iters
            const runs = [...new Set(allGamesData.map(g => g.run))].sort();
            const iters = [...new Set(allGamesData.map(g => g.iter))].sort((a,b) => b-a);

            const runSelect = document.getElementById('run-select');
            runSelect.innerHTML = '<option value="">All</option>' +
                runs.map(r => `<option value="${r}">${r}</option>`).join('');

            const iterSelect = document.getElementById('iter-select');
            iterSelect.innerHTML = '<option value="">All</option>' +
                iters.map(i => `<option value="${i}">${i}</option>`).join('');

            currentPage = 1;
            loadGames();
        }

        function prevPage() {
            if (currentPage > 1) {
                currentPage--;
                loadGames();
            }
        }

        function nextPage() {
            const run = document.getElementById('run-select').value;
            const iter = document.getElementById('iter-select').value;
            let filtered = allGamesData;
            if (run) filtered = filtered.filter(g => g.run === run);
            if (iter) filtered = filtered.filter(g => g.iter === parseInt(iter));
            const totalPages = Math.ceil(filtered.length / gamesPerPage);
            if (currentPage < totalPages) {
                currentPage++;
                loadGames();
            }
        }

        async function loadGames() {
            try {
                const run = document.getElementById('run-select').value;
                const iter = document.getElementById('iter-select').value;

                let games = allGamesData;

                // Filter by run and iter
                if (run) games = games.filter(g => g.run === run);
                if (iter) games = games.filter(g => g.iter === parseInt(iter));

                const totalGames = games.length;
                const totalPages = Math.ceil(totalGames / gamesPerPage);
                const startIdx = (currentPage - 1) * gamesPerPage;
                const endIdx = startIdx + gamesPerPage;
                const pageGames = games.slice(startIdx, endIdx);

                // Update count and page info
                document.getElementById('games-count').textContent =
                    `Showing ${startIdx + 1}-${Math.min(endIdx, totalGames)} of ${totalGames} games`;
                document.getElementById('page-info').textContent = `Page ${currentPage} of ${totalPages}`;
                document.getElementById('prev-btn').disabled = currentPage === 1;
                document.getElementById('next-btn').disabled = currentPage >= totalPages;

                // Show loading, hide table
                document.getElementById('games-loading').style.display = 'block';
                document.getElementById('games-table').style.display = 'none';

                // Small delay to show loading indicator
                await new Promise(resolve => setTimeout(resolve, 10));

                const tbody = document.querySelector('#games-table tbody');
                tbody.innerHTML = pageGames.map(g => {
                const typeLabel = g.type === 'first' ? 'Lowest doom' : (g.type === 'best' ? 'Highest doom' : g.type);
                const breakdownId = 'breakdown-' + g.run + '-' + g.iter + '-' + g.faction.replace(/\\s+/g, '-');
                // Arena: brain won = "Learning", brain lost = "Bot"
                // Self-play: brain won = "Learning", brain lost = "Champion"
                const winner = g.is_win ? 'Learning' : (g.is_arena ? 'Bot' : 'Champion');
                const placement = g.placement || '?';
                return `
                <tr>
                    <td>${g.run}</td>
                    <td>${g.iter}</td>
                    <td>${g.is_arena ? 'Arena' : 'Self-play'}</td>
                    <td>${g.faction}</td>
                    <td>${typeLabel}</td>
                    <td>${g.doom}</td>
                    <td><span class="breakdown" onclick="toggleBreakdown('${breakdownId}')">${g.score !== null ? g.score : '-'}</span></td>
                    <td>${winner}</td>
                    <td>${placement}</td>
                    <td><button class="secondary" onclick="generateReplay('${g.file}')">View</button></td>
                </tr>
                <tr id="${breakdownId}" style="display:none;">
                    <td colspan="10" style="background:#0f3460;padding:15px;">
                        <div style="margin-bottom:15px;padding:10px;background:#16213e;border:2px solid #e94560;">
                            <div><strong>Brain's final 0-1 score:</strong> <span style="font-size:18px;color:#4ade80;">${g.score !== null ? g.score.toFixed(3) : 'N/A'}</span></div>
                            <div style="margin-top:8px;font-size:11px;color:#888;">Components shown below are from action log only. Full score includes: preDoomPower, unitsOnMap, powerUse, sbUse, and other state-based rewards not in action traces.</div>
                        </div>
                        <div style="display:grid;grid-template-columns:repeat(2,1fr);gap:10px;font-size:13px;">
                            ${g.breakdown.split(/\s+/).filter(x => x.includes('=')).map(item => {
                                const [name, count] = item.split('=');
                                return `<div><span style="color:#888;">${name}:</span> <span style="font-weight:bold;">${count}</span></div>`;
                            }).join('')}
                        </div>
                    </td>
                </tr>
                `;
            }).join('');

                // Hide loading, show table
                document.getElementById('games-loading').style.display = 'none';
                document.getElementById('games-table').style.display = 'table';
            } catch (e) {
                console.error('Failed to load games:', e);
                document.getElementById('games-loading').style.display = 'none';
                document.getElementById('games-table').style.display = 'table';
                const tbody = document.querySelector('#games-table tbody');
                tbody.innerHTML = '<tr><td colspan="10" style="text-align:center;padding:20px;color:#e94560;">Error loading games. Check console.</td></tr>';
            }
        }

        function toggleBreakdown(id) {
            const row = document.getElementById(id);
            if (row.style.display === 'none') {
                row.style.display = '';
            } else {
                row.style.display = 'none';
            }
        }

        async function loadWeights() {
            const res = await fetch('/api/weights');
            currentWeights = await res.json();
            const container = document.getElementById('weights-list');
            container.innerHTML = Object.entries(currentWeights).map(([name, w]) => `
                <div class="weight-row">
                    <div class="weight-name"><strong>${name}</strong></div>
                    <div class="weight-desc">${w.desc}</div>
                    <input type="number" step="0.01" class="weight-input" id="weight-${name}" value="${w.weight}" ${w.cap ? `title="Cap: ${w.cap}"` : ''}>
                </div>
            `).join('');
        }

        async function saveWeights() {
            const weights = {};
            Object.keys(currentWeights).forEach(name => {
                const input = document.getElementById(`weight-${name}`);
                if (input) weights[name] = parseFloat(input.value);
            });
            await fetch('/api/weights', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify(weights)
            });
            alert('Weights saved! Will apply to next run.');
        }

        async function generateReplay(traceFile) {
            const res = await fetch('/api/replay', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({trace: traceFile})
            });
            const data = await res.json();
            if (data.success) {
                window.open('/replays/' + data.path.split('/').pop(), '_blank');
            } else {
                alert('Failed to generate replay: ' + data.message);
            }
        }

        async function stopRun() {
            if (confirm('Stop the current training run?')) {
                await fetch('/api/stop', {method: 'POST'});
                updateStatus();
            }
        }

        function showSaveModal() {
            document.getElementById('save-modal').classList.add('show');
            document.getElementById('save-name').value = 'save_' + new Date().toISOString().slice(0,10);
        }

        function closeSaveModal() {
            document.getElementById('save-modal').classList.remove('show');
        }

        async function saveCheckpoint() {
            const name = document.getElementById('save-name').value;
            const res = await fetch('/api/save_checkpoint', {
                method: 'POST',
                headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({name})
            });
            const data = await res.json();
            closeSaveModal();
            if (data.success) {
                alert('Checkpoint saved!');
                loadCheckpoints();
            } else {
                alert('Failed: ' + data.message);
            }
        }

        const FACTIONS = [
            {code: 'gc', name: 'Great Cthulhu', img: '/images/gc-glyph.webp', alt: false},
            {code: 'cc', name: 'Crawling Chaos', img: '/images/cc-glyph.webp', alt: false},
            {code: 'bg', name: 'Black Goat', img: '/images/bg-glyph.webp', alt: false},
            {code: 'ys', name: 'Yellow Sign', img: '/images/ys-glyph.webp', alt: false},
            {code: 'ow', name: 'Opener of the Way', img: '/images/ow-glyph.webp', alt: false},
            {code: 'ow-alt', name: 'Opener of the Way (alt SBs)', img: '/images/ow-glyph.webp', alt: true},
            {code: 'sl', name: 'Sleeper', img: '/images/sl-glyph.webp', alt: false},
            {code: 'sl-alt', name: 'Sleeper (alt SBs)', img: '/images/sl-glyph.webp', alt: true},
            {code: 'ww', name: 'Windwalker', img: '/images/ww-glyph.webp', alt: false},
            {code: 'tt', name: 'Tcho-Tcho', img: '/images/tt-glyph.webp', alt: false},
            {code: 'an', name: 'The Ancients', img: '/images/an-glyph.webp', alt: false},
            {code: 'an-alt', name: 'The Ancients (alt SBs)', img: '/images/an-glyph.webp', alt: true},
            {code: 'ds', name: 'Daemon Sultan', img: '/images/ds-glyph.webp', alt: false},
            {code: 'ds-alt', name: 'Daemon Sultan (alt SBs)', img: '/images/ds-glyph.webp', alt: true},
            {code: 'bb', name: 'Bubastis', img: '/images/bb-glyph.webp', alt: false},
            {code: 'bb-alt', name: 'Bubastis (alt SBs)', img: '/images/bb-glyph-hb.webp', alt: true},
            {code: 'ts', name: 'Tombstalker', img: '/images/ts-glyph.webp', alt: false},
            {code: 'fb', name: 'Firstborn', img: '/images/fb-glyph.webp', alt: false},
            {code: 'dc', name: 'Defilers Court', img: '/images/dc-glyph.webp', alt: false},
            {code: 'fbe', name: 'Faceless Blight', img: '/images/fbe-glyph.webp', alt: false},
            {code: 'xss', name: 'Xyrious Storm', img: '/images/xss-glyph.webp', alt: false},
            {code: 'tb', name: 'The Burrowers Beneath', img: '/images/tb-glyph.webp', alt: false}
        ];

        function updatePlayerSetup() {
            const count = parseInt(document.getElementById('player-count').value);
            const container = document.getElementById('player-setup');
            let html = '';
            for (let i = 1; i <= count; i++) {
                html += `
                <div class="player-row">
                    <div class="player-label">Player ${i}:</div>
                    <select class="player-type" id="player-${i}-type">
                        <option value="bot" selected>Bot</option>
                        <option value="brain">Brain</option>
                    </select>
                    <div class="faction-selector" id="player-${i}-factions">
                        ${FACTIONS.map(f => `
                            <img src="${f.img}"
                                 class="faction-glyph"
                                 data-faction="${f.code}"
                                 title="${f.name}"
                                 onclick="toggleFaction(${i}, '${f.code}')">
                        `).join('')}
                    </div>
                </div>
                `;
            }
            container.innerHTML = html;
        }

        function toggleFaction(player, factionCode) {
            const glyph = document.querySelector(`#player-${player}-factions img[data-faction="${factionCode}"]`);
            glyph.classList.toggle('selected');
        }

        function startRun() {
            const playerCount = parseInt(document.getElementById('player-count').value);
            const setup = [];
            for (let i = 1; i <= playerCount; i++) {
                const type = document.getElementById(`player-${i}-type`).value;
                const factions = Array.from(document.querySelectorAll(`#player-${i}-factions .faction-glyph.selected`))
                    .map(el => el.dataset.faction);
                if (factions.length === 0) {
                    alert(`Player ${i} needs at least one faction selected`);
                    return;
                }
                setup.push({type, factions});
            }

            const iters = document.getElementById('new-iters').value;
            const games = document.getElementById('new-games').value;
            const arena = document.getElementById('arena-every').value;

            alert('Run config:\\n' + JSON.stringify({playerCount, setup, iters, games, arena}, null, 2) +
                  '\\n\\nStarting run requires manual command. Feature coming soon.');
        }

        // Initialize player setup on load
        updatePlayerSetup();

        // Progress charts
        let progressData = {selfplay: [], arena: []};
        let doomTypeFilter = 'both';
        let scoreTypeFilter = 'both';
        let winsTypeFilter = 'both';
        let doomRunsFilter = [];
        let scoreRunsFilter = [];
        let winsRunsFilter = [];
        let doomItersFilter = [];
        let scoreItersFilter = [];

        async function loadProgress() {
            try {
                const res = await fetch('/api/progress');
                progressData = await res.json();
                console.log('Progress data loaded:', progressData);

                // Populate run and iter filters
                const allData = [...progressData.selfplay, ...progressData.arena];
                const uniqueRuns = [...new Set(allData.map(d => d.run))].sort();
                const uniqueIters = [...new Set(allData.map(d => d.iter))].sort((a,b) => a-b);

                const doomRunSelect = document.getElementById('doom-run-filter');
                const scoreRunSelect = document.getElementById('score-run-filter');
                const winsRunSelect = document.getElementById('wins-run-filter');
                const doomIterSelect = document.getElementById('doom-iter-filter');
                const scoreIterSelect = document.getElementById('score-iter-filter');

                doomRunSelect.innerHTML = uniqueRuns.map(r => `<option value="${r}" selected>${r}</option>`).join('');
                scoreRunSelect.innerHTML = uniqueRuns.map(r => `<option value="${r}" selected>${r}</option>`).join('');
                winsRunSelect.innerHTML = uniqueRuns.map(r => `<option value="${r}" selected>${r}</option>`).join('');
                doomIterSelect.innerHTML = uniqueIters.map(i => `<option value="${i}" selected>${i}</option>`).join('');
                scoreIterSelect.innerHTML = uniqueIters.map(i => `<option value="${i}" selected>${i}</option>`).join('');

                // Initialize with all runs and iters selected
                doomRunsFilter = uniqueRuns;
                scoreRunsFilter = uniqueRuns;
                winsRunsFilter = uniqueRuns;
                doomItersFilter = uniqueIters;
                scoreItersFilter = uniqueIters;

                doomRunSelect.onchange = () => {
                    doomRunsFilter = Array.from(doomRunSelect.selectedOptions).map(o => o.value);
                    drawDoomChart();
                };
                scoreRunSelect.onchange = () => {
                    scoreRunsFilter = Array.from(scoreRunSelect.selectedOptions).map(o => o.value);
                    drawScoreChart();
                };
                doomIterSelect.onchange = () => {
                    doomItersFilter = Array.from(doomIterSelect.selectedOptions).map(o => parseInt(o.value));
                    drawDoomChart();
                };
                scoreIterSelect.onchange = () => {
                    scoreItersFilter = Array.from(scoreIterSelect.selectedOptions).map(o => parseInt(o.value));
                    drawScoreChart();
                };
                winsRunSelect.onchange = () => {
                    winsRunsFilter = Array.from(winsRunSelect.selectedOptions).map(o => o.value);
                    drawWinsChart();
                };

                drawDoomChart();
                drawScoreChart();
                drawWinsChart();
            } catch (e) {
                console.error('Failed to load progress:', e);
            }
        }

        function setDoomTypeFilter(filter) {
            doomTypeFilter = filter;
            document.querySelectorAll('[id^="doom-type-"]').forEach(b => b.style.background = '#0f3460');
            document.getElementById('doom-type-' + filter).style.background = '#e94560';
            drawDoomChart();
        }

        function setScoreTypeFilter(filter) {
            scoreTypeFilter = filter;
            document.querySelectorAll('[id^="score-type-"]').forEach(b => b.style.background = '#0f3460');
            document.getElementById('score-type-' + filter).style.background = '#e94560';
            drawScoreChart();
        }

        function setWinsTypeFilter(filter) {
            winsTypeFilter = filter;
            document.querySelectorAll('[id^="wins-type-"]').forEach(b => b.style.background = '#0f3460');
            document.getElementById('wins-type-' + filter).style.background = '#e94560';
            drawWinsChart();
        }

        function drawDoomChart() {
            const canvas = document.getElementById('doom-chart');
            if (!canvas) return;
            const ctx = canvas.getContext('2d');
            const w = canvas.width, h = canvas.height;
            ctx.clearRect(0, 0, w, h);

            let data = [];
            if (doomTypeFilter === 'both') data = [...progressData.selfplay, ...progressData.arena];
            else if (doomTypeFilter === 'selfplay') data = progressData.selfplay;
            else data = progressData.arena;

            // Filter by runs and iters
            data = data.filter(d => doomRunsFilter.includes(d.run) && doomItersFilter.includes(d.iter));

            if (!data.length) {
                ctx.fillStyle = '#888';
                ctx.font = '16px sans-serif';
                ctx.fillText('No data', w/2 - 30, h/2);
                return;
            }

            // Sort by run_iter_game_num to ensure proper ordering
            data.sort((a, b) => a.run_iter_game_num - b.run_iter_game_num);

            // Fixed Y-axis range
            const minY = 10;
            const maxY = 60;
            const rangeY = maxY - minY;

            // Group by iteration
            const runIters = [...new Set(data.map(d => `${d.run}_I${String(d.iter).padStart(2, '0')}`))];
            const showAverages = runIters.length > 10;

            // Map data points to evenly-spaced X positions (0, 1, 2, 3...)
            const positionedData = [];
            let xPos = 0;
            runIters.forEach((runIter, iterIdx) => {
                const [run, iterStr] = runIter.split('_I');
                const iter = parseInt(iterStr);
                const iterGames = data.filter(d => d.run === run && d.iter === iter);

                if (showAverages) {
                    // Show one point per iteration (average)
                    const avgDoom = iterGames.reduce((s, d) => s + d.doom, 0) / iterGames.length;
                    positionedData.push({
                        xPos: xPos++,
                        doom: avgDoom,
                        label: runIter,
                        run: run,
                        iter: iter,
                        games: iterGames
                    });
                } else {
                    // Show individual games
                    iterGames.forEach((game, gameIdx) => {
                        positionedData.push({
                            xPos: xPos++,
                            doom: game.doom,
                            label: game.run_iter_game,
                            run: run,
                            iter: iter,
                            games: [game]
                        });
                    });
                }
            });

            const rangeX = Math.max(1, positionedData.length - 1);

            // Axes
            ctx.strokeStyle = '#555';
            ctx.lineWidth = 2;
            ctx.beginPath();
            ctx.moveTo(60, 20);
            ctx.lineTo(60, h-60);
            ctx.lineTo(w-20, h-60);
            ctx.stroke();

            // Horizontal gridlines (every 10 doom)
            ctx.strokeStyle = '#333';
            ctx.lineWidth = 1;
            ctx.setLineDash([2, 2]);
            for (let doom = 20; doom < maxY; doom += 10) {
                const y = h - 60 - ((doom - minY) / rangeY) * (h - 80);
                ctx.beginPath();
                ctx.moveTo(60, y);
                ctx.lineTo(w - 20, y);
                ctx.stroke();
                ctx.fillStyle = '#666';
                ctx.font = '9px sans-serif';
                ctx.fillText(doom.toString(), 35, y + 3);
            }
            ctx.setLineDash([]);

            // Y-axis labels
            ctx.fillStyle = '#888';
            ctx.font = '11px sans-serif';
            ctx.fillText('Doom', 20, 15);
            ctx.font = '9px sans-serif';
            ctx.fillText(maxY.toString(), 35, 30);
            ctx.fillText(minY.toString(), 35, h-65);

            // X-axis labels - ensure no overlap
            const labelStep = Math.max(1, Math.ceil(positionedData.length / 15));
            ctx.fillStyle = '#888';
            ctx.font = '8px sans-serif';
            positionedData.filter((_, i) => i % labelStep === 0).forEach(d => {
                const x = 60 + (d.xPos / rangeX) * (w - 80);
                ctx.save();
                ctx.translate(x, h-42);
                ctx.rotate(-Math.PI/4);
                ctx.fillText(d.label, 0, 0);
                ctx.restore();
            });

            // Draw data by iteration with regression lines
            const colors = ['#4ade80', '#e94560', '#60a5fa', '#facc15', '#f97316', '#8b5cf6', '#ec4899', '#14b8a6'];

            runIters.forEach((runIter, idx) => {
                const [run, iterStr] = runIter.split('_I');
                const iter = parseInt(iterStr);
                const iterData = positionedData.filter(d => d.run === run && d.iter === iter);

                if (iterData.length === 0) return;

                const color = colors[idx % colors.length];

                // Calculate regression on positioned data
                if (iterData.length >= 2) {
                    const n = iterData.length;
                    const sx = iterData.reduce((s, d) => s + d.xPos, 0);
                    const sy = iterData.reduce((s, d) => s + d.doom, 0);
                    const sxy = iterData.reduce((s, d) => s + d.xPos * d.doom, 0);
                    const sx2 = iterData.reduce((s, d) => s + d.xPos * d.xPos, 0);
                    const m = (n * sxy - sx * sy) / (n * sx2 - sx * sx);
                    const b = (sy - m * sx) / n;

                    // Regression line
                    const x1 = 60 + (iterData[0].xPos / rangeX) * (w - 80);
                    const x2 = 60 + (iterData[iterData.length - 1].xPos / rangeX) * (w - 80);
                    const y1 = h - 60 - ((m * iterData[0].xPos + b - minY) / rangeY) * (h - 80);
                    const y2 = h - 60 - ((m * iterData[iterData.length - 1].xPos + b - minY) / rangeY) * (h - 80);

                    ctx.strokeStyle = color;
                    ctx.lineWidth = 2;
                    ctx.setLineDash([6, 3]);
                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(x2, y2);
                    ctx.stroke();
                    ctx.setLineDash([]);

                    // Legend
                    if (idx < 10) {
                        const ly = 40 + idx * 15;
                        ctx.fillStyle = color;
                        ctx.beginPath();
                        ctx.arc(w - 200, ly, 3, 0, Math.PI * 2);
                        ctx.fill();
                        ctx.fillStyle = '#888';
                        ctx.font = '9px sans-serif';
                        ctx.fillText(`${runIter} m=${m.toFixed(1)}`, w - 190, ly + 3);
                    }
                }

                // Points
                ctx.fillStyle = color;
                iterData.forEach(d => {
                    const x = 60 + (d.xPos / rangeX) * (w - 80);
                    const y = h - 60 - ((d.doom - minY) / rangeY) * (h - 80);
                    ctx.beginPath();
                    ctx.arc(x, y, showAverages ? 4 : 3, 0, Math.PI * 2);
                    ctx.fill();
                });
            });
        }

        function drawScoreChart() {
            const canvas = document.getElementById('score-chart');
            if (!canvas) return;
            const ctx = canvas.getContext('2d');
            const w = canvas.width, h = canvas.height;
            ctx.clearRect(0, 0, w, h);

            let data = [];
            if (scoreTypeFilter === 'both') data = [...progressData.selfplay, ...progressData.arena];
            else if (scoreTypeFilter === 'selfplay') data = progressData.selfplay;
            else data = progressData.arena;

            // Filter by runs and iters
            data = data.filter(d => scoreRunsFilter.includes(d.run) && scoreItersFilter.includes(d.iter));

            if (!data.length) {
                ctx.fillStyle = '#888';
                ctx.font = '16px sans-serif';
                ctx.fillText('No data', w/2 - 30, h/2);
                return;
            }

            // Sort by run_iter_game_num to ensure proper ordering
            data.sort((a, b) => a.run_iter_game_num - b.run_iter_game_num);

            // Fixed Y-axis range (extended to 1.5 to accommodate scores >1.0)
            const minY = 0;
            const maxY = 1.5;
            const rangeY = maxY - minY;

            // Group by iteration
            const runIters = [...new Set(data.map(d => `${d.run}_I${String(d.iter).padStart(2, '0')}`))];
            const showAverages = runIters.length > 10;

            // Map data points to evenly-spaced X positions (0, 1, 2, 3...)
            const positionedData = [];
            let xPos = 0;
            runIters.forEach((runIter, iterIdx) => {
                const [run, iterStr] = runIter.split('_I');
                const iter = parseInt(iterStr);
                const iterGames = data.filter(d => d.run === run && d.iter === iter);

                if (showAverages) {
                    // Show one point per iteration (average)
                    const avgScore = iterGames.reduce((s, d) => s + d.score, 0) / iterGames.length;
                    positionedData.push({
                        xPos: xPos++,
                        score: avgScore,
                        label: runIter,
                        run: run,
                        iter: iter,
                        games: iterGames
                    });
                } else {
                    // Show individual games
                    iterGames.forEach((game, gameIdx) => {
                        positionedData.push({
                            xPos: xPos++,
                            score: game.score,
                            label: game.run_iter_game,
                            run: run,
                            iter: iter,
                            games: [game]
                        });
                    });
                }
            });

            const rangeX = Math.max(1, positionedData.length - 1);

            // Axes
            ctx.strokeStyle = '#555';
            ctx.lineWidth = 2;
            ctx.beginPath();
            ctx.moveTo(60, 20);
            ctx.lineTo(60, h-60);
            ctx.lineTo(w-20, h-60);
            ctx.stroke();

            // Horizontal gridlines (every 0.25 score)
            ctx.strokeStyle = '#333';
            ctx.lineWidth = 1;
            ctx.setLineDash([2, 2]);
            for (let score = 0.25; score < maxY; score += 0.25) {
                const y = h - 60 - ((score - minY) / rangeY) * (h - 80);
                ctx.beginPath();
                ctx.moveTo(60, y);
                ctx.lineTo(w - 20, y);
                ctx.stroke();
                ctx.fillStyle = '#666';
                ctx.font = '9px sans-serif';
                ctx.fillText(score.toFixed(2), 25, y + 3);
            }
            ctx.setLineDash([]);

            // Y-axis labels
            ctx.fillStyle = '#888';
            ctx.font = '11px sans-serif';
            ctx.fillText('Score', 20, 15);
            ctx.font = '9px sans-serif';
            ctx.fillText(maxY.toFixed(1), 25, 30);
            ctx.fillText(minY.toFixed(1), 25, h-65);

            // X-axis labels - ensure no overlap
            const labelStep = Math.max(1, Math.ceil(positionedData.length / 15));
            ctx.fillStyle = '#888';
            ctx.font = '8px sans-serif';
            positionedData.filter((_, i) => i % labelStep === 0).forEach(d => {
                const x = 60 + (d.xPos / rangeX) * (w - 80);
                ctx.save();
                ctx.translate(x, h-42);
                ctx.rotate(-Math.PI/4);
                ctx.fillText(d.label, 0, 0);
                ctx.restore();
            });

            // Draw data by iteration with regression lines
            const colors = ['#4ade80', '#e94560', '#60a5fa', '#facc15', '#f97316', '#8b5cf6', '#ec4899', '#14b8a6'];

            runIters.forEach((runIter, idx) => {
                const [run, iterStr] = runIter.split('_I');
                const iter = parseInt(iterStr);
                const iterData = positionedData.filter(d => d.run === run && d.iter === iter);

                if (iterData.length === 0) return;

                const color = colors[idx % colors.length];

                // Calculate regression on positioned data
                if (iterData.length >= 2) {
                    const n = iterData.length;
                    const sx = iterData.reduce((s, d) => s + d.xPos, 0);
                    const sy = iterData.reduce((s, d) => s + d.score, 0);
                    const sxy = iterData.reduce((s, d) => s + d.xPos * d.score, 0);
                    const sx2 = iterData.reduce((s, d) => s + d.xPos * d.xPos, 0);
                    const m = (n * sxy - sx * sy) / (n * sx2 - sx * sx);
                    const b = (sy - m * sx) / n;

                    // Regression line
                    const x1 = 60 + (iterData[0].xPos / rangeX) * (w - 80);
                    const x2 = 60 + (iterData[iterData.length - 1].xPos / rangeX) * (w - 80);
                    const y1 = h - 60 - ((m * iterData[0].xPos + b - minY) / rangeY) * (h - 80);
                    const y2 = h - 60 - ((m * iterData[iterData.length - 1].xPos + b - minY) / rangeY) * (h - 80);

                    ctx.strokeStyle = color;
                    ctx.lineWidth = 2;
                    ctx.setLineDash([6, 3]);
                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(x2, y2);
                    ctx.stroke();
                    ctx.setLineDash([]);

                    // Legend
                    if (idx < 10) {
                        const ly = 40 + idx * 15;
                        ctx.fillStyle = color;
                        ctx.beginPath();
                        ctx.arc(w - 200, ly, 3, 0, Math.PI * 2);
                        ctx.fill();
                        ctx.fillStyle = '#888';
                        ctx.font = '9px sans-serif';
                        ctx.fillText(`${runIter} m=${m.toFixed(3)}`, w - 190, ly + 3);
                    }
                }

                // Points
                ctx.fillStyle = color;
                iterData.forEach(d => {
                    const x = 60 + (d.xPos / rangeX) * (w - 80);
                    const y = h - 60 - ((d.score - minY) / rangeY) * (h - 80);
                    ctx.beginPath();
                    ctx.arc(x, y, showAverages ? 4 : 3, 0, Math.PI * 2);
                    ctx.fill();
                });
            });
        }

        function drawWinsChart() {
            const canvas = document.getElementById('wins-chart');
            if (!canvas) return;
            const ctx = canvas.getContext('2d');
            const w = canvas.width, h = canvas.height;
            ctx.clearRect(0, 0, w, h);

            let data = [];
            if (winsTypeFilter === 'both') data = [...progressData.selfplay, ...progressData.arena];
            else if (winsTypeFilter === 'selfplay') data = progressData.selfplay;
            else data = progressData.arena;

            // Filter by runs
            data = data.filter(d => winsRunsFilter.includes(d.run));

            if (!data.length) {
                ctx.fillStyle = '#888';
                ctx.font = '16px sans-serif';
                ctx.fillText('No data', w/2 - 30, h/2);
                return;
            }

            // Aggregate by (run, iter) - count wins
            const iterMap = new Map();
            data.forEach(d => {
                const key = `${d.run}_I${String(d.iter).padStart(2, '0')}`;
                if (!iterMap.has(key)) {
                    iterMap.set(key, {run: d.run, iter: d.iter, wins: 0, total: 0});
                }
                const entry = iterMap.get(key);
                entry.total++;
                // Check if this game was a win (FINAL_SCORE = 1.0 or very close)
                if (d.score >= 0.99) entry.wins++;
            });

            const iterData = Array.from(iterMap.values()).sort((a, b) => {
                if (a.run !== b.run) return a.run.localeCompare(b.run);
                return a.iter - b.iter;
            });

            if (!iterData.length) {
                ctx.fillStyle = '#888';
                ctx.font = '16px sans-serif';
                ctx.fillText('No iterations with data', w/2 - 60, h/2);
                return;
            }

            // Fixed Y-axis range
            const minY = 0;
            const maxY = 20;
            const rangeY = maxY - minY;

            // Map iterations to evenly-spaced X positions
            const rangeX = Math.max(1, iterData.length - 1);

            // Axes
            ctx.strokeStyle = '#555';
            ctx.lineWidth = 2;
            ctx.beginPath();
            ctx.moveTo(60, 20);
            ctx.lineTo(60, h-60);
            ctx.lineTo(w-20, h-60);
            ctx.stroke();

            // Horizontal gridlines (every 5 wins)
            ctx.strokeStyle = '#333';
            ctx.lineWidth = 1;
            ctx.setLineDash([2, 2]);
            for (let wins = 5; wins < maxY; wins += 5) {
                const y = h - 60 - ((wins - minY) / rangeY) * (h - 80);
                ctx.beginPath();
                ctx.moveTo(60, y);
                ctx.lineTo(w - 20, y);
                ctx.stroke();
                ctx.fillStyle = '#666';
                ctx.font = '9px sans-serif';
                ctx.fillText(wins.toString(), 35, y + 3);
            }
            ctx.setLineDash([]);

            // Y-axis labels
            ctx.fillStyle = '#888';
            ctx.font = '11px sans-serif';
            ctx.fillText('Wins', 20, 15);
            ctx.font = '9px sans-serif';
            ctx.fillText(maxY.toString(), 35, 30);
            ctx.fillText(minY.toString(), 35, h-65);

            // X-axis labels - ensure no overlap
            const labelStep = Math.max(1, Math.ceil(iterData.length / 15));
            ctx.fillStyle = '#888';
            ctx.font = '8px sans-serif';
            iterData.filter((_, i) => i % labelStep === 0).forEach((d, idx) => {
                const realIdx = idx * labelStep;
                const x = 60 + (realIdx / rangeX) * (w - 80);
                const label = `${d.run} I${String(d.iter).padStart(2, '0')}`;
                ctx.save();
                ctx.translate(x, h-42);
                ctx.rotate(-Math.PI/4);
                ctx.fillText(label, 0, 0);
                ctx.restore();
            });

            // Draw bars
            const colors = ['#4ade80', '#e94560', '#60a5fa', '#facc15', '#f97316', '#8b5cf6', '#ec4899', '#14b8a6'];
            const runColors = {};
            const uniqueRuns = [...new Set(iterData.map(d => d.run))];
            uniqueRuns.forEach((run, idx) => {
                runColors[run] = colors[idx % colors.length];
            });

            iterData.forEach((d, idx) => {
                const x = 60 + (idx / rangeX) * (w - 80);
                const barWidth = Math.max(3, ((w - 80) / rangeX) * 0.6);
                const y = h - 60 - ((d.wins - minY) / rangeY) * (h - 80);
                const barHeight = ((d.wins - minY) / rangeY) * (h - 80);

                ctx.fillStyle = runColors[d.run];
                ctx.fillRect(x - barWidth/2, y, barWidth, barHeight);

                // Draw outline
                ctx.strokeStyle = '#000';
                ctx.lineWidth = 1;
                ctx.strokeRect(x - barWidth/2, y, barWidth, barHeight);
            });

            // Legend
            uniqueRuns.forEach((run, idx) => {
                if (idx < 10) {
                    const ly = 40 + idx * 15;
                    ctx.fillStyle = runColors[run];
                    ctx.fillRect(w - 200, ly - 5, 10, 10);
                    ctx.fillStyle = '#888';
                    ctx.font = '9px sans-serif';
                    ctx.fillText(run, w - 185, ly + 3);
                }
            });
        }

        function initPage() {
            const hash = window.location.hash.substring(1);
            if (hash) {
                showTab(hash);
            }
            updateStatus();
            setInterval(updateStatus, 30000);
        }

        // Run immediately AND on DOMContentLoaded
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', initPage);
        } else {
            initPage();
        }
    </script>
</body>
</html>'''


def main():
    port = 8765
    server = HTTPServer(('0.0.0.0', port), DashboardHandler)

    # Get local IP
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        s.connect(('10.255.255.255', 1))
        local_ip = s.getsockname()[0]
    except:
        local_ip = '127.0.0.1'
    finally:
        s.close()

    print(f"Brain Dashboard running:")
    print(f"  Local:   http://localhost:{port}")
    print(f"  Network: http://{local_ip}:{port}")
    print("Press Ctrl+C to stop")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down...")
        server.shutdown()


if __name__ == "__main__":
    main()
