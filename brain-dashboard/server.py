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

# Ensure dirs exist
LOGS_DIR.mkdir(parents=True, exist_ok=True)

# Current run tracking
current_run_pid = None
current_run_log = None

# Game cache - avoid reparsing 1000+ trace files on every request
_selfplay_cache = None
_selfplay_cache_time = 0
_arena_cache = None
_arena_cache_time = 0
_progress_cache = None
_progress_cache_time = 0
_checkpoints_cache = None
_checkpoints_cache_time = 0
_CACHE_TTL = 30  # seconds

# Auto-rebuild canonical store
_canonical_rebuild_thread = None
_canonical_rebuild_stop = False

# Cache for live log parsing
_live_log_cache = {}
_live_log_cache_timestamp = 0

def get_current_run_status():
    """Parse the current run log for status."""
    # Get disk space
    import subprocess
    try:
        result = subprocess.run(['df', '-h', '/Users/gremus'], capture_output=True, text=True)
        df_line = result.stdout.strip().split('\n')[-1]
        disk_free = df_line.split()[3]  # 4th column is "Avail"
    except:
        disk_free = "?"

    # Calculate Claude storage used (traces + checkpoints + logs)
    claude_storage_gb = 0
    disk_available_gb = 0
    try:
        # Get available disk space in GB
        result = subprocess.run(['df', '-k', '/Users/gremus'], capture_output=True, text=True)
        df_line = result.stdout.strip().split('\n')[-1]
        available_kb = int(df_line.split()[3])
        disk_available_gb = round(available_kb / (1024 * 1024))

        # Calculate size of Claude-related directories
        mcts_root = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
        total_bytes = 0

        # Traces
        traces_dir = mcts_root / "arena-traces"
        if traces_dir.exists():
            total_bytes += sum(f.stat().st_size for f in traces_dir.rglob('*') if f.is_file())

        # Checkpoints
        checkpoints_dir = mcts_root / "checkpoints"
        if checkpoints_dir.exists():
            total_bytes += sum(f.stat().st_size for f in checkpoints_dir.rglob('*') if f.is_file())

        # Dashboard logs
        logs_dir = mcts_root / "brain-dashboard" / "logs"
        if logs_dir.exists():
            total_bytes += sum(f.stat().st_size for f in logs_dir.rglob('*') if f.is_file())

        # Backups
        backups_dir = mcts_root / "arena-traces-backups"
        if backups_dir.exists():
            total_bytes += sum(f.stat().st_size for f in backups_dir.rglob('*') if f.is_file())

        claude_storage_gb = round(total_bytes / (1024 * 1024 * 1024))
    except Exception as e:
        print(f"Error calculating disk space: {e}", flush=True)

    # Find most recent sp_R*.log
    log_files = list(Path("/tmp").glob("sp_R*.log"))
    if not log_files:
        return {"running": False, "message": "No run found", "disk_free": disk_free}

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

        # Check for bootstrap phase
        bootstrap_done = 'bootstrap collection done' in content
        bootstrap_phase = False
        bootstrap_epoch = 0
        bootstrap_total_epochs = 6

        # Check if bootstrap training phase (epochs running)
        # Look for recent "arena @ iter 0" which means bootstrap finished and evaluating
        arena_iter0_line = next((l for l in lines if 'arena @ iter 0 (bootstrap only)' in l), None)

        if bootstrap_done and not any(l.startswith('iter ') for l in lines[-50:]) and not arena_iter0_line:
            # Bootstrap done but no iterations started and not yet evaluating = training epochs
            bootstrap_phase = True

            # Check which phase we're in
            value_epoch_lines = [l for l in lines if 'value epoch' in l and 'done' in l]
            if value_epoch_lines:
                # In value phase
                last_epoch = value_epoch_lines[-1]
                epoch_match = re.search(r'value epoch (\d+)/(\d+)', last_epoch)
                if epoch_match:
                    bootstrap_epoch = int(epoch_match.group(1))
                    bootstrap_total_epochs = int(epoch_match.group(2))
                    bootstrap_phase_name = "value"
                else:
                    bootstrap_epoch = len(value_epoch_lines)
                    bootstrap_total_epochs = 6
                    bootstrap_phase_name = "value"
            else:
                # In policy phase
                policy_epoch_lines = [l for l in lines if 'policy epoch' in l and 'done' in l]
                if policy_epoch_lines:
                    last_epoch = policy_epoch_lines[-1]
                    epoch_match = re.search(r'policy epoch (\d+)/(\d+)', last_epoch)
                    if epoch_match:
                        bootstrap_epoch = int(epoch_match.group(1))
                        bootstrap_total_epochs = int(epoch_match.group(2))
                        bootstrap_phase_name = "policy"
                    else:
                        bootstrap_epoch = len(policy_epoch_lines)
                        bootstrap_total_epochs = 6
                        bootstrap_phase_name = "policy"
                else:
                    bootstrap_epoch = 0
                    bootstrap_total_epochs = 6
                    bootstrap_phase_name = "policy"
        else:
            bootstrap_phase = False
            bootstrap_epoch = 0
            bootstrap_total_epochs = 0
            bootstrap_phase_name = ""

        # Find latest iteration and current game
        iter_lines = [l for l in lines if l.startswith('iter ') and '|' in l]
        current_game = 0
        total_games = 20
        current_iter = 0
        run_mode = "unknown"  # "arena", "selfplay", or "unknown"

        # Parse actual iteration from command line args (more reliable than log labels)
        # cmd_iter extraction removed - for iterarena mode, iteration comes from checkpoint load,
        # not command line. The first arg after mode (e.g. 2000 in "iterarena 2000...") is bootGames,
        # not iteration number.
        cmd_iter = None

        # Try to parse perSeat from DEBUG ARGS line OR game completion lines
        per_seat = None

        # Method 1: Parse from DEBUG ARGS
        debug_args_lines = [l for l in lines if 'DEBUG ARGS:' in l and 'perSeat=' in l]
        if debug_args_lines:
            debug_match = re.search(r'perSeat=(\d+)', debug_args_lines[-1])
            if debug_match:
                per_seat = int(debug_match.group(1))

        # Method 2: Parse from actual game completion lines "game X/Y saved:"
        if not per_seat:
            game_saved_lines = [l for l in lines if 'game ' in l and '/25 saved:' in l or '/20 saved:' in l or '/10 saved:' in l or '/5 saved:' in l]
            if game_saved_lines:
                game_saved_match = re.search(r'game \d+/(\d+) saved:', game_saved_lines[-1])
                if game_saved_match:
                    per_seat = int(game_saved_match.group(1))

        # If we found per_seat, calculate total games (4 factions)
        if per_seat:
            total_games = per_seat * 4

        # Check for arena evaluation in progress
        # Distinguish between training arena (1600 games) and evaluation arena (20 games)
        arena_start_lines = [i for i, l in enumerate(lines) if '-- arena @ iter' in l or 'arena 20 games' in l or 'iter ' in l and '| arena ' in l]
        if arena_start_lines:
            last_arena_start_idx = arena_start_lines[-1]
            arena_line = lines[last_arena_start_idx]

            # Parse game count from arena line to determine if training or evaluation
            arena_game_count = None
            game_count_match = re.search(r'arena (\d+) games', arena_line)
            if game_count_match:
                arena_game_count = int(game_count_match.group(1))

            # If > 100 games, this is a training phase, not evaluation
            # Parse game X/Y lines directly from log for training
            if arena_game_count and arena_game_count > 100:
                # Training mode - parse from game lines
                run_mode = "training"
                lines_after = lines[last_arena_start_idx:]
                game_lines = [l for l in lines_after if re.search(r'game (\d+)/(\d+)', l)]
                if game_lines:
                    last_game_line = game_lines[-1]
                    game_progress_match = re.search(r'game (\d+)/(\d+)', last_game_line)
                    if game_progress_match:
                        current_game = int(game_progress_match.group(1))
                        total_games = int(game_progress_match.group(2))

                # Parse iteration from arena line
                iter_match = re.search(r'iter (\d+)', arena_line)
                if iter_match:
                    current_iter = int(iter_match.group(1))
            else:
                # Evaluation arena mode - use canonical store
                run_mode = "arena"
                # Count game completions after arena start:
                # NEW format: "game X/Y complete: YS score=0.211 doom=12 (177s)"
                # OLD format: "game X/Y saved:" (one line per game per faction)
                lines_after = lines[last_arena_start_idx:]

                # Count from canonical store (source of truth for deduplicated games)
                # Determine which iteration we're looking at
                temp_iter = None
                if cmd_iter is not None:
                    temp_iter = cmd_iter
                else:
                    arena_line = lines[last_arena_start_idx]
                    iter_match = re.search(r'iter (\d+)', arena_line)
                    if iter_match:
                        temp_iter = int(iter_match.group(1))

                # Load canonical store and count games for this run/iteration
                if temp_iter is not None:
                    canonical_path = Path('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json')
                    if canonical_path.exists():
                        try:
                            with open(canonical_path) as f:
                                canonical = json.load(f)
                            # Extract run tag
                            run_tag = "R39"  # default
                            for line in reversed(lines[-100:]):
                                run_match = re.search(r'R(\d+)', line)
                                if run_match:
                                    run_tag = f"R{run_match.group(1)}"
                                    break

                            # Count games matching this run and iteration
                            matching_games = [g for g in canonical['games']
                                            if g.get('run') == run_tag and g.get('iteration') == temp_iter]
                            current_game = len(matching_games)
                        except:
                            # Fallback to log line counting if canonical store fails
                            game_saved_lines = [l for l in lines_after if 'game ' in l and 'saved:' in l and 'arena-' in l]
                            current_game = len(game_saved_lines)
                    else:
                        # Fallback if canonical store doesn't exist - count from log
                        game_saved_lines = [l for l in lines_after if 'game ' in l and 'saved:' in l and 'arena-' in l and re.search(r'game \d+/\d+ saved:', l)]
                        game_complete_lines = [l for l in lines_after if re.search(r'game (\d+)/(\d+) complete:', l)]

                        if game_saved_lines:
                            current_game = len(game_saved_lines)
                            saved_match = re.search(r'game \d+/(\d+) saved:', game_saved_lines[-1])
                            if saved_match:
                                per_faction_total = int(saved_match.group(1))
                                total_games = per_faction_total * 4
                        elif game_complete_lines:
                            current_game = len(game_complete_lines)
                            completion_match = re.search(r'game \d+/(\d+) complete:', game_complete_lines[0])
                            if completion_match:
                                per_faction_total = int(completion_match.group(1))
                                total_games = per_faction_total * 4

                # Determine which iter this arena belongs to (evaluation mode only)
                if cmd_iter is not None:
                    # Use command-line iteration (most reliable)
                    current_iter = cmd_iter
                else:
                    # Fallback to parsing from log label
                    arena_line = lines[last_arena_start_idx]
                    if 'iter 0' in arena_line:
                        current_iter = 0
                    else:
                        arena_iter_match = re.search(r'iter (\d+)', arena_line)
                        if arena_iter_match:
                            current_iter = int(arena_iter_match.group(1))

            # Do NOT use arena_trace.log - it's never cleared and contains old runs
            # Use >>> overall lines as source of truth for completed iterations

        if iter_lines:
            last_iter = iter_lines[-1]
            iter_match = re.match(r'iter\s+(\d+)', last_iter)
            iter_from_line = int(iter_match.group(1)) if iter_match else 0

            # Only update current_iter if we found a more recent iter line
            if iter_from_line > current_iter:
                current_iter = iter_from_line

            # Parse mode and game count
            # Arena mode: "iter N | arena M games" (M is often wrong, ignore it)
            # Selfplay mode: "iter N | selfplay X/Y finished"
            if 'arena' in last_iter:
                run_mode = "arena"
                # Don't trust the "arena N games" number - it's often wrong
                # Instead, look for actual game completion lines
            else:
                # Parse: "selfplay 18/20 finished"
                game_match = re.search(r'selfplay (\d+)/(\d+) finished', last_iter)
                if game_match:
                    current_game = int(game_match.group(1))
                    total_games = int(game_match.group(2))
                    run_mode = "selfplay"
        elif current_iter == 0 and not arena_start_lines:
            current_iter = 0

        # Save the per-iteration total_games before aggregating arena results
        games_per_iter = total_games

        # Find arena results from ALL >>> overall lines (aggregate across run)
        arena_lines = [l for l in lines if '>>> overall' in l]
        total_wins = 0
        total_arena_games = 0
        all_doom_values = []

        if arena_lines:
            for arena_line in arena_lines:
                # Parse: >>> overall 0/20 = 0% | GC:0/5(d19) BG:0/5(d11) YS:0/5(d6) CC:0/5(d7)
                wins_match = re.search(r'overall (\d+)/(\d+)', arena_line)
                if wins_match:
                    total_wins += int(wins_match.group(1))
                    total_arena_games += int(wins_match.group(2))

                # Parse per-faction doom values
                parts = arena_line.split('|')
                if len(parts) >= 2:
                    faction_parts = parts[1].strip().split()
                    for faction_str in faction_parts:
                        # Format: GC:0/5(d19)
                        doom_match = re.match(r'[A-Z]+:\d+/\d+\(d(\d+)\)', faction_str)
                        if doom_match:
                            all_doom_values.append(int(doom_match.group(1)))

            wins = total_wins
            total = total_arena_games
            win_rate = wins / total if total > 0 else 0
            avg_doom = sum(all_doom_values) / len(all_doom_values) if all_doom_values else 0
        else:
            wins, total, win_rate = 0, 0, 0
            avg_doom = 0

        # Restore per-iteration total_games (from perSeat or parsed from iter line)
        total_games = games_per_iter

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

        # Extract target iterations from log
        # Bootstrap mode: "then N self-play iterations"
        # Arena mode: "Target: N arena iterations"
        bootstrap_match = re.search(r'then (\d+) self-play iterations', content)
        arena_match = re.search(r'Target: (\d+) arena iterations', content)
        if arena_match:
            target_iters = int(arena_match.group(1))
        elif bootstrap_match:
            target_iters = int(bootstrap_match.group(1))
        else:
            target_iters = 10  # Default to 10 for arena mode

        # Find warmstart base iter (what the run continued from)
        warmstart_base = 0
        warmstart_match = re.search(r'warm-start.*iter=(\d+)', content)
        if warmstart_match:
            warmstart_base = int(warmstart_match.group(1))

        # Compute total run elapsed time from log file creation
        try:
            run_start_time = latest_log.stat().st_birthtime
        except AttributeError:
            # st_birthtime not available on all systems
            run_start_time = latest_log.stat().st_ctime
        run_elapsed = int(time.time() - run_start_time)

        # Compute current phase elapsed by summing ACTUAL game completion times from log
        phase_elapsed = 0
        if arena_start_lines:
            last_arena_idx = arena_start_lines[-1]
            lines_after_arena = lines[last_arena_idx:]
            # Sum all game completion times: "game X/Y complete: ... (Ns)"
            for line in lines_after_arena:
                time_match = re.search(r'game \d+/\d+ complete:.*\((\d+)s\)', line)
                if time_match:
                    phase_elapsed += int(time_match.group(1))
        # Fallback: if no game times, use file mtime
        if phase_elapsed == 0:
            phase_elapsed = int(time.time() - latest_log.stat().st_mtime)

        # Calculate ETAs AUTOMATICALLY based on historical timing data from log
        iter_eta_seconds = 0
        run_eta_seconds = 0

        # Parse historical epoch/iter timing from log
        def parse_timing_history():
            epoch_times = []
            iter_times = []
            game_times = []

            # Find epoch start/end timestamps
            for i, line in enumerate(lines):
                if 'epoch ' in line and '/6 done' in line:
                    # Try to estimate epoch time from surrounding timestamps
                    # Look for time markers in log (these are approximate)
                    epoch_times.append(50)  # Fallback: ~50s per epoch observed

            # Parse ACTUAL game completion times from arena logs
            # Format: "game 83/100 complete: YS score=0.211 doom=12 (177s)"
            for line in lines:
                match = re.search(r'game (\d+)/(\d+) complete:.*\((\d+)s\)', line)
                if match:
                    time_seconds = int(match.group(3))
                    game_times.append(time_seconds)

            # Calculate iteration times from canonical store (completed iterations)
            try:
                with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
                    store = json.load(f)
                normalized_run = run_tag.split('_')[0] if '_' in run_tag else run_tag
                run_games = [g for g in store.get("games", []) if g.get("run") == normalized_run and g.get("type") == "arena"]

                # Group by iteration and calculate games per iteration
                by_iter = {}
                for g in run_games:
                    iter_num = g.get("iteration", 0)
                    if iter_num not in by_iter:
                        by_iter[iter_num] = []
                    by_iter[iter_num].append(g)

                # For completed iterations (have all games), we can estimate iteration time
                # from the number of games and average game time
                if game_times:
                    avg_game_time = sum(game_times) / len(game_times)
                    for iter_num, games in by_iter.items():
                        if len(games) >= 80:  # Consider "complete" if 80+ games (some may have 100)
                            # Estimate: games * avg_game_time + 30 min training overhead
                            estimated_iter_time = len(games) * avg_game_time + 1800
                            iter_times.append(estimated_iter_time)
            except Exception as e:
                print(f"Error parsing canonical store for timing: {e}", flush=True)

            return epoch_times, iter_times, game_times

        epoch_times, iter_times, game_times = parse_timing_history()

        if bootstrap_phase:
            # Bootstrap: use historical epoch timing
            if epoch_times:
                avg_epoch_time = sum(epoch_times) / len(epoch_times)
            else:
                avg_epoch_time = 50  # Default: 50s per epoch

            remaining_epochs = bootstrap_total_epochs - bootstrap_epoch
            if remaining_epochs > 0:
                iter_eta_seconds = int(remaining_epochs * avg_epoch_time)
            else:
                # Last epoch in progress
                log_age = time.time() - latest_log.stat().st_mtime
                if log_age < 60:
                    iter_eta_seconds = int(avg_epoch_time / 2)
                else:
                    iter_eta_seconds = 0

            # After bootstrap: estimate remaining iters based on historical or default
            if iter_times:
                avg_iter_time = sum(iter_times) / len(iter_times)
            else:
                avg_iter_time = 5100  # Default: 85 min for 20-game iters
            run_eta_seconds = iter_eta_seconds + int(10 * avg_iter_time)
        elif current_iter > 0:
            # Actual absolute iteration = warmstart_base + current_iter
            absolute_iter = warmstart_base + current_iter

            # AUTOMATIC: Calculate time per iteration from historical data
            if iter_times:
                # Use actual measured average from this run
                time_per_iter = sum(iter_times) / len(iter_times)
            elif game_times and len(game_times) > 10:
                # Use actual game times from THIS iteration to estimate
                # Use recent games (last 30) for better accuracy
                recent_games = game_times[-30:] if len(game_times) > 30 else game_times
                avg_game_time = sum(recent_games) / len(recent_games)
                training_overhead = 900  # ~15 min training for small iters, 30 min for large
                if total_games >= 80:
                    training_overhead = 1800  # 30 min for 100-game iters
                time_per_iter = total_games * avg_game_time + training_overhead
            elif current_iter >= 2:
                # Use phase_elapsed / current_iter ONLY if we have 2+ complete iters
                # (current_iter tracks completed iters, so current_iter=1 means we're IN iter 1, not done with it)
                time_per_iter = phase_elapsed / current_iter
            else:
                # Ultimate fallback: estimate from total_games
                if total_games >= 80:
                    time_per_iter = 22800  # ~6.3 hours for 100-game iters
                else:
                    time_per_iter = 5100  # ~85 min for 20-game iters

            # Current iter ETA: time remaining in current iteration
            if current_game > 0 and total_games > 0 and game_times:
                # Use actual game times to calculate remaining time more accurately
                recent_games = game_times[-20:] if len(game_times) > 20 else game_times
                avg_game_time = sum(recent_games) / len(recent_games)
                remaining_games = total_games - current_game
                training_overhead = 900 if total_games < 80 else 1800

                # If we're near the end, reduce training overhead estimate
                if current_game > total_games * 0.9:
                    # Almost done with games, training is coming up
                    iter_eta_seconds = int(remaining_games * avg_game_time + training_overhead)
                else:
                    # Still collecting games
                    iter_eta_seconds = int(remaining_games * avg_game_time + training_overhead)

                iter_eta_seconds = max(60, iter_eta_seconds)
            elif current_game > 0 and total_games > 0:
                # Fallback: use time_per_iter estimate
                progress_frac = current_game / total_games
                if progress_frac > 0.01:  # At least 1% progress
                    time_remaining = time_per_iter * (1.0 - progress_frac)
                    iter_eta_seconds = max(60, int(time_remaining))
                else:
                    iter_eta_seconds = int(time_per_iter)
            else:
                iter_eta_seconds = int(time_per_iter)

            # Run ETA: remaining iterations
            # Extract target from log or use default
            remaining_iters = max(1, target_iters - absolute_iter)
            run_eta_seconds = iter_eta_seconds + int(remaining_iters * time_per_iter)

            # Update current_iter to show absolute
            current_iter = absolute_iter
        else:
            # At iter 0 (bootstrap evaluation) - use default estimates
            # Arena games: ~8-10 min each, 20 games = ~3 hours
            # Self-play iters: ~60-90 min each
            time_per_iter = 0
            iter_eta_seconds = int(3 * 3600)  # 3 hours for iter 0 arena eval
            run_eta_seconds = int(3 * 3600 + 20 * 3600)  # 3h + 20h for 20 more iters
            # Update current_iter to show absolute
            absolute_iter = warmstart_base + current_iter
            current_iter = absolute_iter

        # Determine what phase_elapsed represents
        if bootstrap_phase:
            phase_label = f"{bootstrap_phase_name.title()} Epoch {bootstrap_epoch}"
        elif current_iter > 0:
            phase_label = f"Iter {current_iter}"
        else:
            phase_label = "Iter 0 (Arena Eval)"

        # Calculate current run metrics:
        # 1. Try canonical store (for runs with all-games.txt trace files)
        # 2. Fall back to aggregate from >>> overall lines if no trace files
        try:
            with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
                store = json.load(f)
            normalized_run = run_tag.split('_')[0] if '_' in run_tag else run_tag
            current_run_games = [g for g in store.get("games", []) if g.get("run") == normalized_run and g.get("type") == "arena"]

            if current_run_games:
                # Canonical store has trace files - use them
                # EXCLUDE killed games from averages
                completed_games = [g for g in current_run_games if g.get("result") != "killed"]
                total = len(current_run_games)
                wins = sum(1 for g in completed_games if g.get("won", False))
                win_rate = wins / len(completed_games) if completed_games else 0
                avg_doom = sum(g.get("doom", 0) for g in completed_games) / len(completed_games) if completed_games else 0
                best_score = max((g.get("score", 0) for g in completed_games), default=0)
            # Note: wins, total, avg_doom already calculated correctly at lines 196-221
            # Just ensure best_score is set
            if 'best_score' not in locals():
                best_score = 0
        except Exception as e:
            print(f"Error calculating current run metrics: {e}", flush=True)
            pass

        return {
            "running": running,
            "run_tag": run_tag,
            "run_mode": run_mode,
            "current_iter": current_iter,
            "current_game": current_game,
            "total_games": total_games,
            "target_iters": target_iters,
            "phase_elapsed_seconds": phase_elapsed,
            "phase_elapsed_human": format_duration(phase_elapsed),
            "phase_label": phase_label,
            "run_elapsed_seconds": run_elapsed,
            "run_elapsed_human": format_duration(run_elapsed),
            "disk_free": disk_free,
            "claude_storage_gb": claude_storage_gb,
            "disk_available_gb": disk_available_gb,
            "time_per_iter": time_per_iter if not bootstrap_phase else 0,
            "iter_eta_seconds": iter_eta_seconds,
            "iter_eta_human": format_duration(iter_eta_seconds) if iter_eta_seconds > 0 else "Unknown",
            "run_eta_seconds": run_eta_seconds,
            "run_eta_human": format_duration(run_eta_seconds) if run_eta_seconds > 0 else "Unknown",
            "arena_wins": wins,
            "arena_total": total,
            "arena_win_rate": win_rate,
            "avg_doom": avg_doom,
            "best_score": best_score,
            "best_run_iter": best_run_iter,
            "log_file": str(latest_log),
            "bootstrap_phase": bootstrap_phase,
            "bootstrap_epoch": bootstrap_epoch,
            "bootstrap_total_epochs": bootstrap_total_epochs,
            "bootstrap_phase_name": bootstrap_phase_name if bootstrap_phase else "",
            "run_config": f"Arena mode: {per_seat} games/faction, {total_games} total" if per_seat else "Unknown configuration"
        }
    except Exception as e:
        return {"running": running, "error": str(e)}

def format_duration(seconds):
    """Format seconds as human readable duration (always 2 units max)."""
    if seconds < 1:
        return "0s"

    # Calculate all units
    weeks = int(seconds // (7 * 24 * 3600))
    days = int((seconds % (7 * 24 * 3600)) // (24 * 3600))
    hours = int((seconds % (24 * 3600)) // 3600)
    mins = int((seconds % 3600) // 60)
    secs = int(seconds % 60)

    # Build list of non-zero units
    units = []
    if weeks > 0:
        units.append((weeks, 'w'))
    if days > 0:
        units.append((days, 'd'))
    if hours > 0:
        units.append((hours, 'h'))
    if mins > 0:
        units.append((mins, 'm'))
    if secs > 0:
        units.append((secs, 's'))

    # Return top 2 units
    if len(units) == 0:
        return "0s"
    elif len(units) == 1:
        return f"{units[0][0]}{units[0][1]}"
    else:
        return f"{units[0][0]}{units[0][1]} {units[1][0]}{units[1][1]}"

def get_saved_checkpoints():
    """List available saved checkpoints with arena stats from canonical store."""
    # Load canonical store once
    try:
        with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
            store = json.load(f)
        all_games = store.get("games", [])
    except:
        all_games = []

    checkpoints = []
    for d in CHECKPOINTS.iterdir():
        if d.is_dir() and d.name not in ['.', '..']:
            meta_file = d / "best.meta"
            if meta_file.exists():
                meta = meta_file.read_text()
                # Parse bestgame= (new format) or score= (old format), handle scientific notation
                score_match = re.search(r'(?:bestgame|score)=([0-9.Ee+-]+)', meta)
                tag_match = re.search(r'tag=(\S+)', meta)
                iter_match = re.search(r'iter=(-?\d+)', meta)

                tag = tag_match.group(1) if tag_match else ""
                iter_num = int(iter_match.group(1)) if iter_match else -1

                # Calculate arena stats from canonical store
                arena_wins = 0
                arena_total = 0
                arena_win_rate = 0
                arena_avg_doom = 0

                if tag and iter_num >= 0:
                    # Normalize tag (strip suffixes like "_trace")
                    normalized_tag = tag.split('_')[0] if '_' in tag else tag
                    # Find all arena games for this run+iter
                    arena_games = [g for g in all_games
                                   if g.get("run") == normalized_tag
                                   and g.get("iteration") == iter_num
                                   and g.get("type") == "arena"]
                    if arena_games:
                        # EXCLUDE killed games from averages
                        completed_games = [g for g in arena_games if g.get("result") != "killed"]
                        arena_total = len(arena_games)
                        arena_wins = sum(1 for g in completed_games if g.get("won", False))
                        arena_win_rate = arena_wins / len(completed_games) if completed_games else 0
                        arena_avg_doom = sum(g.get("doom", 0) for g in completed_games) / len(completed_games) if completed_games else 0

                checkpoints.append({
                    "name": d.name,
                    "score": float(score_match.group(1)) if score_match else 0,
                    "tag": tag,
                    "iter": iter_num,
                    "path": str(d),
                    "arena_wins": arena_wins,
                    "arena_total": arena_total,
                    "arena_win_rate": arena_win_rate,
                    "arena_avg_doom": arena_avg_doom,
                })
    # Also add current best/current
    for name in ["best", "current"]:
        meta_file = CHECKPOINTS / f"{name}.meta"
        if meta_file.exists():
            meta = meta_file.read_text()
            score_match = re.search(r'(?:bestgame|score)=([0-9.Ee+-]+)', meta)
            tag_match = re.search(r'tag=(\S+)', meta)
            iter_match = re.search(r'iter=(-?\d+)', meta)

            tag = tag_match.group(1) if tag_match else ""
            iter_num = int(iter_match.group(1)) if iter_match else -1

            # Calculate arena stats from canonical store
            arena_wins = 0
            arena_total = 0
            arena_win_rate = 0
            arena_avg_doom = 0

            if tag and iter_num >= 0:
                normalized_tag = tag.split('_')[0] if '_' in tag else tag
                arena_games = [g for g in all_games
                               if g.get("run") == normalized_tag
                               and g.get("iteration") == iter_num
                               and g.get("type") == "arena"]
                if arena_games:
                    # EXCLUDE killed games from averages
                    completed_games = [g for g in arena_games if g.get("result") != "killed"]
                    arena_total = len(arena_games)
                    arena_wins = sum(1 for g in completed_games if g.get("won", False))
                    arena_win_rate = arena_wins / len(completed_games) if completed_games else 0
                    arena_avg_doom = sum(g.get("doom", 0) for g in completed_games) / len(completed_games) if completed_games else 0

            checkpoints.append({
                "name": name,
                "score": float(score_match.group(1)) if score_match else 0,
                "tag": tag,
                "iter": iter_num,
                "path": str(CHECKPOINTS),
                "arena_wins": arena_wins,
                "arena_total": arena_total,
                "arena_win_rate": arena_win_rate,
                "arena_avg_doom": arena_avg_doom,
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

        # Parse score: try multiple formats
        # Format 1: FINAL_SCORE= (old format)
        # Format 2: INTERIM_SCORE= (killed games)
        # Format 3: Score (0-1): (complete games)
        score = None
        final_score_match = re.search(r'FINAL_SCORE=([0-9.]+)', content)
        if final_score_match:
            score = float(final_score_match.group(1))
        else:
            interim_score_match = re.search(r'INTERIM_SCORE=([0-9.]+)', content)
            if interim_score_match:
                score = float(interim_score_match.group(1))
            else:
                score_01_match = re.search(r'Score \(0-1\):\s*([0-9.]+)', content)
                if score_01_match:
                    score = float(score_01_match.group(1))

        # Parse breakdown: try BREAKDOWN= format
        breakdown_match = re.search(r'BREAKDOWN=(.+)', content)
        if breakdown_match:
            breakdown_str = breakdown_match.group(1).strip()
            return (score, breakdown_str)

        # Try "Shaping breakdown:" format
        shaping_match = re.search(r'Shaping breakdown:\s*(.+)', content)
        if shaping_match:
            breakdown_str = shaping_match.group(1).strip()
            return (score, breakdown_str)

        # No breakdown available
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

        # Try NEW format first: arena-R39-iter14-gc-game1-KILLED.txt or arena-R39-iter14-gc-game1-d17.txt
        new_match = re.match(r'arena-(R\d+)-iter(\d+)-(\w+)-game(\d+)-(?:KILLED|d(\d+))\.txt', name)
        if new_match:
            run_tag_for_game = new_match.group(1)
            iter_no = int(new_match.group(2))
            faction = new_match.group(3).upper()
            game_num = int(new_match.group(4))
            doom_from_filename = int(new_match.group(5)) if new_match.group(5) else 0
            is_killed = 'KILLED' in name

            # Calculate individual game score and breakdown from trace
            game_score, game_breakdown = calculate_game_score_and_breakdown_from_trace(trace)

            # Fallback to faction average if individual calculation fails
            score_key = (run_tag_for_game, iter_no)
            if game_score is None and score_key in score_data and faction in score_data[score_key]:
                game_score = score_data[score_key][faction]["avg"]
                game_breakdown = score_data[score_key][faction]["breakdown"]
            elif game_breakdown == "" and score_key in score_data and faction in score_data[score_key]:
                game_breakdown = score_data[score_key][faction]["breakdown"]

            # Convert breakdown to counts format
            breakdown = convert_breakdown_to_counts(game_breakdown) if game_breakdown else ""

            # Determine placement by doom ranking from trace file
            placement, doom_dict = get_doom_ranking_from_trace(trace, faction)

            # Get actual doom from trace (more accurate than filename)
            actual_doom = doom_dict.get(faction, doom_from_filename) if doom_dict else doom_from_filename

            games.append({
                "file": str(trace),
                "run": run_tag_for_game,
                "iter": iter_no,
                "faction": faction,
                "type": "killed" if is_killed else "arena",
                "doom": actual_doom,
                "is_win": placement == 1,
                "is_arena": True,
                "score": game_score if game_score is not None else 0.0,
                "breakdown": breakdown,
                "placement": placement,
                "doom_dict": doom_dict,
            })
            continue

        # Try FINAL format: arena-R39-final-gc-game1-d11.txt
        final_match = re.match(r'arena-(R\d+)-final-(\w+)-game(\d+)-d(\d+)\.txt', name)
        if final_match:
            run_tag_for_game = final_match.group(1)
            faction = final_match.group(2).upper()
            game_num = int(final_match.group(3))
            doom_from_filename = int(final_match.group(4))

            # For "final" games, they're from the completed iteration (iter 13 for R39)
            # We can determine this from the log file
            iter_no = 13  # Default assumption - will be overridden by actual log parsing if available

            # Calculate individual game score and breakdown from trace
            game_score, game_breakdown = calculate_game_score_and_breakdown_from_trace(trace)

            # Convert breakdown to counts format
            breakdown = convert_breakdown_to_counts(game_breakdown) if game_breakdown else ""

            # Determine placement by doom ranking from trace file
            placement, doom_dict = get_doom_ranking_from_trace(trace, faction)

            # Get actual doom from trace
            actual_doom = doom_dict.get(faction, doom_from_filename) if doom_dict else doom_from_filename

            games.append({
                "file": str(trace),
                "run": run_tag_for_game,
                "iter": iter_no,
                "faction": faction,
                "type": "final",
                "doom": actual_doom,
                "is_win": placement == 1,
                "is_arena": True,
                "score": game_score if game_score is not None else 0.0,
                "breakdown": breakdown,
                "placement": placement,
                "doom_dict": doom_dict,
            })
            continue

        # Fall back to OLD format: arena-iter24-bg-best-d41.txt
        old_match = re.match(r'arena-iter(\d+)-(\w+)-(best|first|WIN).*-d(\d+)', name)
        if old_match:
            iter_no = int(old_match.group(1))
            faction = old_match.group(2).upper()

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

            # Convert breakdown from doom-equivalent to actual counts with 0-1 values
            breakdown = convert_breakdown_to_counts(game_breakdown) if game_breakdown else ""

            # Determine placement by doom ranking from trace file
            placement, doom_dict = get_doom_ranking_from_trace(trace, faction)

            games.append({
                "file": str(trace),
                "run": run_tag_for_game,
                "iter": iter_no,  # Local iteration within this run
                "faction": faction,
                "type": old_match.group(3),
                "doom": int(old_match.group(4)),
                "is_win": "WIN" in name,
                "is_arena": True,  # All traces are arena games
                "score": game_score if game_score is not None else 0.0,
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
    # Load from canonical store
    try:
        with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
            store = json.load(f)
        all_games = store.get("games", [])
    except:
        all_games = []

    data = {"selfplay": [], "arena": []}
    for game in all_games:
        # EXCLUDE killed games from progress charts
        if game.get("result") == "killed":
            continue

        target = game.get("type", "arena")
        run = game.get("run", "?")
        iter_num = game.get("iteration", game.get("iter", -1))
        doom = game.get("doom", 0)
        score = game.get("score", 0.0)

        # Extract faction from filename
        filename = game.get("filename", "")
        faction = "UNK"
        faction_match = re.search(r'-(gc|bg|ys|cc)-', filename)
        if faction_match:
            faction = faction_match.group(1).upper()

        # Extract run number for sorting (handle R36_direct format)
        if run.startswith('R'):
            run_base = run[1:].split('_')[0]  # "R36_direct" -> "36"
            try:
                run_num = int(run_base)
            except ValueError:
                run_num = 0
        else:
            run_num = 0

        # Count games in this iter to assign game number
        existing = [g for g in data[target] if g["run"] == run and g["iter"] == iter_num]
        game_num = len(existing) + 1

        data[target].append({
            "run": run,
            "iter": iter_num,
            "game": game_num,
            "run_iter_game": f"{run}_I{iter_num:02d}_G{game_num:02d}",
            "run_iter_game_num": run_num * 1000000 + iter_num * 1000 + game_num,
            "doom": doom,
            "score": score if score is not None else 0.0,
            "spellbooks": game.get("spellbooks", 0),
            "type": target,
            "faction": faction
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
                iter_stats[key] = {"dooms": [], "scores": [], "spellbooks": [], "wins": 0, "total": 0}

            iter_stats[key]["dooms"].append(doom)
            iter_stats[key]["scores"].append(score)
            iter_stats[key]["spellbooks"].append(0)  # TODO: parse from trace file
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
            avg_sbs = sum(stats["spellbooks"]) / len(stats["spellbooks"]) if stats.get("spellbooks") else 0.0
            history.append({
                "run": run,
                "iter": iter_num,
                "avg_doom": round(avg_doom, 1),
                "avg_score": round(avg_score, 3),
                "avg_sbs": round(avg_sbs, 1),
                "type": game_type,
                "wins": stats["wins"],
                "total": stats["total"]
            })

    # Sort by run (descending) then iter (descending)
    history.sort(key=lambda x: (x["run"], x["iter"]), reverse=True)

    return history

def get_performance_history():
    """Get performance history from canonical store."""
    # Load performance history directly from canonical store
    try:
        with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
            store = json.load(f)
        performance_history = store.get("performanceHistory", [])
        if performance_history:
            # Return the pre-built performance history with faction breakdowns
            return performance_history
        # Fallback to old aggregation if performanceHistory is missing
        all_games = store.get("games", [])
    except Exception as e:
        print(f"ERROR loading canonical store: {e}")
        return []

    # LEGACY: Aggregate by (run, iter, type) if performanceHistory not in store
    iter_stats = {}
    for game in all_games:
        # EXCLUDE killed games from performance history (matches progress charts)
        if game.get("result") == "killed":
            continue

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
            iter_stats[key] = {"dooms": [], "scores": [], "spellbooks": [], "wins": 0, "total": 0}

        iter_stats[key]["dooms"].append(doom)
        iter_stats[key]["scores"].append(score)
        iter_stats[key]["spellbooks"].append(game.get("spellbooks", 0))
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
                            # Parse overall wins/total
                            overall_match = re.search(r'overall (\d+)/(\d+)', line)
                            if not overall_match:
                                continue

                            win_count = int(overall_match.group(1))
                            total_games = int(overall_match.group(2))

                            # Parse per-faction doom values
                            parts = line.split('|')
                            if len(parts) < 2:
                                continue

                            faction_parts = parts[1].strip().split()
                            doom_values = []

                            for faction_str in faction_parts:
                                # Format: GC:0/5(d14)
                                match = re.match(r'[A-Z]+:\d+/\d+\(d(\d+)\)', faction_str)
                                if match:
                                    doom = int(match.group(1))
                                    doom_values.append(doom)

                            # Store in cache
                            key = (run_tag, current_iter, "Arena")
                            if doom_values:
                                avg_doom_live = sum(doom_values) / len(doom_values)
                                _live_log_cache[key] = {
                                    "dooms": [avg_doom_live] * total_games,  # Replicate avg for each game
                                    "scores": [0.0] * total_games,  # No score data in log
                                    "spellbooks": [0.0] * total_games,  # No SB data in log
                                    "wins": win_count,
                                    "total": total_games
                                }
                except Exception as e:
                    print(f"Error parsing live log {log_file}: {e}")
                    continue
        except Exception as e:
            print(f"Error reading live logs: {e}")

        _live_log_cache_timestamp = current_time

    # Merge cached live log data into iter_stats (only if not already in canonical store)
    for key, stats in _live_log_cache.items():
        if key not in iter_stats:
            iter_stats[key] = stats

    # Convert to list
    history = []
    for (run, iter_num, game_type), stats in iter_stats.items():
        if stats["total"] > 0:
            avg_doom = sum(stats["dooms"]) / len(stats["dooms"])
            avg_score = sum(stats["scores"]) / len(stats["scores"])

            # Determine PLANNED total for arena games (not completed count)
            if game_type == "Arena":
                # For R40: 1600 games (400 per faction) curriculum training
                # For R39 iter 11+: 1600 games (400 per faction)
                # For R39 iter 10 and earlier: 100 games (25 per faction)
                if run == "R40":
                    planned_total = 1600
                elif run == "R39" and iter_num >= 11:
                    planned_total = 1600
                else:
                    planned_total = 100
            else:
                # Self-play: use actual count
                planned_total = stats["total"]

            history.append({
                "run": run,
                "iter": iter_num,
                "avg_doom": round(avg_doom, 1),
                "avg_score": round(avg_score, 3),
                "type": game_type,
                "wins": stats["wins"],
                "total": planned_total
            })

    # Sort by run (descending) then iter (descending)
    history.sort(key=lambda x: (x["run"], x["iter"]), reverse=True)

    return history

def get_faction_breakdown(run, iteration):
    """Get per-faction breakdown for a specific run/iteration from canonical store."""
    try:
        with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
            store = json.load(f)
        all_games = store.get("games", [])
    except Exception as e:
        print(f"ERROR loading canonical store: {e}")
        return []

    # Filter games for this run/iteration, EXCLUDE killed games
    games = [g for g in all_games
             if g.get('run') == run
             and g.get('iteration') == iteration
             and g.get('result') != 'killed']

    # Group by faction
    factions = {}
    for g in games:
        fname = g['filename'].lower()
        faction = None
        for f in ['gc', 'ys', 'cc', 'bg']:
            if f'-{f}-' in fname:
                faction = f.upper()
                break

        if faction:
            if faction not in factions:
                factions[faction] = {'dooms': [], 'scores': [], 'wins': 0, 'total': 0}
            factions[faction]['dooms'].append(g.get('doom', 0))
            factions[faction]['scores'].append(g.get('score', 0.0))
            factions[faction]['total'] += 1
            if g.get('won'):
                factions[faction]['wins'] += 1

    # Convert to list
    result = []
    for faction in sorted(factions.keys()):
        data = factions[faction]
        if data['total'] > 0:
            avg_doom = sum(data['dooms']) / len(data['dooms'])
            avg_score = sum(data['scores']) / len(data['scores'])
            result.append({
                'faction': faction,
                'games': data['total'],
                'planned': 400,  # Arena mode = 400 games per faction
                'wins': data['wins'],
                'avg_doom': round(avg_doom, 1),
                'avg_score': round(avg_score, 3)
            })

    return result

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
            run = qs.get("run", [None])[0]
            result = qs.get("result", [None])[0]
            page = qs.get("page", ["1"])[0]
            per_page = qs.get("per_page", ["50"])[0]

            if iter_num:
                iter_num = int(iter_num)
            page = int(page)
            per_page = int(per_page)

            # Get games from canonical store
            with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
                store = json.load(f)
            all_games = store.get("games", [])

            # Filter by run/iter/result if specified
            if run:
                all_games = [g for g in all_games if g.get("run") == run]
            if iter_num is not None:
                all_games = [g for g in all_games if g.get("iteration", g.get("iter", -1)) == iter_num]
            if result:
                if result == "killed":
                    all_games = [g for g in all_games if g.get("result") == "killed"]
                elif result == "won":
                    all_games = [g for g in all_games if g.get("won") and g.get("result") != "killed"]
                elif result == "lost":
                    all_games = [g for g in all_games if not g.get("won") and g.get("result") != "killed"]

            # Sort by run and iter descending BEFORE pagination
            all_games.sort(key=lambda g: (g.get("run", "R0"), g.get("iteration", g.get("iter", 0))), reverse=True)

            # Store total count
            total_count = len(all_games)

            # Apply pagination to filtered/sorted games BEFORE expensive formatting
            start_idx = (page - 1) * per_page
            end_idx = start_idx + per_page
            page_games = all_games[start_idx:end_idx]

            # Format ONLY the paginated subset
            formatted = []
            for g in page_games:
                # Get faction from canonical store (or extract from trace_file as fallback)
                trace_filename = g.get("trace_file", g.get("filename", ""))
                faction = g.get("faction", "?")
                if faction == "?" and trace_filename:
                    parts = trace_filename.split("-")
                    if len(parts) >= 4:
                        faction = parts[3].upper()

                # Find actual trace file path
                trace_file = ""
                run_tag = g.get("run", "")
                iter_num = g.get("iteration", g.get("iter", -1))
                game_num = g.get("game_num", 0)
                doom_val = g.get("doom", 0)
                result = g.get("result", "unknown")

                # First try trace_file field from canonical store
                if trace_filename:
                    # trace_filename may already have .txt extension
                    if trace_filename.endswith('.txt'):
                        direct_path = TRACES_DIR / trace_filename
                    else:
                        direct_path = TRACES_DIR / f"{trace_filename}.txt"
                    if direct_path.exists():
                        trace_file = str(direct_path)

                # If not found and not killed, try best/first patterns
                if not trace_file and run_tag and iter_num >= 0 and faction != "?" and result != "killed":
                    # Check for best trace (highest doom game for this faction/iter)
                    best_pattern = f"arena-{run_tag}-iter{iter_num}-{faction.lower()}-best*.txt"
                    best_files = list(TRACES_DIR.glob(best_pattern))
                    if best_files:
                        trace_file = str(best_files[0])
                    elif game_num == 1:
                        # Check for first trace
                        first_pattern = f"arena-{run_tag}-iter{iter_num}-{faction.lower()}-first*.txt"
                        first_files = list(TRACES_DIR.glob(first_pattern))
                        if first_files:
                            trace_file = str(first_files[0])

                # Format placement and length based on result
                result = g.get("result", "unknown")
                placement = "killed" if result == "killed" else g.get("place", g.get("placement", "?"))

                # Format length: "Killed - AP#" for killed, "#APs" for completed
                if result == "killed":
                    kill_ap = g.get("kill_ap")
                    game_length = f"Killed - AP{kill_ap}" if kill_ap else "Killed"
                else:
                    ap_count = g.get("length", g.get("game_length", 0))
                    game_length = f"{ap_count}APs" if ap_count else "?"

                formatted.append({
                    "run": g.get("run", "?"),
                    "iter": g.get("iteration", g.get("iter", -1)),
                    "doom": g.get("doom", 0),
                    "spellbooks": g.get("spellbooks", 0),
                    "score": g.get("score", 0.0),
                    "won": g.get("won", False),
                    "is_win": g.get("won", False),
                    "filename": trace_filename,
                    "file": trace_file if trace_file else "",
                    "filepath": trace_file if trace_file else "",
                    "has_trace": bool(trace_file),
                    "type": g.get("type", "arena"),
                    "is_arena": g.get("type") == "arena",
                    "faction": faction,
                    "placement": placement,
                    "game_length": game_length,
                    "result": result,
                    "breakdown": g.get("breakdown", ""),
                })

            # Return paginated response with metadata
            self.send_json({
                "games": formatted,
                "total_count": total_count,
                "page": page,
                "per_page": per_page,
                "total_pages": (total_count + per_page - 1) // per_page
            })
        elif path == "/api/games/metadata":
            # Return unique runs and iters without loading all game data
            with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
                store = json.load(f)
            all_games = store.get("games", [])

            # Extract unique runs and iters
            runs = sorted(set(g.get("run", "?") for g in all_games if g.get("run")))
            iters = sorted(set(g.get("iteration", g.get("iter", -1)) for g in all_games), reverse=True)

            self.send_json({
                "runs": runs,
                "iters": [i for i in iters if i >= 0]  # Filter out -1 values
            })
        elif path == "/api/weights":
            self.send_json(get_weights())
        elif path.startswith("/api/training-corpus"):
            qs = parse_qs(parsed.query)
            run = qs.get("run", ["R39"])[0]
            iter_num = qs.get("iter", [None])[0]
            faction = qs.get("faction", ["GC"])[0]

            if iter_num:
                iter_num = int(iter_num)

            # Read canonical store
            with open('/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json', 'r') as f:
                store = json.load(f)
            all_games = store.get("games", [])

            # Filter by run, iter, and faction
            filtered = []
            for g in all_games:
                if g.get("run") != run:
                    continue
                if iter_num is not None and g.get("iteration", g.get("iter", -1)) != iter_num:
                    continue
                # Extract faction from filename
                filename = g.get("filename", "")
                game_faction = "?"
                if filename:
                    parts = filename.split("-")
                    if len(parts) >= 4:
                        game_faction = parts[3].upper()
                if game_faction.upper() != faction.upper():
                    continue

                # Determine if game was used for training based on result
                result = g.get("result", "unknown")
                # "included" and "best" games are used for training
                # "excluded" and "worst" games are NOT used
                used_for_training = result in ["included", "best"]

                # Add to results
                filtered.append({
                    "game_num": g.get("game_num", 0),
                    "faction": game_faction,
                    "result": result,
                    "doom": g.get("doom", 0),
                    "score": g.get("score", 0.0),
                    "spellbooks": g.get("spellbooks", 0),
                    "won": g.get("won", False),
                    "used_for_training": used_for_training,
                    "filename": filename,
                    "source_iter": g.get("iteration", g.get("iter", -1))
                })

            # Sort by game number
            filtered.sort(key=lambda g: g.get("game_num", 0))

            # Count selected vs total
            selected_count = len([g for g in filtered if g.get("used_for_training")])
            total_played = len(filtered)

            self.send_json({
                "games": filtered,
                "selected_count": selected_count,
                "total_played": total_played
            })
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
        elif path == "/api/faction-breakdown":
            # Parse query params: run=R39&iteration=12
            query = parse_qs(parsed.query)
            run = query.get('run', [''])[0]
            iteration = int(query.get('iteration', ['0'])[0]) if query.get('iteration') else 0
            self.send_json(get_faction_breakdown(run, iteration))
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
                # trace_file is just a filename, need to find the full path
                trace_path = None
                for traces_dir in TRACES_DIRS:
                    # Try with and without .txt extension
                    for candidate_name in [trace_file, f"{trace_file}.txt"]:
                        candidate = traces_dir / candidate_name
                        if candidate.exists():
                            trace_path = candidate
                            break
                    if trace_path:
                        break

                if not trace_path:
                    self.send_json({"success": False, "message": f"Trace file not found: {trace_file}"})
                else:
                    output = TRACES_DIR / f"replay-{Path(trace_file).stem}.html"
                    success, msg = generate_replay(str(trace_path), str(output))
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
    # Read from dashboard-static.html file instead of embedded string
    html_file = Path(__file__).parent / "dashboard-static.html"
    if html_file.exists():
        return html_file.read_text()

    # Fallback to embedded HTML if file doesn't exist
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
                <div class="stat-label" id="phase-elapsed-label">Phase Elapsed</div>
                <div class="stat-value" id="phase-elapsed">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">Run Elapsed Time</div>
                <div class="stat-value" id="run-elapsed">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">Disk Free</div>
                <div class="stat-value" id="disk-free">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">Claude Storage</div>
                <div class="stat-value" id="claude-storage">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">Current ETA</div>
                <div class="stat-value" id="current-eta">-</div>
            </div>
            <div class="stat">
                <div class="stat-label">20-Iter Milestone</div>
                <div class="stat-value" id="milestone-eta">-</div>
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
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Avg SBs</th>
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Avg APs</th>
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Avg Score</th>
                            <th style="padding:8px;text-align:left;border-bottom:2px solid #0f3460;">Type</th>
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Wins</th>
                            <th style="padding:8px;text-align:right;border-bottom:2px solid #0f3460;">Total</th>
                        </tr>
                    </thead>
                    <tbody id="performance-history-body">
                        <tr><td colspan="9" style="padding:20px;text-align:center;color:#666;">Loading...</td></tr>
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
                <label style="margin-left:15px;">Faction:
                    <select id="faction-select" onchange="loadGames()">
                        <option value="">All</option>
                    </select>
                </label>
                <label style="margin-left:15px;">Result:
                    <select id="result-select" multiple style="height:60px;">
                        <option value="Won" selected>Won</option>
                        <option value="Lost" selected>Lost</option>
                        <option value="Killed" selected>Killed</option>
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
                <thead><tr><th>Run</th><th>Iter</th><th>Type</th><th>Faction</th><th>Result</th><th>Doom</th><th>SBs</th><th>Score</th><th>Winner</th><th>Length</th><th>Place</th><th>Actions</th></tr></thead>
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
                <div>
                    <label style="display:block;margin-bottom:5px;">Factions:</label>
                    <select id="doom-faction-filter" multiple style="width:150px;height:80px;">
                    </select>
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
                <div>
                    <label style="display:block;margin-bottom:5px;">Factions:</label>
                    <select id="score-faction-filter" multiple style="width:150px;height:80px;">
                    </select>
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

                // Progress: show bootstrap or iteration progress
                let progressText = '-';
                if (data.bootstrap_phase) {
                    const phaseName = data.bootstrap_phase_name === 'value' ? 'Value epoch' : 'Policy epoch';
                    progressText = `${phaseName} ${data.bootstrap_epoch}/${data.bootstrap_total_epochs}`;
                } else if (data.current_iter > 0) {
                    if (data.current_game > 0) {
                        progressText = `Iter ${data.current_iter}: game ${data.current_game}/${data.total_games}`;
                    } else {
                        progressText = `Iter ${data.current_iter}: game 1/${data.total_games} in progress`;
                    }
                } else if (data.current_iter === 0) {
                    // Iter 0 is always arena evaluation
                    if (data.current_game > 0) {
                        progressText = `Iter 0 (Arena): game ${data.current_game}/20 complete`;
                    } else {
                        progressText = `Iter 0 (Arena): game 1/20 in progress`;
                    }
                } else {
                    progressText = 'Starting';
                }
                document.getElementById('progress-status').textContent = progressText;

                // Phase elapsed (with dynamic label)
                const phaseLabel = data.phase_label || 'Phase';
                document.getElementById('phase-elapsed-label').textContent = `${phaseLabel} Elapsed`;
                document.getElementById('phase-elapsed').textContent = data.phase_elapsed_human || '-';

                // Run elapsed
                document.getElementById('run-elapsed').textContent = data.run_elapsed_human || '-';

                // Disk free and Claude storage
                document.getElementById('disk-free').textContent = data.disk_free || '-';
                const claudeStorage = data.claude_storage_gb !== undefined ? data.claude_storage_gb + ' GB' : '-';
                document.getElementById('claude-storage').textContent = claudeStorage;

                // Current ETA (epoch or current iteration)
                document.getElementById('current-eta').textContent = data.iter_eta_human || '-';

                // 20-Iter Milestone ETA
                document.getElementById('milestone-eta').textContent = data.run_eta_human || '-';

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
                tbody.innerHTML = '<tr><td colspan="9" style="padding:20px;text-align:center;color:#666;">No data</td></tr>';
                document.getElementById('performance-page-info').textContent = 'No data';
                return;
            }

            const totalPages = Math.ceil(allPerformanceData.length / performancePerPage);
            const start = (performancePage - 1) * performancePerPage;
            const end = start + performancePerPage;
            const pageData = allPerformanceData.slice(start, end);

            const rows = pageData.map(row => {
                const winRate = row.total_games > 0 ? ((row.wins / row.total_games) * 100).toFixed(0) + '%' : '-';
                const avgSBs = row.avg_spellbooks !== undefined ? row.avg_spellbooks.toFixed(1) : '-';
                const avgAPs = row.avg_length !== undefined ? row.avg_length.toFixed(1) : '-';
                const avgDoom = row.avg_doom !== undefined ? row.avg_doom.toFixed(1) : '-';
                const avgScore = row.avg_score !== undefined ? row.avg_score.toFixed(3) : '-';
                const breakdownId = `perf-breakdown-${row.run}-${row.iter}`;

                // Build faction breakdown HTML if factions data exists
                let factionHtml = '';
                if (row.factions && Object.keys(row.factions).length > 0) {
                    factionHtml = Object.entries(row.factions).map(([faction, stats]) => `
                        <div style="padding:8px;background:#16213e;border-radius:4px;margin:5px;">
                            <div style="font-weight:bold;color:#4ade80;margin-bottom:5px;">${faction}</div>
                            <div style="display:grid;grid-template-columns:repeat(3,1fr);gap:8px;font-size:12px;">
                                <div><span style="color:#888;">Games:</span> ${stats.games || 0}</div>
                                <div><span style="color:#888;">Wins:</span> ${stats.wins || 0} (${stats.win_rate ? (stats.win_rate * 100).toFixed(0) : 0}%)</div>
                                <div><span style="color:#888;">Avg Doom:</span> ${stats.avg_doom !== undefined ? stats.avg_doom.toFixed(1) : '-'}</div>
                                <div><span style="color:#888;">Avg SBs:</span> ${stats.avg_sbs !== undefined ? stats.avg_sbs.toFixed(1) : '-'}</div>
                                <div><span style="color:#888;">Total Doom:</span> ${stats.doom || 0}</div>
                                <div><span style="color:#888;">Total SBs:</span> ${stats.sbs || 0}</div>
                            </div>
                        </div>
                    `).join('');
                }

                return `
                    <tr style="border-bottom:1px solid #16213e;cursor:pointer;" onclick="toggleBreakdown('${breakdownId}')">
                        <td style="padding:8px;">${row.run}</td>
                        <td style="padding:8px;">I${String(row.iter).padStart(2, '0')}</td>
                        <td style="padding:8px;text-align:right;">${avgDoom}</td>
                        <td style="padding:8px;text-align:right;">${avgSBs}</td>
                        <td style="padding:8px;text-align:right;">${avgAPs}</td>
                        <td style="padding:8px;text-align:right;">${avgScore}</td>
                        <td style="padding:8px;">${row.type || '-'}</td>
                        <td style="padding:8px;text-align:right;">${row.wins}/${row.total_games} (${winRate})</td>
                        <td style="padding:8px;text-align:right;">${row.total_games}</td>
                    </tr>
                    ${factionHtml ? `<tr id="${breakdownId}" style="display:none;"><td colspan="9" style="background:#0f3460;padding:15px;"><div style="display:grid;grid-template-columns:repeat(2,1fr);gap:10px;">${factionHtml}</div></td></tr>` : ''}
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

            // Get unique runs, iters, and factions
            const runs = [...new Set(allGamesData.map(g => g.run))].sort();
            const iters = [...new Set(allGamesData.map(g => g.iter))].sort((a,b) => b-a);
            const factions = [...new Set(allGamesData.map(g => g.faction))].sort();

            const runSelect = document.getElementById('run-select');
            runSelect.innerHTML = '<option value="">All</option>' +
                runs.map(r => `<option value="${r}">${r}</option>`).join('');

            const iterSelect = document.getElementById('iter-select');
            iterSelect.innerHTML = '<option value="">All</option>' +
                iters.map(i => `<option value="${i}">${i}</option>`).join('');

            const factionSelect = document.getElementById('faction-select');
            factionSelect.innerHTML = '<option value="">All</option>' +
                factions.map(f => `<option value="${f}">${f}</option>`).join('');

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
            const faction = document.getElementById('faction-select').value;
            let filtered = allGamesData;
            if (run) filtered = filtered.filter(g => g.run === run);
            if (iter) filtered = filtered.filter(g => g.iter === parseInt(iter));
            if (faction) filtered = filtered.filter(g => g.faction === faction);
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
                const faction = document.getElementById('faction-select').value;
                const resultSelect = document.getElementById('result-select');
                const selectedResults = Array.from(resultSelect.selectedOptions).map(opt => opt.value);

                let games = allGamesData;

                // Filter by run, iter, faction, and result
                if (run) games = games.filter(g => g.run === run);
                if (iter) games = games.filter(g => g.iter === parseInt(iter));
                if (faction) games = games.filter(g => g.faction === faction);

                // Result filtering (Won/Lost/Killed)
                games = games.filter(g => {
                    const rawResult = g.result || 'unknown';
                    if (rawResult === 'killed') return selectedResults.includes('Killed');
                    if (g.is_win || g.won) return selectedResults.includes('Won');
                    return selectedResults.includes('Lost');
                });

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
                // Map result to Won/Lost/Killed display
                const rawResult = g.result || 'unknown';
                let resultDisplay = 'Unknown';
                if (rawResult === 'killed') {
                    resultDisplay = 'Killed';
                } else if (g.is_win || g.won) {
                    resultDisplay = 'Won';
                } else {
                    resultDisplay = 'Lost';
                }

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
                    <td>${resultDisplay}</td>
                    <td>${g.doom}</td>
                    <td>${g.spellbooks || 0}</td>
                    <td><span class="breakdown" onclick="toggleBreakdown('${breakdownId}')">${g.score !== null ? g.score.toFixed(3) : '-'}</span></td>
                    <td>${winner}</td>
                    <td>${g.game_length || '?'}</td>
                    <td>${placement}</td>
                    <td><button class="secondary" onclick="generateReplay('${g.file}')">View</button></td>
                </tr>
                <tr id="${breakdownId}" style="display:none;">
                    <td colspan="12" style="background:#0f3460;padding:15px;">
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
                tbody.innerHTML = '<tr><td colspan="11" style="text-align:center;padding:20px;color:#e94560;">Error loading games. Check console.</td></tr>';
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
        let doomFactionsFilter = [];
        let scoreFactionsFilter = [];

        async function loadProgress() {
            try {
                const res = await fetch('/api/progress');
                progressData = await res.json();
                console.log('Progress data loaded:', progressData);

                // Populate run, iter, and faction filters
                const allData = [...progressData.selfplay, ...progressData.arena];
                const uniqueRuns = [...new Set(allData.map(d => d.run))].sort();
                const uniqueIters = [...new Set(allData.map(d => d.iter))].sort((a,b) => a-b);
                const uniqueFactions = [...new Set(allData.map(d => d.faction))].sort();

                const doomRunSelect = document.getElementById('doom-run-filter');
                const scoreRunSelect = document.getElementById('score-run-filter');
                const winsRunSelect = document.getElementById('wins-run-filter');
                const doomIterSelect = document.getElementById('doom-iter-filter');
                const scoreIterSelect = document.getElementById('score-iter-filter');
                const doomFactionSelect = document.getElementById('doom-faction-filter');
                const scoreFactionSelect = document.getElementById('score-faction-filter');

                doomRunSelect.innerHTML = uniqueRuns.map(r => `<option value="${r}" selected>${r}</option>`).join('');
                scoreRunSelect.innerHTML = uniqueRuns.map(r => `<option value="${r}" selected>${r}</option>`).join('');
                winsRunSelect.innerHTML = uniqueRuns.map(r => `<option value="${r}" selected>${r}</option>`).join('');
                doomIterSelect.innerHTML = uniqueIters.map(i => `<option value="${i}" selected>${i}</option>`).join('');
                scoreIterSelect.innerHTML = uniqueIters.map(i => `<option value="${i}" selected>${i}</option>`).join('');
                doomFactionSelect.innerHTML = uniqueFactions.map(f => `<option value="${f}" selected>${f}</option>`).join('');
                scoreFactionSelect.innerHTML = uniqueFactions.map(f => `<option value="${f}" selected>${f}</option>`).join('');

                // Initialize with all runs, iters, and factions selected
                doomRunsFilter = uniqueRuns;
                scoreRunsFilter = uniqueRuns;
                winsRunsFilter = uniqueRuns;
                doomItersFilter = uniqueIters;
                scoreItersFilter = uniqueIters;
                doomFactionsFilter = uniqueFactions;
                scoreFactionsFilter = uniqueFactions;

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
                doomFactionSelect.onchange = () => {
                    doomFactionsFilter = Array.from(doomFactionSelect.selectedOptions).map(o => o.value);
                    drawDoomChart();
                };
                scoreFactionSelect.onchange = () => {
                    scoreFactionsFilter = Array.from(scoreFactionSelect.selectedOptions).map(o => o.value);
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

            // Filter by runs, iters, and factions
            data = data.filter(d => doomRunsFilter.includes(d.run) && doomItersFilter.includes(d.iter) && doomFactionsFilter.includes(d.faction));

            if (!data.length) {
                ctx.fillStyle = '#888';
                ctx.font = '16px sans-serif';
                ctx.fillText('No data', w/2 - 30, h/2);
                return;
            }

            // Sort by run_iter_game_num to ensure proper ordering
            data.sort((a, b) => a.run_iter_game_num - b.run_iter_game_num);

            // Fixed Y-axis range
            const minY = 0;
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

            // Draw overall run regression lines
            const uniqueRuns = [...new Set(positionedData.map(d => d.run))];
            uniqueRuns.forEach(run => {
                const runData = positionedData.filter(d => d.run === run);
                if (runData.length < 2) return;

                // Calculate regression across entire run
                const n = runData.length;
                const sx = runData.reduce((s, d) => s + d.xPos, 0);
                const sy = runData.reduce((s, d) => s + d.doom, 0);
                const sxy = runData.reduce((s, d) => s + d.xPos * d.doom, 0);
                const sx2 = runData.reduce((s, d) => s + d.xPos * d.xPos, 0);
                const m = (n * sxy - sx * sy) / (n * sx2 - sx * sx);
                const b = (sy - m * sx) / n;

                // Draw overall regression line (solid, thicker, white)
                const x1 = 60 + (runData[0].xPos / rangeX) * (w - 80);
                const x2 = 60 + (runData[runData.length - 1].xPos / rangeX) * (w - 80);
                const y1 = h - 60 - ((m * runData[0].xPos + b - minY) / rangeY) * (h - 80);
                const y2 = h - 60 - ((m * runData[runData.length - 1].xPos + b - minY) / rangeY) * (h - 80);

                ctx.strokeStyle = '#ffffff';
                ctx.lineWidth = 3;
                ctx.setLineDash([]);
                ctx.beginPath();
                ctx.moveTo(x1, y1);
                ctx.lineTo(x2, y2);
                ctx.stroke();

                // Add to legend
                ctx.fillStyle = '#ffffff';
                ctx.font = '11px sans-serif';
                ctx.fillText(`${run} OVERALL m=${m.toFixed(2)}`, w - 200, 25);
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

            // Filter by runs, iters, and factions
            data = data.filter(d => scoreRunsFilter.includes(d.run) && scoreItersFilter.includes(d.iter) && scoreFactionsFilter.includes(d.faction));

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

            // Draw overall run regression lines
            const uniqueRuns = [...new Set(positionedData.map(d => d.run))];
            uniqueRuns.forEach(run => {
                const runData = positionedData.filter(d => d.run === run);
                if (runData.length < 2) return;

                // Calculate regression across entire run
                const n = runData.length;
                const sx = runData.reduce((s, d) => s + d.xPos, 0);
                const sy = runData.reduce((s, d) => s + d.score, 0);
                const sxy = runData.reduce((s, d) => s + d.xPos * d.score, 0);
                const sx2 = runData.reduce((s, d) => s + d.xPos * d.xPos, 0);
                const m = (n * sxy - sx * sy) / (n * sx2 - sx * sx);
                const b = (sy - m * sx) / n;

                // Draw overall regression line (solid, thicker, white)
                const x1 = 60 + (runData[0].xPos / rangeX) * (w - 80);
                const x2 = 60 + (runData[runData.length - 1].xPos / rangeX) * (w - 80);
                const y1 = h - 60 - ((m * runData[0].xPos + b - minY) / rangeY) * (h - 80);
                const y2 = h - 60 - ((m * runData[runData.length - 1].xPos + b - minY) / rangeY) * (h - 80);

                ctx.strokeStyle = '#ffffff';
                ctx.lineWidth = 3;
                ctx.setLineDash([]);
                ctx.beginPath();
                ctx.moveTo(x1, y1);
                ctx.lineTo(x2, y2);
                ctx.stroke();

                // Add to legend
                ctx.fillStyle = '#ffffff';
                ctx.font = '11px sans-serif';
                ctx.fillText(`${run} OVERALL m=${m.toFixed(4)}`, w - 200, 25);
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

        function refreshActiveTab() {
            // Refresh data for the currently active tab
            const activeContent = document.querySelector('.content.active');
            if (!activeContent) return;

            const tabId = activeContent.id;
            console.log('Refreshing tab:', tabId);

            if (tabId === 'status') {
                updateStatus();
                loadPerformanceHistory();
            } else if (tabId === 'control') {
                loadCheckpoints();
            } else if (tabId === 'games') {
                loadIters();
            } else if (tabId === 'progress') {
                loadProgress();
            }
        }

        function initPage() {
            const hash = window.location.hash.substring(1);
            if (hash) {
                showTab(hash);
            }
            // Initial load
            updateStatus();
            loadPerformanceHistory();
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


def rebuild_canonical_store():
    """Rebuild canonical_games.json from trace files."""
    try:
        print("Rebuilding canonical store...", flush=True)
        result = subprocess.run(
            ["python3", "rebuild_canonical_clean.py"],
            cwd=str(Path(__file__).parent),
            capture_output=True,
            text=True,
            timeout=30
        )
        if result.returncode == 0:
            print("Canonical store rebuilt successfully", flush=True)
        else:
            print(f"Canonical store rebuild failed: {result.stderr}", flush=True)
    except Exception as e:
        print(f"Error rebuilding canonical store: {e}", flush=True)

def auto_rebuild_worker():
    """Background thread that rebuilds canonical store periodically when run is active."""
    global _canonical_rebuild_stop
    while not _canonical_rebuild_stop:
        # Check if PolicyRun is running
        try:
            result = subprocess.run(["pgrep", "-f", "PolicyRun"], capture_output=True, text=True)
            running = bool(result.stdout.strip())
        except:
            running = False

        if running:
            rebuild_canonical_store()

        # Sleep 10 seconds, but check stop flag every second
        for _ in range(10):
            if _canonical_rebuild_stop:
                break
            time.sleep(1)

def main():
    global _canonical_rebuild_thread, _canonical_rebuild_stop

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

    # Start auto-rebuild thread
    _canonical_rebuild_thread = threading.Thread(target=auto_rebuild_worker, daemon=True)
    _canonical_rebuild_thread.start()

    print(f"Brain Dashboard running:")
    print(f"  Local:   http://localhost:{port}")
    print(f"  Network: http://{local_ip}:{port}")
    print("  Auto-rebuild: enabled (10s interval when run active)")
    print("Press Ctrl+C to stop")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down...")
        _canonical_rebuild_stop = True
        if _canonical_rebuild_thread:
            _canonical_rebuild_thread.join(timeout=2)
        server.shutdown()


if __name__ == "__main__":
    main()
