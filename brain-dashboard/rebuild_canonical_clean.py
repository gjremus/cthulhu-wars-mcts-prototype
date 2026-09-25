#!/usr/bin/env python3
"""
Rebuild canonical store from:
1. all-games.txt files (completed iterations)
2. Live PolicyRun logs (in-progress run)
"""
import json
import os
import re
from pathlib import Path
from datetime import datetime

def parse_all_games_file(filepath):
    """Parse an all-games.txt file and return list of game dicts."""
    filename = os.path.basename(filepath)
    match = re.match(r'arena-([^-]+)-iter(\d+)-(\w+)-all-games\.txt', filename)
    if not match:
        return []

    run_tag = match.group(1)
    iteration = int(match.group(2))
    faction = match.group(3).upper()

    games = []
    with open(filepath, 'r') as f:
        for line in f:
            if not line.startswith('game='):
                continue

            game_match = re.search(r'game=(\d+)', line)
            doom_match = re.search(r'doom=(\d+)', line)
            score_match = re.search(r'score=([0-9.Ee+-]+)', line)
            won_match = re.search(r'won=(true|false)', line)
            leader_match = re.search(r'leader=(\d+)', line)
            all_doom_match = re.search(r'all_doom=\[([^\]]+)\]', line)
            breakdown_match = re.search(r'breakdown=\[([^\]]+)\]', line)

            if not all([game_match, doom_match, score_match, won_match]):
                continue

            game_num = int(game_match.group(1))
            doom = int(doom_match.group(1))
            score = float(score_match.group(1))
            won = won_match.group(1) == 'true'
            leader = int(leader_match.group(1)) if leader_match else 0
            breakdown = breakdown_match.group(1) if breakdown_match else ''

            # Parse all_doom and calculate placement
            placement = "?"
            if all_doom_match:
                # Parse "CC=44 YS=29 BG=18 GC=5" format
                all_doom_str = all_doom_match.group(1)
                doom_dict = {}
                for entry in all_doom_str.split():
                    if '=' in entry:
                        f, d = entry.split('=')
                        doom_dict[f] = int(d)

                # Rank by doom (highest first)
                ranked = sorted(doom_dict.items(), key=lambda x: -x[1])
                # Find this faction's rank
                for rank, (f, d) in enumerate(ranked, 1):
                    if f == faction.upper():
                        if rank == 1:
                            placement = "1st"
                        elif rank == 2:
                            placement = "2nd"
                        elif rank == 3:
                            placement = "3rd"
                        elif rank == 4:
                            placement = "4th"
                        break

            try:
                timestamp = int(Path(filepath).stat().st_mtime)
            except:
                timestamp = 0

            games.append({
                'run': run_tag,
                'iteration': iteration,
                'type': 'arena',
                'doom': doom,
                'score': score,
                'won': won,
                'game_num': game_num,
                'filename': filename.replace(f"-{faction.lower()}-all-games.txt", f"-{faction.lower()}-game{game_num}"),
                'filepath': str(filepath),
                'timestamp': timestamp,
                'game_length': 0,
                'era': 'new',
                'placement': placement,
                'breakdown': breakdown
            })

    return games

def parse_individual_trace_files():
    """Parse individual arena-R*-iter*-*-game*.txt trace files for in-progress iterations."""
    trace_dir = '/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces'
    games = []

    # Pattern 1: Completed games - arena-R39-iter9-gc-game1-d12.txt
    trace_files = list(Path(trace_dir).glob("arena-R*-iter*-*-game*-d*.txt"))

    # Pattern 2: Killed games - arena-R39-iter9-gc-game1-KILLED.txt
    killed_files = list(Path(trace_dir).glob("arena-R*-iter*-*-game*-KILLED.txt"))

    # Process killed games first
    for filepath in killed_files:
        filename = filepath.name
        match = re.match(r'arena-R(\d+)-iter(\d+)-([a-z]+)-game(\d+)-KILLED\.txt', filename)
        if not match:
            continue

        run_num = int(match.group(1))
        iteration = int(match.group(2))
        faction = match.group(3).upper()
        game_num = int(match.group(4))

        run_tag = f"R{run_num}"

        # Read the trace file to get stats
        try:
            content = filepath.read_text()
            doom_match = re.search(r'Doom: (\d+)', content)
            ap_match = re.search(r'Estimated APs: (\d+)', content)
            score_match = re.search(r'Interim Score \(0-1\): ([0-9.]+)', content)
            breakdown_match = re.search(r'Shaping breakdown: ([^\n]+)', content)
            # Extract kill_ap from reason: "Doom too low at AP~15: 23 < 25"
            kill_reason_match = re.search(r'at AP~(\d+)', content)

            doom = int(doom_match.group(1)) if doom_match else 0
            ap_length = int(ap_match.group(1)) if ap_match else 0
            kill_ap = int(kill_reason_match.group(1)) if kill_reason_match else None
            score = float(score_match.group(1)) if score_match else 0.0
            breakdown = breakdown_match.group(1) if breakdown_match else ''
        except Exception as e:
            print(f"Error parsing killed game {filename}: {e}")
            continue

        try:
            timestamp = int(Path(filepath).stat().st_mtime)
        except:
            timestamp = 0

        games.append({
            'run': run_tag,
            'iteration': iteration,
            'type': 'arena',
            'doom': doom,
            'score': score,
            'won': False,
            'game_num': game_num,
            'filename': filename,
            'filepath': str(filepath),
            'timestamp': timestamp,
            'game_length': ap_length,
            'era': 'new',
            'placement': 'N/A',
            'breakdown': breakdown,
            'result': 'killed',
            'kill_ap': kill_ap
        })

    # Process completed games
    for filepath in trace_files:
        filename = filepath.name
        match = re.match(r'arena-R(\d+)-iter(\d+)-([a-z]+)-game(\d+)-d(\d+)\.txt', filename)
        if not match:
            continue

        run_num = int(match.group(1))
        iteration = int(match.group(2))
        faction = match.group(3).upper()
        game_num = int(match.group(4))
        doom = int(match.group(5))

        run_tag = f"R{run_num}"

        # Read the trace file to get score and result
        try:
            content = filepath.read_text()
            # Try new format first (FINAL_SCORE=), then old format (Score (0-1):)
            score_match = re.search(r'FINAL_SCORE=([0-9.eE+-]+)', content)
            if not score_match:
                score_match = re.search(r'Score \(0-1\): ([0-9.]+)', content)

            result_match = re.search(r'Result: (WIN|LOSS)', content)
            # Try new format first (BREAKDOWN=), then old format (Shaping breakdown:)
            breakdown_match = re.search(r'BREAKDOWN=([^\n]+)', content)
            if not breakdown_match:
                breakdown_match = re.search(r'Shaping breakdown: ([^\n]+)', content)

            if not score_match:
                continue

            score = float(score_match.group(1))
            won = result_match.group(1) == 'WIN' if result_match else False
            breakdown = breakdown_match.group(1) if breakdown_match else ''

            # Count ActionPhaseAction occurrences for game length
            game_length = content.count('ActionPhaseAction')

            # Calculate placement from breakdown or ALL_DOOM
            placement = "?"

            # Try new format first (ALL_DOOM=BG=31 YS=27 GC=13 CC=4)
            all_doom_match = re.search(r'ALL_DOOM=([^\n]+)', content)
            if all_doom_match:
                doom_scores = {}
                all_doom_str = all_doom_match.group(1)
                for part in all_doom_str.split():
                    if '=' in part:
                        f, d = part.split('=')
                        doom_scores[f.strip()] = int(d)
                # Rank by doom (highest first)
                if doom_scores:
                    ranked = sorted(doom_scores.items(), key=lambda x: -x[1])
                    # Find this faction's rank
                    for rank, (f, d) in enumerate(ranked, 1):
                        if f == faction.upper():
                            if rank == 1:
                                placement = "1st"
                            elif rank == 2:
                                placement = "2nd"
                            elif rank == 3:
                                placement = "3rd"
                            elif rank == 4:
                                placement = "4th"
                            break
            elif breakdown:
                # Parse old "GC=0.28, BG=0.97, YS=0.74, CC=0.40" format
                doom_scores = {}
                for part in breakdown.split(','):
                    if '=' in part:
                        f, s = part.strip().split('=')
                        doom_scores[f.strip()] = float(s)
                # Rank by score (highest first)
                if doom_scores:
                    ranked = sorted(doom_scores.items(), key=lambda x: -x[1])
                    # Find this faction's rank
                    for rank, (f, s) in enumerate(ranked, 1):
                        if f == faction.upper():
                            if rank == 1:
                                placement = "1st"
                            elif rank == 2:
                                placement = "2nd"
                            elif rank == 3:
                                placement = "3rd"
                            elif rank == 4:
                                placement = "4th"
                            break

            games.append({
                'run': run_tag,
                'iteration': iteration,
                'type': 'arena',
                'doom': doom,
                'score': score,
                'won': won,
                'game_num': game_num,
                'filename': filename.replace('.txt', ''),
                'filepath': str(filepath),
                'timestamp': int(filepath.stat().st_mtime),
                'game_length': game_length,
                'era': 'new',
                'placement': placement,
                'breakdown': breakdown
            })
        except Exception as e:
            print(f"Error parsing trace file {filepath}: {e}")
            continue

    return games

def parse_live_policyrun_logs():
    """Parse live PolicyRun logs - ONLY return aggregate stats, NO synthetic games."""
    # This function used to generate fake game data - that was wrong.
    # Arena trace log has winners but not doom scores.
    # Main log >>> overall lines have doom but only for completed iterations.
    # Solution: Return NOTHING here. Only show iterations with real all-games.txt files.
    return []

def parse_live_policyrun_logs_OLD_BROKEN():
    """DISABLED - was generating fake data."""
    import random
    games = []

    tmp_dir = Path('/tmp')
    log_files = list(tmp_dir.glob('sp_R*.log'))

    for log_file in log_files:
        try:
            content = log_file.read_text()

            # Extract run tag from checkpoint or filename
            run_tag = None
            checkpoint_match = re.search(r'Loaded checkpoint \[tag=(\w+)', content)
            if checkpoint_match:
                run_tag = checkpoint_match.group(1).split('_')[0]
            else:
                filename_match = re.match(r'sp_(\w+)', log_file.name)
                if filename_match:
                    run_tag = filename_match.group(1).split('_')[0]

            if not run_tag:
                continue

            lines = content.split('\n')
            current_iter = None

            for line in lines:
                iter_match = re.match(r'^iter (\d+) \| arena', line)
                if iter_match:
                    current_iter = int(iter_match.group(1))

                # Parse: >>> overall 0/20 = 0% | GC:0/5(d14) BG:0/5(d9) YS:0/5(d4) CC:0/5(d5)
                if '>>> overall' in line and current_iter is not None:
                    parts = line.split('|')
                    if len(parts) < 2:
                        continue

                    # Parse per-faction stats
                    faction_stats = {}
                    faction_parts = parts[1].strip().split()
                    for faction_str in faction_parts:
                        match = re.match(r'([A-Z]+):(\d+)/(\d+)\(d(\d+)\)', faction_str)
                        if match:
                            faction = match.group(1)
                            wins = int(match.group(2))
                            total = int(match.group(3))
                            avg_doom = int(match.group(4))
                            faction_stats[faction] = {
                                'wins': wins,
                                'total': total,
                                'avg_doom': avg_doom
                            }

                    if not faction_stats:
                        continue

                    # Generate synthetic games - one entry per game (no duplicates)
                    game_num = 1
                    factions = list(faction_stats.keys())
                    games_per_faction = faction_stats[factions[0]]['total']

                    # Create games for each faction as brain
                    # Bot average doom (estimated from historical data: bots typically get 20-25 doom)
                    BOT_AVG_DOOM = 22
                    BOT_DOOM_VARIANCE = 5

                    for brain_faction in factions:
                        brain_avg_doom = faction_stats[brain_faction]['avg_doom']

                        for game_idx in range(games_per_faction):
                            # Use fixed random seed for this specific game so data doesn't change
                            seed = hash(f"{run_tag}_{current_iter}_{brain_faction}_{game_num}") % (2**31)
                            rng = random.Random(seed)

                            # Generate doom for brain (vary around its average)
                            brain_doom = max(1, brain_avg_doom + rng.randint(-4, 4))

                            # Generate doom for 3 bot opponents (all around bot average)
                            game_dooms = {brain_faction: brain_doom}
                            for faction in factions:
                                if faction != brain_faction:
                                    bot_doom = max(1, BOT_AVG_DOOM + rng.randint(-BOT_DOOM_VARIANCE, BOT_DOOM_VARIANCE))
                                    game_dooms[faction] = bot_doom

                            # Rank ALL 4 factions by doom (descending) to determine placement
                            ranked = sorted(game_dooms.items(), key=lambda x: (-x[1], x[0]))  # Sort by doom desc, then faction name for ties

                            # Find brain faction's rank
                            brain_rank = next((i for i, (f, d) in enumerate(ranked, 1) if f == brain_faction), 4)
                            placement = {1: "1st", 2: "2nd", 3: "3rd", 4: "4th"}.get(brain_rank, "4th")

                            # Won = 1st place
                            won = (placement == "1st")

                            # Calculate synthetic score from doom (higher doom = higher score)
                            base_score = brain_doom / 60.0
                            score = base_score + rng.uniform(-0.05, 0.05)
                            score = max(0.01, min(0.99, score))

                            games.append({
                                'run': run_tag,
                                'iteration': current_iter,
                                'type': 'arena',
                                'doom': brain_doom,
                                'score': round(score, 4),
                                'won': won,
                                'game_num': game_num,
                                'filename': f"arena-{run_tag}-iter{current_iter}-{brain_faction.lower()}-game{game_num}",
                                'filepath': str(log_file),
                                'timestamp': int(log_file.stat().st_mtime),
                                'game_length': 0,
                                'era': 'new',
                                'placement': placement
                            })
                            game_num += 1

        except Exception as e:
            print(f"Error parsing live log {log_file}: {e}")
            continue

    return games

if __name__ == '__main__':
    trace_dir = '/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces'
    canonical_path = '/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json'

    # Find all all-games.txt files
    all_games_files = sorted(Path(trace_dir).glob("*-all-games.txt"))
    print(f"Found {len(all_games_files)} all-games.txt files")

    # Parse all files
    all_games = []
    for filepath in all_games_files:
        games = parse_all_games_file(filepath)
        all_games.extend(games)

    print(f"Parsed {len(all_games)} games from all-games.txt files")

    # Parse individual trace files (in-progress iterations)
    trace_games = parse_individual_trace_files()

    # DEDUPLICATE trace games - keep only newest file per (run, iter, faction, game_num)
    # Multiple attempts at same iteration create duplicate game numbers with different timestamps
    trace_by_key = {}
    for g in trace_games:
        faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
        faction = faction_match.group(1) if faction_match else ''
        key = (g['run'], g['iteration'], faction, g['game_num'])

        # Keep only the newest file per key
        if key not in trace_by_key or g['timestamp'] > trace_by_key[key]['timestamp']:
            trace_by_key[key] = g

    trace_games_deduped = list(trace_by_key.values())
    print(f"Deduplicated {len(trace_games)} trace files -> {len(trace_games_deduped)} unique games")

    # Filter out traces that are already in all_games
    # (completed iterations have both all-games.txt AND individual traces)
    # NOTE: Must include faction in key, as game numbers are per-faction
    existing_keys = set()
    for g in all_games:
        # Extract faction from filename
        faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
        if faction_match:
            faction = faction_match.group(1)
            existing_keys.add((g['run'], g['iteration'], faction, g['game_num']))
        else:
            # Fallback: use key without faction for games without faction info
            existing_keys.add((g['run'], g['iteration'], '', g['game_num']))

    trace_games_filtered = []
    for g in trace_games_deduped:
        # Extract faction from filename
        faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
        faction = faction_match.group(1) if faction_match else ''
        key = (g['run'], g['iteration'], faction, g['game_num'])
        if key not in existing_keys:
            trace_games_filtered.append(g)

    all_games.extend(trace_games_filtered)
    print(f"Parsed {len(trace_games_filtered)} games from individual trace files")

    # Parse live PolicyRun logs
    live_games = parse_live_policyrun_logs()
    all_games.extend(live_games)
    print(f"Parsed {len(live_games)} games from live logs")
    print(f"Total: {len(all_games)} games")

    # SPECIAL CASE: Filter R39 iter 11 to top 100 per faction
    # Iteration 11 was misconfigured to generate 1600 games (400 per faction)
    # instead of 400 TOTAL (100 per faction). Filter to match intended size.
    r39_iter11_games = [g for g in all_games if g['run'] == 'R39' and g['iteration'] == 11]
    if r39_iter11_games:
        print(f"\nFiltering R39 iter 11 from {len(r39_iter11_games)} to top 100 per faction...")
        other_games = [g for g in all_games if not (g['run'] == 'R39' and g['iteration'] == 11)]

        # Group by faction
        by_faction = {}
        for g in r39_iter11_games:
            faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
            if faction_match:
                faction = faction_match.group(1).upper()
                if faction not in by_faction:
                    by_faction[faction] = []
                by_faction[faction].append(g)

        # Keep top 100 per faction by score
        filtered_iter11 = []
        for faction in sorted(by_faction.keys()):
            faction_games = sorted(by_faction[faction], key=lambda x: -x['score'])
            top_100 = faction_games[:100]
            filtered_iter11.extend(top_100)
            print(f"  {faction}: {len(faction_games)} -> 100 games (score {top_100[0]['score']:.3f} to {top_100[-1]['score']:.3f})")

        all_games = other_games + filtered_iter11
        print(f"R39 iter 11 filtered: {len(r39_iter11_games)} -> {len(filtered_iter11)} games")
        print(f"Total after filter: {len(all_games)} games")

    # Count by run and iteration
    by_run_iter = {}
    for g in all_games:
        key = (g['run'], g['iteration'])
        by_run_iter[key] = by_run_iter.get(key, 0) + 1

    print(f"\nGames by run/iteration:")
    for key in sorted(by_run_iter.keys())[:20]:
        print(f"  {key[0]} iter {key[1]}: {by_run_iter[key]} games")

    # Fill in game_length for games from all-games.txt files (game_length=0)
    print(f"\nFilling in game_length from individual trace files...")
    trace_dir = Path('/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces')
    filled_count = 0
    for g in all_games:
        if g.get('game_length', 0) == 0 and g.get('result') != 'killed':
            # Construct individual trace filename
            # Format: arena-R39-iter12-bg-game1-d11.txt
            faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
            if faction_match:
                faction = faction_match.group(1)
                # Try to find the trace file
                pattern = f"arena-{g['run']}-iter{g['iteration']}-{faction}-game{g['game_num']}-d*.txt"
                matches = list(trace_dir.glob(pattern))
                if matches:
                    try:
                        content = matches[0].read_text()
                        g['game_length'] = content.count('ActionPhaseAction')
                        filled_count += 1
                    except:
                        pass
    print(f"  Filled game_length for {filled_count} games")

    # Calculate "result" field for each game
    print(f"\nCalculating result field (best/worst/included/excluded)...")

    # Group games by (run, faction, iteration)
    by_run_faction_iter = {}
    for g in all_games:
        # Extract faction from filename
        faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
        if faction_match:
            faction = faction_match.group(1).upper()
        else:
            faction = "UNK"

        key = (g['run'], faction, g['iteration'])
        if key not in by_run_faction_iter:
            by_run_faction_iter[key] = []
        by_run_faction_iter[key].append(g)

    # Calculate median AND 75th percentile per faction/iteration (excluding killed games)
    medians = {}
    percentile_75 = {}
    for key, games in by_run_faction_iter.items():
        # Filter out killed games when calculating thresholds
        scores = sorted([g['score'] for g in games if g.get('result') != 'killed'])
        if scores:
            median = scores[len(scores) // 2]
            # 75th percentile = score at 75% position
            p75_idx = int(len(scores) * 0.75)
            p75 = scores[min(p75_idx, len(scores) - 1)]
            medians[key] = median
            percentile_75[key] = p75

    # Assign result field to each game
    for g in all_games:
        # Skip killed games - they already have result='killed' and should not be included in training
        if g.get('result') == 'killed':
            continue

        faction_match = re.search(r'-(gc|bg|ys|cc)-', g['filename'])
        faction = faction_match.group(1).upper() if faction_match else "UNK"

        key = (g['run'], faction, g['iteration'])
        games_in_group = by_run_faction_iter.get(key, [])

        if not games_in_group:
            g['result'] = 'unknown'
            continue

        # Find best and worst in this group (excluding killed games)
        scores = [game['score'] for game in games_in_group if game.get('result') != 'killed']
        if not scores:
            g['result'] = 'unknown'
            continue

        best_score = max(scores)
        worst_score = min(scores)

        if g['score'] == best_score:
            g['result'] = 'best'
        elif g['score'] == worst_score:
            g['result'] = 'worst'
        else:
            # Check against previous iteration's median
            prev_key = (g['run'], faction, g['iteration'] - 1)
            prev_median = medians.get(prev_key, 0.0)

            if g['iteration'] == 0 or prev_median == 0.0:
                # Iter 0 or no previous iteration: use 75% of current best
                threshold = best_score * 0.75
                g['result'] = 'included' if g['score'] >= threshold else 'excluded'
            else:
                # Iter 1+: MORE AGGRESSIVE - use previous 75th percentile instead of median
                # This trains only on clearly good games, not "barely better than last time"
                prev_p75_key = (g['run'], faction, g['iteration'] - 1)
                prev_p75 = percentile_75.get(prev_p75_key, prev_median)

                # Fallback to median if 75th percentile not available
                threshold = prev_p75 if prev_p75 > 0 else prev_median
                g['result'] = 'included' if g['score'] > threshold else 'excluded'

    # Build canonical store
    store = {
        'generated': datetime.now().strftime('%Y-%m-%d %H:%M'),
        'total_games': len(all_games),
        'games': all_games
    }

    # Save
    with open(canonical_path, 'w') as f:
        json.dump(store, f, indent=2)

    print(f"\nSaved {len(all_games)} games to {canonical_path}")
