# KILLED Games - Partial Trace System

## Overview

During brain training, games that perform poorly are terminated early ("KILLED") to save compute time. These KILLED games still produce partial trace files containing all actions up to the termination point.

## How It Works

### Termination Criteria

Games are checked periodically (approximately every 5 Action Points) against doom thresholds:
- AP ~5: doom must be >= 8
- AP ~10: doom must be >= 15  
- AP ~15: doom must be >= 25

If a game's doom falls below the threshold, it is terminated immediately.

### Trace File Format

KILLED trace files have the same format as complete games:
- **Action sequence**: All actions taken up to termination point
- **Result marker**: `Result: KILLED`
- **Doom achieved**: Final doom value when killed
- **Interim score**: Score calculated at termination
- **Actions taken**: Total action count
- **Estimated APs**: Approximate Action Points when killed
- **Breakdown**: Per-faction interim scores
- **Termination reason**: Why the game was killed (e.g., "Doom too low at AP~5: 5 < 8 (threshold)")

### File Naming

KILLED traces use the same naming convention as complete games:
```
arena-{run}-iter{iteration}-{faction}-game{number}-KILLED.txt
```

Example: `arena-R39-iter14-bg-game626-KILLED.txt`

## Viewing KILLED Games

KILLED game traces are viewable in the dashboard:
1. Filter games by "KILLED" result
2. Click "View" button to generate replay
3. Replay shows all actions up to termination point
4. Game ends with termination message

## Statistics

For R39 iteration 14:
- Total games: 650+ (in progress)
- KILLED games: ~631 (96-97% kill rate)
- Complete games: ~19 (3-4%)

High kill rates indicate the brain is performing poorly and needs reward/training adjustments.

## Technical Implementation

### Trace File Creation
- PolicyRun.scala writes partial trace when game is killed
- All actions, state, and termination reason included
- File size: ~171 lines / 345 bytes typical for early termination

### Dashboard Detection
- server.py checks for trace file existence
- Filename stored in canonical_games.json with `.txt` extension
- `has_trace` boolean indicates if file exists and is viewable

### Replay Generation
- build-replay.py processes KILLED traces same as complete games
- Termination reason displayed at end of replay
- Partial game state accurately reconstructed

## Purpose

KILLED traces serve multiple purposes:
1. **Debugging**: Understand why brain makes poor early decisions
2. **Analysis**: Identify patterns in failed games
3. **Training data**: Use negative examples (though currently excluded from training corpus)
4. **Monitoring**: Track kill rate as health metric for training progress
