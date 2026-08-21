# Brain Training Dashboard - Changelog

## 2026-08-21: Champion Selection, Caching, R26 Support

### Champion Head-to-Head Selection (NEW)
**User Directive:** "IF THE BRAIN WINS MORE THAN HALF OF ITS GAMES AGAINST THE 'CHAMPION' - THEN THAT'S THE NEW CHAMPION."

**Implementation:**
- **Files:** `mcts-src/PolicyRun.scala`, `mcts-src/Arena.scala`
- **Function:** `Arena.evaluateVsChampion(current, champion, sims, nGames)`
- **Behavior:**
  - Each iteration after self-play: Play 20 head-to-head games
  - Current brain gets 2 seats, champion gets 2 seats (mixed games)
  - Promote if: win rate >50% OR current 0-1 score > champion 0-1 score
  - Replaces old avgDoom comparison (which was noisy)

**Why Better:**
- Direct competition signal (who actually wins)
- Win rate >50% = objectively stronger
- 0-1 score bounded and normalized (not noisy like doom)
- Clearer promotion criteria

### Score >1.0 Bug Fix (Option C)
**File:** `mcts-src/Outcome.scala` lines 104-119

**Problem:**
- Placement bonus was added AFTER [0,1] clamping
- Caused scores up to 1.3, corrupting all training since Aug 14
- Neural networks require outputs in [0,1] range (sigmoid activation)

**Fix:**
- Lowered ceiling to 0.7 to make room for placement bonus
- Max placement bonus is +0.3 (0.1 per place above last)
- Total guaranteed ≤1.0: 0.7 + 0.3 = 1.0

**Code:**
```scala
val cappedWithRoom = 0.7 * NonWinnerCeiling * math.max(0.0, math.min(1.0, shaping))
val placementBonus = if (winners.isEmpty) {
    StalematePenalty * cappedWithRoom
} else {
    val allFactions = game.setup.toList
    val doomRanking = allFactions.sortBy(f => -game.players(f).doom)
    val myRank = doomRanking.indexOf(me)
    val placesAboveLast = allFactions.size - 1 - myRank
    val bonus = placesAboveLast * 0.1
    cappedWithRoom + bonus  // Now guaranteed ≤1.0
}
```

### Dashboard Performance Fixes
**File:** `brain-dashboard/server.py`

**Problems:**
- Server hung parsing 1000+ trace files on every request
- Progress/performance APIs timing out (>30s)
- Auto-replay generation causing massive delays

**Solutions:**

#### 1. Added Caching (30s TTL)
```python
# Global cache variables
_selfplay_cache = None
_selfplay_cache_time = 0
_arena_cache = None
_arena_cache_time = 0
_progress_cache = None
_progress_cache_time = 0
_checkpoints_cache = None
_checkpoints_cache_time = 0
_CACHE_TTL = 30  # seconds
```

**Functions with caching:**
- `get_selfplay_games()` - wraps `_parse_all_selfplay_games()`
- `get_arena_games()` - wraps `_parse_all_arena_games()`

#### 2. Reused Cached Data
**Functions:**
- `get_progress_data()` - now reuses cached games instead of reparsing files
- `get_performance_history()` - now reuses cached games instead of reparsing files

**Impact:** APIs now respond in <1s after initial cache build (vs >30s timeouts)

#### 3. Disabled Auto-Replay Generation
```python
# DISABLED: Auto-replay generation causes 30s+ delays on first load
# Replays can be generated on-demand via the API instead
```

### Run Detection Fixes
**File:** `brain-dashboard/server.py`

**Added R26 Timestamp Detection:**
```python
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
```

**Fixed R25 Arena/Selfplay Classification:**
```python
# R25 traces are mislabeled: even iterations are arena, odd are selfplay
# (R25 ran before evaluateWithExamplesAndTraces was added)
is_arena = (run_n == 25 and iter_n % 2 == 0)
game_type = "arena" if is_arena else "selfplay"
```

**Why This Matters:**
- R25 traces ALL named "selfplay-*" (misleading)
- Even iterations (2,4,6,8...) were actually arena games
- Odd iterations (1,3,5,7...) were actual selfplay games
- Dashboard now classifies them correctly

### Dashboard Data Summary
**Current State (2026-08-21):**
- Total games: 1100+
- R24: 46 games (selfplay only)
- R25: 933 games (475 arena + 458 selfplay)
- R26: 21+ games (arena at iters 2,4,6,8,10,12,14)

**APIs Working:**
- `/api/games` - All games with correct run/iter/type
- `/api/weights` - 23 reward categories
- `/api/status` - Training status
- `/api/checkpoints` - Available checkpoints
- `/api/progress` - Per-game doom/score data (fast with caching)
- `/api/performance-history` - Per-iteration stats (fast with caching)

## R25 Data Loss Incident

**What Happened:**
- R25 training ran Aug 15-19, reached iter 40
- Training was restarted, overwriting `checkpoints/current.*` files
- R25 checkpoint weights (.policy/.value files) are LOST forever
- Neural network parameters cannot be recovered from game traces

**What Survives:**
- 1004 game trace files (arena-traces/selfplay-iter*.txt)
- Iterations 1-40 game logs with scores, doom, decisions
- Can analyze R25 performance from traces
- Cannot resume training from R25 checkpoint

**Documentation:**
- Created `checkpoints_R25/` directory
- Added `checkpoints_R25/README.txt` explaining the loss
- Added synthetic `checkpoints_R25/best.meta` for dashboard

**Lesson:**
- Never delete checkpoint files - only move to trash
- Always check what files will be overwritten before restarting training
- Checkpoint weights are irreplaceable (unlike traces which are data)

## Training Configuration

**Current Run:** R26
- Started: Aug 20, 2026 08:58
- Warm-started from: R24 I20
- Champion selection: Head-to-head (>50% WR or higher 0-1 score)
- Score fix: Option C (ceiling 0.7 + placement 0.3 = max 1.0)
- Arena traces: Saved with `evaluateWithExamplesAndTraces`
- Iteration: Currently at iter 15+

**Environment:**
- CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces
- CW_RUNTAG=R26

**Checkpoints:**
- Best: R26 iter 14, score=24.4
- Current: R26 iter 15, score=21.85
