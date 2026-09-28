# Recent Training Improvements and Fixes - September 2026

**Last Updated:** 2026-09-25  
**Scope:** Iterations 11-15, R39 run cycle  
**Key Focus:** Undo-on-bad-outcome, checkpoint cloning, dashboard fixes, training degradation analysis

---

## 1. Undo-on-Bad-Outcome Implementation (AdaptiveRollout.scala)

### Overview
Neural network-guided MCTS rollouts can make poor tactical decisions that tank game score dramatically. Instead of accepting all outcomes, AdaptiveRollout uses real checkpoint/restore to undo catastrophic moves and try alternatives.

### File Location
- **Primary:** `/Users/gremus/cthulhu-wars-mcts-prototype/mcts-src/AdaptiveRollout.scala` (222 lines)
- **Integration:** Wired into rollout pipeline via `EarlyTermination.rolloutWithAdaptiveUndo()`

### Key Components

#### Threshold Calibration: 0.10 (10 Percentage Points)
Derived from analysis of R39 iter 14 killed games:
- **Killed game score range:** 0.055-0.212 (5-22% below decision thresholds)
- **Undo threshold:** 0.10 = catastrophically bad (double-digit score cliff)
- **This threshold catches major blunders without triggering on normal variance**

#### Configuration (Environment Variables)
```bash
CW_ADAPTIVE_UNDO=true        # Enable undo logic (default: false)
CW_UNDO_THRESHOLD=0.10       # Min score drop to undo (default: 0.10)
CW_MAX_RETRIES=2             # Max retry attempts per turn (default: 2)
```

#### Core Algorithm

**Checkpoint Creation:**
- On each brain turn, save: game state (via `Cloning.copy()`), faction score pre-decision, action index, blocked actions set
- Store attempt history: list of (action, scoreAfter) pairs for audit trail

**Decision Sequence:**
1. Brain chooses action from available set (excluding previously-blocked bad actions)
2. Opponent plays response sequence
3. After opponent moves, measure score delta: `scoreDelta = scoreAfter - scoreBefore`

**Undo Trigger:**
- If `scoreDelta < -threshold` (e.g., -0.15) AND retries remaining (`retriesThisTurn < maxRetries`):
  - Restore checkpoint (game state + continuation)
  - Truncate action list back to checkpoint point
  - Add this action to blocked set
  - Increment retry counter
  - Increment total undo counter (reported in trace)

**Least-Bad Selection Logic:**
- If max retries exhausted but still scoring poorly:
  - Review all attempted actions' scores
  - Select action with **highest** scoreAfter value
  - Force replay that action (block all others)
  - Reset retry counter for forced execution

**Acceptance:**
- If outcome acceptable (scoreDelta ≥ -threshold): clear checkpoint, reset retries

### Return Values
- Standard 5-tuple: (winners, wasKilled, killReason, actionCount, actionList)
- **NEW 6th value:** undoCount (total times undo was triggered this game)

### Impact on Training Data
- Games that would be killed due to bad early moves can recover
- Partial moves are backed out of trace files
- Training corpus gets "cleaner" data (fewer catastrophic failure modes)
- However: adds replay overhead (checkpoint/restore via Cloning.copy() has cost)

### Example Scenario
```
Turn 1 (GC): Decide on action A → score 0.65
Turn 2-4 (Opponent): Play response → damage GC position
Turn 5 (GC): Check outcome → score 0.32 (delta -0.33, below 0.10)
  ↓ UNDO TRIGGERED
Action A blocked; try action B
Turn 2-4 (Opponent): Play response → different outcome
Turn 5 (GC): Check outcome → score 0.58 (delta -0.07, acceptable)
  ↓ ACCEPTED, continue with action B
```

---

## 2. Checkpoint/Restore Using Cloning.copy()

### Implementation
- **File:** `/Users/gremus/cthulhu-wars-mcts-prototype/mcts-src/Cloning.scala`
- **Function:** `def copy(game: Game, cont: Continue): (Game, Continue)`

### Usage in AdaptiveRollout
```scala
val scoreBefore = trajectory.score(game).getOrElse(brainSeat, 0.0)
val (gClone, contClone) = Cloning.copy(game, cont)
checkpoint = Some(Checkpoint(
    gClone,                    // Cloned game state
    contClone,                 // Cloned continuation (decision state)
    scoreBefore,               // Score at checkpoint
    actionList.size,           // Index in action sequence
    Set.empty,                 // No blocked actions yet
    List.empty                 // Empty attempt history
))
```

### Performance Characteristics
- Deep clone via serialization/deserialization (expensive)
- Only called at brain decision points (~5-10 times per 1600-action game)
- Enables true state restoration vs. partial undos

### State Preservation
Captures complete board state:
- Unit positions and health
- Gate/cultist counts per faction
- Doom track per faction
- Spell book inventory
- Action Point allocations
- Ritual state
- All other game state needed for accurate outcome measurement

---

## 3. Dashboard Fixes (September 22, 2026)

### Change 1: Result Column Display
**File:** `/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/dashboard-static.html` (line ~758)

**Before:** Displayed game TYPE labels ("Best", "Worst", "Top 5%", "Middle")  
**After:** Displays actual game RESULT ("Won", "Lost", "Killed")

```javascript
const resultLabel = g.result === 'killed' ? 'Killed' :
                   (g.is_win ? 'Won' : 'Lost');
```

### Change 2: Result Filter Dropdown
**File:** `/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/dashboard-static.html` (after line 212)

Added filter control:
```html
<label style="margin-left:15px;">Result:
    <select id="result-select" onchange="loadGames()">
        <option value="">All</option>
        <option value="won">Won</option>
        <option value="lost">Lost</option>
        <option value="killed">Killed</option>
    </select>
</label>
```

JavaScript integration:
```javascript
const result = document.getElementById('result-select').value;
if (result) apiUrl += `&result=${result}`;
```

### Change 3: Server-Side Result Filtering
**File:** `/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/server.py` (after line 1740)

Backend API now accepts `&result=` parameter:
```python
result = qs.get("result", [None])[0]
if result:
    if result == "killed":
        all_games = [g for g in all_games if g.get("result") == "killed"]
    elif result == "won":
        all_games = [g for g in all_games if g.get("won") and g.get("result") != "killed"]
    elif result == "lost":
        all_games = [g for g in all_games if not g.get("won") and g.get("result") != "killed"]
```

### Impact
Users can now isolate killed games to debug early termination patterns, review winning vs. losing games separately, understand failure distribution by result type.

---

## 4. Canonical Store Rebuild Fix

### Problem
Dashboard canonical_games.json was out of sync with actual trace files and arena-traces/ directory state.

### Solution
When dashboard initializes or on manual rebuild:
1. Scan `/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces/` for all trace files
2. Group by run + iteration + faction
3. Rebuild `canonical_games.json` with fresh metadata
4. Verify `has_trace: true` only for files that exist on disk

### Performance History Correction
- **Before:** Field names mismatched storage format
- **After:** Field names match actual `game` object schema:
  - `doom` (not `doom_avg`)
  - `score` (not `game_score`)
  - `won` (boolean, not string)
  - `result` (string: "won", "lost", "killed")

---

## 5. Training Degradation: Iterations 11-14 Analysis

### Observed Pattern

| Iteration | Completion Rate | Status | Notes |
|-----------|-----------------|--------|-------|
| **Iter 11** | 100% | ✓ Best | Completed all games; strong performance |
| **Iter 12** | 85% | ⚠ Drop | ~85% of expected game count |
| **Iter 13** | 3% | ⚠ Critical | Only 3% completion; massive failure |
| **Iter 14** | 9% | ⚠ Critical | Only 9% completion; did not recover |

### Root Causes Identified

#### Issue 1: Wrong Early Termination Thresholds
Training used incorrect threshold values instead of intended ones:
- **Actual used:** 0.28, 0.10, 0.16, 0.14 (inconsistent, poorly calibrated)
- **Intended:** 0.50, 0.25, 0.20, 0.15 (geometric decline per AP phase)

This caused games to be killed prematurely when brain's doom fell only slightly below phase target.

#### Issue 2: Corpus Training Fiasco (Iter 14)
- **Planned:** Train on top 100 games per faction from iterations 11-13 (400 total)
- **Actual:** Trained on only 17 games (75% threshold per faction was too aggressive)
- **Result:** Severely under-trained checkpoint entered arena run
- **Detection:** User caught at game 110/400; iter 14 partially completed

#### Issue 3: Policy Gradient Without Outcome Weighting
Early iterations (11-14) trained on **all** qualifying games with equal weight. Games scoring 0.35 (barely acceptable) got same gradient strength as games scoring 0.75 (near-wins). This prevented convergence on strong play patterns.

### Decision: Revert to Iter 11

Given degradation through iter 14 and complexity of diagnosing exact training corruption:

**Action Taken:** Restored iter 11 checkpoint as reference/best-performing brain
- **Location:** `/Users/gremus/cthulhu-wars-mcts-prototype/checkpoints/R39_iter11_20260916_155201/`
- **Rationale:** Iter 11 completed, had strong performance metrics, is stable reference point
- **Next Strategy:** Implement recommended training improvements before advancing further (see BRAIN_ITERATION_GUIDE recommendations #1-3: score weighting, bot mixing, threshold tuning)

---

## 6. Files Modified and Created

### Scala Source Code
| File | Changes |
|------|---------|
| `/mcts-src/AdaptiveRollout.scala` | NEW: Full undo implementation with retry logic |
| `/mcts-src/PolicyRun.scala` | Integration point for AdaptiveRollout (calls rolloutWithAdaptiveUndo) |

### Dashboard
| File | Changes |
|------|---------|
| `/brain-dashboard/dashboard-static.html` | Result column, filter dropdown |
| `/brain-dashboard/server.py` | Result filter API, canonical store rebuild |

### Documentation
| File | Purpose |
|------|---------|
| `/brain-training-setup.md` | Iter 14 corpus + training command (updated) |
| `/brain-training-scripts/iter15_recovery_20260922_1240.md` | Iter 14 error recovery, iter 15 corpus construction |
| `/brain-training-scripts/iter15_dashboard_fixes_20260922_1325.md` | Dashboard result column, filter fixes |
| `RECENT_TRAINING_IMPROVEMENTS_2026-09.md` | THIS FILE: comprehensive summary |

### Configuration
| File | Purpose |
|------|---------|
| `run-policyrun-safe.sh` | Wrapper ensuring CW_SAVE_TRACES is set (prevents data loss) |

---

## 7. Recommended Next Steps

### Immediate (Before Iter 15+)
1. **Implement score-weighted learning:** Train examples on high-scoring games with stronger gradients
2. **Calibrate early termination thresholds:** Use documented 0.50→0.15 progression, not 0.28/0.10/0.16/0.14
3. **Mix bot baseline back in:** Prevent catastrophic forgetting by anchoring to bootstrap corpus

### After Iter 15 Evaluation
1. **If degradation persists:** Switch to smaller corpus (50 games max per faction, top-10% only)
2. **Monitor undo counts:** If >20% of decisions undo, threshold may be too tight (increase to 0.12-0.15)
3. **Profile checkpoint/restore cost:** If >10% of rollout time in Cloning.copy(), consider cheaper snapshot approach

### Long-term (Brain Deployment)
1. AdaptiveRollout logic ships to client-side `/brain/` once iter N reaches stable >0.80 average score
2. Learn-from-user framework (LearnFromUser.scala) ingests saved user replays for online adaptation
3. Undo threshold + max retries tunable per user feedback

---

## 8. Key Metrics and Thresholds

### Undo System Calibration
- **Threshold:** 0.10 (10 percentage point score drop)
- **Max Retries:** 2 per turn
- **Trigger Source:** Killed game analysis (iter 14 data)

### Early Termination (Deprecated Values in Iter 11-14)
- **Wrong:** 0.28, 0.10, 0.16, 0.14
- **Correct:** 0.50 (AP~5), 0.25 (AP~10), 0.20 (AP~15), 0.15 (AP~20+)

### Training Corpus Recommendations
- **Minimum:** Top 50 games per faction (200 total)
- **Standard:** Top 100 games per faction (400 total)
- **Maximum:** Top 250 games per faction (1000 total)
- **Avoid:** Percentage-based (e.g., "75%") - too sensitive to iteration variance

### Dashboard Result Categories
- **Won:** Game ended with brain's faction winning (result: "won")
- **Lost:** Game ended with brain's faction losing (result: "lost")
- **Killed:** Game terminated early via early termination check (result: "killed")

---

## 9. Reference: Timeline

| Date | Event |
|------|-------|
| 2026-09-16 | Iter 11 completed, checkpoint saved |
| 2026-09-21 | Iter 13 completed; degradation pattern observed |
| 2026-09-22 09:17 | Iter 11 pre-training backup created |
| 2026-09-22 12:40 | Iter 15 recovery begins; corpus built (top 100 per faction) |
| 2026-09-22 13:25 | Dashboard fixes deployed (Result column, filter dropdown) |
| 2026-09-25 | This documentation created |

---

## 10. Checkpoints for Reference

### Key Saved States
```
/checkpoint_backups/
├── iter11_pre_corpus_training_20260922_0917/    [stable reference point]
├── R39_iter11_20260916_155201/                  [iter 11 source checkpoint]
├── R39_iter13_20260921_002124/                  [iter 13 archived]
├── R39_iter14_FAILED/                           [degraded - not used]
└── cumulative_corpus/                           [39 backed-up trace files]

/checkpoints/
├── best.policy, best.value, best.meta           [current champion]
└── R39_iter15_YYYYMMDD_HHMMSS/                  [staged iter 15]
```

### Checkpoint Metadata Format
```
tag=R39 iter=11 winrate=0.0 bestgame=0.614225 dinS=777 dinA=128 hidden=256
```
- `tag`: Run identifier (R39, R40, etc.)
- `iter`: Iteration number within run
- `winrate`: Win rate vs. bots in eval
- `bestgame`: Best single game score observed
- `dinS`: State feature dimension
- `dinA`: Action feature dimension
- `hidden`: Network hidden layer size

---

## 11. Known Issues and Workarounds

### Issue: Trace Files Mixed with HTML Markup
**Symptom:** TrainTrace parsing fails on HTML recap sections in arena trace files.  
**Workaround:** Filter action lines by suffix (ends with `)` or known action keywords) before passing to LearnFromUser.examplesFrom().  
**Fix Required:** Separate action section from HTML section in trace generation (build-replay.py).

### Issue: KILLED Traces Don't Contain Full Action Logs
**Symptom:** Early-terminated games have minimal trace format (no action list, no HTML).  
**Workaround:** Use full-format traces only for training; KILLED traces useful only for debugging early-termination patterns.  
**Fix Required:** Modify EarlyTermination to log actions during rollout, pass to writeKilledTrace().

### Issue: Early Termination Thresholds Hard-Coded
**Symptom:** Thresholds were wrong (0.28/0.10/0.16/0.14) and couldn't be tuned without recompile.  
**Status:** FIXED in AdaptiveRollout via EarlyTermination.Thresholds config object.

---

## Summary

This documentation records the major training system improvements deployed in September 2026:
1. **Undo-on-bad-outcome** enabled via AdaptiveRollout.scala with 0.10 threshold
2. **Checkpoint cloning** uses Cloning.copy() for state restoration
3. **Dashboard improvements** added result filtering and canonical store rebuild
4. **Training degradation** analyzed; iter 11 restored as reference point
5. **Recommended improvements** documented for iter 15+

All changes maintain backward compatibility with existing checkpoint formats and training pipelines.
