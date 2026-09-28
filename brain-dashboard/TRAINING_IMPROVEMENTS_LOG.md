# Training Improvements Log - September 2026

## Quick Overview

This log tracks recent improvements to the brain training system and lessons learned from degradation analysis.

## 1. Undo-on-Bad-Outcome (NEW)

**What:** Neural network rollouts now checkpoint game state and restore if a decision causes catastrophic score drop.

**Where:** `/mcts-src/AdaptiveRollout.scala` (222 lines)

**Calibration:** 0.10 threshold derived from killed game analysis
- Killed games had scores 5-22% below pass threshold
- 0.10 = double-digit cliff (definitively bad)
- Avoids false positives on normal variance

**Config:**
```bash
export CW_ADAPTIVE_UNDO=true       # Enable
export CW_UNDO_THRESHOLD=0.10      # Min drop to undo (try 0.08-0.12)
export CW_MAX_RETRIES=2            # Max attempts per turn
```

**Implementation:**
1. Checkpoint: Save game state + faction score + action index
2. Decide: Brain chooses action
3. Opponent plays response
4. Measure: Check score delta vs. checkpoint
5. If delta < -0.10 and retries remain: Restore checkpoint, block this action, retry

**Retry Logic:**
- If all attempts still score poorly: Use **least-bad** (highest score of all attempts)
- Avoids thrashing; picks best-of-bad options

## 2. Dashboard Improvements

### Result Column (September 22)
- **Before:** Showed game TYPE ("Best", "Worst", "Top 5%")
- **After:** Shows RESULT ("Won", "Lost", "Killed")
- **Code:** dashboard-static.html line ~758

### Result Filter (NEW)
- **Added:** Dropdown to filter by Won/Lost/Killed
- **Server:** server.py supports `&result=` API parameter
- **Use case:** Isolate failed games to debug patterns

### Canonical Store Rebuild
- Scans `/arena-traces/` to rebuild game metadata
- Syncs with actual trace files (prevents ghost entries)
- Fixes `has_trace` field accuracy

## 3. Training Degradation Root Causes

### Pattern Observed
```
Iter 11: 100% completion ✓
Iter 12: 85% completion ⚠
Iter 13: 3% completion 🔥
Iter 14: 9% completion 🔥 (aborted at game 110/400)
```

### Root Cause #1: Wrong Early Termination Thresholds
- **Used in iters 11-14:** 0.28, 0.10, 0.16, 0.14
- **Intended:** 0.50, 0.25, 0.20, 0.15 (per AP phase)
- **Impact:** Games killed prematurely when doom slightly below phase target
- **Fix:** Update constants; make tunable via config object

### Root Cause #2: Iter 14 Corpus Too Small
- **Planned:** Top 100 games per faction = 400 total
- **Actual:** 75% threshold per faction = 17 total (way too aggressive)
- **Result:** Under-trained checkpoint entered arena → 97% kill rate
- **Lesson:** Use fixed counts, not percentages (fewer surprises)

### Root Cause #3: No Score-Weighted Training
- **Old method:** All qualifying games same gradient weight
- **Problem:** Games scoring 0.35 (barely passing) get same strength as 0.75 (near-win)
- **Result:** Network learns average of mediocre+good play, regression toward mean
- **Fix:** Apply `effectiveLR = lr * (gameScore / 0.5)` in training loop

## 4. Checkpoint Cloning

**File:** `/mcts-src/Cloning.scala`

**Function:**
```scala
def copy(game: Game, cont: Continue): (Game, Continue)
```

**Used By:** AdaptiveRollout to snapshot game state for undo

**Performance:** ~1-2ms per clone; only called at brain decision points (~5-10 times per 1600-move game)

## 5. Recovery Strategy

### Immediate (Iter 15)
- Revert to iter 11 checkpoint (stable reference)
- Use top 100 games/faction corpus (400 total)
- Train on iter 11 (avoid iter 14 corruption)

### Before Iter 16
- Implement score-weighted learning
- Implement bot corpus anchoring (80% bootstrap, 20% arena)
- Fix early termination thresholds
- Raise score threshold to ≥0.60

### Validation
- Run iters 16-20 with improvements
- Monitor for continued degradation
- If still declining, reduce corpus to 50 games/faction

## 6. Key Files

| File | Purpose |
|------|---------|
| `/mcts-src/AdaptiveRollout.scala` | Undo implementation |
| `/mcts-src/PolicyRun.scala` | Integration point |
| `/brain-dashboard/dashboard-static.html` | Result column + filter |
| `/brain-dashboard/server.py` | Result filter API |
| `/brain-training-setup.md` | Updated with degradation analysis |
| `/RECENT_TRAINING_IMPROVEMENTS_2026-09.md` | Full technical reference |

## 7. Thresholds and Tuning

### Undo System
| Parameter | Default | Range | Notes |
|-----------|---------|-------|-------|
| CW_UNDO_THRESHOLD | 0.10 | 0.08-0.12 | Try 0.12 if >50% undo rate |
| CW_MAX_RETRIES | 2 | 1-3 | More retries = slower but safer |

### Early Termination (CORRECT VALUES)
| AP Range | Threshold | Source |
|----------|-----------|--------|
| ~5 | 0.50 | Documented in BRAIN_ITERATION_GUIDE |
| ~10 | 0.25 | Geometric decline pattern |
| ~15 | 0.20 | Tighter as game progresses |
| ~20+ | 0.15 | Final phase |

### Training Corpus
| Size | Use Case | Cost |
|------|----------|------|
| 50/faction (200 total) | Minimum, fast iteration | ~30 min train |
| 100/faction (400 total) | Standard, balanced | ~60 min train |
| 250/faction (1000 total) | Maximum, slow | ~150 min train |
| Percentage-based | ❌ Avoid | Noisy, non-reproducible |

## 8. Monitoring Metrics

### Health Checks (per iteration)
- **Undo frequency:** Target <10% of brain turns
- **Game completion rate:** Target >90% (not killed)
- **Average score:** Target >0.75 (vs. iter 11 baseline 0.67)
- **Training loss:** Should decrease each epoch

### Degradation Warning Signs
- Undo frequency >20% → threshold too tight
- Completion rate <80% → corpus quality poor or thresholds wrong
- Score declining per-iteration → training corrupted or configuration wrong

## 9. References

- **Full technical reference:** `/Users/gremus/cthulhu-wars-mcts-prototype/RECENT_TRAINING_IMPROVEMENTS_2026-09.md`
- **Training guide:** BRAIN_ITERATION_GUIDE.md (Drive folder)
- **Dashboard audit:** COMPREHENSIVE_AUDIT_RESULTS.md
- **Degradation recommendations:** Fix_Degradation_Recommendations.md (Drive folder)

## 10. Timeline

| Date | Event | Status |
|------|-------|--------|
| 2026-09-16 | Iter 11 completed | ✓ Stable |
| 2026-09-21 | Iter 13 failure observed | 🔥 3% completion |
| 2026-09-22 09:17 | Iter 11 backup created | ✓ Recovery point |
| 2026-09-22 12:40 | Iter 15 recovery begins | In progress |
| 2026-09-22 13:25 | Dashboard fixes deployed | ✓ Complete |
| 2026-09-25 | Documentation updated | ✓ This entry |

---

**Last Updated:** 2026-09-25  
**Scope:** R39 run, iterations 11-15  
**Status:** Recovery in progress; improvements staged for iter 16+
