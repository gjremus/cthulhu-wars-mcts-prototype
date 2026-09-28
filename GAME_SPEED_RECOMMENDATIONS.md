# Game Speed Optimization Recommendations

## Current Performance
- **R40 Pace:** 0.7-2.1 min/game (varying, currently accelerating)
- **Target Rate:** 1600 games in ~8-10 hours
- **Current ETA:** 237 remaining games = ~2.8 hours (at 0.7 min/game)

## Immediate Optimizations (Est. 20-40% speedup)

### 1. Parallel Game Execution
**Current:** Games run sequentially (1 at a time)  
**Recommendation:** Run 2-4 games in parallel on separate cores  
**Impact:** 2-4x speedup (linear with parallel count)  
**Implementation:**
- PolicyRun already supports batch execution
- Test with `--parallelGames 2` flag first
- Monitor memory usage (currently 3.9GB, can handle 2-3x)
- CPU has plenty of headroom (93% on single core = 1/8th of M1 Max)

### 2. Reduce MCTS Simulation Count
**Current:** Unknown (check PolicyRun args)  
**Recommendation:** Test 256 → 128 → 64 sims per decision  
**Impact:** 25-75% speedup (proportional to reduction)  
**Risk:** Lower quality decisions, but Stage 1 curriculum is basic (1 SB, 5 doom)  
**Test:** Run 50 games at each level, compare doom/SB metrics

### 3. Early Termination for Stage 1
**Current:** Games run to full completion (win/loss)  
**Recommendation:** Kill games after reaching Stage 1 targets (1 SB + 5 doom)  
**Impact:** 30-50% speedup (games end at ~5-7 APs instead of full length)  
**Implementation:** Add curriculum-aware early exit in PolicyRun

## Medium-Term Optimizations (Est. 10-30% speedup)

### 4. Compiled Scala Native
**Current:** JVM with G1GC  
**Recommendation:** Compile to Scala Native for reduced overhead  
**Impact:** 10-20% speedup from eliminating JVM startup/GC pauses  
**Effort:** Medium (requires build system changes)

### 5. Decision Caching
**Current:** Every decision recalculates from scratch  
**Recommendation:** Cache evaluations for repeated game states  
**Impact:** 15-30% speedup (especially in early game repeated positions)  
**Implementation:** LRU cache of (game state hash → evaluation)

### 6. Optimized Feature Extraction
**Current:** Full board scan for every MCTS simulation  
**Recommendation:** Incremental updates as actions modify state  
**Impact:** 10-20% speedup  
**Implementation:** Delta-based feature computation

## Long-Term Optimizations (Est. 50-200% speedup)

### 7. GPU Inference
**Current:** CPU-only ONNX inference  
**Recommendation:** Use Metal/CoreML on M1 Max GPU  
**Impact:** 2-5x speedup on inference (if inference-bound)  
**Requires:** Profile to confirm inference is bottleneck

### 8. Batch Inference
**Current:** One MCTS simulation at a time  
**Recommendation:** Accumulate N simulations, run batch inference  
**Impact:** 2-3x speedup on inference (amortizes overhead)  
**Works with:** GPU inference for maximum effect

### 9. Distributed Training
**Current:** Single machine  
**Recommendation:** Run N machines in parallel, merge experiences  
**Impact:** Nx speedup (linear with machine count)  
**Complexity:** Requires distributed coordination system

## Quick Win: Recommended First Step

**Start with Parallel Games (2-3 games simultaneously)**  
- Zero code changes if PolicyRun already supports it
- Immediate 2-3x speedup
- Monitor for race conditions/memory issues
- If stable, increase to 4 parallel games

**Command to test:**
```bash
# Current (1 game at a time):
sbt "runMain cws.PolicyRun iterarena 0 0 256 true 0.02 640 5 100 1600"

# Test parallel (2 games):
sbt "runMain cws.PolicyRun iterarena 0 0 256 true 0.02 640 5 100 1600 --parallelGames 2"
```

## Monitoring

Track these metrics during optimization:
1. **Games/minute** - primary speed metric
2. **Avg Doom** - quality metric (should stay ~5.0)
3. **Avg SBs** - quality metric (should stay ~1.9)
4. **Memory usage** - watch for OOM
5. **CPU utilization** - should increase with parallelization

## Risk Assessment

| Optimization | Risk | Mitigation |
|-------------|------|------------|
| Parallel games | Race conditions, memory | Start with 2, monitor closely |
| Reduced MCTS sims | Quality degradation | A/B test at each level |
| Early termination | Incomplete learning | Only for Stage 1 |
| GPU inference | Platform-specific bugs | Keep CPU fallback |
| Distributed | Coordination complexity | Start with 2 machines |
