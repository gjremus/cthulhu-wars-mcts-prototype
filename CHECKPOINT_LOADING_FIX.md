# Checkpoint Loading Fix (2026-09-08)

## Problem
Post-epoch checkpoints (e.g., R36_direct with hidden=256) were not loading correctly. PolicyRun showed "Checkpoint incompatible" and used default hidden=64 instead.

## Root Cause
The `run-policyrun-safe.sh` script was NOT passing arguments correctly to sbt. When calling:
```bash
./run-policyrun-safe.sh R39 iterarena 0 10 256 false 0.0001 640 5 10 20
```

Only "iterarena" reached main(), all numeric arguments were lost. The script uses:
```bash
sbt "runMain cws.PolicyRun $@" 2>&1 | tee "$LOG_FILE"
```

This SHOULD work but doesn't - likely an sbt version or shell expansion issue.

## Solution
**DO NOT use run-policyrun-safe.sh for iterarena launches.**

Instead, launch directly with arguments embedded in the sbt command:
```bash
export CW_RUNTAG=R39
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces
nohup sbt "runMain cws.PolicyRun iterarena 0 10 256 false 0.0001 640 5 10 20" \
    > /tmp/sp_R39_direct.log 2>&1 &
```

This correctly passes all arguments to main().

## Checkpoint Loading Requirements
1. **Copy checkpoint to best.{policy,value,meta}**: PolicyRun loads from "best" files, not arbitrary names
2. **Match hidden parameter**: hidden= in checkpoint metadata MUST match hidden= argument to PolicyRun
3. **Verify load in log**: Look for "Loaded checkpoint [tag=... hidden=...]" line, not "Checkpoint incompatible"

## Standard Baseline Setup
```bash
cd /Users/gremus/cthulhu-wars-mcts-prototype/checkpoints
cp standard_baseline.policy best.policy
cp standard_baseline.value best.value
cp standard_baseline.meta best.meta
```

The standard_baseline is R36_direct post-epoch (hidden=256, dinS=777, dinA=128, best game 0.878).

## Verification
Check these in startup log:
- `DEBUG ARGS: nGames=0 epochs=10 hidden=256 ...` (confirms args parsed)
- `state dim=777, action dim=128, hidden=256` (confirms network size)
- `Loaded checkpoint [tag=... hidden=256 ...]` (confirms checkpoint loaded)

If you see "Checkpoint incompatible", check:
1. Are arguments reaching main()? (Check DEBUG ARGS line)
2. Does best.meta have `hidden=256`?
3. Does PolicyRun hidden= argument match checkpoint?
