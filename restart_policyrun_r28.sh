#!/bin/bash
# Restart PolicyRun for R28 - ARENA ONLY mode
# CRITICAL: Uses new run tag to NEVER overwrite R27 data

# Kill existing PolicyRun
echo "Stopping current PolicyRun..."
pkill -f "PolicyRun"
sleep 3

# Navigate to build directory
cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# Set environment variables
export CW_RUNTAG=R28
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces

# PROTECTION: Check if CW_RUNTAG would overwrite existing data
CANONICAL_STORE="/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json"
if [ -f "$CANONICAL_STORE" ]; then
    HIGHEST_RUN=$(python3 -c "import json; store = json.load(open('$CANONICAL_STORE')); runs = [r for r in set(g['run'] for g in store['games']) if r.startswith('R')]; print(max(runs, key=lambda r: int(r[1:])) if runs else 'R0')")
    CURRENT_NUM=$(echo $CW_RUNTAG | sed 's/R//')
    HIGHEST_NUM=$(echo $HIGHEST_RUN | sed 's/R//')

    if [ "$CURRENT_NUM" -le "$HIGHEST_NUM" ]; then
        echo "ERROR: CW_RUNTAG=$CW_RUNTAG would overwrite existing data"
        echo "Highest existing run: $HIGHEST_RUN"
        echo "Change CW_RUNTAG to R$((HIGHEST_NUM + 1)) or higher"
        exit 1
    fi
    echo "Protection check passed: $CW_RUNTAG > $HIGHEST_RUN"
fi

# Start PolicyRun R28 - ITERATIVE ARENA
echo "Starting PolicyRun R28 - ITERATIVE ARENA..."
nohup sbt "runMain cws.PolicyRun iterarena 60 6 64 true 0.02 160 5 100 20" > /tmp/sp_R28.log 2>&1 &

echo "PolicyRun R28 started in arena mode"
echo "Log: /tmp/sp_R28.log"
echo "PID: $!"
