#!/bin/bash
# Restart iter 12 with early termination and resume offsets
# Resume from: GC=400 (complete), BG=365, YS=100, CC=100

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# Early termination thresholds
export CW_MIN_DOOM_AP5=8
export CW_MIN_DOOM_AP10=15
export CW_MIN_DOOM_AP15=25
export CW_MIN_SCORE_AP5=0.12
export CW_MIN_SCORE_AP10=0.20

# Resume offsets (completed games per faction - will skip these and continue)
export CW_RESUME_FROM="gc:400,bg:377,ys:100,cc:100"

# Trace saving directory
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces

# MCTS sims (already set, but making explicit)
export CW_SIMS=640

# Run tag to match checkpoint (CRITICAL for resuming from iter 11)
export CW_RUNTAG=R39

echo "Starting PolicyRun with early termination enabled"
echo "Resume from: gc:400 (done), bg:377 (need 23 more), ys:100 (need 300 more), cc:100 (need 300 more)"
echo "Early term thresholds: doom AP5=$CW_MIN_DOOM_AP5 AP10=$CW_MIN_DOOM_AP10 AP15=$CW_MIN_DOOM_AP15"
echo "Early term thresholds: score AP5=$CW_MIN_SCORE_AP5 AP10=$CW_MIN_SCORE_AP10"
echo ""

sbt "runMain cws.PolicyRun iterarena 12 10 256 false 0.0001 640 400 1000 400" > /tmp/sp_R39_iter12_direct.log 2>&1 &

echo "Started with PID $!"
echo "Log: /tmp/sp_R39_iter12_direct.log"
