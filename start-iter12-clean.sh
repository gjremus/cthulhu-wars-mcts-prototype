#!/bin/bash
# Start iter 12 training fresh (will load iter 11 checkpoint, train on filtered iter 12 games)

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# Early termination thresholds
export CW_MIN_DOOM_AP5=8
export CW_MIN_DOOM_AP10=15
export CW_MIN_DOOM_AP15=25
export CW_MIN_SCORE_AP5=0.12
export CW_MIN_SCORE_AP10=0.20

# Trace saving directory
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces

# MCTS sims
export CW_SIMS=640

# Run tag
export CW_RUNTAG=R39

echo "Starting PolicyRun from iter 12"
echo "Will load iter 11 checkpoint, train on filtered iter 12 games (31 games kept)"
echo "Early term thresholds: doom AP5=$CW_MIN_DOOM_AP5 AP10=$CW_MIN_DOOM_AP10 AP15=$CW_MIN_DOOM_AP15"
echo "Early term thresholds: score AP5=$CW_MIN_SCORE_AP5 AP10=$CW_MIN_SCORE_AP10"
echo ""

sbt "runMain cws.PolicyRun iterarena 12 10 256 false 0.0001 640 400 1000 400" > /tmp/sp_R39_iter12_clean.log 2>&1 &

echo "Started with PID $!"
echo "Log: /tmp/sp_R39_iter12_clean.log"
