#!/bin/bash
# Start iter 13 from iter 12 checkpoint with 1600 games (400 per faction)

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces
export CW_SIMS=640
export CW_RUNTAG=R39
export CW_MIN_DOOM_AP5=8
export CW_MIN_DOOM_AP10=15
export CW_MIN_DOOM_AP15=25
export CW_MIN_SCORE_AP5=0.12
export CW_MIN_SCORE_AP10=0.20

echo "Starting iter 13 from iter 12 checkpoint"
echo "  Will play 1600 games (400 per faction)"
echo "  Early term thresholds: doom AP5=$CW_MIN_DOOM_AP5 AP10=$CW_MIN_DOOM_AP10 AP15=$CW_MIN_DOOM_AP15"
echo ""

# iterarena: <bootGames> <bootEpochs> <hidden> [par] [lr] [sims] [perSeat] [iters] [gamesPerIter]
# Start at iter 13, play 1600 games per iteration
nohup sbt "runMain cws.PolicyRun iterarena 2000 10 256 false 0.0001 640 400 100 1600" > /tmp/sp_R39_iter13_start.log 2>&1 &

PID=$!
echo "Started with PID $PID"
echo "Log: /tmp/sp_R39_iter13_start.log"
echo ""
echo "This will:"
echo "  1. Load iter 12 checkpoint (best.* files)"
echo "  2. Play 1600 arena games for iter 13 (400 GC, 400 BG, 400 YS, 400 CC)"
echo "  3. Apply 75% quality filtering per faction"
echo "  4. Train and save iter 13 checkpoint"
echo "  5. Continue with iter 14, 15, ... (1600 games each)"
