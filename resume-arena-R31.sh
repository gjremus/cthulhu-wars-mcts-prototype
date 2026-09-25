#!/bin/bash
# Start R31 arena training with:
# - SCORE FILTERING: Only train on games scoring >= 0.35 (NEW)
# - Reject degradation: reload best if worse
# - 640 MCTS sims (4× deeper search)
# - 256 hidden units (4× network capacity)

export CW_RUNTAG=R31
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces
export CW_FRESH=1

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# Fresh start with score-filtered training
# Args: boot_games boot_epochs hidden parallel lr sims gamesPerSeat iters gamesPerIter
nohup sbt "runMain cws.PolicyRun iterarena 60 6 256 true 0.02 640 5 600 20" \
    > /tmp/sp_R31.log 2>&1 &

echo "R31 started with SCORE-FILTERED TRAINING (>= 0.35) + REJECT-DEGRADATION + 640 sims + 256 hidden - PID: $!"
echo "This solves: training on losses drowning out the 1/20 winning game"
echo "Now: that 1 win teaches the network, 19 losses are ignored"
echo "Expect ~3 hours per iteration"
echo "Log: tail -f /tmp/sp_R31.log"
