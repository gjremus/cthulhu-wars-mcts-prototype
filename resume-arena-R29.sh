#!/bin/bash
# Start R29 arena training FRESH with reward-score fix

export CW_RUNTAG=R29
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces
export CW_FRESH=1

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# Fresh start - no resume
# Args: boot_games boot_epochs hidden parallel lr sims gamesPerSeat iters gamesPerIter
nohup sbt "runMain cws.PolicyRun iterarena 60 6 64 true 0.02 160 5 600 20" \
    > /tmp/sp_R29.log 2>&1 &

echo "R29 started FRESH - PID: $!"
echo "Log: tail -f /tmp/sp_R29.log"
