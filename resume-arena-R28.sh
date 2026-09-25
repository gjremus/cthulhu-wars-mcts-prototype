#!/bin/bash
# Resume R28 arena training with trace saving enabled

export CW_RUNTAG=R28
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# Resume from iter 55
# Args: boot_games boot_epochs hidden parallel lr sims gamesPerSeat iters gamesPerIter
nohup sbt "runMain cws.PolicyRun iterarena 60 6 64 true 0.02 160 5 600 20" \
    > /tmp/sp_R28.log 2>&1 &

echo "R28 resumed - PID: $!"
echo "Log: tail -f /tmp/sp_R28.log"
