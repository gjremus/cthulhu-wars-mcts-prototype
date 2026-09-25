#!/bin/bash
# R30 with 256 hidden + 640 sims - play ONE game to see results

export CW_RUNTAG=R30
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces
# Don't set CW_FRESH - should resume from existing bootstrap

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# Same config but only 1 game per iteration
# Args: boot_games boot_epochs hidden parallel lr sims gamesPerSeat iters gamesPerIter
nohup sbt "runMain cws.PolicyRun iterarena 60 6 256 true 0.02 640 5 1 1" \
    > /tmp/sp_R30_1game.log 2>&1 &

echo "R30 restarted - will play 1 game with 256 hidden + 640 sims - PID: $!"
echo "Will complete naturally after game finishes"
echo "Log: tail -f /tmp/sp_R30_1game.log"
