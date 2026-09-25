#!/bin/bash
# R35 - Bot-bootstrap iterative arena training
# Fixed: bot-vs-bot bootstrap (NOT policy gradient)
# Parameters: 2000 bootstrap games, 6 epochs, 256 hidden, parallel, lr=0.02, 640 sims, 5/seat, 100 iters, 20/iter

cd /Users/gremus/cthulhu-wars-mcts-prototype/build
export CW_RUNTAG="R35"
export CW_FRESH="1"
rm -f /tmp/cw_bootstrap/*.dat
rm -f /tmp/sp_R35.log
rm -f /Users/gremus/cthulhu-wars-mcts-prototype/checkpoints/best.*

nohup sbt "runMain cws.PolicyRun iterarena 2000 6 256 true 0.02 640 5 100 20" > /tmp/sp_R35.log 2>&1 &

echo "R35 started with PID $!"
echo "Log: tail -f /tmp/sp_R35.log"
