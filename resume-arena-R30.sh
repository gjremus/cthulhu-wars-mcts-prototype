#!/bin/bash
# Start R32 arena training with:
# - VISIT DISTRIBUTION TRAINING (THE BREAKTHROUGH - AlphaGo policy improvement)
# - Score filtering: Only train on games >= 0.35
# - Reject degradation: reload best if worse
# - 640 MCTS sims (4× deeper search)
# - 256 hidden units (4× network capacity)

export CW_RUNTAG=R30
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces
export CW_FRESH=1

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# Fresh start with VISIT DISTRIBUTION training (AlphaGo's missing piece)
# Args: boot_games boot_epochs hidden parallel lr sims gamesPerSeat iters gamesPerIter
nohup sbt "runMain cws.PolicyRun iterarena 60 6 256 true 0.02 640 5 600 20" \
    > /tmp/sp_R30.log 2>&1 &

echo "R30 started with VISIT DISTRIBUTION TRAINING (AlphaGo policy improvement) - PID: $!"
echo ""
echo "THE BREAKTHROUGH:"
echo "- Policy trains on MCTS visit distributions [60%, 30%, 7%, 3%]"
echo "- NOT just final pick (which was just [0, 1, 0, 0])"
echo "- NEGATIVE REINFORCEMENT: bad moves get weakened (7% → lower probability)"
echo "- Good moves get strengthened (60% → higher probability)"
echo "- Works even in LOSING games (MCTS rejects worst moves during search)"
echo ""
echo "Combined with:"
echo "- Score filtering >= 0.35 (ignore bad games)"
echo "- Reject degradation (reload best if worse)"
echo "- 640 sims + 256 hidden"
echo ""
echo "Expect ~3 hours per iteration"
echo "Log: tail -f /tmp/sp_R30.log"
