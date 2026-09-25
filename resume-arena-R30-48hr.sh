#!/bin/bash
# R34 - POLICY GRADIENT (RL) TRAINING
# 256 hidden + 640 sims + 2000 policy-sampled bootstrap games
# CRITICAL CHANGE: policy SAMPLES actions and learns from advantages (not MCTS imitation)
# Bootstrap: 2000 RL games sequential, advantages calculated from outcomes
# Streams training from disk (6 epochs), samples 10% for mixing
# Bootstrap reuse: if iter 0 >= 0.65, skip re-bootstrapping
# Saves checkpoints after EVERY iteration (including iter 0)

export CW_RUNTAG=R34
export CW_SAVE_TRACES=/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces
export CW_FRESH=1  # Fresh start, will save iter 0 checkpoint

cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# 3600 bot games (time varies by batch size found) + evaluation (~4 hours)
# Then ~4 hours per subsequent iteration
# Args: boot_games boot_epochs hidden parallel lr sims gamesPerSeat iters gamesPerIter
nohup sbt "runMain cws.PolicyRun iterarena 2000 6 256 true 0.02 640 5 100 20" \
    > /tmp/sp_R34.log 2>&1 &

echo "R34 started (POLICY GRADIENT) - PID: $!"
echo ""
echo "REINFORCEMENT LEARNING MODE:"
echo "  - Policy SAMPLES actions from network (not MCTS)"
echo "  - Advantages calculated from game outcomes"
echo "  - Policy gradient: advantage × ∇log(π(a|s))"
echo "  - Connects decisions directly to results"
echo ""
echo "Configuration: 256 hidden units, 640 MCTS eval sims, 2000 RL bootstrap games"
echo "Bootstrap: policy-sampled games with advantage calculation"
echo "Training: policy gradient on advantages (not supervised MCTS imitation)"
echo "Expected: ~30 minutes collection + ~20 minutes training = ~50 minutes bootstrap"
echo "Checkpoints save after EVERY iteration (including iter 0)"
echo ""
echo "KEY DIFFERENCE FROM SUPERVISED:"
echo "  OLD: Train policy to match MCTS visit distributions (imitation)"
echo "  NEW: Train policy with advantage-scaled gradients (RL)"
echo "  RESULT: Policy learns from EXPERIENCE, not just copying bots"
echo ""
echo "Log: tail -f /tmp/sp_R34.log"
