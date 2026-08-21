R25 Training Run - August 15-19, 2026

Checkpoint files (.policy/.value) were LOST when training was restarted.
Neural network weights are UNRECOVERABLE.

However, all game trace data survives:
- 967 trace files in arena-traces/selfplay-iter*.txt
- Iterations 1-40 (partial iter 40)
- Seating: GC/BG/YS/CC (base-4, Earth 3.5)
- MCTS sims: 160

This directory exists solely to document the R25 run in the dashboard.
The meta file is synthetic - actual checkpoint scores are unknown.

R25 trace files are labeled "selfplay" because they predate the
evaluateWithExamplesAndTraces refactor that added proper naming.
