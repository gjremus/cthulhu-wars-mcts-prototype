#!/usr/bin/env python3
"""
Remedial training for R39: Train on top 50% of qualified games from iters 0-9
with increased epochs and quality weighting.
"""
import os
import sys
import subprocess

# Read game list
with open('/tmp/r39_top50_qualified_games.txt', 'r') as f:
    game_list = [line.strip() for line in f if line.strip()]

print(f"Loaded {len(game_list)} games for remedial training")

# Create a temporary directory with symlinks to selected traces
trace_dir = "/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces"
temp_dir = "/tmp/r39_remedial_traces"
os.makedirs(temp_dir, exist_ok=True)

# Clean temp dir
for f in os.listdir(temp_dir):
    os.remove(os.path.join(temp_dir, f))

# Create symlinks for selected games
linked = 0
for game_name in game_list:
    # Find matching trace file
    for trace_file in os.listdir(trace_dir):
        if trace_file.startswith(game_name):
            src = os.path.join(trace_dir, trace_file)
            dst = os.path.join(temp_dir, trace_file)
            os.symlink(src, dst)
            linked += 1
            break

print(f"Linked {linked} trace files to {temp_dir}")

# Now run a modified training pass
# We need to modify PolicyRun to:
# 1. Load iter 8 checkpoint
# 2. Load examples from temp_dir
# 3. Train for 8 epochs with quality weighting
# 4. Save as iter 9 checkpoint

print("\nRemedial training plan:")
print(f"  - Load: R39 iter 8 checkpoint")
print(f"  - Train on: {linked} games (top 50% of qualified)")
print(f"  - Epochs: 8 (vs normal 3)")
print(f"  - Quality weighting: gameScore^2")
print(f"  - Save as: R39 iter 9 checkpoint")
print(f"\nTo execute, run Scala code that:")
print(f"  1. Loads checkpoint from R39_iter8_*")
print(f"  2. Parses traces from {temp_dir}")
print(f"  3. Trains policy/value heads with increased epochs")
print(f"  4. Saves to R39_iter9_REMEDIAL_*")
