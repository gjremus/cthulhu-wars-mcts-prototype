#!/bin/bash
# CRITICAL: Always run PolicyRun through this script to ensure trace saving
#
# This script GUARANTEES:
# 1. CW_SAVE_TRACES is set (so arena games are saved to disk)
# 2. CW_RUNTAG is set (so runs are numbered correctly)
# 3. Output is logged and flushed
# 4. System.out.flush() after every >>> overall line (already in PolicyRun.scala)
#
# NEVER run PolicyRun directly - you will lose all game data

set -e

# Get run tag from argument or prompt
RUN_TAG="${1:-}"
if [ -z "$RUN_TAG" ]; then
    echo "ERROR: No run tag specified"
    echo "Usage: $0 <run-tag> [additional-args...]"
    echo "Example: $0 R36 iterarena 2000 6 256 true 0.02 640 5 100 20"
    exit 1
fi

shift  # Remove RUN_TAG from args

# Set CRITICAL environment variables
export CW_SAVE_TRACES="/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces"
export CW_RUNTAG="$RUN_TAG"

# Ensure trace directory exists
mkdir -p "$CW_SAVE_TRACES"

# Log file
LOG_FILE="/tmp/sp_${RUN_TAG}_trace.log"

echo "========================================="
echo "Starting PolicyRun with TRACE SAVING"
echo "========================================="
echo "Run tag:      $RUN_TAG"
echo "Trace dir:    $CW_SAVE_TRACES"
echo "Log file:     $LOG_FILE"
echo "Args:         $@"
echo ""
echo "CRITICAL: Traces WILL be saved to disk"
echo "========================================="
echo ""

# Change to build directory
cd /Users/gremus/cthulhu-wars-mcts-prototype/build

# DEBUG: Show what we're about to run
echo "DEBUG SCRIPT: \$@ = [$@]" | tee -a "$LOG_FILE"
echo "DEBUG SCRIPT: Full command: sbt \"runMain cws.PolicyRun $@\"" | tee -a "$LOG_FILE"

# Run with sbt, unbuffered output
sbt "runMain cws.PolicyRun $@" 2>&1 | tee "$LOG_FILE"

echo ""
echo "Run $RUN_TAG completed"
echo "Traces saved to: $CW_SAVE_TRACES"
echo "Log saved to: $LOG_FILE"
