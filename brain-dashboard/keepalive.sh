#!/bin/bash
# Keepalive script for dashboard - runs every 5 minutes
# 1. Checks if dashboard is up, restarts if down
# 2. Rebuilds canonical store to pick up new traces

LOG="/tmp/dashboard_keepalive.log"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] Keepalive check" >> "$LOG"

# Check if dashboard is responding
if ! curl -s -f -m 5 http://localhost:8765/api/status > /dev/null 2>&1; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Dashboard not responding - restarting" >> "$LOG"
    bash /Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/restart_dashboard.sh >> "$LOG" 2>&1
else
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Dashboard OK" >> "$LOG"
fi

# Rebuild canonical store (runs in background, max 2 minutes)
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Rebuilding canonical store..." >> "$LOG"
cd /Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard
python3 build_canonical_store.py > /dev/null 2>&1 &
REBUILD_PID=$!

# Wait max 2 minutes for rebuild
for i in {1..120}; do
    if ! kill -0 $REBUILD_PID 2>/dev/null; then
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Rebuild completed" >> "$LOG"
        exit 0
    fi
    sleep 1
done

# If still running after 2 min, kill it
kill -9 $REBUILD_PID 2>/dev/null
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Rebuild timed out - killed" >> "$LOG"
