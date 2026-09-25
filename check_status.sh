#!/bin/bash
# Quick status check for brain training and dashboard

echo "═══════════════════════════════════════════════════════"
echo "BRAIN & DASHBOARD STATUS CHECK"
echo "═══════════════════════════════════════════════════════"
echo

# Check dashboard
echo "📊 DASHBOARD:"
DASHBOARD_PID=$(ps aux | grep "python3.*server.py.*dashboard" | grep -v grep | awk '{print $2}')
if [ -n "$DASHBOARD_PID" ]; then
    echo "  ✓ Running (PID: $DASHBOARD_PID)"
    echo "  ✓ URL: http://localhost:8765"
else
    echo "  ✗ NOT RUNNING"
    echo "  → Run: bash /Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/restart_dashboard.sh"
fi
echo

# Check brain
echo "🧠 BRAIN:"
BRAIN_PID=$(ps aux | grep "PolicyRun" | grep -v grep | awk '{print $2}')
if [ -n "$BRAIN_PID" ]; then
    echo "  ✓ Running (PID: $BRAIN_PID)"

    # Get status from API
    if command -v jq &> /dev/null && curl -s http://localhost:8765/api/status &> /dev/null; then
        STATUS=$(curl -s http://localhost:8765/api/status)
        RUN=$(echo "$STATUS" | jq -r '.run_tag')
        ITER=$(echo "$STATUS" | jq -r '.current_iter')
        GAME=$(echo "$STATUS" | jq -r '.current_game')
        TOTAL=$(echo "$STATUS" | jq -r '.total_games')
        ITER_ETA=$(echo "$STATUS" | jq -r '.iter_eta_human')
        FULL_ETA=$(echo "$STATUS" | jq -r '.full_run_eta_human')

        echo "  → Run: $RUN"
        echo "  → Progress: iter $ITER, game $GAME/$TOTAL"
        echo "  → Iter ETA: $ITER_ETA"
        echo "  → Full Run ETA: $FULL_ETA"
    fi

    echo "  ✓ Log: /tmp/sp_R28.log"
else
    echo "  ✗ NOT RUNNING"
    echo "  → Run: bash /Users/gremus/cthulhu-wars-mcts-prototype/restart_policyrun_r28.sh"
fi
echo

# Check latest traces
echo "📁 LATEST TRACES:"
LATEST_TRACES=$(ls -t /Users/gremus/cthulhu-wars-mcts-prototype/arena-traces/*.txt 2>/dev/null | head -5)
if [ -n "$LATEST_TRACES" ]; then
    echo "$LATEST_TRACES" | while read -r trace; do
        BASENAME=$(basename "$trace")
        echo "  $(ls -lh "$trace" | awk '{print $6, $7, $8}') $BASENAME"
    done
else
    echo "  No traces found"
fi
echo

echo "═══════════════════════════════════════════════════════"
