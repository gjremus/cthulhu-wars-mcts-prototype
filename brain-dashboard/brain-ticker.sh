#!/bin/bash
# Brain Dashboard Ticker
# Runs every 15 minutes (00, 15, 30, 45)
# ONLY handles brain training progress - NOT Library/MNU tasks

set -e

LOG="/tmp/brain_ticker.log"
DOCX="/Users/gremus/Library/CloudStorage/GoogleDrive-gremus@salesforce.com/My Drive/Personal/Games/Cthulhu Wars/Self-Play Brain/Brain Current Status.docx"
DASHBOARD_DIR="/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard"
PUBLIC_SITE="http://cwofreeddns.com/brain-dashboard/"

echo "[$(date '+%Y-%m-%d %H:%M:%S')] === Brain Ticker Start ===" >> "$LOG"

# Get current minute to determine which tasks to run
MINUTE=$(date +%M)
HALF_HOURLY=false
HOURLY=false

if [ "$MINUTE" = "00" ] || [ "$MINUTE" = "30" ]; then
    HALF_HOURLY=true
fi

if [ "$MINUTE" = "00" ]; then
    HOURLY=true
fi

# === EVERY TICK: Check Brain Current Status.docx and report progress ===
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Checking Brain Current Status.docx" >> "$LOG"

if [ ! -f "$DOCX" ]; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: Brain Current Status.docx not found" >> "$LOG"
    exit 1
fi

# Parse training log for current metrics
LOG_FILE="/tmp/sp_R39_iter14.log"
if [ -f "$LOG_FILE" ]; then
    # Count completed vs killed games
    COMPLETED=$(grep "complete:" "$LOG_FILE" | wc -l | tr -d ' ')
    KILLED=$(grep "KILLED:" "$LOG_FILE" | wc -l | tr -d ' ')
    TOTAL=$((COMPLETED + KILLED))

    # Get avg doom from completed games
    AVG_DOOM=$(grep "complete:" "$LOG_FILE" | tail -20 | grep -oE "doom=[0-9]+" | cut -d= -f2 | awk '{sum+=$1; n++} END {if(n>0) printf "%.1f", sum/n; else print "0"}')

    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Training Progress: $COMPLETED completed, $KILLED killed ($TOTAL total) | Avg doom (last 20): $AVG_DOOM" >> "$LOG"
else
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] No training log found" >> "$LOG"
fi

# === HALF-HOURLY (00, 30): Push dashboard snapshot to public site ===
if [ "$HALF_HOURLY" = true ]; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Half-hourly: Pushing dashboard to public site" >> "$LOG"

    cd "$DASHBOARD_DIR"
    if [ -f "deploy-dashboard.sh" ]; then
        bash deploy-dashboard.sh >> "$LOG" 2>&1
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Dashboard deployed" >> "$LOG"
    else
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] WARNING: deploy-dashboard.sh not found" >> "$LOG"
    fi

    # Audit public site
    if curl -s -f -m 10 "$PUBLIC_SITE" > /dev/null 2>&1; then
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Public site OK" >> "$LOG"
    else
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] WARNING: Public site not responding" >> "$LOG"
    fi
fi

# === HOURLY (00): Full dashboard audit ===
if [ "$HOURLY" = true ]; then
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] Hourly: Full dashboard audit" >> "$LOG"

    # Check each dashboard section
    for SECTION in "current-run" "performance-history" "game-browser" "training-corpus" "progress-charts"; do
        ENDPOINT="http://localhost:8765/api/$SECTION"
        if curl -s -f -m 5 "$ENDPOINT" > /dev/null 2>&1; then
            echo "[$(date '+%Y-%m-%d %H:%M:%S')]   $SECTION: OK" >> "$LOG"
        else
            echo "[$(date '+%Y-%m-%d %H:%M:%S')]   $SECTION: FAILED" >> "$LOG"
        fi
    done

    # Audit public site
    if curl -s -f -m 10 "$PUBLIC_SITE" > /dev/null 2>&1; then
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Public site audit: OK" >> "$LOG"
    else
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] Public site audit: FAILED" >> "$LOG"
    fi
fi

echo "[$(date '+%Y-%m-%d %H:%M:%S')] === Brain Ticker Complete ===" >> "$LOG"
