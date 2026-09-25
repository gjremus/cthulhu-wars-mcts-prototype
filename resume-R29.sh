#!/bin/bash
# Resume paused R29 training

PID=63343

if ps -p $PID > /dev/null 2>&1; then
    STATE=$(ps -p $PID -o state= | tr -d ' ')
    if [ "$STATE" = "T" ]; then
        kill -CONT $PID
        echo "R29 training resumed (PID: $PID)"
        echo "Log: tail -f /tmp/sp_R29.log"
    else
        echo "Process $PID is already running (state: $STATE)"
    fi
else
    echo "Process $PID not found - training may have stopped"
    echo "To restart from checkpoint, run: ./resume-arena-R29.sh"
fi
