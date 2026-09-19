#!/bin/bash
# Restart dashboard server with all fixes

echo "Stopping dashboard server..."
pkill -f "python3.*server.py.*dashboard" || true
sleep 2

# Verify port is free
if lsof -ti :8765 >/dev/null 2>&1; then
    echo "Force killing process on port 8765..."
    lsof -ti :8765 | xargs kill -9
    sleep 2
fi

# Start server
cd /Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard
echo "Starting dashboard server..."
nohup python3 server.py > /tmp/dashboard_server.log 2>&1 &
SERVER_PID=$!

sleep 3

# Verify it started
if ps -p $SERVER_PID > /dev/null; then
    echo "✓ Dashboard server running (PID: $SERVER_PID)"
    echo "✓ Access at: http://localhost:8765"
    echo "✓ Log: /tmp/dashboard_server.log"
else
    echo "✗ Server failed to start"
    tail -20 /tmp/dashboard_server.log
    exit 1
fi
