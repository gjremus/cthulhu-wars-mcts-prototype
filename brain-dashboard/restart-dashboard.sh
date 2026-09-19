#!/bin/bash
cd /Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard
pkill -f "python.*server.py"
sleep 1
nohup python3 server.py > /tmp/dashboard.log 2>&1 &
sleep 2
ps aux | grep 'python3.*server.py' | grep -v grep
