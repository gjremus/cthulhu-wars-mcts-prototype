#!/bin/bash
# Rebuild canonical store and sync to freeddns after each iteration

set -e

cd /Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard

# Rebuild canonical store (scans all traces)
echo "[$(date '+%H:%M:%S')] Rebuilding canonical store..."
python3 build_canonical_store.py > /dev/null 2>&1 || echo "  Warning: rebuild failed"

# Deploy to remote server
SSH_KEY="/Users/gremus/My Drive/Personal/Games/Cthulhu Wars/Maps/Library at Celaeno/Server Deployment/oracle_cw_ed25519"
HOST="oracle-cw-server@35.255.125.91"

echo "[$(date '+%H:%M:%S')] Deploying to freeddns..."

# Generate static HTML from latest data
python3 export-static.py > /dev/null 2>&1

# Upload to brainadmin
scp -i "$SSH_KEY" -q -C dashboard-static.html "$HOST:/opt/cwo/brainadmin/index.html" 2>/dev/null

echo "[$(date '+%H:%M:%S')] Done - canonical store rebuilt and deployed"
