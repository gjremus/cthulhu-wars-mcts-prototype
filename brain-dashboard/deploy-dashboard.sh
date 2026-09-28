#!/bin/bash
# Deploy brain dashboard to cwo.freeddns.org/brainadmin/
# Run periodically to update with latest training data

set -e

SSH_KEY="/Users/gremus/Library/CloudStorage/GoogleDrive-gremus@salesforce.com/My Drive/Personal/Games/Cthulhu Wars/Maps/Library at Celaeno/Server Deployment/oracle_cw_ed25519"
HOST="oracle-cw-server@34.27.40.209"
LOCAL_DASH="/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard"
TRACES_DIR="/Users/gremus/cthulhu-wars-mcts-prototype/selfplay-traces"

echo "🧠 Deploying brain dashboard..."

# Generate latest static HTML
cd "$LOCAL_DASH"
python3 export-static.py
echo "✓ Generated static dashboard"

# Upload dashboard HTML
scp -i "$SSH_KEY" -C "$LOCAL_DASH/dashboard-static.html" "$HOST:/opt/cwo/brainadmin/index.html"
echo "✓ Uploaded dashboard HTML"

# Upload trace files if they exist
if [ -d "$TRACES_DIR" ]; then
    echo "Uploading trace files..."
    rsync -az --progress -e "ssh -i '$SSH_KEY'" "$TRACES_DIR/"*.txt "$HOST:/opt/cwo/brainadmin/" 2>/dev/null || true
    echo "✓ Uploaded trace files"

    echo "Uploading replay files..."
    rsync -az --progress -e "ssh -i '$SSH_KEY'" "$TRACES_DIR/"replay-*.html "$HOST:/opt/cwo/brainadmin/" 2>/dev/null || true
    echo "✓ Uploaded replay files"
else
    echo "⚠ No traces directory yet"
fi

echo "✅ Dashboard deployed: https://cwo.freeddns.org/brainadmin/"
