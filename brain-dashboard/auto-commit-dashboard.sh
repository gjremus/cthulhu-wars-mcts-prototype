#!/bin/bash
# Auto-commit all brain dashboard changes to git

set -e

cd /Users/gremus/cthulhu-wars-mcts-prototype

# Check if there are changes
if [[ -z $(git status --porcelain brain-dashboard/) ]]; then
    echo "No brain dashboard changes to commit"
    exit 0
fi

# Get current timestamp
TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")

# Stage all brain-dashboard changes
git add brain-dashboard/

# Create commit with timestamp
git commit -m "Dashboard auto-commit: $TIMESTAMP

Auto-generated commit of brain dashboard changes.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"

# Push to origin
git push origin master

echo "✓ Dashboard changes committed and pushed"
