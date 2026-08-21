#!/bin/bash
# Add statusLine configuration to Claude Code settings

SETTINGS_FILE="$HOME/.claude/settings.json"
BACKUP_FILE="$HOME/.claude/settings.json.backup"

if [ ! -f "$SETTINGS_FILE" ]; then
    echo "ERROR: Settings file not found: $SETTINGS_FILE"
    exit 1
fi

# Create backup
cp "$SETTINGS_FILE" "$BACKUP_FILE"

# Check if statusLine already exists
if grep -q '"statusLine"' "$SETTINGS_FILE"; then
    echo "statusLine already configured in settings.json"
    exit 0
fi

# Add statusLine before the closing brace
python3 << 'EOF'
import json
import sys

with open("/Users/gremus/.claude/settings.json", "r") as f:
    config = json.load(f)

config["statusLine"] = {
    "type": "command",
    "command": "cat > /dev/null; TASKS_FILE='/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/ACTIVE_TASKS.md'; if [ ! -f \"$TASKS_FILE\" ]; then printf 'ACTIVE_TASKS.md not found'; exit 0; fi; active=$(grep '^## \\[ \\]' \"$TASKS_FILE\" | sed 's/^## \\[ \\] //' | sed 's/^\\([0-9]*\\)\\. /\\1: /'); if [ -z \"$active\" ]; then recent=$(grep '^## \\[' \"$TASKS_FILE\" | tail -3 | sed 's/^## \\[[x\\/]\\] /✓ /' | sed 's/^\\([0-9]*\\)\\. /\\1: /'); if [ -n \"$recent\" ]; then printf 'Recent: %s' \"$(echo \"$recent\" | tr '\\n' ' | ' | sed 's/ | $//')\"; else printf 'No active tasks'; fi; else printf 'ACTIVE: %s' \"$(echo \"$active\" | tr '\\n' ' | ' | sed 's/ | $//')\"; fi"
}

with open("/Users/gremus/.claude/settings.json", "w") as f:
    json.dump(config, f, indent=2)

print("statusLine configured successfully!")
EOF

echo "Settings updated. Backup saved to: $BACKUP_FILE"
echo "Restart Claude Code for changes to take effect."
