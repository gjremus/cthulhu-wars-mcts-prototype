#!/bin/bash
# Monitor current.meta and save timestamped backups when tag changes

CKPT_DIR="/Users/gremus/cthulhu-wars-mcts-prototype/checkpoints"
BACKUP_DIR="$CKPT_DIR/bootstrap_backups"
LAST_TAG_FILE="/tmp/checkpoint_last_tag.txt"

mkdir -p "$BACKUP_DIR"

# Initialize last tag
if [ -f "$CKPT_DIR/current.meta" ]; then
    grep "^tag=" "$CKPT_DIR/current.meta" | cut -d= -f2 | cut -d' ' -f1 > "$LAST_TAG_FILE"
fi

echo "Checkpoint backup monitor started at $(date)"
echo "Watching: $CKPT_DIR/current.meta"
echo "Backups: $BACKUP_DIR"

while true; do
    if [ -f "$CKPT_DIR/current.meta" ]; then
        CURRENT_TAG=$(grep "^tag=" "$CKPT_DIR/current.meta" | cut -d= -f2 | cut -d' ' -f1)
        LAST_TAG=$(cat "$LAST_TAG_FILE" 2>/dev/null || echo "")

        if [ "$CURRENT_TAG" != "$LAST_TAG" ] && [ -n "$CURRENT_TAG" ]; then
            echo "[$(date '+%Y-%m-%d %H:%M:%S')] New checkpoint detected: $CURRENT_TAG (was: $LAST_TAG)"

            # Copy files with tag in filename
            cp "$CKPT_DIR/current.policy" "$BACKUP_DIR/${CURRENT_TAG}.policy"
            cp "$CKPT_DIR/current.value" "$BACKUP_DIR/${CURRENT_TAG}.value"
            cp "$CKPT_DIR/current.meta" "$BACKUP_DIR/${CURRENT_TAG}.meta"

            echo "[$(date '+%Y-%m-%d %H:%M:%S')] Backed up to: $BACKUP_DIR/${CURRENT_TAG}.*"

            # Update last tag
            echo "$CURRENT_TAG" > "$LAST_TAG_FILE"
        fi
    fi

    sleep 5
done
