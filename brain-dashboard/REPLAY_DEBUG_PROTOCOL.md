# Replay Debug Protocol - How to Actually Verify Replays Work

## The Problem

Black screen = something failed to load. Could be:
1. JavaScript error (check browser console)
2. Missing game log entries
3. Missing board state
4. Missing images
5. Malformed HTML

## Verification Checklist

### Step 1: Check Trace File Format
```bash
# Trace MUST have 3 sections separated by blank lines:
# 1. Action strings (PlayDirectionAction...)
# 2. HTML log entries (<div class="log-entry">...)
# 3. Metadata (Brain seat:, Result:, etc.)

head -50 arena-R40-iter1-gc-game1-d6.txt
# Should show action strings

grep -c '<div class="log-entry">' arena-R40-iter1-gc-game1-d6.txt
# Should be > 0 (typically 400-500)

tail -20 arena-R40-iter1-gc-game1-d6.txt
# Should show metadata
```

### Step 2: Generate Replay and Check Output
```bash
cd /Users/gremus/Claude-Projects/cthulhu-wars-tools/Replay
python3 build-replay.py <trace.txt> <output.html> "Title" 2>&1

# Check output messages:
# "X action strings, Y HTML log lines" - Y should be > 0
# "Z snapshots, N AP boundaries" - should have snapshots
# "WARNING: X unmatched events" - some is OK, but not 100%
```

### Step 3: Check Generated Replay HTML
```bash
ls -lh replay.html
# Should be 300KB+ (not 100KB or less)

grep -c 'class="log-entry"' replay.html
# Should be > 0

grep -c 'const DISPLAY_LINES' replay.html
# Should be 1 (JavaScript array of log entries)

grep 'DISPLAY_LINES = \[' replay.html | head -c 500
# Should show array of log entry objects
```

### Step 4: Open in Browser and Check Console
```bash
open replay.html
```

**In browser:**
1. Right-click → Inspect → Console tab
2. Look for JavaScript errors (red text)
3. Common errors:
   - "Cannot read property X of undefined" → data structure issue
   - "Image failed to load" → missing images (OK, should still work)
   - "DISPLAY_LINES is not defined" → generation failed

### Step 5: Check Visual Elements
**If page loads but is blank:**
- Check if game board div exists: Inspect → find id="game-board"
- Check if log panel exists: find id="game-log"
- Check if they have content inside them

**What should be visible:**
- Game board (map image or colored regions)
- Action Phase buttons (AP0, AP1, AP2, etc.)
- Game log panel on right side
- Log entries scrollable

### Step 6: Test Interaction
- Click AP buttons → board should update
- Scroll log → should have hundreds of entries
- Hover over units → should show tooltips

## Common Failures and Fixes

### Black screen + no errors in console
**Cause:** No game log entries
**Check:** `grep -c 'log-entry' replay.html` returns 0
**Fix:** Trace file needs HTML log section

### Black screen + "DISPLAY_LINES not defined"
**Cause:** build-replay.py didn't generate JavaScript array
**Check:** `grep 'DISPLAY_LINES' replay.html` returns nothing
**Fix:** build-replay.py failed to parse HTML log lines

### Partial display (board but no log)
**Cause:** HTML log lines present but not parsed correctly
**Check:** Look for WARNING messages in build-replay.py output
**Fix:** HTML log format doesn't match parser expectations

### Blank board (log works but no map)
**Cause:** Missing images or map detection failed
**Check:** build-replay.py output says "0 images embedded"
**Fix:** Set CW_REPLAY_IMAGE_DIR environment variable

## Automation Script

```python
#!/usr/bin/env python3
"""Verify replay actually works - not just that file exists."""

import subprocess
import sys
from pathlib import Path

def verify_replay(replay_html):
    replay_path = Path(replay_html)
    if not replay_path.exists():
        return False, "File doesn't exist"
    
    size = replay_path.stat().st_size
    if size < 200000:
        return False, f"File too small ({size} bytes, expected 200KB+)"
    
    content = replay_path.read_text()
    
    # Check for log entries in JavaScript
    if 'DISPLAY_LINES = [' not in content:
        return False, "No DISPLAY_LINES array found"
    
    log_count = content.count('"log-entry"')
    if log_count == 0:
        return False, "No log entries in DISPLAY_LINES"
    
    # Check for game board
    if 'id="game-board"' not in content:
        return False, "No game board element"
    
    return True, f"OK: {log_count} log entries, {size} bytes"

# Use: python3 verify_replay.py replay.html
if __name__ == '__main__':
    success, msg = verify_replay(sys.argv[1])
    print(f"{'✓' if success else '✗'} {msg}")
    sys.exit(0 if success else 1)
```

## Summary

**DON'T JUST CHECK:** File exists, file has size, grep finds string
**ACTUALLY VERIFY:** 
1. Trace has HTML log section (grep -c '<div class="log-entry">' > 0)
2. Replay has DISPLAY_LINES array (grep 'DISPLAY_LINES = \[')
3. Replay size > 200KB
4. Open in browser, check console for errors
5. Visual check: board visible, log entries scrollable
