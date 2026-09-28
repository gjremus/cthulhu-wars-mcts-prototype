# Brain Dashboard Ticker

**CRITICAL: This ticker is ONLY for the brain training dashboard. It is NOT the Library/MNU tasks ticker.**

## Schedule

Runs every 15 minutes on 00, 15, 30, 45 after the hour.

## Ticker Duties

### Every Tick (00, 15, 30, 45)
- Check "Brain Current Status.docx" for updates (full path: /Users/gremus/Library/CloudStorage/GoogleDrive-gremus@salesforce.com/My Drive/Personal/Games/Cthulhu Wars/Self-Play Brain/Brain Current Status.docx)
- Report training progress with FULL metrics: avg doom, avg APs to game end, avg SBs, games with winner vs no winner

### Half-Hourly (00, 30)
- Push snapshot of current dashboard to public dashboard
- Audit public site (http://cwofreeddns.com/brain-dashboard/) is showing current data

### Hourly (00)
- Audit dashboard data accuracy:
  - Current Run metrics
  - Performance History
  - Game Browser
  - Training Corpus  
  - Progress Charts
- Audit public site (http://cwofreeddns.com/brain-dashboard/) is showing current data

## What NOT To Do

DO NOT:
- Check admin queue
- Check tasks docx
- Check @@@ markers
- Check broken games
- Process fixes
- Apply Library/MNU ticker rules
- Read feedback_check_admin_every_tick.md (that's for Library ticker)
- Read feedback_proactive_broken_game_drain.md (that's for Library ticker)
- Any other work

## Implementation

The ticker does ONLY the three duties listed above. Nothing else.

## Memory Block List

The following memories are for the Library/MNU tasks ticker and DO NOT APPLY to the brain ticker:
- `feedback_check_admin_every_tick.md` - Library ticker only
- `feedback_proactive_broken_game_drain.md` - Library ticker only
- `feedback_walk_open_inprogress_every_tick.md` - Library ticker only
- `feedback_reread_top_every_tick.md` - Library ticker only
