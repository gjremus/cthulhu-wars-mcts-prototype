# Fix R35 Data Display - Task List

## CRITICAL PROBLEMS
1. Performance History - avg scores all 0
2. Performance History - win rate shows dash
3. Performance History - best score shows dash  
4. Game Browser - all games same doom per faction
5. Game Browser - all ties
6. Game Browser - all scores zero
7. Game Browser - all ranks question mark

## ROOT CAUSE
Created synthetic games from live log aggregate data (>>> overall lines) but:
- All games for a faction get same doom (the average)
- All scores = 0.0 (not in log)
- All placement = '?' (not in log)
- Win assignment is wrong

## TASKS (EXECUTE IN ORDER, AUDIT EACH)

### Task 1: Check if arena trace files exist elsewhere
- [ ] Search for any R30/R35 trace files
- [ ] Check if Arena writes traces during run or only at end

### Task 2: Revert synthetic game generation
- [ ] Remove parse_live_policyrun_logs() from rebuild
- [ ] Rebuild canonical store without synthetic games
- [ ] Verify R35 games removed from canonical store

### Task 3: Add live log aggregation to Performance History API
- [ ] Modify get_performance_history() to ALSO read live logs
- [ ] Parse >>> overall lines for iter stats
- [ ] Return aggregate data for in-progress runs
- [ ] AUDIT: Check Performance History on website shows R35 with correct stats

### Task 4: Fix Game Browser to handle missing data
- [ ] When no trace files exist, show "In progress - game details not available"
- [ ] Don't show placeholder zero/dash data
- [ ] AUDIT: Check Game Browser on website handles R35 gracefully

### Task 5: Fix Run Status to show R35 metrics
- [ ] Get wins/doom from live log aggregation
- [ ] AUDIT: Check Run Status shows R35 current metrics

## AUDIT CHECKLIST (FOR EACH TASK)
- [ ] Open http://localhost:8765
- [ ] Check specific section
- [ ] Verify R35 data correct or gracefully handled
- [ ] Screenshot if passing
- [ ] Mark task complete only after visual audit passes
