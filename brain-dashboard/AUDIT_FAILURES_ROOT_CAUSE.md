# Root Cause Analysis: Why Audits Have Failed

## The Core Problem

**I've been auditing SYNTAX instead of SEMANTICS**

### What I Did Wrong

1. **Grepped HTML instead of rendering**: Checked if `<th>Avg SBs</th>` exists in HTML source, but didn't verify JavaScript actually populates the table with data when loaded in browser

2. **Tested API exists instead of data correctness**: Verified `/api/performance-history` returns JSON, but didn't verify:
   - All fields have values (not null/-/?)
   - Values are mathematically correct
   - Values match what's shown in rendered dashboard

3. **Checked file generation instead of content**: Verified replay HTML file exists and has size, but didn't verify:
   - File contains full game log with hundreds of actions
   - Actions are properly formatted and clickable
   - All Action Phases are navigable

4. **Assumed deployment success from HTTP 200**: Checked public site returns HTML, but didn't verify:
   - HTML contains CURRENT training data (R40, not stale R39)
   - All tabs/sections work
   - Data matches local dashboard

### Why This Kept Happening

**Root cause: No verification checklist**

I kept repeating the same shallow checks because I had no systematic audit process. Each time I:
- Checked ONE thing (file exists, API responds, HTML has string)
- Declared success
- Moved on without verifying the ACTUAL FUNCTIONALITY

### What A Proper Audit Requires

#### 1. Performance History Audit
- [ ] API returns data for all runs (R37-R40)
- [ ] Every entry has non-null values for: run, iter, type, avg_doom, avg_spellbooks, avg_length, avg_score, wins, total_games
- [ ] Newest entry (R40) is shown FIRST
- [ ] Click R40 entry → faction breakdown appears with per-faction stats
- [ ] All per-faction stats have values (not dashes)
- [ ] Math check: sum of faction games = total_games

#### 2. Game Browser Audit  
- [ ] Shows R40 games (1348 total)
- [ ] Every game has: run, iter, faction, doom, spellbooks, length, place, score
- [ ] Place values are 1-4 (not ?)
- [ ] Click VIEW button → generates replay
- [ ] Replay loads in browser
- [ ] Replay shows: full game board, action phases buttons, game log with hundreds of entries
- [ ] Click action phase buttons → board updates
- [ ] Scroll game log → see MoveAction, SummonAction, BuildGateAction, RitualAction entries

#### 3. Public Dashboard Audit
- [ ] Site loads: https://cwo.freeddns.org/brainadmin/
- [ ] Shows "Brain Training Dashboard" title
- [ ] Performance History tab shows R40 as newest entry
- [ ] R40 data matches local dashboard (same doom/sbs/aps)
- [ ] Game Browser tab shows R40 games
- [ ] All tabs functional (not showing "Loading..." indefinitely)

#### 4. Placement Calculation Audit
- [ ] Sample 10 random R40 games
- [ ] For each: parse trace → get all 4 factions' SBs and doom
- [ ] Calculate expected placement: 6-SB group by doom desc, then <6-SB group by doom desc
- [ ] Verify canonical store has correct place value
- [ ] Verify Game Browser displays correct place

### Implementation

Created proper audit checklist. Will execute EVERY item before declaring anything fixed.

## Fixes Applied

1. **Rescoring script**: Parses SpellbookAction lines from existing traces to get all factions' SBs, recalculates correct placement
2. **Audit checklist**: Systematic verification of every field and interaction
3. **Replay verification**: Check actual content, not just file size

## Next Steps

1. Run rescore script
2. Rebuild canonical store
3. Execute FULL audit checklist for both dashboards
4. Report actual verification results (not assumptions)
