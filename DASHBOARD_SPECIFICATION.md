# Brain Dashboard Specification
**Created:** 2026-09-28 23:16
**Purpose:** Define ALL required dashboard metrics and functionality

## CRITICAL: This document defines what the dashboard MUST show

##Run Status Section

**Required metrics:**
1. Current run number (e.g., "R40")
2. Current iteration number (e.g., "iter 1" or "iter 2")
3. Game type (arena vs selfplay)
4. Games completed / target (e.g., "1626 / 1600")
5. Progress percentage (e.g., "101.6%")
6. Avg Doom
7. Avg Spellbooks
8. Avg Action Phases
9. Win rate (arena: % "best" results, selfplay: % "win" results)
10. Best 0-1 score
11. ETA for completion
12. PolicyRun process status (PID, elapsed time)
13. **Claude disk space** (MB/GB available)

## Performance History Table

**Sort order:** NEWEST FIRST (R40 at top, then R39, R38, etc.)

**Required columns:**
1. Run
2. Iter
3. Avg Doom
4. Avg SBs (Spellbooks)
5. Avg APs (Action Phases)
6. Avg Score (0-1)
7. Type (arena/selfplay)
8. Wins
9. Total games

**Expandable faction breakdown:** Click row to show per-faction stats

## Game Browser

**Required functionality:**
1. Filter by run + iteration
2. Show all games for selected run/iter
3. Columns: Faction, Doom, Spellbooks, Length, Place, Score, Result
4. VIEW button links to full trace file
5. Pagination (50 games per page)

## Progress Charts

**Required charts:**
1. Doom over time (all runs)
2. Spellbooks over time (all runs)
3. Action Phases over time (all runs)
4. Win rate over time (all runs)

**Chart controls:**
1. Filter by run (dropdown)
2. Filter by iteration range
3. Default: show latest run

## Training Corpus View

**Required info:**
1. Total games in corpus
2. Breakdown by run/iter
3. Filter controls
4. Sample game links

## Audit Requirements

**Dashboard audit MUST verify:**
1. All metrics present (no "undefined", no "-", no "null")
2. Performance History sorted newest first
3. Game Browser returns games
4. All charts render
5. Claude disk space显示
6. No missing columns
7. No JavaScript errors in console
