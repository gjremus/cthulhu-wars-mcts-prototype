# TASK LIST - WORK UNTIL COMPLETE

## [✓] 1. GAME BROWSER PAGINATION
Load 50 games at a time with page controls - DONE

## [⚠] 2. PROGRESS CHARTS - COMPLETE REWRITE
Backend ready with run_iter data. Frontend needs:
- Delete old chart functions (lines 1492-1864)
- Insert new run_iter-based chart functions
- Multi-select dropdowns functional
STATUS: 70% complete

## [✓] 3. CHECK DATA SIZE
ANSWER PROVIDED: 108KB HTML + 37MB for 100 traces = ~40-55MB total
RECOMMENDATION: Push only 100-150 most recent traces

## [⚠] 4. ONLINE SITE - FULL DASHBOARD
Currently exports static games list only
NEEDS: Either deploy Python server OR create fuller static export with all tabs
STATUS: Requires decision on deployment model

## [ ] 5. MOBILE VIEW - CLICKABLE BREAKDOWN
Fix auto-expand score breakdown to be clickable on mobile
STATUS: Not started

## [ ] 6. AUTO-PUSH TO ONLINE
Set up brain program to push data to online server at end of each run
DEPENDENCY: Needs Task 4 completed first
STATUS: Not started

## [✓] 7. ADMIN SITE LINKS
Added cwo.freeddns.org/brainadmin/ links to:
- Sign-in page (below sign in button) - DONE
- Live games page top - DONE

## CURRENT STATUS
- Dashboard running at: http://localhost:8765
- Games browser: Working with pagination
- Status tab: Labels correct (Avg Doom (R25 I40), Best Score (R24 I20))
- Admin links: Deployed
