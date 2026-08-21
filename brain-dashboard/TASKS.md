# Brain Dashboard Tasks - 2026-08-17

## IMMEDIATE PRIORITY (from user)

1. **Add loading indicator to game browser** - Show "Loading..." while games are fetching
2. **Optimize game browser performance** - Games taking too long to load (463 games)
3. **Complete progress charts rewrite** with:
   - X-axis as run_iter format (R23_01, R24_01, R25_01, etc OR 23001, 24001, 25001)
   - Linear regression line for EACH RUN separately
   - Multi-select dropdown for runs
   - Multi-select dropdown for iters
   - Show ALL runs that have data (R23, R24, R25 at minimum)

4. **Online deployment to cwo.freeddns.org/brainadmin/**:
   - Currently only shows game log, needs full site
   - Mobile view auto-expands score breakdown - make it clickable instead
   - Confirm data size requirements
   - Set up brain program to push new data to online server at end of each run

5. **Embed link in main admin site**:
   - Add link to brain site on sign-in page (below sign in)
   - Add link at top of live games page (replace existing game link)

## COMPLETED
- ✅ Fixed game browser showing no games (score was string not number)
- ✅ Fixed run assignment (R24 for Aug 11 traces, R25 for current)
- ✅ Fixed Avg Doom label to show "(R25 I40)" in the label itself
- ✅ Fixed Best Score label to show "(R24 I20)" from best.meta
- ✅ ETA shows "(Self-play)" or "(Arena)" label
- ✅ Game browser shows both Arena and Self-play games
- ✅ All iters showing (not just even)

## STATUS
- Current run: **R25** iteration 40
- Games in database: 463 (R24: 51, R25: 412)
- Best checkpoint: R24 I20 score=30.45
