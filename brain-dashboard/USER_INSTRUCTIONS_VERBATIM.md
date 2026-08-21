# USER INSTRUCTIONS VERBATIM - 2026-08-17

## CRITICAL BEHAVIORAL RULES
- NEVER ask "should I continue"
- NEVER stop and say "still need"
- KEEP WORKING until ALL tasks complete
- ALWAYS TEST every change before reporting done
- NEVER describe what I'm going to do without doing it

## TASKS FROM THIS SESSION

### 1. PROGRESS CHARTS (INCOMPLETE)
"for the charts. i want data for every run. i want them sequential - the X axis should be Run0Iter. So for run 23 iter 01, it should be 23001. if it can be run_iter (23_01) even better, but if that causes problems, i'm fine with it being a pure number with no underscore. I want a linear regression for each seperate run and iter. and i want a dropdown filter for both run and iter that allows me to select multiple runs or 1 run, multiple iters or 1 iter."

"for the game browser, remember, i want EVERY ITER FROM EVERY RUN THAT HAS DATA. That should go back to run 23."

"FUCKING FIGURE OUT WHAT RUN WE ARE ON, REMEMBER IT, KEEP TRACK OF IT, AND STOP FUCKING IT UP." (WE ARE ON RUN 25)

### 2. STATUS TAB LABELS
"for the Avg Doom on the run status, i wanted the RX IY to FUCKING REPLACE THE LABEL ABOVE IT IN PARENTHESES. SO IT READS: Avg Doom (RX IY). NOT ON THE SAME FUCKING ROW AS THE DOOM ITSELF."

"BEST SCORE DOES NOT HAVE THE RX IY I TOLD YOU TO FUCKING ADD." (needs the run/iter FROM THE ACTUAL ITER AND RUN THAT HAD THE BEST FUCKING SCORE)

"to the right of ETA, a label of Self Play or Arena (depending on what this run/ iter is)"

### 3. GAME BROWSER PERFORMANCE
"FOR THE GAME LIST - JUST FUCKING MAKE IT LOAD 50 AT A TIME, WITH FUCKING PAGES."

### 4. ONLINE DEPLOYMENT
"embed the online cwo.freeddnes link to this brain site in the main admin site. bioth on the sign in page just below sign in, and at the tope of the live games page, where the linke to the game is - replace it with the link to the brain site."

"the online brain site only shows the game log. it needs to be the full site."

"for the mobile view, it just auto-expands the whole score breakout. that needs to be adjusted to be clickable on mobile also."

"for the online site, i'm assuming the data required for all of this is not enormous? let's set it up so that at the end of each run, the brain running program pushes the new data to the online server as well, assuming the data required is small. confirm the data size with me before ding this."

### 5. ADMIN PAGE LINKS
Need to embed cwo.freeddns.org/brainadmin/ link in:
- Sign-in page (below sign in button)
- Live games page at top (replace existing game link)

## CURRENT STATE
- Run: R25 iteration 40
- Games: 463 total (R24: 51, R25: 412)
- Best checkpoint: R24 I20 score=30.45
- Dashboard: http://localhost:8765
- Online: https://cwo.freeddns.org/brainadmin/

## NEW INSTRUCTIONS - 2026-08-17 (continued)

### 6. MOBILE REPLAY LAYOUT
"only thing that mobile replay really needs is that whenthe screen is very vertical, game log should shift to the left of the control/ status bar o nthe right, and should come up flush to the bottom of the map."

### 7. GAME BROWSER FIXES
"the \"prev\" and \"next\" buttons on the game browser have messaged up characters in them"

"the \"loading\" graphic for the game browswer is stil lmissing. MAKE A FUCKING TASK LIST AT THE BOTTOM OF THE TERMINAL SCREEN."

### 8. CHARTS - GAME-LEVEL DATA
"the charts were suppposed to have all runs, all games. i should have been clearer. R25_I01_G01. And then the regression chart should be across the games from each iter. right now it's showing one run (25) and each data point is an iter not a game. I want More data. I want to see how each iter performs over time. across all run sthat we have data"

### 9. ONLINE STORAGE
"let's keep the online storage to the last 5 runs."
