# R39/R40 Trace Writing Failure - Postmortem

## Summary
R39 and R40 games (1579 R39 games + 254 R40 games = 1833 games) have only 6-line summary traces with NO action logs. Replays cannot be generated. Training data is intact but game sequences are lost.

## Root Cause
In commit e7a98dc (R39, 2026-09-19), I created `collectRLArenaGames()` as a new iterative arena training mode. I wrote only 6-line summary traces:
```scala
pw.println(f"Brain seat: ${brainSeat.short}")
pw.println(f"Result: ${if (brainWon) "WIN" else "LOSS"}")
pw.println(f"Doom: $brainDoom")
pw.println(f"Score (0-1): $gameScore%.3f")
pw.println(f"Adaptive undos: $undoCount")
pw.println(f"Shaping breakdown: ${shaping.map { case (f, v) => f"${f.short}=$v%.2f" }.mkString(", ")}")
pw.close()
```

**I omitted the action serialization that Arena.scala had:**
```scala
val serializer = new Serialize(g)
val actionLines = (startActions.toList ++ acts.toList).map(serializer.write)
actionLines.foreach(pw.println)
```

## What I Should Have Done
Copy the full trace-writing pattern from Arena.scala (lines 262, 551, 572, 645) into collectRLArenaGames. Always write:
1. Full action sequence first
2. Blank lines
3. Metadata summary last

## Impact
- R39 (all 1579 games): 6-line summaries only
- R40 (games 1-254): 6-line summaries only  
- R40 (games 255+): Will have full traces after recompile
- Lost: Game sequences for 1833 games
- Intact: Training examples, scores, doom values

## Fix Applied (2026-09-26 14:34)
Added full trace writing to PolicyRun.scala lines 487-522:
```scala
// Save FULL trace immediately after game completes (R40+ format)
traceDir.foreach { dir =>
    val brainWon = winners.contains(brainSeat)
    val brainDoom = g.players(brainSeat).doom
    val brainSBs = g.players(brainSeat).spellbookCount
    val brainAPs = g.turnNum

    // Serialize full action sequence
    val serializer = new Serialize(g)
    val actionLines = actions.map(serializer.write)

    val pw = new java.io.PrintWriter(new java.io.File(s"$dir/arena-$runTag-iter$iterNum-${brainSeat.short.toLowerCase}-game${factionGameNumber}-d$brainDoom.txt"))

    // Write full action log first (like R39 format)
    actionLines.foreach(pw.println)
    pw.println()
    pw.println()

    // Then write metadata summary at end
    pw.println(f"Brain seat: ${brainSeat.short}")
    pw.println(f"Result: ${if (brainWon) "WIN" else "LOSS"}")
    pw.println(f"Doom: $brainDoom")
    pw.println(f"Spellbooks: $brainSBs")
    pw.println(f"Action Phases: $brainAPs")
    pw.println(f"Score (0-1): $gameScore%.3f")
    pw.println(f"Adaptive undos: $undoCount")
    pw.println(f"Shaping breakdown: ${shaping.map { case (f, v) => f"${f.short}=$v%.2f" }.mkString(", ")}")

    // Add all-faction doom data for placement calculation
    val allDoom = SelfPlay.fixedSeating.map(f => (f, g.players(f).doom)).sortBy(-_._2)
    val allDoomStr = allDoom.map { case (f, d) => f"${f.short}=$d" }.mkString(" ")
    pw.println(f"ALL_DOOM=$allDoomStr")
    pw.println(f"FINAL_SCORE=$gameScore%.3f")

    pw.close()
}
```

## Status
- Fix: Code written, NOT compiled yet
- R40 running: OLD code from 10:56AM (before fix)
- Current R40 games (254): Still 6-line summaries
- Future R40 games (255+): Will have full traces after recompile + restart

## Never Again
**ALWAYS write full action logs. NEVER write summary-only traces. Action sequences are irreplaceable - they cannot be reconstructed from scores and doom values.**
