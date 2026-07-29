package cws

import hrf.colmat._

// Step census + split timing — isolated prototype, touches no build.
//
// Answers two questions the 0.9s/game floor raised:
//   (1) Of the ~2360 raw steps/game, how many are MEANINGFUL DECISIONS
//       (Ask with >1 legal action) vs. forced/chance/bookkeeping overhead?
//   (2) Of the ~378us/step, how much is the BOT thinking (askFaction) vs.
//       the ENGINE mutating state (perform)?
//
// Same loop shape as Host.main / SimRunner, but instrumented.

object StepCensus {

    def fixedSeating : $[Faction] = $(FB, GC, TS, DS)
    def newGame() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, false, $(UseGhast))

    // Buckets
    class Census {
        var decisions   = 0L  // Ask with >1 action  (the real count)
        var forcedAsk   = 0L  // Ask with exactly 1 action (auto-resolved)
        var chance      = 0L  // RollD6/RollBattle/RollAgony/DrawES
        var bookkeeping = 0L  // Force/Then/MultiAsk/DelayedContinue/other
        var total       = 0L
        // choice breadth histogram for decisions
        var branchSum   = 0L
        var branchMax    = 0
        def classify(c : Continue) : Unit = {
            total += 1
            c match {
                case Ask(_, actions) if actions.length > 1 =>
                    decisions += 1
                    branchSum += actions.length
                    if (actions.length > branchMax) branchMax = actions.length
                case Ask(_, _)                => forcedAsk += 1
                case _ : RollD6               => chance += 1
                case _ : RollBattle           => chance += 1
                case _ : RollAgony            => chance += 1
                case _ : DrawES               => chance += 1
                case _                        => bookkeeping += 1
            }
        }
    }

    def main(args : Array[String]) : Unit = {
        println("=== CW step census + split timing ===")
        // warmup
        (1 to 5).foreach(_ => runOne(new Census(), timed = false))

        val N = 40
        val census = new Census()
        var askNanos = 0L
        var performNanos = 0L
        var games = 0
        val t0 = System.nanoTime()
        (1 to N).foreach { _ =>
            val (an, pn) = runOneTimed(census)
            askNanos += an
            performNanos += pn
            games += 1
        }
        val wall = System.nanoTime() - t0

        val perGame = census.total.toDouble / games
        println(f"\ngames:                  $games")
        println(f"wall:                   ${wall/1e9}%.2f s   (${wall/1e9/games*1000}%.0f ms/game)")
        println(f"\n--- STEP CENSUS (per game averages) ---")
        println(f"total steps/game:       $perGame%.0f")
        println(f"  meaningful decisions: ${census.decisions.toDouble/games}%.0f   <== the real decision count")
        println(f"  forced (1-option):    ${census.forcedAsk.toDouble/games}%.0f")
        println(f"  chance (dice/draw):   ${census.chance.toDouble/games}%.0f")
        println(f"  bookkeeping:          ${census.bookkeeping.toDouble/games}%.0f")
        val overhead = census.total - census.decisions
        println(f"overhead (non-decision) share: ${overhead.toDouble/census.total*100}%.1f%%")
        println(f"avg choices per decision:      ${census.branchSum.toDouble/math.max(1,census.decisions)}%.1f  (max seen ${census.branchMax}%d)")

        println(f"\n--- SPLIT TIMING (where the time goes) ---")
        val askMs = askNanos/1e6
        val perfMs = performNanos/1e6
        println(f"askFaction total:       $askMs%.0f ms   (bot thinking + move-gen inside Ask)")
        println(f"perform total:          $perfMs%.0f ms   (engine state mutation)")
        val usAskPerStep = askNanos/1000.0/census.total
        val usPerfPerStep = performNanos/1000.0/census.total
        println(f"per raw step: askFaction ${usAskPerStep}%.1f us  |  perform ${usPerfPerStep}%.1f us")
        val usAskPerDec = askNanos/1000.0/census.decisions
        println(f"askFaction per MEANINGFUL decision: ${usAskPerDec}%.1f us")
        println(f"\nInterpretation: if askFaction dominates, the current bots' scoring is the cost")
        println(f"(and self-play would replace it). If perform dominates, engine state-mutation is")
        println(f"the floor and the flat-array rewrite is what matters.")
    }

    def runOne(census : Census, timed : Boolean) : Unit = runOneTimed(census); ()

    // Returns (askNanos, performNanos)
    def runOneTimed(census : Census) : (Long, Long) = {
        val game = newGame()
        var askN = 0L
        var perfN = 0L
        val (_, cc) = game.perform(StartAction)
        var c = cc
        var n = 0
        while (!c.isInstanceOf[GameOver]) {
            n += 1
            census.classify(c)
            val a0 = System.nanoTime()
            val a = Host.askFaction(game, c)
            askN += System.nanoTime() - a0
            val p0 = System.nanoTime()
            val (_, cc2) = game.perform(a.unwrap)
            perfN += System.nanoTime() - p0
            c = cc2
            if (n > 7000) throw new RuntimeException("step cap")
        }
        (askN, perfN)
    }
}
