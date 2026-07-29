package cws

import hrf.colmat._

// Phase 2 verification (cheap correctness first, then a bounded self-play game).
//
// A full self-play game where EVERY move triggers N full random rollouts is
// enormously expensive (a game is ~760 decisions; each rollout is itself much of
// a game). So we verify correctness in two bounded steps:
//
//   (A) SINGLE-DECISION probe: advance a real game to a handful of genuine
//       decisions and call MCTSPolicy.decide at each. This exercises the search
//       (clone → select → expand → rollout → backprop), proves it returns a LEGAL
//       exploded action, and — critically — proves the cloned-singleton fix (no
//       MatchError on DoomPhaseAction etc.). Cheap: a few decisions, few sims.
//
//   (B) BOUNDED self-play game: play ONE full game with MCTS in all four seats at
//       a tiny sim count, to prove it drives to a real GameOver with a winner and
//       to measure end-to-end cost. This is the exact one-brain/four-seats config
//       the self-play loop will use.

object MCTSTest {

    def fixedSeating : $[Faction] = $(FB, GC, TS, DS)
    def newGame() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, false, $(UseGhast))

    /** Advance a fresh game to the k-th genuine decision (skips forced/chance). */
    def gameAtDecision(k : Int) : (Game, Faction, $[Action], Continue) = {
        val g = newGame()
        var sit = Engine.start(g)
        var seen = 0
        while (true) {
            sit match {
                case Ended(_) => throw new RuntimeException("game ended before decision " + k)
                case Decision(f, acts, c) =>
                    seen += 1
                    if (seen >= k) return (g, f, acts, c)
                    // step past this decision using the fast bots so state stays sane
                    val a = BotPolicy.decide(g, f, acts)
                    val (_, next) = g.perform(a.unwrap)
                    sit = Engine.advanceToDecision(g, next)
            }
        }
        throw new IllegalStateException("unreachable")
    }

    def probeDecisions(sims : Int, points : Seq[Int]) : Boolean = {
        val mcts = new MCTSPolicy(sims = sims)
        var allOk = true
        points.foreach { k =>
            try {
                val (g, f, acts, _) = gameAtDecision(k)
                val t0 = System.nanoTime()
                val chosen = mcts.decide(g, f, acts)
                val ms = (System.nanoTime() - t0) / 1e6
                val legal = acts.exists(_ eq chosen)
                if (!legal) allOk = false
                println(f"  decision #$k%-4d faction=${f.short}%-3s legalMoves=${acts.num}%3d  chose=${if (legal) "LEGAL" else "*** ILLEGAL ***"}%s  ${ms}%.0f ms ($sims sims)")
            } catch { case e : Throwable =>
                allOk = false
                println(f"  decision #$k%-4d CRASH: ${e.getClass.getSimpleName}%s: ${e.getMessage}%s @ ${e.getStackTrace.head}%s")
                e.getStackTrace.take(5).foreach(s => println("      " + s))
            }
        }
        allOk
    }

    def boundedSelfPlay(sims : Int) : Unit = {
        val mcts = new MCTSPolicy(sims = sims)
        var decisions = 0
        val counting = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                decisions += 1
                mcts.decide(game, faction, actions)
            }
        }
        val g = newGame()
        val s0 = Engine.start(g)
        val t0 = System.nanoTime()
        try {
            val winners = Engine.rollout(g, s0, counting)
            val ms = (System.nanoTime() - t0) / 1e6
            println(f"  self-play game: $decisions%d decisions, winners=${winners.map(_.short).mkString("/")}, ${ms / 1000}%.1f s ($sims sims/move)")
            println(f"  ms/decision:    ${ms / math.max(1, decisions)}%.0f")
        } catch { case e : Throwable =>
            println("  self-play CRASH: " + e.getClass.getSimpleName + ": " + e.getMessage + " @ " + e.getStackTrace.head)
            e.getStackTrace.take(6).foreach(s => println("      " + s))
        }
    }

    def main(args : Array[String]) : Unit = {
        println("=== Option A Phase 2: MCTS search test ===\n")

        val sims = if (args.length > 0) args(0).toInt else 8

        println(s"(A) single-decision probes at sims=$sims — legality + cloned-singleton fix:")
        val okA = probeDecisions(sims, Seq(1, 5, 20, 60, 120, 250))

        println(s"\n(B) one bounded self-play game, MCTS in all 4 seats, sims=$sims:")
        boundedSelfPlay(sims)

        println("\n=== interpretation ===")
        println(f"(A) PASSES if every probe returned a LEGAL move with no MatchError — that")
        println(f"    proves MCTS only plays exploded leaves and the cloned-singleton fix holds.")
        println(f"(B) PASSES if the game reached a winner. ms/decision x ~760 decisions = ms/game;")
        println(f"    the search factor over Phase 1 is exactly the compute self-play buys quality with.")
        println(f"Result A: ${if (okA) "PASS" else "FAIL"}")
    }
}
