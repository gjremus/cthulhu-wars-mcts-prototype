package cws

import hrf.colmat._

// Phase 1 verification: the engine harness plays full games under a pluggable
// policy, reaching GameOver with a winner, and the two shipped policies behave
// as expected (BotPolicy = the real bots; RandomPolicy = uniform random).

object EngineTest {

    def fixedSeating : $[Faction] = $(FB, GC, TS, DS)
    def newGame() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, false, $(UseGhast))

    def playOne(policy : DecisionPolicy) : ($[Faction], Boolean) = {
        val g = newGame()
        val s0 = Engine.start(g)
        try {
            val winners = Engine.rollout(g, s0, policy)
            (winners, true)
        } catch { case e : Throwable =>
            println("    CRASH: " + e.getClass.getSimpleName + ": " + e.getMessage + " @ " + e.getStackTrace.head)
            ($, false)
        }
    }

    def main(args : Array[String]) : Unit = {
        println("=== Option A Phase 1: engine harness test ===")

        // Warmup
        (1 to 3).foreach(_ => playOne(BotPolicy))

        val N = 20
        println(f"\n--- $N%d games under BotPolicy (the real hand-tuned bots) ---")
        var botOk = 0; var botWinners = Map[String, Int]().withDefaultValue(0); var noWin = 0
        val t0 = System.nanoTime()
        (1 to N).foreach { _ =>
            val (w, ok) = playOne(BotPolicy)
            if (ok) { botOk += 1; if (w.isEmpty) noWin += 1 else w.foreach(f => botWinners = botWinners.updated(f.short, botWinners(f.short) + 1)) }
        }
        val botMs = (System.nanoTime() - t0) / 1e6
        println(f"  completed to GameOver: $botOk%d / $N%d")
        println(f"  ms/game:               ${botMs / N}%.0f")
        println(f"  winners by faction:    ${botWinners.toList.sortBy(-_._2).map { case (f, n) => s"$f=$n" }.mkString(", ")}")
        println(f"  games with no winner:  $noWin%d")

        println(f"\n--- $N%d games under RandomPolicy (uniform-random baseline) ---")
        var rndOk = 0; var rndWinners = Map[String, Int]().withDefaultValue(0)
        (1 to N).foreach { _ =>
            val (w, ok) = playOne(RandomPolicy)
            if (ok) { rndOk += 1; w.foreach(f => rndWinners = rndWinners.updated(f.short, rndWinners(f.short) + 1)) }
        }
        println(f"  completed to GameOver: $rndOk%d / $N%d")
        println(f"  winners by faction:    ${rndWinners.toList.sortBy(-_._2).map { case (f, n) => s"$f=$n" }.mkString(", ")}")

        println("\n=== interpretation ===")
        println("Phase 1 PASSES if BotPolicy games all reach GameOver with winners, at roughly")
        println("the same ms/game as SimRunner, and RandomPolicy also completes (weaker/messier).")
        println("This proves the pluggable-policy rollout primitive that MCTS + self-play sit on.")
    }
}
