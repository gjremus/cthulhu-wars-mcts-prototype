package cws

import hrf.colmat._

// MCTS feasibility spike — isolated prototype, touches no build.
//
// Measures the two numbers that decide whether MCTS is viable on this engine:
//   Bench A: raw throughput of the existing engine (games/sec, per-perform time)
//            = the total compute budget a decision-time search would draw from.
//   Bench B: cost of the ONLY "fork a position" mechanism that exists today —
//            replay-from-scratch via Serialize. MCTS needs thousands of forks
//            per decision; this shows what each fork costs at various depths,
//            i.e. the bar a native deep-clone would have to beat.
//
// Deterministic construction mirrors Host.main / SimRunner.

object CloneStepBench {

    // A fixed 4-player seating so runs are comparable. GC must lead if present.
    def fixedSeating : $[Faction] = $(FB, GC, TS, DS)

    def newGame() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, false, $(UseGhast))

    // Play one full game; return (steps, actionLogChrono).
    def playFull(record : Boolean) : (Int, $[Action]) = {
        val game = newGame()
        var aa : $[Action] = $
        val (_, cc) = game.perform(StartAction)
        var c = cc
        var n = 0
        while (!c.isInstanceOf[GameOver]) {
            n += 1
            val a = Host.askFaction(game, c)
            if (record) aa +:= a
            val (_, cc2) = game.perform(a.unwrap)
            c = cc2
            if (n > 7000) throw new RuntimeException("step cap")
        }
        (n, aa.reverse)
    }

    def time[T](body : => T) : (T, Long) = {
        val t0 = System.nanoTime()
        val r = body
        (r, System.nanoTime() - t0)
    }

    def main(args : Array[String]) : Unit = {
        println("=== CW MCTS feasibility spike ===")
        println("Java: " + System.getProperty("java.version"))

        // --- Warmup (JIT) ---
        print("Warmup (5 games)... ")
        (1 to 5).foreach(_ => playFull(false))
        println("done")

        // --- Bench A: raw throughput ---
        val N = 60
        var totalSteps = 0L
        val (_, elapsedA) = time {
            (1 to N).foreach { _ =>
                val (steps, _) = playFull(false)
                totalSteps += steps
            }
        }
        val secA = elapsedA / 1e9
        val gamesPerSec = N / secA
        val stepsPerSec = totalSteps / secA
        val usPerStep = (elapsedA / 1000.0) / totalSteps
        println(f"\n--- Bench A: raw engine throughput ---")
        println(f"games:            $N")
        println(f"total steps:      $totalSteps  (avg ${totalSteps.toDouble/N}%.0f steps/game)")
        println(f"wall time:        $secA%.2f s")
        println(f"games/sec:        $gamesPerSec%.1f")
        println(f"steps/sec:        $stepsPerSec%,.0f")
        println(f"time per perform: $usPerStep%.1f us")

        // --- Bench B: replay-from-scratch fork cost at various depths ---
        // Record one representative full game's action log, then measure the
        // cost to rebuild state to depth D (what one MCTS fork costs today).
        print("\nRecording a reference game for replay-clone bench... ")
        val (refSteps, refLog) = playFull(true)
        println(s"$refSteps steps recorded")

        val serializer = new Serialize(newGame()) // serializer just needs symbol tables
        val depths = List(20, 50, 100, 200, 400, refSteps).distinct.filter(_ <= refSteps).sorted

        println(f"\n--- Bench B: replay-from-scratch fork cost (the only fork that exists today) ---")
        println(f"depth = actions replayed to reconstruct that position")
        println("%-8s %-14s %-16s".format("depth", "clone(ms)", "forks/sec@depth"))
        depths.foreach { d =>
            // Pre-serialize the prefix once (write cost is amortizable / cacheable);
            // measure the reconstruction (parse+perform) which is the unavoidable per-fork cost.
            val prefix = refLog.take(d)
            val serialized = prefix.map(a => serializer.write(a.unwrap))
            val reps = if (d <= 50) 200 else if (d <= 200) 60 else 20
            val (_, el) = time {
                var i = 0
                while (i < reps) {
                    val g = newGame()
                    g.perform(StartAction)
                    val s = new Serialize(g)
                    var j = 0
                    while (j < serialized.length) {
                        g.perform(s.parseAction(serialized(j)).unwrap)
                        j += 1
                    }
                    i += 1
                }
            }
            val msPerClone = (el / 1e6) / reps
            val forksPerSec = 1000.0 / msPerClone
            println(f"$d%-8d ${msPerClone}%-14.2f ${forksPerSec}%-16.0f")
        }

        println("\n=== interpretation ===")
        println("MCTS budget: (steps/sec from Bench A) shared across all rollouts in a decision.")
        println("Replay-clone forks/sec (Bench B) FALLS as the game deepens — that is the")
        println("scaling problem. A native deep-clone would be ~constant regardless of depth.")
        println("Compare forks/sec@depth-200 to the thousands-of-forks-per-decision MCTS needs.")
    }
}
