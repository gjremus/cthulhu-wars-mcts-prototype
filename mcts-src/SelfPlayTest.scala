package cws

import hrf.colmat._

// Phase 3+4 verification and the real performance measurement.
//
//   (1) SPEED: same search, two leaf estimators — RolloutEval (play to the end)
//       vs ModelEval (one evaluation, no playout). Reports ms/decision and the
//       effective games/second for each, so we can quote the true number and the
//       speedup the value net buys.
//   (2) LEARNING: run several self-play training iterations with ModelEval and
//       show the logistic loss dropping — proof the loop closes (the model's
//       win-probability estimate improves from self-play outcomes alone).

object SelfPlayTest {

    // A whole game is ~800 decisions; time a fixed slice of decisions instead of
    // whole games so the speed test is quick and comparable across estimators.
    // We call the searcher (the measured work), then advance the real game with the
    // fast bot so we keep feeding it a steady stream of mid-game decisions.
    def timeDecisionsClean(searcher : MCTSPolicy, nDecisions : Int) : Double = {
        var g = SelfPlay.newGame()
        var sit = Engine.start(g)
        var done = 0
        val t0 = System.nanoTime()
        while (done < nDecisions) {
            sit match {
                case Ended(_) => g = SelfPlay.newGame(); sit = Engine.start(g)
                case Decision(f, acts, _) =>
                    searcher.decide(g, f, acts)                 // the measured work
                    val a = BotPolicy.decide(g, f, acts)        // advance the game
                    val (_, next) = g.perform(a.unwrap)
                    sit = Engine.advanceToDecision(g, next)
                    done += 1
            }
        }
        (System.nanoTime() - t0) / 1e6
    }

    // Assume a game is this many decisions when converting ms/decision -> games/sec.
    val DecisionsPerGame = 800.0

    def main(args : Array[String]) : Unit = {
        println("=== Option A Phase 3+4: value model, speed, and self-play learning ===\n")

        val sims  = if (args.length > 0) args(0).toInt else 30
        val nDec  = if (args.length > 1) args(1).toInt else 40
        val iters = if (args.length > 2) args(2).toInt else 5
        val gpi   = if (args.length > 3) args(3).toInt else 3

        // ---------- (1) SPEED: rollout leaf vs model leaf ----------
        println(s"(1) SPEED — $nDec decisions each, sims=$sims per move:\n")
        val model = ValueModel.initial

        val modelSearcher   = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
        val rolloutSearcher = new MCTSPolicy(sims = sims, leaf = RolloutEval(RandomPolicy))

        // warmup
        timeDecisionsClean(modelSearcher, 3); timeDecisionsClean(rolloutSearcher, 1)

        val msModel   = timeDecisionsClean(modelSearcher, nDec)
        val perDecModel = msModel / nDec
        println(f"  ModelEval   : ${perDecModel}%.1f ms/decision -> ~${1000.0 / (perDecModel * DecisionsPerGame)}%.3f games/sec (1 core)")

        val rolloutDec = math.max(6, nDec / 4)   // rollout is slow; time fewer
        val msRollout = timeDecisionsClean(rolloutSearcher, rolloutDec)
        val perDecRollout = msRollout / rolloutDec
        println(f"  RolloutEval : ${perDecRollout}%.1f ms/decision -> ~${1000.0 / (perDecRollout * DecisionsPerGame)}%.4f games/sec (1 core)")
        println(f"  => ModelEval speedup over RolloutEval: ${perDecRollout / perDecModel}%.0fx")

        // ---------- (2) LEARNING: self-play training iterations ----------
        println(f"\n(2) LEARNING — $iters%d iterations x $gpi%d games, sims=$sims, ModelEval self-play:\n")
        println("  iter | games | examples | decisions | loss(before->after) | winners")
        val trainModel = ValueModel.initial
        var it = 0
        while (it < iters) {
            val st = SelfPlay.trainIteration(trainModel, sims = sims, gamesPerIter = gpi, lr = 0.05, epochs = 4)
            val winStr = st.wins.toList.sortBy(-_._2).map { case (f, n) => s"${f.short}=$n" }.mkString(" ") +
                         (if (st.noWinner > 0) s" (none=${st.noWinner})" else "")
            println(f"  ${it + 1}%4d | ${st.games}%5d | ${st.examples}%8d | ${st.decisions}%9d | ${st.lossBefore}%.4f -> ${st.lossAfter}%.4f | $winStr%s")
            it += 1
        }

        println("\n  final learned weights (vs initial hand-set):")
        val init = ValueModel.initial.weights
        trainModel.weights.indices.foreach { i =>
            println(f"    w$i%-2d ${init(i)}%+.3f -> ${trainModel.weights(i)}%+.3f")
        }

        println("\n=== interpretation ===")
        println("(1) ModelEval should be many-x faster than RolloutEval (no whole-game playout")
        println("    per leaf) — that is the main performance lever and the real games/sec.")
        println("(2) loss trending DOWN across iterations = the self-play loop is learning a")
        println("    faction-agnostic value from win/loss alone. That is the AlphaZero closure.")
    }
}
