package cws

import hrf.colmat._

// Option A — the actual training run. Trains the self-play value model for a set
// number of iterations with a replay buffer, and every `evalEvery` iterations
// pits the current brain against the hand-tuned bots (the go/no-go signal).
//
// Args: iters gamesPerIter sims bufferCap evalEvery evalGames [parallel] [mlp|logistic] [hidden]
//   e.g.  runMain cws.TrainRun 40 8 100 20000 5 12 parallel mlp 32

object TrainRun {

    // The brain is tested in the GC seat (strong hand-tuned bot → demanding target);
    // the other three seats (BG, YS, CC) are the real bots. Seating is the four base
    // factions on Earth 3.5, no neutrals/options — see SelfPlay.fixedSeating.
    val brainSeat : Faction = GC

    def main(args : Array[String]) : Unit = {
        val iters       = arg(args, 0, 40)
        val gamesPerIter= arg(args, 1, 8)
        val sims        = arg(args, 2, 100)
        val bufferCap   = arg(args, 3, 20000)
        val evalEvery   = arg(args, 4, 5)
        val evalGames   = arg(args, 5, 12)
        val parallel    = args.length > 6 && args(6).toLowerCase.startsWith("par")
        val modelKind   = if (args.length > 7) args(7).toLowerCase else "mlp"
        val hidden      = if (args.length > 8) args(8).toInt else 32

        Trainer.parallelPlay = parallel
        val cores = Runtime.getRuntime.availableProcessors

        val model : ValueNet =
            if (modelKind.startsWith("log")) ValueModel.initial
            else MLPModel.initial(Features.dim, hidden)

        println("=== Options A+B — training run ===")
        println(f"iters=$iters gamesPerIter=$gamesPerIter sims=$sims bufferCap=$bufferCap")
        println(f"model: ${model.describe}")
        println(f"reward: blended win/doom + 10-term partial-credit shaping (Options B)")
        println(f"seating: ${SelfPlay.fixedSeating.map(_.short).mkString("/")} (base-4, Earth 3.5, no neutrals/options)")
        println(f"eval every $evalEvery iters vs hand-tuned bots ($evalGames games, brain=${brainSeat.short})")
        println(f"parallel self-play: $parallel (machine has $cores cores)\n")
        val buffer = new Trainer.ReplayBuffer(bufferCap)
        val rng = new scala.util.Random(12345L)

        val t0 = System.nanoTime()
        var it = 0
        while (it < iters) {
            val (examples, wins, noWinner, decisions) = Trainer.playBatch(model, sims, gamesPerIter)
            buffer.add(examples)
            val (before, after) = Trainer.trainOnBuffer(model, buffer, lr = 0.05, epochs = 4, rng)

            val winStr = wins.toList.sortBy(-_._2).map { case (f, n) => s"${f.short}=$n" }.mkString(" ") +
                         (if (noWinner > 0) s" none=$noWinner" else "")
            val elapsed = (System.nanoTime() - t0) / 1e9
            println(f"iter ${it + 1}%3d | buf ${buffer.size}%6d | loss $before%.4f->$after%.4f | selfplay winners: $winStr%-28s | ${elapsed}%.0fs")

            if ((it + 1) % evalEvery == 0) {
                val r = Arena.evaluate(model, sims, evalGames, Set(brainSeat))
                val wr = if (r.brainWins + r.botWins > 0) 100.0 * r.brainWins / (r.brainWins + r.botWins) else 0.0
                println(f"        >>> ARENA vs bots: brain(${brainSeat.short}) wins ${r.brainWins}%d, bots win ${r.botWins}%d, none ${r.noWinner}%d " +
                        f"(brain win-rate ${wr}%.0f%%) | brain doom avg ${r.brainDoomAvg}%.1f vs leader ${r.leaderDoomAvg}%.1f")
            }
            it += 1
        }

        val total = (System.nanoTime() - t0) / 1e9
        println(f"\nDone. ${iters}%d iters in ${total}%.0fs (${total / iters}%.1fs/iter).")

        // Final, larger arena check.
        println("\n=== final arena (24 games) ===")
        val fr = Arena.evaluate(model, sims, 24, Set(brainSeat))
        val fwr = if (fr.brainWins + fr.botWins > 0) 100.0 * fr.brainWins / (fr.brainWins + fr.botWins) else 0.0
        println(f"brain(${brainSeat.short}) wins ${fr.brainWins}%d, bots win ${fr.botWins}%d, none ${fr.noWinner}%d (brain win-rate ${fwr}%.0f%%)")
        println(f"brain doom avg ${fr.brainDoomAvg}%.1f vs game leader ${fr.leaderDoomAvg}%.1f")

        model match {
            case vm : ValueModel =>
                println("\nfinal learned weights (logistic):")
                val init = ValueModel.initial.weights
                vm.weights.indices.foreach(i => println(f"  w$i%-2d ${init(i)}%+.3f -> ${vm.weights(i)}%+.3f"))
            case mlp : MLPModel =>
                println(f"\nfinal model: ${mlp.describe} (weights omitted — ${mlp.w1.length + mlp.w2.length} params)")
            case _ =>
        }
    }

    def arg(a : Array[String], i : Int, d : Int) : Int = if (a.length > i) a(i).toInt else d
}
