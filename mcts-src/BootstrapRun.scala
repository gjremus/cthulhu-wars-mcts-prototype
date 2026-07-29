package cws

import hrf.colmat._

// Path 1 driver — bootstrap the value net from bot-vs-bot games, then test it vs the
// bots. Two modes:
//
//   time  <nGames> [parallel]
//       Just play nGames bot-vs-bot games and report throughput (games/sec, win
//       tally, avg decisions). No training. Use this to size a big run BEFORE
//       committing to it.
//
//   run   <nGames> <batch> <epochs> <sims> <evalGames> [parallel] [mlp|logistic] [hidden]
//       Play nGames bot games total, in batches of `batch`, training the value net on
//       a growing replay buffer after each batch. Every few batches, test the brain
//       vs the bots in the arena. Reports the win tally (proof the corpus has wins),
//       loss, and arena trend.

object BootstrapRun {

    val brainSeat : Faction = GC

    def main(args : Array[String]) : Unit = {
        if (args.isEmpty) { println("usage: time|run ..."); return }
        args(0) match {
            case "time" => timeMode(args)
            case "run"  => runMode(args)
            case other  => println(s"unknown mode: $other")
        }
    }

    // ---- timing probe -------------------------------------------------------
    def timeMode(args : Array[String]) : Unit = {
        val nGames   = intArg(args, 1, 50)
        val parallel = args.length > 2 && args(2).toLowerCase.startsWith("par")
        Trainer.parallelPlay = parallel
        val cores = Runtime.getRuntime.availableProcessors

        println(f"=== bootstrap TIMING probe ===")
        println(f"bot-vs-bot games=$nGames, parallel=$parallel (cores=$cores)")
        println(f"seating: ${SelfPlay.fixedSeating.map(_.short).mkString("/")}\n")

        val t0 = System.nanoTime()
        val r = Bootstrap.playBatch(nGames)
        val secs = (System.nanoTime() - t0) / 1e9

        val gps = nGames / secs
        val winStr = r.wins.toList.sortBy(-_._2).map { case (f, n) => s"${f.short}=$n" }.mkString(" ")
        println(f"played $nGames games in ${secs}%.1fs  =>  ${gps}%.2f games/sec")
        println(f"examples recorded: ${r.examples.size} (avg ${r.examples.size.toDouble / nGames}%.0f decisions/game)")
        println(f"winners: $winStr  | noWinner=${r.noWinner}  | hitCap=${r.hitCap}")
        println(f"\nprojection: 10,000 games ~= ${10000 / gps / 60}%.1f min   (${10000 / gps}%.0f s)")
    }

    // ---- full bootstrap training run ---------------------------------------
    def runMode(args : Array[String]) : Unit = {
        val nGames    = intArg(args, 1, 2000)
        val batch     = intArg(args, 2, 100)
        val epochs    = intArg(args, 3, 4)
        val sims      = intArg(args, 4, 100)
        val evalGames = intArg(args, 5, 8)
        val parallel  = args.length > 6 && args(6).toLowerCase.startsWith("par")
        val modelKind = if (args.length > 7) args(7).toLowerCase else "mlp"
        val hidden    = if (args.length > 8) args(8).toInt else 32

        Trainer.parallelPlay = parallel
        val cores = Runtime.getRuntime.availableProcessors
        val model : ValueNet =
            if (modelKind.startsWith("log")) ValueModel.initial
            else MLPModel.initial(Features.dim, hidden)

        // Replay buffer big enough to hold the whole corpus's examples (each game is
        // hundreds of decisions), so late training still sees early winning games.
        // Capped at 1.2M: at dim=772 that's ~7.4GB of feature arrays, safe under the
        // 22GB heap (2M would be ~12.4GB and risk OOM once MLP+JVM overhead is added).
        val buffer = new Trainer.ReplayBuffer(1200000)
        val rng = new scala.util.Random(12345L)

        println("=== Path 1 — BOOTSTRAP FROM BOTS ===")
        println(f"total bot games=$nGames in batches of $batch, epochs/batch=$epochs")
        println(f"model: ${model.describe}")
        println(f"reward: WIN-weighted blend (win=1.0, non-winner<=0.6, doom 0.75/shaping 0.25)")
        println(f"seating: ${SelfPlay.fixedSeating.map(_.short).mkString("/")} (base-4, Earth 3.5, no neutrals/options)")
        println(f"arena brain=${brainSeat.short}, $evalGames games; parallel=$parallel (cores=$cores)\n")

        val t0 = System.nanoTime()
        var played = 0
        var totalWins = 0
        var b = 0
        while (played < nGames) {
            val thisBatch = math.min(batch, nGames - played)
            val r = Bootstrap.playBatch(thisBatch)
            buffer.add(r.examples)
            // lr 0.01 (was 0.03): the higher rate oscillated (loss bounced 0.61-0.64)
            // once the buffer capped at 2M — too aggressive a step for that sample size.
            val (before, after) = Trainer.trainOnBuffer(model, buffer, lr = 0.01, epochs = epochs, rng)

            val batchWins = r.wins.values.sum
            totalWins += batchWins
            played += thisBatch
            b += 1
            val winStr = r.wins.toList.sortBy(-_._2).map { case (f, n) => s"${f.short}=$n" }.mkString(" ")
            val elapsed = (System.nanoTime() - t0) / 1e9
            println(f"batch $b%3d | games $played%5d/$nGames | buf ${buffer.size}%7d | loss $before%.4f->$after%.4f | " +
                    f"wins[$winStr%-20s] none=${r.noWinner}%d cap=${r.hitCap}%d | totWins=$totalWins%d | ${elapsed}%.0fs")

            // Arena every 5 batches.
            if (b % 5 == 0) arena(model, sims, evalGames)
        }

        val total = (System.nanoTime() - t0) / 1e9
        println(f"\nDone. $nGames%d bot games, $totalWins%d contained a win, in ${total}%.0fs.")
        println("\n=== final arena (12/seat) ===")
        arena(model, sims, 12)

        model match {
            case mlp : MLPModel => println(f"\nfinal model: ${mlp.describe}")
            case vm  : ValueModel =>
                println("\nfinal weights (logistic):")
                val init = ValueModel.initial.weights
                vm.weights.indices.foreach(i => println(f"  w$i%-2d ${init(i)}%+.3f -> ${vm.weights(i)}%+.3f"))
            case _ =>
        }
    }

    // Per-seat arena: brain plays each of the 4 seats in turn (1 brain vs 3 bots),
    // so a 0% in the bot-weak GC chair doesn't hide competence in the others.
    def arena(model : ValueNet, sims : Int, gamesPerSeat : Int) : Unit = {
        val perSeat = Arena.evaluatePerSeat(model, sims, gamesPerSeat)
        var totWins = 0; var totGames = 0
        val seatStrs = SelfPlay.fixedSeating.toList.map { f =>
            val r = perSeat(f)
            totWins += r.brainWins; totGames += (r.brainWins + r.botWins)
            f"${f.short}:${r.brainWins}%d/${r.brainWins + r.botWins}%d(d${r.brainDoomAvg}%.0f)"
        }
        val wr = if (totGames > 0) 100.0 * totWins / totGames else 0.0
        println(f"        >>> ARENA (brain rotates all seats, ${gamesPerSeat}%d/seat): overall ${totWins}%d/${totGames}%d = ${wr}%.0f%% | " +
                seatStrs.mkString(" "))
    }

    def intArg(a : Array[String], i : Int, d : Int) : Int = if (a.length > i) a(i).toInt else d
}
