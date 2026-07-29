package cws

import hrf.colmat._
import scala.collection.mutable.ArrayBuffer

// Option A, Phase 5 — the real training driver: replay buffer + (optionally
// parallel) self-play + a longer run loop. This is what turns the proven-but-tiny
// Phase 4 loop into something that can actually be trained for hours.
//
// Two changes over SelfPlay.trainIteration:
//   1. REPLAY BUFFER. Instead of training only on the last batch (which made the
//      loss bounce, because 4 games is a tiny, high-variance sample), we keep a
//      rolling window of the most recent N examples and train on a shuffled sample
//      of the whole window each iteration. Standard RL practice; it stabilises the
//      target and reuses hard-won game data many times.
//   2. PARALLEL SELF-PLAY (guarded). Games are independent, so they can run one
//      per core — BUT only if no shared mutable global state races across them.
//      The DS-singleton refactor removed the known offender; a state audit gates
//      whether we flip `parallelPlay` on. Until verified, it stays sequential and
//      correct.

object Trainer {

    // A rolling replay buffer of labelled examples (most recent kept).
    final class ReplayBuffer(val capacity : Int) {
        private val buf = new ArrayBuffer[Example]()
        def add(examples : Seq[Example]) : Unit = {
            buf ++= examples
            val over = buf.length - capacity
            if (over > 0) buf.remove(0, over)   // drop oldest
        }
        def size : Int = buf.length
        def snapshot : Array[Example] = buf.toArray
    }

    /** Master switch for parallel self-play. Left FALSE until the global-state audit
     *  confirms nothing races across games; flip to true once verified. */
    var parallelPlay : Boolean = false

    /** Play `nGames` self-play games with the current model, sequentially or in
     *  parallel per `parallelPlay`. Returns (all examples, per-faction win tally,
     *  noWinner count, total decisions). Each game builds its OWN searcher so no
     *  search state is shared between threads. */
    def playBatch(model : ValueNet, sims : Int, nGames : Int)
        : (Seq[Example], Map[Faction, Int], Int, Int) = {

        def playIndexed(i : Int) : ($[Example], $[Faction], Int) = {
            // Fresh searcher per game: the MCTS tree is per-decision and per-thread,
            // and ModelEval only READS the shared weights (never writes during play),
            // so this is safe to run concurrently.
            val searcher = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
            SelfPlay.playGame(searcher)
        }

        val results : Seq[($[Example], $[Faction], Int)] =
            if (parallelPlay) {
                import scala.collection.parallel.CollectionConverters._
                (0 until nGames).par.map(playIndexed).toList
            } else {
                (0 until nGames).map(playIndexed).toList
            }

        val examples = results.flatMap(_._1)
        var wins = Map[Faction, Int]().withDefaultValue(0)
        var noWinner = 0
        var decisions = 0
        results.foreach { case (_, winners, d) =>
            decisions += d
            if (winners.isEmpty) noWinner += 1
            else winners.foreach(f => wins = wins.updated(f, wins(f) + 1))
        }
        (examples, wins, noWinner, decisions)
    }

    /** SGD over a shuffled minibatch stream from the buffer, `epochs` passes. */
    def trainOnBuffer(model : ValueNet, buffer : ReplayBuffer, lr : Double,
                      epochs : Int, rng : scala.util.Random) : (Double, Double) = {
        val data = buffer.snapshot
        if (data.isEmpty) return (0.0, 0.0)
        val before = SelfPlay.logLoss(model, data)
        var e = 0
        while (e < epochs) {
            val order = rng.shuffle(data.indices.toList).toArray   // real shuffle now
            var k = 0
            while (k < order.length) {
                val ex = data(order(k))
                model.train(ex.features, ex.label, lr)
                k += 1
            }
            e += 1
        }
        (before, SelfPlay.logLoss(model, data))
    }

    final case class RunStats(iter : Int, bufferSize : Int, wins : Map[Faction, Int],
                              noWinner : Int, decisions : Int, lossBefore : Double, lossAfter : Double)

    /**
     * Full training run. Returns the trained model and per-iteration stats.
     * onIter is called after each iteration for live logging.
     */
    def run(iters : Int, gamesPerIter : Int, sims : Int, bufferCap : Int,
            lr : Double, epochs : Int, seed : Long,
            onIter : RunStats => Unit) : (ValueModel, Seq[RunStats]) = {
        val model = ValueModel.initial
        val buffer = new ReplayBuffer(bufferCap)
        val rng = new scala.util.Random(seed)
        val stats = new ArrayBuffer[RunStats]()

        var it = 0
        while (it < iters) {
            val (examples, wins, noWinner, decisions) = playBatch(model, sims, gamesPerIter)
            buffer.add(examples)
            val (before, after) = trainOnBuffer(model, buffer, lr, epochs, rng)
            val s = RunStats(it + 1, buffer.size, wins, noWinner, decisions, before, after)
            stats += s
            onIter(s)
            it += 1
        }
        (model, stats.toList)
    }
}
