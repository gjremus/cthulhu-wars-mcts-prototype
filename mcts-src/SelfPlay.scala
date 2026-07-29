package cws

import hrf.colmat._

// Option A, Phase 4 — the self-play + training loop. This is the closure that
// turns "a search + a blank evaluator" into "a bot that taught itself".
//
// The AlphaZero cycle, one iteration:
//   1. PLAY: MCTS(ModelEval(currentModel)) sits in ALL FOUR chairs and plays a
//      batch of complete games. At every genuine decision we record the deciding
//      faction's feature vector (a training INPUT). One brain, four seats — the
//      config the user asked about.
//   2. LABEL: when a game ends, every recorded example from that game is labelled
//      1.0 if its faction was among the winners, else 0.0 (the training TARGET).
//   3. TRAIN: gradient-descent the value model on those (features, label) pairs so
//      its win-probability estimate better matches what actually won.
//   4. REPEAT: the next batch plays with the improved model. Better evaluation →
//      better search → better games → better labels → better evaluation.
//
// Nothing here is faction-specific. The features are universal and the label is
// just "did this seat win", so the SAME loop produces a competent policy for a
// faction that never had a hand-written bot — which is the entire point.

/** One training example: how a state looked to the faction about to move, and
 *  (filled in when the game ends) whether that faction went on to win. */
final class Example(val features : Array[Double], val faction : Faction) {
    var label : Double = -1.0   // set at game end: 1.0 win, 0.0 loss
}

object SelfPlay {

    // The FOUR BASE factions in a 4-player game on Earth 3.5: Great Cthulhu (GC),
    // Black Goat (BG), Yellow Sign (YS), Crawling Chaos (CC). All four are audit-
    // confirmed clone-safe (no per-game singleton vars, unlike TSExpansion), so MCTS
    // cloning is faithful and parallel self-play is safe. NO neutral units, NO neutral
    // spellbooks, NO other options — a clean base-game board. Doom track / game
    // structure are the stock RitualTrack.for4 / EarthMap4v35, unchanged.
    def fixedSeating : $[Faction] = $(GC, BG, YS, CC)
    def newGame() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, false, $())

    /** Same board/seating as newGame() but with logging ON and the MapEarth35 option
     *  set, so `perform` emits the HTML game-log lines and the Options line carries the
     *  map token build-replay.py detects (earth35). Used only for producing watchable
     *  replay traces — never inside search/self-play (logging has per-perform overhead). */
    def newGameLogged() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, true, $(MapEarth35))

    /**
     * Play ONE full self-play game with `searcher` in every seat, recording an
     * Example at each genuine decision. Returns (examples, winners, decisionCount).
     * The searcher should already wrap the current model (ModelEval).
     */
    // Self-play games are capped well below the strict 20000 guard: a real base-4
    // game to 30 doom resolves in a couple thousand decisions, so a game still going
    // at 4000 is a weak-play stalemate. We ABANDON it (no winner) rather than burn ~5x
    // the compute churning it — and thanks to shaping it's still useful training data.
    def playGame(searcher : MCTSPolicy, decisionCap : Int = 4000) : ($[Example], $[Faction], Int) = {
        val g = newGame()
        val examples = scala.collection.mutable.ArrayBuffer[Example]()
        val trajectory = new Trajectory(g.setup)

        val recording = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                // Observe the live game for the shaping trajectory (gate loss/capture
                // diffs, per-turn AP power/gate snapshots), then record how the state
                // looks to THIS faction before it acts.
                trajectory.observe(game)
                examples += new Example(Features.of(game, faction), faction)
                searcher.decide(game, faction, actions)
            }
        }

        val s0 = Engine.start(g)
        // Capped, non-throwing: an abandoned stalemate returns no winner but leaves the
        // game state readable, so its shaped labels still train the model.
        val (winners, _) = Engine.rolloutCapped(g, s0, recording, decisionCap, throwOnCap = false)

        // Label every example by the BLENDED terminal value: win = 1.0, otherwise a
        // mix of doom-relative standing and the ten shaping milestones (Outcome +
        // Shaping) — this is the partial-credit signal that gives a gradient even when
        // no faction won, the exact failure the flat run hit. Shaping is read at game
        // end when all snapshots and totals are final.
        val shaping = trajectory.score(g)
        examples.foreach(e => e.label = Outcome.valueShaped(g, winners, e.faction, shaping.getOrElse(e.faction, 0.0)))
        (examples.toList, winners, examples.length)
    }

    /**
     * One training iteration over `gamesPerIter` games. Mutates `model` in place.
     * Returns per-iteration stats for reporting.
     */
    def trainIteration(model : ValueNet, sims : Int, gamesPerIter : Int,
                       lr : Double, epochs : Int) : IterStats = {
        val searcher = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
        val allExamples = scala.collection.mutable.ArrayBuffer[Example]()
        var wins = Map[Faction, Int]().withDefaultValue(0)
        var noWinner = 0
        var totalDecisions = 0

        var gi = 0
        while (gi < gamesPerIter) {
            val (ex, winners, decisions) = playGame(searcher)
            allExamples ++= ex
            totalDecisions += decisions
            if (winners.isEmpty) noWinner += 1
            else winners.foreach(f => wins = wins.updated(f, wins(f) + 1))
            gi += 1
        }

        // Train: shuffle and SGD for a few epochs over the batch.
        val data = allExamples.toArray
        var beforeLoss = logLoss(model, data)
        var e = 0
        while (e < epochs) {
            val order = data.indices.toArray
            // Deterministic-ish shuffle without Math.random (index-based rotation).
            var k = 0
            while (k < order.length) {
                val ex = data(order(k))
                model.train(ex.features, ex.label, lr)
                k += 1
            }
            e += 1
        }
        val afterLoss = logLoss(model, data)

        IterStats(gamesPerIter, data.length, totalDecisions, wins, noWinner, beforeLoss, afterLoss)
    }

    /** Mean logistic loss of the model over a labelled batch (lower = better fit). */
    def logLoss(model : ValueNet, data : Array[Example]) : Double = {
        if (data.isEmpty) return 0.0
        var s = 0.0; var i = 0
        while (i < data.length) {
            val p = math.min(1.0 - 1e-9, math.max(1e-9, model.eval(data(i).features)))
            val y = data(i).label
            s += -(y * math.log(p) + (1 - y) * math.log(1 - p))
            i += 1
        }
        s / data.length
    }

    final case class IterStats(games : Int, examples : Int, decisions : Int,
                               wins : Map[Faction, Int], noWinner : Int,
                               lossBefore : Double, lossAfter : Double)
}
