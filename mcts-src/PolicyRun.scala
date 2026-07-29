package cws

import hrf.colmat._
import scala.collection.mutable.ArrayBuffer

// Option A, Phase 7 — behavior-cloning driver + test for the POLICY head.
//
// The value net learned "how good is this board" from bot-game OUTCOMES but never
// "which move to make" — move-blind, 0% vs the bots across three feature sizes. This
// driver trains the missing piece: it plays whole BOT-vs-bot games and, at every real
// decision, records (state vector, every candidate's action vector, the index the bot
// actually chose). The PolicyModel is then trained by cross-entropy to reproduce the
// bot's choice — imitation of MOVES, not just outcomes.
//
// Test metric (intuitive, per the user's directive to avoid raw log-loss): MOVE-MATCH
// ACCURACY — of held-out decisions, how often the policy's top-scored candidate is the
// one the bot actually played, reported next to the random-guess baseline (1/K averaged
// over the decisions). If the policy can "tell every decision apart", match accuracy
// climbs far above the random baseline.
//
// Faction-agnostic throughout: state via Features.of, candidates via ActionFeatures.of,
// never a per-faction branch. So a policy cloned from the four base bots is expected to
// transfer to bot-less factions — the whole point.

/** One behavior-cloning example: how the state looked, the feature vector of EVERY
 *  legal candidate, and which candidate the deciding faction actually chose. */
final class PolicyExample(
    val state   : Array[Double],
    val actions : Array[Array[Double]],
    val chosen  : Int
)

object PolicyRun {

    def main(args : Array[String]) : Unit = {
        // Modes:
        //   clone    <games> <epochs> <hidden> [par] [lr]                     — behavior-clone + move-match test
        //   arena    <games> <epochs> <hidden> [par] [lr] [sims] [perSeat]    — clone, then greedy & PUCT (policy only) vs bots
        //   combined <games> <epochs> <hidden> [par] [lr] [sims] [perSeat]    — train BOTH heads, then PUCT (policy prior + value net) vs bots
        val modes = Set("clone", "arena", "combined", "selfplay", "stalltrace")
        val mode = if (args.nonEmpty && modes(args(0))) args(0) else "clone"
        val a    = if (args.nonEmpty && modes(args(0))) args.drop(1) else args

        val nGames   = intArg(a, 0, 200)
        val epochs   = intArg(a, 1, 6)
        val hidden   = intArg(a, 2, 64)
        val parallel = a.length > 3 && a(3).toLowerCase.startsWith("par")
        val lr       = if (a.length > 4) a(4).toDouble else 0.02
        val sims     = intArg(a, 5, 60)
        val perSeat  = intArg(a, 6, 8)

        Trainer.parallelPlay = parallel
        val cores = Runtime.getRuntime.availableProcessors

        println(s"=== Path 1b — POLICY HEAD (mode=$mode) ===")
        println(f"state dim=${Features.dim}, action dim=${ActionFeatures.dim}, hidden=$hidden")
        println(f"bot games=$nGames, epochs=$epochs, lr=$lr, parallel=$parallel (cores=$cores)")
        println(f"seating: ${SelfPlay.fixedSeating.map(_.short).mkString("/")} (base-4, Earth 3.5, no neutrals/options)\n")

        val t0 = System.nanoTime()

        if (mode == "selfplay") {
            // selfplay <bootGames> <bootEpochs> <hidden> [par] [lr] [sims] [perSeat] [iters] [gamesPerIter] [arenaEvery]
            val iters        = intArg(a, 7, 20)
            val gamesPerIter = intArg(a, 8, 60)
            val arenaEvery   = intArg(a, 9, 2)
            runSelfPlay(nGames, epochs, hidden, lr, parallel, sims, perSeat, iters, gamesPerIter, arenaEvery)
            println(f"\ntotal time ${(System.nanoTime() - t0) / 1e9}%.0fs")
            return
        }

        if (mode == "combined") {
            runCombined(nGames, epochs, hidden, lr, parallel, sims, perSeat)
            println(f"\ntotal time ${(System.nanoTime() - t0) / 1e9}%.0fs")
            return
        }

        if (mode == "stalltrace") {
            runStallTrace(nGames, epochs, hidden, lr, parallel, sims)
            println(f"\ntotal time ${(System.nanoTime() - t0) / 1e9}%.0fs")
            return
        }

        val (model, finalTest, randomBaseline) = trainClone(nGames, epochs, hidden, lr, parallel)

        if (mode == "arena") {
            println(f"\n=== ARENA: cloned policy vs the hand-tuned bots (sims=$sims, $perSeat/seat) ===")

            // 1) GREEDY clone — NO search, just the net's top move. The honest test of
            //    what behavior cloning alone bought us.
            println("\n-- greedy clone (no search) --")
            val gPer = Arena.evaluatePerSeatBrain(() => new GreedyPolicy(model), perSeat)
            reportPerSeat(gPer)

            // 2) PUCT-MCTS — policy as prior, no value net (leaf value 0.5 + real
            //    terminal outcomes). Tests whether lookahead on top of the prior beats
            //    the bots where the value-only MCTS went 0%.
            println(f"\n-- PUCT-MCTS (policy prior, sims=$sims) --")
            val mPer = Arena.evaluatePerSeatBrain(
                () => new MCTSPolicy(sims = sims, leaf = PolicyValueEval(model, null)), perSeat)
            reportPerSeat(mPer)
        }

        println(f"\ntotal time ${(System.nanoTime() - t0) / 1e9}%.0fs")
    }

    /** Collect the bot corpus, train the policy head, print the per-epoch move-match,
     *  and return (trained model, final held-out match %, random baseline %). */
    def trainClone(nGames : Int, epochs : Int, hidden : Int, lr : Double, parallel : Boolean)
        : (PolicyModel, Double, Double) = {
        val tc = System.nanoTime()
        val examples = playPolicyBatch(nGames, parallel)
        val playSecs = (System.nanoTime() - tc) / 1e9

        val all = examples.toArray
        val nTest = math.max(1, (all.length * 0.15).toInt)
        val test  = all.drop(all.length - nTest)
        val train = all.take(all.length - nTest)
        val avgK = all.map(_.actions.length).sum.toDouble / all.length
        val randomBaseline = 100.0 * all.map(e => 1.0 / e.actions.length).sum / all.length
        println(f"corpus: ${all.length}%d decisions from $nGames games in ${playSecs}%.0fs " +
                f"(avg ${avgK}%.1f candidates/decision) | train=${train.length} test=$nTest")
        println(f"random-guess move-match baseline: ${randomBaseline}%.1f%%  (i.e. avg 1/K)\n")

        val model = PolicyModel.initial(Features.dim, ActionFeatures.dim, hidden)
        println(s"model: ${model.describe}\n")
        val rng = new scala.util.Random(12345L)

        println(f"  epoch | train-match | test-match | (baseline ${randomBaseline}%.1f%%)")
        var e = 0
        while (e < epochs) {
            val order = rng.shuffle(train.indices.toList).toArray
            var k = 0
            while (k < order.length) {
                val ex = train(order(k)); model.trainDecision(ex.state, ex.actions, ex.chosen, lr); k += 1
            }
            println(f"  ${e + 1}%5d | ${matchAccuracy(model, train)}%9.1f%% | ${matchAccuracy(model, test)}%8.1f%% |")
            e += 1
        }
        val finalTest = matchAccuracy(model, test)
        println(f"\nFINAL held-out move-match: ${finalTest}%.1f%%  vs random ${randomBaseline}%.1f%%  " +
                f"(lift +${finalTest - randomBaseline}%.1f points)")
        (model, finalTest, randomBaseline)
    }

    /**
     * COMBINED two-headed AlphaZero brain. One pass of bot games feeds BOTH heads:
     *   - the POLICY net learns which move to make (behavior-clone the bot's pick), and
     *   - the VALUE net learns how good a position is, labelled by PURE WIN/LOSS so it
     *     SELF-OPTIMIZES how much each intermediate goal (spellbooks, doom, gates, …)
     *     is worth — the weights are discovered from real outcomes, not hand-set.
     * Then PUCT-MCTS runs with the policy as move-prior AND the value net at the leaf:
     * "move like a winner" + "steer toward states that actually win".
     */
    def runCombined(nGames : Int, epochs : Int, hidden : Int, lr : Double,
                    parallel : Boolean, sims : Int, perSeat : Int) : Unit = {
        // --- collect BOTH corpora in one pass of bot games -------------------------
        val tc = System.nanoTime()
        val games = collectBoth(nGames, parallel)
        val polAll = games.flatMap(_._1).toArray
        val valAll = games.flatMap(_._2).toArray
        val playSecs = (System.nanoTime() - tc) / 1e9

        val nPolTest = math.max(1, (polAll.length * 0.15).toInt)
        val polTest  = polAll.drop(polAll.length - nPolTest)
        val polTrain = polAll.take(polAll.length - nPolTest)
        val randomBaseline = 100.0 * polAll.map(e => 1.0 / e.actions.length).sum / polAll.length
        val wins = valAll.count(_.label >= 0.5)
        println(f"corpus: ${polAll.length}%d move-decisions + ${valAll.length}%d value-states from $nGames games in ${playSecs}%.0fs")
        println(f"value labels: ${wins}%d winning-seat states / ${valAll.length}%d (win/loss ground truth — value net self-tunes goal weights)")
        println(f"random-guess move-match baseline: ${randomBaseline}%.1f%%\n")

        // --- train the POLICY head (move-picker) -----------------------------------
        val policy = PolicyModel.initial(Features.dim, ActionFeatures.dim, hidden)
        val rng = new scala.util.Random(12345L)
        println("-- training POLICY head (which move) --")
        println(f"  epoch | train-match | test-match | (baseline ${randomBaseline}%.1f%%)")
        var e = 0
        while (e < epochs) {
            val order = rng.shuffle(polTrain.indices.toList).toArray
            var k = 0
            while (k < order.length) { val ex = polTrain(order(k)); policy.trainDecision(ex.state, ex.actions, ex.chosen, lr); k += 1 }
            println(f"  ${e + 1}%5d | ${matchAccuracy(policy, polTrain)}%9.1f%% | ${matchAccuracy(policy, polTest)}%8.1f%% |")
            e += 1
        }
        println(f"  policy held-out move-match: ${matchAccuracy(policy, polTest)}%.1f%%\n")

        // --- train the VALUE head (how good a position, weights self-tuned) --------
        val value = MLPModel.initial(Features.dim, hidden)
        println("-- training VALUE head (win probability; goal weights learned from outcomes) --")
        val vEpochs = math.max(epochs, 6)
        var ve = 0
        while (ve < vEpochs) {
            val before = SelfPlay.logLoss(value, valAll)
            val order = rng.shuffle(valAll.indices.toList).toArray
            var k = 0
            while (k < order.length) { val ex = valAll(order(k)); value.train(ex.features, ex.label, lr); k += 1 }
            val after = SelfPlay.logLoss(value, valAll)
            println(f"  epoch ${ve + 1}%2d | win-call confidence ${confidencePct(before)}%.0f%% -> ${confidencePct(after)}%.0f%%")
            ve += 1
        }
        println()

        // --- ARENA: the two heads together, vs the bots ----------------------------
        println(f"=== ARENA: COMBINED brain (policy prior + value leaf) vs bots (sims=$sims, $perSeat/seat) ===")
        val per = Arena.evaluatePerSeatBrain(
            () => new MCTSPolicy(sims = sims, leaf = PolicyValueEval(policy, value)), perSeat)
        reportPerSeat(per)
    }

    /**
     * SELF-PLAY POLICY IMPROVEMENT — the lever that beats the imitation ceiling.
     *
     * Plain English: behavior cloning (the combined run) copies the bots and is capped
     * at their skill — the imitator is right ~61% of the time and never better. This
     * loop instead lets the brain IMPROVE ITSELF:
     *   0. BOOTSTRAP — start both heads from the bots (the proven 61%/92% net), so the
     *      brain begins competent instead of random.
     *   1. PLAY — the brain plays whole games AGAINST ITSELF, and at every move the
     *      MCTS search (looking ahead many sims) works out a BETTER move-distribution
     *      than the raw net first suggested. That sharpened distribution is recorded.
     *   2. LEARN THE MOVE — train the policy net TOWARD the search's distribution (not
     *      toward the bots). Because the search is stronger than the bare net, the net
     *      gets better than it was — the step a pure imitator cannot take.
     *   3. LEARN THE VALUE — relabel every visited state by who actually won the
     *      self-play game and retrain the board-judge, so it self-tunes to the brain's
     *      own improving play.
     *   4. TEST + REPEAT — periodically play the bots to see if the win rate climbs;
     *      the improved net makes the next round of self-play stronger still.
     */
    def runSelfPlay(bootGames : Int, bootEpochs : Int, hidden : Int, lr : Double,
                    parallel : Boolean, sims : Int, perSeat : Int,
                    iters : Int, gamesPerIter : Int, arenaEvery : Int) : Unit = {
        // --- 0. BOOTSTRAP both heads from the bots (proven competent start) ----------
        println("========== SELF-PLAY POLICY IMPROVEMENT ==========")
        println(f"bootstrap: $bootGames bot games -> clone policy + value; then $iters self-play iterations of $gamesPerIter games (sims=$sims)\n")
        val tb = System.nanoTime()
        val games = collectBoth(bootGames, parallel)
        val polAll = games.flatMap(_._1).toArray
        val valBoot = games.flatMap(_._2).toArray
        val randomBaseline = 100.0 * polAll.map(e => 1.0 / e.actions.length).sum / polAll.length
        println(f"bootstrap corpus: ${polAll.length}%d move-decisions + ${valBoot.length}%d value-states from $bootGames games in ${(System.nanoTime() - tb) / 1e9}%.0fs")

        val policy = PolicyModel.initial(Features.dim, ActionFeatures.dim, hidden)
        val value  = MLPModel.initial(Features.dim, hidden)
        val rng = new scala.util.Random(12345L)

        // clone the policy head (single-move target) as the starting point
        var e = 0
        while (e < bootEpochs) {
            val order = rng.shuffle(polAll.indices.toList).toArray
            var k = 0
            while (k < order.length) { val ex = polAll(order(k)); policy.trainDecision(ex.state, ex.actions, ex.chosen, lr); k += 1 }
            e += 1
        }
        // bootstrap the value head (win/loss target)
        var ve = 0
        while (ve < math.max(bootEpochs, 6)) {
            val order = rng.shuffle(valBoot.indices.toList).toArray
            var k = 0
            while (k < order.length) { val ex = valBoot(order(k)); value.train(ex.features, ex.label, lr); k += 1 }
            ve += 1
        }
        println(f"bootstrapped: policy move-match ${matchAccuracy(policy, polAll)}%.1f%% (vs random ${randomBaseline}%.1f%%), value confidence ${confidencePct(SelfPlay.logLoss(value, valBoot))}%.0f%%\n")

        // baseline arena BEFORE any self-play improvement (this is the combined-run result)
        println("-- arena @ iter 0 (bootstrap only, = combined run) --")
        reportPerSeat(Arena.evaluatePerSeatBrain(
            () => new MCTSPolicy(sims = sims, leaf = PolicyValueEval(policy, value)), perSeat))
        println()

        // ANTI-COLLAPSE #1 — ANCHOR (user directive 2026-07-28). Self-play was PEAKING at
        // iter 1 (which inherits the bots' ritual/gate habits) then DECAYING: training on
        // its own degrading games with no competent reference, rituals drained 1.3->0.0.
        // Fix: keep the bots "in the room" — every iteration also trains BOTH heads on a
        // sample of the bootstrap bot corpus (hard move-targets + real win/loss labels), so
        // the policy can never drift far below the bots' competent play. Sample size ~half
        // an iteration's self-play volume so anchoring guides without dominating.
        val anchorPolN = math.min(polAll.length,  math.max(2000, polAll.length / 2))
        val anchorValN = math.min(valBoot.length, math.max(2000, valBoot.length / 2))
        println(f"anchor: reusing $anchorPolN%d bot move-targets + $anchorValN%d bot value-states each iter\n")

        // ANTI-COLLAPSE #2 — BEST CHECKPOINT. Keep a snapshot of the best net so a later
        // decayed iteration can never overwrite the best one we found. "Best" = a composite
        // of what we actually want: games that FINISH + rituals happening (both are what
        // collapsed). Reported and used for the final arena.
        var bestPolicy = policy.copy
        var bestValue  = value.copy
        var bestScore  = -1.0
        var bestIter   = 0

        // --- 1-4. ITERATE: self-play -> train toward search -> relabel value -> test -
        var it = 1
        while (it <= iters) {
            val ti = System.nanoTime()
            // 1. PLAY self-play games; each records (state, candidates, MCTS visit dist)
            //    and (state, faction) for value, labelled by the self-play winner.
            val batch = selfPlayBatch(sims, gamesPerIter, policy, value, parallel)
            val pol   = batch.flatMap(_.targets).toArray    // PolicyTarget (soft move-target)
            val vals  = batch.flatMap(_.examples).toArray   // Example (dense shaped value)
            val spWins = batch.count(_.hadWinner)           // self-play games that actually FINISHED
            val avgLen = batch.map(_.decisions).sum.toDouble / batch.size

            // 2. LEARN THE MOVE — train policy toward the search's sharpened distribution,
            //    AND anchor to a fresh sample of bot move-targets each epoch (interleaved,
            //    so the policy improves toward search without drifting below the bots).
            var pe = 0
            while (pe < bootEpochs) {
                val order = rng.shuffle(pol.indices.toList).toArray
                var k = 0
                while (k < order.length) { val t = pol(order(k)); policy.trainDecisionSoft(t.state, t.actions, t.visits, lr); k += 1 }
                // anchor pass: hard bot move-targets (a random subsample of the bootstrap corpus)
                val aOrder = rng.shuffle(polAll.indices.toList).take(anchorPolN).toArray
                var ak = 0
                while (ak < aOrder.length) { val ex = polAll(aOrder(ak)); policy.trainDecision(ex.state, ex.actions, ex.chosen, lr); ak += 1 }
                pe += 1
            }
            // 3. LEARN THE VALUE — retrain the board-judge on self-play outcomes, ALSO
            //    anchored to a subsample of real bot win/loss value-states each epoch.
            val vBefore = SelfPlay.logLoss(value, vals)
            var vee = 0
            while (vee < math.max(bootEpochs, 6)) {
                val order = rng.shuffle(vals.indices.toList).toArray
                var k = 0
                while (k < order.length) { val ex = vals(order(k)); value.train(ex.features, ex.label, lr); k += 1 }
                val aOrder = rng.shuffle(valBoot.indices.toList).take(anchorValN).toArray
                var ak = 0
                while (ak < aOrder.length) { val ex = valBoot(aOrder(ak)); value.train(ex.features, ex.label, lr); ak += 1 }
                vee += 1
            }
            val vAfter = SelfPlay.logLoss(value, vals)
            val secs = (System.nanoTime() - ti) / 1e9
            println(f"iter $it%2d | selfplay ${spWins}%d/$gamesPerIter%d finished (avg len ${avgLen}%.0f) moves=${pol.length}%d | value conf ${confidencePct(vBefore)}%.0f%%->${confidencePct(vAfter)}%.0f%% | ${secs}%.0fs")
            // Intermediate-metrics diagnostic: average the 10 metrics across every seat in
            // this iteration's self-play games, so we can see WHICH sub-goal the brain is
            // failing at vs the all-bot baseline (not just that win rate is 0%).
            println("   " + avgMetricsLine(batch.flatMap(_.metrics)))
            // Scorecard breakdown: which shaping components are actually driving the reward,
            // ranked, so we can align the label with the metrics above and spot which reward
            // needs tweaking (e.g. avgEndGates near 0 => gate control still the gap).
            println("   " + avgScorecardLine(batch.flatMap(_.scorecards)))

            // BEST-CHECKPOINT: score this iteration by what actually collapsed — games
            // that FINISH and rituals happening. finishedRate in [0,1] plus rituals/game
            // (the two symptoms of the good iter-1 shape); snapshot the net if it's a new best.
            val finishedRate = spWins.toDouble / gamesPerIter
            val ritPerGame   = batch.flatMap(_.metrics).map(_.rituals).sum.toDouble / math.max(1, batch.size)
            val iterScore    = finishedRate + 0.3 * ritPerGame
            if (iterScore > bestScore) {
                bestScore = iterScore; bestIter = it
                bestPolicy = policy.copy; bestValue = value.copy
                println(f"   >>> new best checkpoint @ iter $it (finished=${finishedRate}%.2f rit/g=${ritPerGame}%.2f score=${iterScore}%.2f)")
            }

            // 4. TEST vs the bots every `arenaEvery` iterations (and on the last one).
            if (it % arenaEvery == 0 || it == iters) {
                print(f"   arena @ iter $it: ")
                reportPerSeat(Arena.evaluatePerSeatBrain(
                    () => new MCTSPolicy(sims = sims, leaf = PolicyValueEval(policy, value)), perSeat))
            }
            it += 1
        }

        // FINAL: evaluate the BEST checkpoint (not necessarily the last iteration, which
        // may have decayed). This is the net we'd actually keep/deploy.
        println(f"\n== BEST checkpoint = iter $bestIter (score ${bestScore}%.2f) ==")
        print("   arena @ BEST: ")
        reportPerSeat(Arena.evaluatePerSeatBrain(
            () => new MCTSPolicy(sims = sims, leaf = PolicyValueEval(bestPolicy, bestValue)), perSeat))
    }

    /** Play one self-play game with the CURRENT two-headed brain in all four seats,
     *  recording the MCTS visit-distribution move-targets and DENSE-shaped value states.
     *
     *  Value labels use `Outcome.valueShaped` (win=1.0, else co-equal doom+spellbook
     *  progress + milestone shaping), NOT pure win/loss: early self-play games rarely
     *  produce a true winner, so pure win/loss collapses to the degenerate "everyone
     *  loses" constant (flat gradient, fake 100% confidence). The dense reward gives a
     *  real gradient from turn one — the same partial-credit signal SelfPlay.playGame
     *  uses — so the value net keeps improving before the brain can finish a game.
     *
     *  `decisionCap` is lower than the 4000 self-play default: a game still going at the
     *  cap is a weak-play stalemate, and at ~all-four-seats-searching cost each extra
     *  decision is expensive, so we abandon sooner (shaped label still trains). */
    /** Result of one self-play game: move-targets, value states, whether it had a real
     *  winner, and how many decisions it ran (game length, the stalemate signal). */
    final class SelfPlayGame(val targets : Seq[PolicyTarget], val examples : Seq[Example],
                             val hadWinner : Boolean, val decisions : Int,
                             val metrics : Seq[FactionMetrics],
                             // Per-faction shaping-scorecard breakdown: (term-name -> contribution),
                             // the exact components summing to that faction's shaping label. Averaged
                             // across the batch to report WHAT drove the reward this iteration.
                             val scorecards : Seq[(Faction, Seq[(String, Double)])])

    def selfPlayGame(sims : Int, policy : PolicyModel, value : ValueNet,
                     decisionCap : Int = 1600) : SelfPlayGame = {
        val g = SelfPlay.newGame()
        val brain = new MCTSPolicy(sims = sims, leaf = PolicyValueEval(policy, value))
        brain.recorder = scala.collection.mutable.ArrayBuffer[PolicyTarget]()
        val vals = scala.collection.mutable.ArrayBuffer[Example]()
        val trajectory = new Trajectory(g.setup)
        var decisions = 0

        val recording = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                trajectory.observe(game)
                vals += new Example(Features.of(game, faction), faction)
                decisions += 1
                brain.decide(game, faction, actions)
            }
        }
        val s0 = Engine.start(g)
        val (winners, _) = Engine.rolloutCapped(g, s0, recording, decisionCap, throwOnCap = false)
        val shaping = trajectory.score(g)
        vals.foreach(ex => ex.label = Outcome.valueShaped(g, winners, ex.faction, shaping.getOrElse(ex.faction, 0.0)))
        // Same 10 faction-agnostic intermediate metrics the all-bot baseline emits, so the
        // brain's self-play games are measured by identical code — this is the diagnostic
        // for WHERE the brain falls short of the bots (SBs, GOOs, rituals, doom/AP, etc.).
        val scorecards = g.setup.toList.map(f => f -> trajectory.scoreBreakdown(g, f))
        new SelfPlayGame(brain.recorder.toList, vals.toList, winners.nonEmpty, decisions,
                         GameMetrics.all(g, trajectory), scorecards)
    }

    /** DIAGNOSTIC: bootstrap the policy exactly like self-play, then run ONE self-play
     *  game logging WHAT the brain actually does — action-kind tally, per-faction acted
     *  distribution, and the turn/phase progression — to distinguish a behavioral stall
     *  (won't end turns / won't do productive moves) from a driver/search LOOP (same menu
     *  action forever, which no reward can fix). Cheap: one game, prints and returns. */
    def runStallTrace(bootGames : Int, bootEpochs : Int, hidden : Int, lr : Double,
                      parallel : Boolean, sims : Int) : Unit = {
        println("========== STALL TRACE (1 self-play game) ==========")
        val games   = collectBoth(bootGames, parallel)
        val polAll  = games.flatMap(_._1).toArray
        val valBoot = games.flatMap(_._2).toArray
        val policy  = PolicyModel.initial(Features.dim, ActionFeatures.dim, hidden)
        val value   = MLPModel.initial(Features.dim, hidden)
        val rng = new scala.util.Random(12345L)
        var e = 0
        while (e < bootEpochs) {
            val order = rng.shuffle(polAll.indices.toList).toArray
            var k = 0
            while (k < order.length) { val ex = polAll(order(k)); policy.trainDecision(ex.state, ex.actions, ex.chosen, lr); k += 1 }
            e += 1
        }
        var ve = 0
        while (ve < math.max(bootEpochs, 6)) {
            val order = rng.shuffle(valBoot.indices.toList).toArray
            var k = 0
            while (k < order.length) { val ex = valBoot(order(k)); value.train(ex.features, ex.label, lr); k += 1 }
            ve += 1
        }
        println(f"bootstrapped from $bootGames games; now tracing ONE self-play game (sims=$sims, cap=1600)\n")

        val g = SelfPlay.newGame()
        val brain = new MCTSPolicy(sims = sims, leaf = PolicyValueEval(policy, value))
        val kindCount = scala.collection.mutable.LinkedHashMap[String, Int]()
        val actedByFaction = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
        var decisions = 0
        var lastTurn = -1
        var lastPhase = -1
        // Show the first N decisions verbatim so we can eyeball an immediate loop.
        val SHOW = 40

        val tracing = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                decisions += 1
                if (game.turn != lastTurn || game.phase != lastPhase) {
                    println(f"  [dec $decisions%5d] --> turn ${game.turn}%d phase ${game.phase}%d (doom=${game.doomPhase})  gates: " +
                        game.setup.toList.map(f => f.short + "=" + game.players(f).allGates.num).mkString(" "))
                    lastTurn = game.turn; lastPhase = game.phase
                }
                val chosen = brain.decide(game, faction, actions)
                val kind = chosen.unwrap match { case p : Product => p.productPrefix; case x => x.getClass.getSimpleName }
                kindCount(kind) = kindCount.getOrElse(kind, 0) + 1
                actedByFaction(faction.short) = actedByFaction(faction.short) + 1
                if (decisions <= SHOW)
                    println(f"    dec $decisions%4d ${faction.short}%3s : chose $kind%-28s (of ${actions.num}%d legal)")
                chosen
            }
        }
        val s0 = Engine.start(g)
        val (winners, _) = Engine.rolloutCapped(g, s0, tracing, 1600, throwOnCap = false)

        println(f"\n=== RESULT: $decisions%d decisions, reached turn ${g.turn}%d, winners=${winners.map(_.short).mkString(",")}%s ===")
        println("action-kind tally (most-chosen first):")
        kindCount.toList.sortBy(-_._2).foreach { case (k, n) =>
            println(f"   ${n}%6d  ${100.0 * n / decisions}%5.1f%%  $k")
        }
        println("decisions per faction: " + actedByFaction.toList.sortBy(_._1).map { case (f, n) => s"$f=$n" }.mkString(" "))
    }

    def selfPlayBatch(sims : Int, nGames : Int, policy : PolicyModel, value : ValueNet,
                      parallel : Boolean) : Seq[SelfPlayGame] = {
        def one(i : Int) = selfPlayGame(sims, policy, value)
        if (parallel) {
            import scala.collection.parallel.CollectionConverters._
            (0 until nGames).par.map(one).toList
        } else (0 until nGames).map(one).toList
    }

    /** One bot game recording BOTH a PolicyExample per real decision AND an Example
     *  (state + faction) per decision, the latter labelled by pure win/loss at game end. */
    def collectBothGame(decisionCap : Int = 8000) : (Seq[PolicyExample], Seq[Example]) = {
        val g = SelfPlay.newGame()
        val pol = ArrayBuffer[PolicyExample]()
        val vals = ArrayBuffer[Example]()

        val recording = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                val chosen = BotPolicy.decide(game, faction, actions)
                vals += new Example(Features.of(game, faction), faction)
                if (actions.num > 1) {
                    val acts = actions.toArray
                    val idx  = { val r = acts.indexWhere(_ eq chosen); if (r >= 0) r else acts.indexWhere(_ == chosen) }
                    if (idx >= 0) pol += new PolicyExample(Features.of(game, faction), acts.map(a => ActionFeatures.of(game, faction, a)), idx)
                }
                chosen
            }
        }

        val s0 = Engine.start(g)
        val (winners, _) = Engine.rolloutCapped(g, s0, recording, decisionCap, throwOnCap = false)
        // Value labels = pure win/loss ground truth (so goal weights are learned, not imposed).
        vals.foreach(e => e.label = Outcome.winLoss(g, winners, e.faction))
        (pol.toList, vals.toList)
    }

    def collectBoth(nGames : Int, parallel : Boolean) : Seq[(Seq[PolicyExample], Seq[Example])] = {
        def one(i : Int) = collectBothGame()
        if (parallel) {
            import scala.collection.parallel.CollectionConverters._
            (0 until nGames).par.map(one).toList
        } else (0 until nGames).map(one).toList
    }

    /** Turn a mean log-loss into the intuitive "win-call confidence" %: how often the
     *  brain's yes/no win call is right (0.5 loss = 50% coin-flip, lower = better). */
    def confidencePct(logLoss : Double) : Double = 100.0 * math.exp(-logLoss)

    /** Average the 10 intermediate metrics across a batch of per-faction game results
     *  into one compact diagnostic line (same quantities the all-bot baseline reports). */
    def avgMetricsLine(ms : Seq[FactionMetrics]) : String = {
        if (ms.isEmpty) return "metrics: (none)"
        val n = ms.size.toDouble
        val doom   = ms.map(_.doom).sum / n
        val sbCnt  = ms.map(_.spellbookCount).sum / n
        val sbrCnt = ms.map(_.sbrsAchieved.size).sum / n
        val gooCnt = ms.map(_.goosAwakened.size).sum / n
        val aps    = ms.map(_.aps).sum / n
        val dpAP   = ms.map(_.doomPerAP).sum / n
        val spAP   = ms.map(_.avgStartPow).sum / n
        val rit    = ms.map(_.rituals).sum / n
        val dpRit  = ms.map(_.doomPerRit).sum / n
        val esRit  = ms.map(_.esPerRit).sum / n
        val es     = ms.map(_.elderSigns).sum / n
        val gAP    = ms.map(_.avgEndGates).sum / n
        val abAP   = ms.map(_.avgEndAband).sum / n
        f"avg/seat: doom=$doom%.1f SB=$sbCnt%.1f/6 SBR=$sbrCnt%.1f GOO=$gooCnt%.2f APs=$aps%.1f " +
        f"doom/AP=$dpAP%.1f startPow/AP=$spAP%.1f rit=$rit%.1f doom/rit=$dpRit%.1f ES/rit=$esRit%.1f ES=$es%.1f " +
        f"gates/AP=$gAP%.1f aband/AP=$abAP%.1f"
    }

    /** Average shaping SCORECARD across every seat in a batch, sorted by contribution, so
     *  we can see WHAT drove the shaping reward this iteration and align it with the metrics.
     *  `cards` is one (faction, breakdown) per seat per game; we average each named term. */
    def avgScorecardLine(cards : Seq[(Faction, Seq[(String, Double)])]) : String = {
        if (cards.isEmpty) return "scorecard: (none)"
        val n = cards.size.toDouble
        val sums = scala.collection.mutable.LinkedHashMap[String, Double]()
        cards.foreach { case (_, terms) => terms.foreach { case (k, v) => sums(k) = sums.getOrElse(k, 0.0) + v } }
        val avg   = sums.map { case (k, s) => (k, s / n) }
        val total = avg.values.sum
        val ranked = avg.toList.sortBy(-_._2)
        val parts  = ranked.map { case (k, v) => f"$k=$v%.3f" }
        f"scorecard avg/seat (total=$total%.3f): " + parts.mkString(" ")
    }

    /** Print a per-seat arena result block (brain rotates all four seats). */
    def reportPerSeat(perSeat : Map[Faction, Arena.ArenaResult]) : Unit = {
        var totWins = 0; var totGames = 0
        val seatStrs = SelfPlay.fixedSeating.toList.map { f =>
            val r = perSeat(f)
            totWins += r.brainWins; totGames += (r.brainWins + r.botWins)
            f"${f.short}:${r.brainWins}%d/${r.brainWins + r.botWins}%d(d${r.brainDoomAvg}%.0f)"
        }
        val wr = if (totGames > 0) 100.0 * totWins / totGames else 0.0
        println(f"   >>> overall ${totWins}%d/${totGames}%d = ${wr}%.0f%% | " + seatStrs.mkString(" "))
    }

    /** Play `nGames` bot-vs-bot games, recording a PolicyExample at every real decision
     *  (the bot's chosen index among the exploded candidates). */
    def playPolicyBatch(nGames : Int, parallel : Boolean) : Seq[PolicyExample] = {
        def one(i : Int) : Seq[PolicyExample] = playPolicyGame()
        if (parallel) {
            import scala.collection.parallel.CollectionConverters._
            (0 until nGames).par.map(one).toList.flatten
        } else {
            (0 until nGames).flatMap(one)
        }
    }

    /** One bot-vs-bot game. At each decision we encode the state and EVERY candidate,
     *  let the bot choose, and record which candidate index that was. */
    def playPolicyGame(decisionCap : Int = 8000) : Seq[PolicyExample] = {
        val g = SelfPlay.newGame()
        val out = ArrayBuffer[PolicyExample]()

        val recording = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                val chosen = BotPolicy.decide(game, faction, actions)   // the bot's real move
                if (actions.num > 1) {
                    // Encode state + every candidate; find the bot's pick by identity.
                    val acts = actions.toArray
                    val idx  = {
                        val byRef = acts.indexWhere(_ eq chosen)
                        if (byRef >= 0) byRef else acts.indexWhere(_ == chosen)
                    }
                    if (idx >= 0) {
                        val state = Features.of(game, faction)
                        val af = acts.map(a => ActionFeatures.of(game, faction, a))
                        out += new PolicyExample(state, af, idx)
                    }
                }
                chosen
            }
        }

        val s0 = Engine.start(g)
        Engine.rolloutCapped(g, s0, recording, decisionCap, throwOnCap = false)
        out.toList
    }

    /** Fraction of decisions where the policy's argmax equals the bot's choice, in %. */
    def matchAccuracy(model : PolicyModel, data : Array[PolicyExample]) : Double = {
        if (data.isEmpty) return 0.0
        var hit = 0; var i = 0
        while (i < data.length) {
            val ex = data(i)
            if (model.argmax(ex.state, ex.actions) == ex.chosen) hit += 1
            i += 1
        }
        100.0 * hit / data.length
    }

    def intArg(a : Array[String], i : Int, d : Int) : Int = if (a.length > i) a(i).toInt else d
}
