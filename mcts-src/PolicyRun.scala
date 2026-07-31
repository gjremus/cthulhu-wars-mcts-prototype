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
        val modes = Set("clone", "arena", "combined", "selfplay", "stalltrace", "replaygame")
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

        if (mode == "replaygame") {
            // replaygame <bootGames> <bootEpochs> <hidden> [par] [lr] [sims] [gamesPerSeat] [outDir]
            //   Bootstrap both heads from the bots (same as self-play iter-0), then play
            //   `gamesPerSeat` LOGGED brain-vs-3-bots games in EACH seat, pick the best
            //   brain game, and write its replay trace (action-strings + blank + HTML log)
            //   to `outDir` for build-replay.py. No net is persisted anywhere else, so this
            //   is the only way to get a watchable brain game onto disk.
            val gamesPerSeat = intArg(a, 6, 4)
            val outDir = if (a.length > 7) a(7) else "replay-traces"
            runReplayGame(nGames, epochs, hidden, lr, parallel, sims, gamesPerSeat, outDir)
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

        // WARM-START (cross-run persistence). If a compatible best-checkpoint from a prior
        // run exists on disk, adopt it OVER the fresh bootstrap so learning ACCUMULATES
        // across runs instead of relearning from zero every time (the old behaviour, which
        // also made each run redraw a different trajectory via nondeterministic parallel
        // self-play). Bootstrap still ran above (its bot corpus is reused as the per-iter
        // anchor regularizer), but the net we CONTINUE from is the saved best. Set env
        // CW_FRESH=1 to force a cold from-bootstrap start. A dim mismatch (config change)
        // makes load return None -> clean fall back to the bootstrapped net.
        val forceFresh = sys.env.get("CW_FRESH").exists(v => v == "1" || v.equalsIgnoreCase("true"))
        var warmStarted = false
        if (forceFresh) {
            println("warm-start: CW_FRESH set -> ignoring any on-disk checkpoint (cold bootstrap start)\n")
        } else if (Checkpoint.exists) {
            (Checkpoint.loadPolicy(Features.dim, ActionFeatures.dim, hidden), Checkpoint.loadValue(Features.dim, hidden)) match {
                case (Some(p), Some(v)) =>
                    policy.adopt(p); value.adopt(v); warmStarted = true
                    println(f"warm-start: LOADED best checkpoint from disk [${Checkpoint.metaLine}] -> continuing from it (not the fresh bootstrap)\n")
                case _ =>
                    println("warm-start: on-disk checkpoint present but INCOMPATIBLE with current dims -> cold bootstrap start\n")
            }
        } else {
            println("warm-start: no on-disk checkpoint found -> cold bootstrap start (this run will create one)\n")
        }

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
        // Anchor is a REGULARIZER, not a training set — it only needs enough bot samples
        // to keep the policy from drifting below competent play. Sizing it at corpus/2
        // (old) made a 200-game bootstrap anchor on ~75k targets EVERY epoch, exploding
        // per-iter runtime (~2h/iter) with no accuracy benefit. Cap at a fixed absolute
        // sample so anchor cost is O(1) in bootstrap size; a fresh random subsample is
        // drawn each epoch, so over many epochs the whole corpus is still seen.
        val AnchorCap  = 6000
        val anchorPolN = math.min(polAll.length,  AnchorCap)
        val anchorValN = math.min(valBoot.length, AnchorCap)
        println(f"anchor: reusing $anchorPolN%d bot move-targets + $anchorValN%d bot value-states each iter\n")

        // ANTI-COLLAPSE #2 — BEST CHECKPOINT. Keep a snapshot of the best net so a later
        // decayed iteration can never overwrite the best one we found. "Best" = a composite
        // of what we actually want: games that FINISH + rituals happening (both are what
        // collapsed). Reported and used for the final arena.
        var bestPolicy = policy.copy
        var bestValue  = value.copy
        // Seed the best-bar from the on-disk checkpoint's score when we warm-started, so a
        // WEAKER new run cannot overwrite a stronger saved net (that would re-introduce the
        // exact "throw away the good brain" bug this feature fixes). Cold start keeps -1.0
        // so the first iter always sets an initial best.
        var bestScore  = if (warmStarted) Checkpoint.savedScore.getOrElse(-1.0) else -1.0
        var bestIter   = 0
        // Last arena win-rate reading, carried forward between arena evals (measured only
        // every `arenaEvery` iters) so the best-checkpoint composite always has a value.
        // 0.0 until the first arena eval fires.
        var lastArenaWR = 0.0
        if (warmStarted) println(f"best-bar seeded from disk score=${bestScore}%.2f (a new best must beat this to overwrite the saved net)\n")

        // LEAGUE POOL (lever c). Frozen opponents the learner must beat. SEEDED with the
        // bootstrap clone (a snapshot of the just-cloned bot-style net) — the closest
        // in-process stand-in for the hand-tuned arena bots, so beating it attacks the arena
        // gap head-on — and GROWN with each new best-checkpoint (progressively stronger past
        // selves, the AlphaZero-league idea). Capped so per-game opponent variety stays bounded
        // and old weak snapshots age out (keep the seed + the most recent checkpoints).
        val LeagueCap = 4
        // Weight on the arena win-rate term in the best-checkpoint composite. Sized so a real
        // arena win is a decisive tiebreaker between comparable brains without letting a lone
        // 1/32 crown an otherwise-weak net: 3.0 × (1/32 = 0.031) ≈ +0.09, i.e. one arena win
        // is worth ~0.3 rituals/game. A full sweep (32/32 = 1.0) would add +3.0 — dominant, as
        // it should be once the brain genuinely beats the bots.
        val ArenaWeight = 3.0
        val leaguePool = scala.collection.mutable.ArrayBuffer[(PolicyModel, ValueNet)]((policy.copy, value.copy))
        println(f"league: seeded with 1 bootstrap-clone opponent; grows with each best-checkpoint (cap $LeagueCap)\n")

        // --- 1-4. ITERATE: self-play -> train toward search -> relabel value -> test -
        var it = 1
        while (it <= iters) {
            val ti = System.nanoTime()
            // 1. PLAY self-play games; each records (state, candidates, MCTS visit dist)
            //    and (state, faction) for value, labelled by the self-play winner.
            val batch = selfPlayBatch(sims, gamesPerIter, policy, value, leaguePool.toList, parallel)
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

            // 4. TEST vs the bots every `arenaEvery` iterations (and on the last one).
            //    Run this BEFORE the best-checkpoint decision so the arena win-rate can feed
            //    the composite: now that the brain actually beats bots (R16 it34, first ever),
            //    the saved brain MUST be selected partly on WINNING arena games, not only on
            //    finishing self-play games + rituals (which ignored the real target entirely).
            if (it % arenaEvery == 0 || it == iters) {
                print(f"   arena @ iter $it: ")
                val (aw, ag) = reportPerSeat(Arena.evaluatePerSeatBrain(
                    () => new MCTSPolicy(sims = sims, leaf = PolicyValueEval(policy, value)), perSeat))
                lastArenaWR = if (ag > 0) aw.toDouble / ag else 0.0
            }

            // BEST-CHECKPOINT: score this iteration by what actually collapsed — games
            // that FINISH and rituals happening (finishedRate in [0,1] plus rituals/game, the
            // two symptoms of the good iter-1 shape) — PLUS the arena win-rate term so the
            // saved/warm-started brain tracks the one that beats the hand-tuned bots. Arena
            // is only measured every `arenaEvery` iters, so we carry the last reading forward;
            // ArenaWeight is large enough that a real arena win is a decisive tiebreaker but
            // can't by itself crown an otherwise-weak brain (1/32 ≈ 0.031 → +0.09 bonus).
            val finishedRate = spWins.toDouble / gamesPerIter
            val ritPerGame   = batch.flatMap(_.metrics).map(_.rituals).sum.toDouble / math.max(1, batch.size)
            val iterScore    = finishedRate + 0.3 * ritPerGame + ArenaWeight * lastArenaWR
            if (iterScore > bestScore) {
                bestScore = iterScore; bestIter = it
                bestPolicy = policy.copy; bestValue = value.copy
                // LEAGUE (lever c): admit this new best as a frozen opponent (a stronger past
                // self). Keep the seed (index 0, the bot-style clone) always, and age out the
                // OLDEST checkpoint beyond it when over cap so recent, stronger selves dominate.
                leaguePool += ((policy.copy, value.copy))
                if (leaguePool.length > LeagueCap) leaguePool.remove(1)
                // PERSIST across runs: write the new best to disk so the next run can
                // warm-start from it instead of relearning from zero. Only overwrites the
                // on-disk best when THIS run's best beats it (guard below) — set CW_RUNTAG
                // to label which run produced it.
                val runTag = sys.env.getOrElse("CW_RUNTAG", "selfplay")
                (bestPolicy, bestValue) match {
                    case (bp : PolicyModel, bv : MLPModel) => Checkpoint.save(bp, bv, it, iterScore, runTag)
                    case _ =>
                }
                println(f"   >>> new best checkpoint @ iter $it (finished=${finishedRate}%.2f rit/g=${ritPerGame}%.2f arenaWR=${lastArenaWR}%.3f score=${iterScore}%.2f) | league=${leaguePool.length} | saved to disk")
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

    /**
     * REPLAY-GAME PRODUCER — bootstrap the two-headed brain from the bots (exactly the
     * self-play iter-0 net), then play `gamesPerSeat` LOGGED games in each of the four
     * seats (1 brain vs 3 hand-tuned bots), and save the BEST brain game as a trace the
     * Python replay engine (build-replay.py) can render into a watchable HTML.
     *
     * "Best" = the game where the brain's seat did best: prefer a game the brain WON;
     * otherwise the game where the brain reached the highest doom (the tempo the runs
     * showed topping out at ~15-21 vs the bots' 30). This is the game to watch to see
     * WHERE the brain loses tempo.
     *
     * The trace format matches SimRunner's win-log dump precisely:
     *   <serialized action>\n … \n\n <div class='p'>log line</div>\n …
     * so no changes to build-replay.py are needed.
     */
    def runReplayGame(bootGames : Int, bootEpochs : Int, hidden : Int, lr : Double,
                      parallel : Boolean, sims : Int, gamesPerSeat : Int, outDir : String) : Unit = {
        println("========== REPLAY-GAME PRODUCER ==========")
        println(f"bootstrap: $bootGames bot games -> clone policy+value; then $gamesPerSeat logged games/seat (sims=$sims)\n")

        // --- bootstrap both heads (identical to runSelfPlay's iter-0) ---------------
        val tb = System.nanoTime()
        val games   = collectBoth(bootGames, parallel)
        val polAll  = games.flatMap(_._1).toArray
        val valBoot = games.flatMap(_._2).toArray
        val randomBaseline = 100.0 * polAll.map(e => 1.0 / e.actions.length).sum / polAll.length
        println(f"bootstrap corpus: ${polAll.length}%d move-decisions + ${valBoot.length}%d value-states in ${(System.nanoTime() - tb) / 1e9}%.0fs")

        val policy = PolicyModel.initial(Features.dim, ActionFeatures.dim, hidden)
        val value  = MLPModel.initial(Features.dim, hidden)
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
        println(f"bootstrapped: policy move-match ${matchAccuracy(policy, polAll)}%.1f%% (vs random ${randomBaseline}%.1f%%), value confidence ${confidencePct(SelfPlay.logLoss(value, valBoot))}%.0f%%\n")

        // --- play logged brain-vs-bots games, one seat at a time -------------------
        new java.io.File(outDir).mkdirs()
        // A candidate replay: which seat the brain played, whether it won, its doom, the
        // leader's doom, the serialized action lines and the HTML log lines.
        final case class Cand(seat : Faction, brainWon : Boolean, brainDoom : Int, leaderDoom : Int,
                              actionLines : Seq[String], logLines : Seq[String], decisions : Int)

        val cands = ArrayBuffer[Cand]()
        SelfPlay.fixedSeating.toList.foreach { seat =>
            var gi = 0
            while (gi < gamesPerSeat) {
                val g = SelfPlay.newGameLogged()
                val brain = new MCTSPolicy(sims = sims, leaf = PolicyValueEval(policy, value))
                val routing : Map[Faction, DecisionPolicy] =
                    g.setup.map(f => f -> (if (f == seat) (brain : DecisionPolicy) else BotPolicy)).toMap
                val policyMix = new MixedPolicy(routing)
                val serializer = new Serialize(g)

                // Capture the full trace: startLogged feeds the start/Options/setup block
                // into `sink`; rolloutLogged returns its OWN action/log buffers for the rest
                // of the game. Concatenate the two, in order.
                val startActions = ArrayBuffer[Action]()
                val startLog     = ArrayBuffer[String]()
                val sink : (Action, $[String]) => Unit = (act, ls) => { startActions += act; startLog ++= ls.toList }
                val startSit = Engine.startLogged(g, sink)
                val (winners, hitCap, acts, log) =
                    Engine.rolloutLogged(g, startSit, policyMix, Arena.ArenaDecisionCap)

                val actionLines = (startActions.toList ++ acts.toList).map(serializer.write)
                val logLines    = (startLog.toList ++ log.toList)

                val brainDoom  = g.players(seat).doom
                val leaderDoom = g.setup.map(f => g.players(f).doom).max
                val brainWon   = winners.nonEmpty && winners.contains(seat)
                val decisions  = acts.length
                cands += Cand(seat, brainWon, brainDoom, leaderDoom, actionLines, logLines, decisions)
                println(f"  ${seat.short}%2s game ${gi + 1}%d/$gamesPerSeat%d: brainDoom=$brainDoom%2d leaderDoom=$leaderDoom%2d won=$brainWon%-5s cap=$hitCap%-5s decisions=$decisions%d logLines=${logLines.length}%d")
                gi += 1
            }
        }

        if (cands.isEmpty) { println("no games produced"); return }

        // Pick the best: wins first, then highest brain doom, then closest to leader.
        val best = cands.sortBy(c => (if (c.brainWon) 0 else 1, -c.brainDoom, c.leaderDoom - c.brainDoom)).head
        val label = if (best.brainWon) "WIN" else "best"
        println(f"\n>>> BEST brain game: seat=${best.seat.short} won=${best.brainWon} brainDoom=${best.brainDoom} leaderDoom=${best.leaderDoom} decisions=${best.decisions}")

        // Write the trace in build-replay.py format: actions, blank line, HTML-wrapped log.
        val fname = outDir + "/brain-" + best.seat.short.toLowerCase + "-" + label + "-d" + best.brainDoom + ".txt"
        val body = best.actionLines.mkString("\n") + "\n\n" +
                   best.logLines.map(l => "<div class='p'>" + l + "</div>").mkString("\n")
        java.nio.file.Files.write(java.nio.file.Paths.get(fname), body.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        println(s">>> TRACE SAVED: $fname")
        println(f"    (${best.actionLines.length}%d action lines + ${best.logLines.length}%d log lines)")

        // Also dump a compact index of every candidate so the user can pick a different one.
        val idx = cands.zipWithIndex.map { case (c, i) =>
            f"$i%2d ${c.seat.short}%2s won=${c.brainWon}%-5s brainDoom=${c.brainDoom}%2d leaderDoom=${c.leaderDoom}%2d decisions=${c.decisions}%d"
        }.mkString("\n")
        java.nio.file.Files.write(java.nio.file.Paths.get(outDir + "/candidates-index.txt"),
            idx.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        println(s">>> candidate index: $outDir/candidates-index.txt")
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

    // PLATEAU LEVER (c), 2026-07-30 — LEAGUE / PAST-CHECKPOINT OPPONENTS.
    // R13 plateaued: best-checkpoint flat 8 iters (11-18) at 3.51, resolved-game doom stuck
    // 21.6-25.0 (never 30), arena 0/32 the entire run — the SAME arena flatline every run.
    // ROOT CAUSE of the arena gap: `selfPlayGame` drove ALL FOUR seats with the ONE current
    // net, so the learner only ever trained against clones of its OWN CURRENT STYLE. It never
    // had to beat a DIFFERENT player, so it overfits a self-consistent equilibrium that the
    // hand-tuned bots (a different style) simply sidestep — hence 0/32 forever, independent of
    // the reward. LEAGUE FIX (standard AlphaZero-league play): each self-play game now has ONE
    // rotating LEARNER seat (the current, still-training net, the only seat recorded) versus
    // THREE FROZEN league opponents drawn from a pool = the bootstrap/bot-style clone (the
    // closest in-process proxy to the arena bots — beating it directly attacks the arena gap)
    // plus accumulated best-checkpoints (strong past selves). The learner must now learn play
    // that beats OTHER styles, not just itself. Faction-agnostic: seats rotate over all four.
    def selfPlayGame(sims : Int, policy : PolicyModel, value : ValueNet,
                     league : Seq[(PolicyModel, ValueNet)], learnerSeat : Faction,
                     gameIdx : Int, decisionCap : Int = 1600) : SelfPlayGame = {
        val g = SelfPlay.newGame()
        // Learner: the current net, the ONLY seat whose decisions we record + train on.
        val learnerBrain = new MCTSPolicy(sims = sims, leaf = PolicyValueEval(policy, value))
        learnerBrain.recorder = scala.collection.mutable.ArrayBuffer[PolicyTarget]()
        // Opponents: one FROZEN league net per non-learner seat, chosen deterministically by
        // (gameIdx, seat) so the pool is exercised evenly with no RNG. Fallback to self if the
        // pool is somehow empty (shouldn't happen — it's always seeded with the bootstrap clone).
        val pool = if (league.nonEmpty) league else Seq((policy, value))
        val brainByFaction : Map[Faction, MCTSPolicy] = {
            val m = scala.collection.mutable.Map[Faction, MCTSPolicy](learnerSeat -> learnerBrain)
            g.setup.toList.filter(_ != learnerSeat).zipWithIndex.foreach { case (f, i) =>
                val (op, ov) = pool((gameIdx + i) % pool.length)
                m(f) = new MCTSPolicy(sims = sims, leaf = PolicyValueEval(op, ov))
            }
            m.toMap
        }
        val vals = scala.collection.mutable.ArrayBuffer[Example]()
        val trajectory = new Trajectory(g.setup)
        var decisions = 0

        // Decision index of each recorded example, so we can compute its position in the
        // game (progress = idx / total) for the progress-blended label below.
        val exIdx = scala.collection.mutable.ArrayBuffer[Int]()
        val recording = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                trajectory.observe(game)
                // ONLY the learner seat's states become value-training examples: its terminal
                // label + progress-blended Φ(s) is the learning signal. Opponent seats are
                // frozen league nets — we do not train the value head on their perspective.
                if (faction == learnerSeat) {
                    val ex = new Example(Features.of(game, faction), faction)
                    // Capture Φ(s) from the LIVE board now — the per-move position quality the
                    // smeared terminal label never provided. (Read here, not at game end, so it
                    // reflects THIS state, not the final one.)
                    ex.potential = Outcome.statePotential(game, faction)
                    vals += ex
                    exIdx += decisions   // GLOBAL decision index (game length denominator below)
                }
                decisions += 1
                brainByFaction(faction).decide(game, faction, actions)
            }
        }
        val s0 = Engine.start(g)
        val (winners, _) = Engine.rolloutCapped(g, s0, recording, decisionCap, throwOnCap = false)
        val shaping = trajectory.score(g)
        // PROGRESS-BLENDED per-state label (2026-07-29): each state's target blends its own
        // captured potential Φ(s_t) with the terminal outcome, weighted by how deep into the
        // game it sat. This replaces the old single terminal label smeared across every state
        // — the value net now gets a per-move gradient (early states judged by local position,
        // late states by the actual result). The terminal value is the same valueShaped label
        // used before (win=1.0, else doom/spellbook/shaping blend), so a true win still anchors
        // the endgame at 1.0. Falls back to pure terminal if potential wasn't captured.
        val total = math.max(1, decisions)
        val vArr = vals.toArray
        var vi = 0
        while (vi < vArr.length) {
            val ex = vArr(vi)
            val terminal = Outcome.valueShaped(g, winners, ex.faction, shaping.getOrElse(ex.faction, 0.0))
            ex.label =
                if (ex.potential < 0.0) terminal
                else Outcome.progressBlended(ex.potential, terminal, exIdx(vi).toDouble / total)
            vi += 1
        }
        // Same 10 faction-agnostic intermediate metrics the all-bot baseline emits, so the
        // brain's self-play games are measured by identical code — this is the diagnostic
        // for WHERE the brain falls short of the bots (SBs, GOOs, rituals, doom/AP, etc.).
        // LEAGUE (lever c): report ONLY the LEARNER seat — the other three are frozen league
        // opponents, so folding their metrics/scorecards in would dilute the learner's signal
        // (best-checkpoint scoring and the per-iter diagnostic must reflect the net we train).
        val scorecards = List(learnerSeat -> trajectory.scoreBreakdown(g, learnerSeat))
        val metrics    = GameMetrics.all(g, trajectory).filter(_.faction == learnerSeat)
        new SelfPlayGame(learnerBrain.recorder.toList, vals.toList, winners.nonEmpty, decisions,
                         metrics, scorecards)
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
                      league : Seq[(PolicyModel, ValueNet)], parallel : Boolean) : Seq[SelfPlayGame] = {
        // LEAGUE (lever c): rotate the LEARNER seat across the four factions round-robin so the
        // net learns to WIN from every seat vs the league pool (faction-agnostic training), and
        // pass gameIdx so opponent-seat selection walks the pool deterministically.
        val seats = SelfPlay.fixedSeating.toArray
        def one(i : Int) = selfPlayGame(sims, policy, value, league, seats(i % seats.length), i)
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

    /** Print a per-seat arena result block (brain rotates all four seats) and RETURN
     *  (totalBrainWins, totalGames) so the caller can feed the arena win-rate into the
     *  best-checkpoint composite. Callers that only want the printout can ignore it. */
    def reportPerSeat(perSeat : Map[Faction, Arena.ArenaResult]) : (Int, Int) = {
        var totWins = 0; var totGames = 0
        val seatStrs = SelfPlay.fixedSeating.toList.map { f =>
            val r = perSeat(f)
            totWins += r.brainWins; totGames += (r.brainWins + r.botWins)
            f"${f.short}:${r.brainWins}%d/${r.brainWins + r.botWins}%d(d${r.brainDoomAvg}%.0f)"
        }
        val wr = if (totGames > 0) 100.0 * totWins / totGames else 0.0
        println(f"   >>> overall ${totWins}%d/${totGames}%d = ${wr}%.0f%% | " + seatStrs.mkString(" "))
        (totWins, totGames)
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
