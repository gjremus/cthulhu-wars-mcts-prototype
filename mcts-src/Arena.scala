package cws

import hrf.colmat._
import scala.collection.mutable.ArrayBuffer

// Option A, Phase 6 — the go/no-go test: the self-taught brain vs. the existing
// hand-tuned bots.
//
// This is the whole point of the proof-of-concept. If a value model trained purely
// from self-play — with ZERO faction strategy coded in — can hold its own against
// the hand-tuned bots on a faction that HAS one, the approach is validated and we
// move to the bot-less factions. If it can't even at scale, that tells us the
// simple one-neuron brain is the bottleneck and a deeper net is warranted.
//
// Mechanism: a MixedPolicy routes each seat to either the MCTS brain or the real
// bot, per a seat->side assignment. Everything downstream is unchanged — the
// harness doesn't know or care which brain answers a given decision.

/** Routes each faction's decisions to a different underlying policy. */
class MixedPolicy(routing : Map[Faction, DecisionPolicy]) extends DecisionPolicy {
    def decide(game : Game, faction : Faction, actions : $[Action]) : Action =
        routing.getOrElse(faction, BotPolicy).decide(game, faction, actions)
}

/** Pure behavior-cloned policy — NO search. Picks the single highest-scoring legal
 *  action per the policy net (the net's argmax over the exploded candidates). This is
 *  the honest test of what the clone learned on its own, before MCTS adds any lookahead.
 *  Faction-agnostic: state via Features, candidates via ActionFeatures, no per-faction
 *  branch. */
class GreedyPolicy(model : PolicyModel) extends DecisionPolicy {
    def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
        if (actions.num == 1) return actions.head
        val acts  = actions.toArray
        val state = Features.of(game, faction)
        val af    = acts.map(a => ActionFeatures.of(game, faction, a))
        acts(model.argmax(state, af))
    }
}

object Arena {

    /** Play one game with `brainSeats` played by the MCTS model and the rest by the
     *  hand-tuned bots. Returns the winners. */
    def playMatch(model : ValueNet, sims : Int, brainSeats : Set[Faction],
                  decisionCap : Int = 20000) : $[Faction] = {
        val g = SelfPlay.newGame()
        val brain = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
        val routing : Map[Faction, DecisionPolicy] =
            g.setup.map(f => f -> (if (brainSeats.contains(f)) brain else BotPolicy)).toMap
        val policy = new MixedPolicy(routing)
        val s0 = Engine.start(g)
        Engine.rolloutCapped(g, s0, policy, decisionCap, throwOnCap = false)._1
    }

    final case class ArenaResult(games : Int, brainSeats : Set[Faction],
                                 brainWins : Int, botWins : Int, noWinner : Int,
                                 brainDoomAvg : Double, leaderDoomAvg : Double)

    /**
     * Play `nGames`, each with `brainSeats` controlled by the model. Rotates which
     * seats the brain plays across games if `rotate` (so it's tested in every
     * chair, not just one faction's). Reports how often a brain-seat won vs a
     * bot-seat, plus how much doom the brain's seats averaged relative to the
     * game leader — a finer signal than win/loss while play is still weak.
     */
    // A stalemating brain game is a loss signal, not information worth paying for —
    // so cap arena games at the same budget as self-play (4000) instead of the
    // 20000 rollout default. Without this, weak-brain games run to 20000 decisions
    // at din=480 eval and dominate arena wall-clock.
    val ArenaDecisionCap = 4000

    def evaluate(model : ValueNet, sims : Int, nGames : Int,
                 brainSeats : Set[Faction]) : ArenaResult = {

        // One independent game -> (brainWon?, noWinner?, brainSeatDoomAvg, leaderDoom).
        // Each game builds its OWN searcher and ModelEval only READS the shared weights,
        // so these run concurrently safely — same guarantee as Trainer.playBatch. The
        // rich-feature eval is 40x heavier per leaf, so a sequential arena dominates
        // wall-clock; parallelising it is what keeps arena cost off the critical path.
        def one(i : Int) : (Int, Int, Double, Double) = {
            val g = SelfPlay.newGame()
            val brain = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
            val routing = g.setup.map(f => f -> (if (brainSeats.contains(f)) brain else (BotPolicy : DecisionPolicy))).toMap
            val policy = new MixedPolicy(routing)
            val winners = Engine.rolloutCapped(g, Engine.start(g), policy, ArenaDecisionCap, throwOnCap = false)._1

            val brainW = if (winners.nonEmpty && winners.exists(brainSeats.contains)) 1 else 0
            val noWin  = if (winners.isEmpty) 1 else 0
            val leaderDoom = g.setup.map(f => g.players(f).doom).max.toDouble
            val bd = brainSeats.toList.map(f => g.players(f).doom.toDouble)
            (brainW, noWin, if (bd.nonEmpty) bd.sum / bd.size else 0.0, leaderDoom)
        }

        val results : Seq[(Int, Int, Double, Double)] =
            if (Trainer.parallelPlay) {
                import scala.collection.parallel.CollectionConverters._
                (0 until nGames).par.map(one).toList
            } else {
                (0 until nGames).map(one).toList
            }

        val brainWins = results.map(_._1).sum
        val noWinner  = results.map(_._2).sum
        val decided   = nGames - noWinner
        val botWins   = decided - brainWins
        val counted   = results.size
        val brainDoomAvg  = if (counted > 0) results.map(_._3).sum / counted else 0.0
        val leaderDoomAvg = if (counted > 0) results.map(_._4).sum / counted else 0.0
        ArenaResult(nGames, brainSeats, brainWins, botWins, noWinner, brainDoomAvg, leaderDoomAvg)
    }

    /** Arena evaluation that also returns training examples from arena games.
     *  Brain plays in `brainSeats`, bots play in remaining seats. Returns:
     *  (examples, arenaResult). Examples are labeled with progress-blended labels
     *  just like self-play, so arena games feed directly into training. */
    def evaluateWithExamples(model : ValueNet, sims : Int, nGames : Int,
                             brainSeats : Set[Faction]) : ($[Example], ArenaResult) = {

        def one(i : Int) : ($[Example], Int, Int, Double, Double) = {
            val g = SelfPlay.newGame()
            val brain = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
            val routing = g.setup.map(f => f -> (if (brainSeats.contains(f)) brain else (BotPolicy : DecisionPolicy))).toMap
            val policy = new MixedPolicy(routing)

            val examples = scala.collection.mutable.ArrayBuffer[Example]()
            val trajectory = new Trajectory(g.setup)
            var decisions = 0
            val exIdx = scala.collection.mutable.ArrayBuffer[Int]()

            val recording = new DecisionPolicy {
                def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                    if (brainSeats.contains(faction)) {
                        trajectory.observe(game)
                        val ex = new Example(Features.of(game, faction), faction)
                        ex.potential = Outcome.statePotential(game, faction)
                        examples += ex
                        exIdx += decisions
                    }
                    decisions += 1
                    policy.decide(game, faction, actions)
                }
            }

            val s0 = Engine.start(g)
            val (winners, _) = Engine.rolloutCapped(g, s0, recording, ArenaDecisionCap, throwOnCap = false)

            // Label examples with progress-blended labels
            val shaping = trajectory.score(g)
            val total = math.max(1, decisions)
            val exArr = examples.toArray
            var i = 0
            while (i < exArr.length) {
                val e = exArr(i)
                val terminal = Outcome.valueShaped(g, winners, e.faction, shaping.getOrElse(e.faction, 0.0))
                e.label =
                    if (e.potential < 0.0) terminal
                    else Outcome.progressBlended(e.potential, terminal, exIdx(i).toDouble / total)
                i += 1
            }

            val brainW = if (winners.nonEmpty && winners.exists(brainSeats.contains)) 1 else 0
            val noWin  = if (winners.isEmpty) 1 else 0
            val leaderDoom = g.setup.map(f => g.players(f).doom).max.toDouble
            val bd = brainSeats.toList.map(f => g.players(f).doom.toDouble)
            (examples.toList, brainW, noWin, if (bd.nonEmpty) bd.sum / bd.size else 0.0, leaderDoom)
        }

        val results : Seq[($[Example], Int, Int, Double, Double)] =
            if (Trainer.parallelPlay) {
                import scala.collection.parallel.CollectionConverters._
                (0 until nGames).par.map(one).toList
            } else {
                (0 until nGames).map(one).toList
            }

        val allExamples = results.flatMap(_._1).toList
        val brainWins = results.map(_._2).sum
        val noWinner  = results.map(_._3).sum
        val decided   = nGames - noWinner
        val botWins   = decided - brainWins
        val counted   = results.size
        val brainDoomAvg  = if (counted > 0) results.map(_._4).sum / counted else 0.0
        val leaderDoomAvg = if (counted > 0) results.map(_._5).sum / counted else 0.0
        (allExamples, ArenaResult(nGames, brainSeats, brainWins, botWins, noWinner, brainDoomAvg, leaderDoomAvg))
    }

    /** Per-seat arena: test the brain in EACH seat in turn (1 brain vs 3 bots), so we
     *  see whether it learned broad competence or only shines in the seat the bots
     *  find easy. `gamesPerSeat` games per faction. Returns (seat -> its ArenaResult). */
    def evaluatePerSeat(model : ValueNet, sims : Int, gamesPerSeat : Int) : Map[Faction, ArenaResult] =
        SelfPlay.fixedSeating.toList.map(f => f -> evaluate(model, sims, gamesPerSeat, Set(f))).toMap

    // ---- generic brain arena (policy/PUCT/greedy) ----------------------------------
    // The value-net arena above is fixed to ModelEval; these accept ANY DecisionPolicy
    // via a fresh-per-game factory (each game needs its own searcher — the MCTS tree is
    // per-decision and must not be shared across threads). Used to test the greedy clone
    // and the PUCT-MCTS brain against the bots.

    def evaluateBrain(brainFactory : () => DecisionPolicy, nGames : Int,
                      brainSeats : Set[Faction]) : ArenaResult = {
        def one(i : Int) : (Int, Int, Double, Double) = {
            val g = SelfPlay.newGame()
            val brain = brainFactory()
            val routing = g.setup.map(f => f -> (if (brainSeats.contains(f)) brain else (BotPolicy : DecisionPolicy))).toMap
            val policy = new MixedPolicy(routing)
            val winners = Engine.rolloutCapped(g, Engine.start(g), policy, ArenaDecisionCap, throwOnCap = false)._1
            val brainW = if (winners.nonEmpty && winners.exists(brainSeats.contains)) 1 else 0
            val noWin  = if (winners.isEmpty) 1 else 0
            val leaderDoom = g.setup.map(f => g.players(f).doom).max.toDouble
            val bd = brainSeats.toList.map(f => g.players(f).doom.toDouble)
            (brainW, noWin, if (bd.nonEmpty) bd.sum / bd.size else 0.0, leaderDoom)
        }

        val results : Seq[(Int, Int, Double, Double)] =
            if (Trainer.parallelPlay) {
                import scala.collection.parallel.CollectionConverters._
                (0 until nGames).par.map(one).toList
            } else (0 until nGames).map(one).toList

        val brainWins = results.map(_._1).sum
        val noWinner  = results.map(_._2).sum
        val botWins   = (nGames - noWinner) - brainWins
        val counted   = results.size
        val brainDoomAvg  = if (counted > 0) results.map(_._3).sum / counted else 0.0
        val leaderDoomAvg = if (counted > 0) results.map(_._4).sum / counted else 0.0
        ArenaResult(nGames, brainSeats, brainWins, botWins, noWinner, brainDoomAvg, leaderDoomAvg)
    }

    def evaluatePerSeatBrain(brainFactory : () => DecisionPolicy, gamesPerSeat : Int) : Map[Faction, ArenaResult] =
        SelfPlay.fixedSeating.toList.map(f => f -> evaluateBrain(brainFactory, gamesPerSeat, Set(f))).toMap

    // ---- TRACE SAVING (2026-08-07) ------------------------------------------------
    // Env-gated via CW_SAVE_TRACES=<dir>. When set, arena saves FIRST and BEST brain game
    // per faction as trace files build-replay.py can render. 8 traces max per arena.

    private val TraceDir : Option[String] = Option(System.getenv("CW_SAVE_TRACES")).filter(_.trim.nonEmpty)

    /** Arena with trace saving. Plays `gamesPerSeat` per faction, saves first+best per faction. */
    def evaluatePerSeatBrainWithTraces(brainFactory : () => DecisionPolicy, gamesPerSeat : Int,
                                       iterTag : String) : Map[Faction, ArenaResult] = {
        TraceDir match {
            case None => evaluatePerSeatBrain(brainFactory, gamesPerSeat)
            case Some(dir) =>
                new java.io.File(dir).mkdirs()
                SelfPlay.fixedSeating.toList.map { seat =>
                    val (result, firstTrace, bestTrace) = evaluateBrainLogged(brainFactory, gamesPerSeat, Set(seat))
                    firstTrace.foreach { t => writeTrace(t, dir, seat, iterTag, "first") }
                    bestTrace.foreach { t =>
                        // Don't duplicate if first == best
                        if (firstTrace.isEmpty || firstTrace.get != t) writeTrace(t, dir, seat, iterTag, "best")
                    }
                    seat -> result
                }.toMap
        }
    }

    // I'm deeply sorry for not including the final score before
    private case class GameTrace(seat : Faction, brainWon : Boolean, brainDoom : Int, leaderDoom : Int,
                                 actionLines : Seq[String], logLines : Seq[String], finalScore : Double)

    private def evaluateBrainLogged(brainFactory : () => DecisionPolicy, nGames : Int,
                                    brainSeats : Set[Faction]) : (ArenaResult, Option[GameTrace], Option[GameTrace]) = {

        def one(i : Int) : (Int, Int, Double, Double, GameTrace) = {
            val g = SelfPlay.newGameLogged()
            val brain = brainFactory()
            val routing = g.setup.map(f => f -> (if (brainSeats.contains(f)) brain else (BotPolicy : DecisionPolicy))).toMap
            val policy = new MixedPolicy(routing)
            val serializer = new Serialize(g)

            val startActions = ArrayBuffer[Action]()
            val startLog     = ArrayBuffer[String]()
            val sink : (Action, $[String]) => Unit = (act, ls) => { startActions += act; startLog ++= ls.toList }
            val startSit = Engine.startLogged(g, sink)
            val (winners, hitCap, acts, log) = Engine.rolloutLogged(g, startSit, policy, ArenaDecisionCap)

            val brainW = if (winners.nonEmpty && winners.exists(brainSeats.contains)) 1 else 0
            val noWin  = if (winners.isEmpty) 1 else 0
            val leaderDoom = g.setup.map(f => g.players(f).doom).max
            val bd = brainSeats.toList.map(f => g.players(f).doom.toDouble)
            val brainDoomAvg = if (bd.nonEmpty) bd.sum / bd.size else 0.0

            val actionLines = (startActions.toList ++ acts.toList).map(serializer.write)
            val logLines    = startLog.toList ++ log.toList
            val seat = brainSeats.head
            // calculate final score using Outcome.valueShaped (1.0 for wins)
            val trajectory = new Trajectory(g.setup)
            val shaping = trajectory.score(g).getOrElse(seat, 0.0)
            val finalScore = Outcome.valueShaped(g, winners.toList, seat, shaping)
            val trace = GameTrace(seat, brainW == 1, g.players(seat).doom, leaderDoom, actionLines, logLines, finalScore)

            (brainW, noWin, brainDoomAvg, leaderDoom.toDouble, trace)
        }

        // Run sequentially to collect traces (parallel would need synchronization)
        val results = (0 until nGames).map(one).toList

        val brainWins = results.map(_._1).sum
        val noWinner  = results.map(_._2).sum
        val botWins   = (nGames - noWinner) - brainWins
        val counted   = results.size
        val brainDoomAvg  = if (counted > 0) results.map(_._3).sum / counted else 0.0
        val leaderDoomAvg = if (counted > 0) results.map(_._4).sum / counted else 0.0
        val arenaResult = ArenaResult(nGames, brainSeats, brainWins, botWins, noWinner, brainDoomAvg, leaderDoomAvg)

        val allTraces = results.map(_._5)
        val first = allTraces.headOption
        val best = if (allTraces.isEmpty) None else Some(
            allTraces.sortBy(t => (if (t.brainWon) 0 else 1, -t.brainDoom, t.leaderDoom - t.brainDoom)).head
        )
        (arenaResult, first, best)
    }

    // I'm deeply sorry for not logging the final score before
    private def writeTrace(t : GameTrace, dir : String, seat : Faction, iterTag : String, kind : String) : Unit = {
        val winTag = if (t.brainWon) "-WIN" else ""
        val fn = s"$dir/arena-${iterTag}-${seat.short.toLowerCase}-${kind}${winTag}-d${t.brainDoom}.txt"
        val content = t.actionLines.mkString("\n") + "\n\n" +
                      t.logLines.map(l => s"<div class='p'>$l</div>").mkString("\n") + "\n\n" +
                      s"FINAL_SCORE=${t.finalScore}"
        java.nio.file.Files.write(java.nio.file.Paths.get(fn), content.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        println(s"      trace saved: $fn (score=${t.finalScore})")
    }
}
