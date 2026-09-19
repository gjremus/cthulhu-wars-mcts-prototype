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
            TraceLog.log(s"Arena game $i start")
            val g = SelfPlay.newGame()
            val brain = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
            val routing = g.setup.map(f => f -> (if (brainSeats.contains(f)) brain else (BotPolicy : DecisionPolicy))).toMap
            val policy = new MixedPolicy(routing)
            TraceLog.log(s"Arena game $i calling Engine.rolloutCapped")
            val winners = Engine.rolloutCapped(g, Engine.start(g), policy, ArenaDecisionCap, throwOnCap = false)._1
            TraceLog.log(s"Arena game $i rollout complete")

            val brainW = if (winners.nonEmpty && winners.exists(brainSeats.contains)) 1 else 0
            val noWin  = if (winners.isEmpty) 1 else 0
            val leaderDoom = g.setup.map(f => g.players(f).doom).max.toDouble
            val bd = brainSeats.toList.map(f => g.players(f).doom.toDouble)
            TraceLog.log(s"Arena game $i complete")
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

    /** Arena with BOTH examples AND trace saving (2026-08-19 fix).
     *  Combines evaluateWithExamples with trace-saving logic from evaluateBrainLogged.
     *  If CW_SAVE_TRACES is set, saves first+best traces per seat. */
    def evaluateWithExamplesAndTraces(model : ValueNet, sims : Int, nGames : Int,
                                      brainSeats : Set[Faction], iterTag : String) : ($[Example], ArenaResult) = {
        // If no trace dir, fall back to regular evaluateWithExamples
        val traceDirOpt = Option(System.getenv("CW_SAVE_TRACES")).filter(_.trim.nonEmpty)
        if (traceDirOpt.isEmpty) {
            return evaluateWithExamples(model, sims, nGames, brainSeats)
        }

        val traceDir = traceDirOpt.get
        new java.io.File(traceDir).mkdirs()

        // Play games WITH logging to capture traces
        def one(i : Int) : ($[Example], Int, Int, Double, Double, GameTrace) = {
            val g = SelfPlay.newGameLogged()
            val brain = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
            val routing = g.setup.map(f => f -> (if (brainSeats.contains(f)) brain else (BotPolicy : DecisionPolicy))).toMap
            val policy = new MixedPolicy(routing)
            val serializer = new Serialize(g)

            val examples = scala.collection.mutable.ArrayBuffer[Example]()
            val trajectory = new Trajectory(g.setup)
            var decisions = 0
            val exIdx = scala.collection.mutable.ArrayBuffer[Int]()

            val startActions = scala.collection.mutable.ArrayBuffer[Action]()
            val startLog     = scala.collection.mutable.ArrayBuffer[String]()
            val sink : (Action, $[String]) => Unit = (act, ls) => { startActions += act; startLog ++= ls.toList }

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

            val s0 = Engine.startLogged(g, sink)
            val (winners, hitCap, acts, log) = Engine.rolloutLogged(g, s0, recording, ArenaDecisionCap)

            // Label examples
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

            // Build trace
            val seat = brainSeats.head  // Assume single seat for now
            val brainWon = winners.nonEmpty && winners.contains(seat)
            val brainDoom = g.players(seat).doom
            val leaderDoom = g.setup.map(f => g.players(f).doom).max
            val finalScore = Outcome.valueShaped(g, winners.toList, seat, shaping.getOrElse(seat, 0.0))
            val breakdown = trajectory.scoreBreakdown(g, seat)
            val allDoom = g.setup.map(f => f -> g.players(f).doom).toMap

            val actionLines = (startActions.toList ++ acts.toList).map(serializer.write)
            val logLines = startLog.toList ++ log.toList

            val trace = GameTrace(seat, brainWon, brainDoom, leaderDoom, actionLines, logLines, finalScore, breakdown, allDoom)

            val brainW = if (brainWon) 1 else 0
            val noWin  = if (winners.isEmpty) 1 else 0
            val bd = brainSeats.toList.map(f => g.players(f).doom.toDouble)
            (examples.toList, brainW, noWin, if (bd.nonEmpty) bd.sum / bd.size else 0.0, leaderDoom.toDouble, trace)
        }

        // Sequential execution to collect traces
        val results = (0 until nGames).map(one).toList

        // Save first and best traces
        if (results.nonEmpty) {
            val allTraces = results.map(_._6)
            val firstTrace = allTraces.head
            val bestTrace = allTraces.sortBy(t => (if (t.brainWon) 0 else 1, -t.brainDoom, t.leaderDoom - t.brainDoom)).head
            val seat = brainSeats.head

            writeTrace(firstTrace, traceDir, seat, iterTag, "first")
            if (firstTrace != bestTrace) {
                writeTrace(bestTrace, traceDir, seat, iterTag, "best")
            }
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

    // ---- HEAD-TO-HEAD CHAMPION EVALUATION (2026-08-19) ----------------------------
    // USER DIRECTIVE: "IF THE BRAIN WINS MORE THAN HALF OF ITS GAMES AGAINST THE
    // 'CHAMPION' - THEN THAT'S THE NEW CHAMPION." This plays current vs champion in
    // mixed games (each gets half the seats) and returns win counts + average 0-1 scores.

    case class ChampionChallengeResult(
        gamesPlayed: Int,
        currentWins: Int,
        championWins: Int,
        draws: Int,
        currentAvgScore: Double,  // average 0-1 FINAL_SCORE
        championAvgScore: Double
    )

    def evaluateVsChampion(current: ValueNet, champion: ValueNet, sims: Int,
                          nGames: Int): ChampionChallengeResult = {
        val seating = SelfPlay.fixedSeating.toList
        require(seating.size == 4, "Champion challenge requires 4-player games")

        // Split seats: first 2 get current, last 2 get champion
        val currentSeats = seating.take(2).toSet
        val championSeats = seating.drop(2).toSet

        var currentWins = 0
        var championWins = 0
        var draws = 0
        var currentScores = List.empty[Double]
        var championScores = List.empty[Double]

        for (gIdx <- 0 until nGames) {
            val g = SelfPlay.newGame()
            val currentBrain = new MCTSPolicy(sims = sims, leaf = ModelEval(current))
            val championBrain = new MCTSPolicy(sims = sims, leaf = ModelEval(champion))
            val routing = seating.map { f =>
                f -> (if (currentSeats.contains(f)) currentBrain else championBrain)
            }.toMap
            val policy = new MixedPolicy(routing)

            // Track trajectory during play
            val trajectory = new Trajectory(g.setup)
            val recording = new DecisionPolicy {
                def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                    trajectory.observe(game)
                    policy.decide(game, faction, actions)
                }
            }

            // Run the game
            val s0 = Engine.start(g)
            val (winners, _) = Engine.rolloutCapped(g, s0, recording, 20000, throwOnCap = false)
            val shaping = trajectory.score(g)

            // Count wins
            if (winners.isEmpty) {
                draws += 1
            } else {
                val currentWon = winners.exists(currentSeats.contains)
                val championWon = winners.exists(championSeats.contains)
                if (currentWon && !championWon) currentWins += 1
                else if (championWon && !currentWon) championWins += 1
                else draws += 1  // Both won or neither
            }

            // Collect 0-1 scores using valueShaped
            currentSeats.foreach { f =>
                val score = Outcome.valueShaped(g, winners, f, shaping.getOrElse(f, 0.0))
                currentScores = score :: currentScores
            }
            championSeats.foreach { f =>
                val score = Outcome.valueShaped(g, winners, f, shaping.getOrElse(f, 0.0))
                championScores = score :: championScores
            }
        }

        val currentAvg = if (currentScores.nonEmpty) currentScores.sum / currentScores.size else 0.0
        val championAvg = if (championScores.nonEmpty) championScores.sum / championScores.size else 0.0

        ChampionChallengeResult(nGames, currentWins, championWins, draws, currentAvg, championAvg)
    }

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

    private val TraceDir : Option[String] = {
        val env = System.getenv("CW_SAVE_TRACES")
        val debugFile = new java.io.PrintWriter(new java.io.FileWriter("/tmp/tracedir_debug.log", true))
        debugFile.println(s"[${java.time.Instant.now()}] TraceDir init: CW_SAVE_TRACES='${env}'")
        val result = Option(env).filter(_.trim.nonEmpty)
        debugFile.println(s"[${java.time.Instant.now()}] TraceDir result: ${result}")
        debugFile.close()
        result
    }

    /** Arena with trace saving. Plays `gamesPerSeat` per faction, saves first+best per faction. */
    def evaluatePerSeatBrainWithTraces(brainFactory : () => DecisionPolicy, gamesPerSeat : Int,
                                       iterTag : String) : (Map[Faction, ArenaResult], Double) = {
        TraceDir match {
            case None => (evaluatePerSeatBrain(brainFactory, gamesPerSeat), 0.0)
            case Some(dir) =>
                new java.io.File(dir).mkdirs()

                // TRACE LOGGING (check /tmp/arena_trace_enable.txt to toggle)
                def traceLog(msg : String) : Unit = {
                    if (new java.io.File("/tmp/arena_trace_enable.txt").exists()) {
                        val pw = new java.io.PrintWriter(new java.io.FileWriter("/tmp/arena_trace.log", true))
                        pw.println(s"[${java.time.Instant.now()}] $msg")
                        pw.close()
                    }
                }

                traceLog(s"START evaluatePerSeatBrainWithTraces: $iterTag, $gamesPerSeat games/seat")
                traceLog(s"  fixedSeating = ${SelfPlay.fixedSeating.toList}")

                var maxScore = 0.0
                val results = SelfPlay.fixedSeating.toList.map { seat =>
                    traceLog(s"  BEGIN seat=$seat")
                    val (result, allTraces, firstTrace, bestTrace) = evaluateBrainLogged(brainFactory, gamesPerSeat, Set(seat), dir, iterTag)
                    traceLog(s"  DONE evaluateBrainLogged seat=$seat: ${allTraces.length} games")

                    // Track best 0-1 score across all games
                    allTraces.foreach { t => if (t.finalScore > maxScore) maxScore = t.finalScore }

                    // Save metadata for ALL games
                    traceLog(s"  WRITE metadata seat=$seat")
                    writeAllGameMetadata(allTraces, dir, seat, iterTag)

                    // Save full traces for first and best
                    firstTrace.foreach { t =>
                        traceLog(s"  WRITE first trace seat=$seat")
                        writeTrace(t, dir, seat, iterTag, "first")
                    }
                    bestTrace.foreach { t =>
                        // Don't duplicate if first == best
                        if (firstTrace.isEmpty || firstTrace.get != t) {
                            traceLog(s"  WRITE best trace seat=$seat")
                            writeTrace(t, dir, seat, iterTag, "best")
                        }
                    }
                    traceLog(s"  COMPLETE seat=$seat")
                    seat -> result
                }.toMap
                traceLog(s"DONE evaluatePerSeatBrainWithTraces: $iterTag")
                (results, maxScore)
        }
    }

    // I'm deeply sorry for not including the final score before
    private case class GameTrace(seat : Faction, brainWon : Boolean, brainDoom : Int, leaderDoom : Int,
                                 actionLines : Seq[String], logLines : Seq[String], finalScore : Double,
                                 breakdown : Seq[(String, Double)], allDoom : Map[Faction, Int])

    private def evaluateBrainLogged(brainFactory : () => DecisionPolicy, nGames : Int,
                                    brainSeats : Set[Faction], writeDir : String, iterTag : String) : (ArenaResult, List[GameTrace], Option[GameTrace], Option[GameTrace]) = {

        println(s"DEBUG: evaluateBrainLogged CALLED: nGames=$nGames seats=$brainSeats iterTag=$iterTag")
        System.out.flush()

        // TRACE LOGGING
        def traceLog(msg : String) : Unit = {
            if (new java.io.File("/tmp/arena_trace_enable.txt").exists()) {
                val pw = new java.io.PrintWriter(new java.io.FileWriter("/tmp/arena_trace.log", true))
                pw.println(s"[${java.time.Instant.now()}] $msg")
                pw.close()
            }
        }

        traceLog(s"  evaluateBrainLogged: $nGames games for seats=$brainSeats")

        val seat = brainSeats.head
        println(s"DEBUG: seat=$seat (${seat.short})")
        System.out.flush()

        def one(i : Int) : (Int, Int, Double, Double, GameTrace) = {
            traceLog(s"    game $i: START")
            val t0 = System.nanoTime()
            val g = SelfPlay.newGameLogged()
            val brain = brainFactory()
            val routing = g.setup.map(f => f -> (if (brainSeats.contains(f)) brain else (BotPolicy : DecisionPolicy))).toMap
            val policy = new MixedPolicy(routing)
            val serializer = new Serialize(g)

            // Track trajectory to get full breakdown
            val trajectory = new Trajectory(g.setup)

            val startActions = ArrayBuffer[Action]()
            val startLog     = ArrayBuffer[String]()
            val sink : (Action, $[String]) => Unit = (act, ls) => { startActions += act; startLog ++= ls.toList }
            val startSit = Engine.startLogged(g, sink)

            // Wrap policy to observe game states for trajectory
            val observingPolicy = new DecisionPolicy {
                def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                    trajectory.observe(game)
                    policy.decide(game, faction, actions)
                }
            }

            val (winners, hitCap, acts, log) = Engine.rolloutLogged(g, startSit, observingPolicy, ArenaDecisionCap)

            val brainW = if (winners.nonEmpty && winners.exists(brainSeats.contains)) 1 else 0
            val noWin  = if (winners.isEmpty) 1 else 0
            val leaderDoom = g.setup.map(f => g.players(f).doom).max
            val bd = brainSeats.toList.map(f => g.players(f).doom.toDouble)
            val brainDoomAvg = if (bd.nonEmpty) bd.sum / bd.size else 0.0

            val actionLines = (startActions.toList ++ acts.toList).map(serializer.write)
            val logLines    = startLog.toList ++ log.toList
            val seat = brainSeats.head

            // Get full breakdown from trajectory (which observed the game during playthrough)
            val breakdown = trajectory.scoreBreakdown(g, seat)
            val shaping = trajectory.score(g).getOrElse(seat, 0.0)
            val finalScore = Outcome.valueShaped(g, winners.toList, seat, shaping)
            val allDoom = g.setup.map(f => f -> g.players(f).doom).toMap
            val trace = GameTrace(seat, brainW == 1, g.players(seat).doom, leaderDoom, actionLines, logLines, finalScore, breakdown, allDoom)

            val elapsed = (System.nanoTime() - t0) / 1e9
            traceLog(s"    game $i: DONE in ${elapsed}s, brainW=$brainW, doom=$brainDoomAvg")

            // IMMEDIATE WRITE: Write trace file as soon as game completes
            val winTag = if (brainW == 1) "-WIN" else ""
            val fn = s"$writeDir/arena-${iterTag}-${seat.short.toLowerCase}-game${i+1}${winTag}-d${g.players(seat).doom}.txt"

            val breakdownStr = breakdown.map { case (name, value) => s"$name=${"%+.3f".format(value)}" }.mkString(" ")
            val doomStr = allDoom.toList.sortBy(-_._2).map { case (f, d) => s"${f.short}=$d" }.mkString(" ")

            val content = actionLines.mkString("\n") + "\n\n" +
                          logLines.map(l => s"<div class='p'>$l</div>").mkString("\n") + "\n\n" +
                          s"FINAL_SCORE=${finalScore}\n" +
                          s"BREAKDOWN=$breakdownStr\n" +
                          s"ALL_DOOM=$doomStr"
            java.nio.file.Files.write(java.nio.file.Paths.get(fn), content.getBytes(java.nio.charset.StandardCharsets.UTF_8))

            // Print to stdout so dashboard can detect immediately
            println(s"      game ${i+1}/${nGames} saved: $fn (doom=${g.players(seat).doom} score=${finalScore})")
            System.out.flush()

            (brainW, noWin, brainDoomAvg, leaderDoom.toDouble, trace)
        }

        println(s"DEBUG: About to check resume offsets")
        System.out.flush()

        // Check for resume offset from environment variable (format: "gc:400,bg:365,ys:100,cc:100")
        val resumeEnv = sys.env.get("CW_RESUME_FROM")
        println(s"DEBUG: CW_RESUME_FROM = $resumeEnv")
        System.out.flush()

        val resumeOffsets = resumeEnv.map { str =>
            val offsets = str.split(",").map { pair =>
                val parts = pair.split(":")
                parts(0).toLowerCase -> parts(1).toInt
            }.toMap
            println(s"DEBUG: Parsed offsets = $offsets")
            offsets
        }.getOrElse(Map.empty[String, Int])

        val seatKey = seat.short.toLowerCase
        val startGame = resumeOffsets.getOrElse(seatKey, 0)
        println(s"DEBUG: seat=${seat.short} seatKey=$seatKey startGame=$startGame nGames=$nGames")
        println(s"DEBUG: Will run games ${startGame+1} through $nGames (${nGames - startGame} total)")
        System.out.flush()

        // Run sequentially to collect traces (parallel would need synchronization)
        println(s"DEBUG: Starting game loop from $startGame until $nGames")
        System.out.flush()
        traceLog(s"  START game loop: running games ${startGame+1} through $nGames (${nGames - startGame} new games)")
        val results = (startGame until nGames).map(one).toList
        println(s"DEBUG: Game loop completed: ${results.length} games")
        System.out.flush()
        traceLog(s"  END game loop: ${results.length} games completed")

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
        (arenaResult, allTraces, first, best)
    }

    // I'm deeply sorry for not logging the final score before
    private def writeTrace(t : GameTrace, dir : String, seat : Faction, iterTag : String, kind : String) : Unit = {
        val winTag = if (t.brainWon) "-WIN" else ""
        val fn = s"$dir/arena-${iterTag}-${seat.short.toLowerCase}-${kind}${winTag}-d${t.brainDoom}.txt"

        // Format breakdown as "name=value" pairs
        val breakdownStr = t.breakdown.map { case (name, value) => s"$name=${"%+.3f".format(value)}" }.mkString(" ")

        // Format all faction dooms
        val doomStr = t.allDoom.toList.sortBy(-_._2).map { case (f, d) => s"${f.short}=$d" }.mkString(" ")

        val content = t.actionLines.mkString("\n") + "\n\n" +
                      t.logLines.map(l => s"<div class='p'>$l</div>").mkString("\n") + "\n\n" +
                      s"FINAL_SCORE=${t.finalScore}\n" +
                      s"BREAKDOWN=$breakdownStr\n" +
                      s"ALL_DOOM=$doomStr"
        java.nio.file.Files.write(java.nio.file.Paths.get(fn), content.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        println(s"      trace saved: $fn (score=${t.finalScore})")
    }

    // Save metadata for ALL games (even if we don't save full traces)
    private def writeAllGameMetadata(traces : List[GameTrace], dir : String, seat : Faction, iterTag : String) : Unit = {
        val metaFile = s"$dir/arena-${iterTag}-${seat.short.toLowerCase}-all-games.txt"
        val lines = traces.zipWithIndex.map { case (t, i) =>
            val doomStr = t.allDoom.toList.sortBy(-_._2).map { case (f, d) => s"${f.short}=$d" }.mkString(" ")
            val breakdownStr = t.breakdown.map { case (name, value) => s"$name=${"%+.3f".format(value)}" }.mkString(" ")
            s"game=${i+1} doom=${t.brainDoom} score=${t.finalScore} won=${t.brainWon} leader=${t.leaderDoom} all_doom=[$doomStr] breakdown=[$breakdownStr]"
        }
        val content = lines.mkString("\n")
        java.nio.file.Files.write(java.nio.file.Paths.get(metaFile), content.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        println(s"      metadata saved: $metaFile (${traces.size} games)")
    }
}
