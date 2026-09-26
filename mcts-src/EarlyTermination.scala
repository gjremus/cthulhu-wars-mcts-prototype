package cws

import hrf.colmat._

/** Early termination system for killing hopeless games mid-play to reduce noise.
 *
 *  ENVIRONMENT VARIABLES (all optional, 0 = disabled):
 *    CW_MIN_SCORE_GC   - minimum 0-1 score for GC (default 0.28)
 *    CW_MIN_SCORE_CC   - minimum 0-1 score for CC (default 0.10)
 *    CW_MIN_SCORE_YS   - minimum 0-1 score for YS (default 0.16)
 *    CW_MIN_SCORE_BG   - minimum 0-1 score for BG (default 0.14)
 *
 *  FACTION-SPECIFIC SCORE THRESHOLDS targeting 30% completion per faction.
 *  Games below thresholds are killed and marked with result='killed' in traces.
 */
object EarlyTermination {

    case class Thresholds(
        scoreGC: Double,
        scoreCC: Double,
        scoreYS: Double,
        scoreBG: Double
    )

    def loadThresholds(): Thresholds = {
        def getEnvDouble(name: String, default: Double): Double = {
            val v = sys.env.get(name)
            if (v.isEmpty || v.get.trim.isEmpty) default
            else try v.get.trim.toDouble catch { case _: Exception => default }
        }

        Thresholds(
            scoreGC = getEnvDouble("CW_MIN_SCORE_GC", 0.0),
            scoreCC = getEnvDouble("CW_MIN_SCORE_CC", 0.0),
            scoreYS = getEnvDouble("CW_MIN_SCORE_YS", 0.0),
            scoreBG = getEnvDouble("CW_MIN_SCORE_BG", 0.0)
        )
    }

    /** Check if game should be terminated early. Returns Some(reason) if should kill. */
    def shouldTerminate(game: Game, brainSeat: Faction, actionCount: Int,
                       trajectory: Trajectory, thresholds: Thresholds): Option[String] = {
        // Estimate APs: 4 factions, ~8 actions per faction per AP
        val estimatedAPs = actionCount / 32

        if (estimatedAPs < 5) return None  // Too early to judge

        // Calculate shaping-based interim score
        val shaping = trajectory.score(game)
        val brainShaping = shaping.getOrElse(brainSeat, 0.0)

        // Interim score: just shaping component (0-1 normalized)
        val interimScore = math.max(0.0, math.min(1.0, brainShaping))

        // Faction-specific score threshold
        val threshold = brainSeat match {
            case GC => thresholds.scoreGC
            case CC => thresholds.scoreCC
            case YS => thresholds.scoreYS
            case BG => thresholds.scoreBG
            case _ => 0.0  // No threshold for other factions
        }

        if (threshold > 0 && interimScore < threshold) {
            return Some(f"Score too low for ${brainSeat.short} at AP~$estimatedAPs: $interimScore%.3f < $threshold%.3f (faction threshold)")
        }

        None
    }

    /** Write trace for early-terminated game */
    def writeKilledTrace(traceDir: String, runTag: String, iterNum: Int,
                        brainSeat: Faction, gameNum: Int, game: Game,
                        actionCount: Int, reason: String, trajectory: Trajectory,
                        actions: List[Action]): Unit = {
        val brainDoom = game.players(brainSeat).doom
        val shaping = trajectory.score(game)
        val brainShaping = shaping.getOrElse(brainSeat, 0.0)
        val interimScore = math.max(0.0, math.min(1.0, brainShaping))
        val estimatedAPs = actionCount / 32

        val serializer = new Serialize(game)
        val actionLines = actions.map(serializer.write)

        val pw = new java.io.PrintWriter(
            new java.io.File(s"$traceDir/arena-$runTag-iter$iterNum-${brainSeat.short.toLowerCase}-game$gameNum-KILLED.txt"))

        // Write action sequence first (same format as completed games)
        actionLines.foreach(pw.println)
        pw.println()
        pw.println()

        // Then write metadata
        pw.println(f"Brain seat: ${brainSeat.short}")
        pw.println(f"Result: KILLED")
        pw.println(f"Doom: $brainDoom")
        pw.println(f"INTERIM_SCORE=$interimScore%.3f")
        pw.println(f"Actions taken: $actionCount")
        pw.println(f"Estimated APs: $estimatedAPs")
        pw.println(f"BREAKDOWN=${shaping.map { case (f, v) => f"${f.short}=${"%+.3f".format(v)}" }.mkString(" ")}")
        pw.println(f"")
        pw.println(f"EARLY TERMINATION REASON:")
        pw.println(f"  $reason")

        pw.close()
    }

    /** Rollout with early termination checks. Returns (winners, wasKilled, killReason, actionCount, actionList). */
    def rolloutWithTerminationCheck(game: Game, sit: Situation, policy: DecisionPolicy,
                                    brainSeat: Faction, trajectory: Trajectory,
                                    thresholds: Thresholds,
                                    maxDecisions: Int = 8000): ($[Faction], Boolean, Option[String], Int, List[Action]) = {
        var s = sit
        var decisions = 0
        var lastCheckDecisions = 0
        val actionList = scala.collection.mutable.ArrayBuffer[Action]()

        while (true) {
            s match {
                case Ended(winners) =>
                    return (winners, false, None, decisions, actionList.toList)

                case Decision(faction, actions, c) =>
                    decisions += 1

                    // Check for early termination every 32 decisions (~1 AP)
                    if (decisions - lastCheckDecisions >= 32) {
                        shouldTerminate(game, brainSeat, decisions, trajectory, thresholds) match {
                            case Some(reason) =>
                                return ($(), true, Some(reason), decisions, actionList.toList)
                            case None =>
                                lastCheckDecisions = decisions
                        }
                    }

                    if (decisions > maxDecisions) {
                        return ($(), true, Some(f"Hit decision cap at $maxDecisions"), decisions, actionList.toList)
                    }

                    val a = policy.decide(game, faction, actions)
                    actionList += a.unwrap
                    val (_, next) = game.perform(a.unwrap)
                    s = Engine.advanceToDecision(game, next)
            }
        }
        throw new IllegalStateException("unreachable")
    }
}
