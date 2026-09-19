package cws

import hrf.colmat._

/** Early termination system for killing hopeless games mid-play to reduce noise.
 *
 *  ENVIRONMENT VARIABLES (all optional, 0 = disabled):
 *    CW_MIN_DOOM_AP5   - minimum doom at 5 APs (default 8)
 *    CW_MIN_DOOM_AP10  - minimum doom at 10 APs (default 15)
 *    CW_MIN_DOOM_AP15  - minimum doom at 15 APs (default 25)
 *    CW_MIN_SCORE_AP5  - minimum 0-1 score at 5 APs (default 0.12)
 *    CW_MIN_SCORE_AP10 - minimum 0-1 score at 10 APs (default 0.20)
 *
 *  Thresholds are based on baseline: winning games average 40+ doom in 8 APs.
 *  Games below thresholds are killed and marked with result='killed' in traces.
 */
object EarlyTermination {

    case class Thresholds(
        doomAP5: Int,
        doomAP10: Int,
        doomAP15: Int,
        scoreAP5: Double,
        scoreAP10: Double
    )

    def loadThresholds(): Thresholds = {
        def getEnvInt(name: String, default: Int): Int = {
            val v = sys.env.get(name)
            if (v.isEmpty || v.get.trim.isEmpty) default
            else try v.get.trim.toInt catch { case _: Exception => default }
        }

        def getEnvDouble(name: String, default: Double): Double = {
            val v = sys.env.get(name)
            if (v.isEmpty || v.get.trim.isEmpty) default
            else try v.get.trim.toDouble catch { case _: Exception => default }
        }

        Thresholds(
            doomAP5 = getEnvInt("CW_MIN_DOOM_AP5", 8),
            doomAP10 = getEnvInt("CW_MIN_DOOM_AP10", 15),
            doomAP15 = getEnvInt("CW_MIN_DOOM_AP15", 25),
            scoreAP5 = getEnvDouble("CW_MIN_SCORE_AP5", 0.12),
            scoreAP10 = getEnvDouble("CW_MIN_SCORE_AP10", 0.20)
        )
    }

    /** Check if game should be terminated early. Returns Some(reason) if should kill. */
    def shouldTerminate(game: Game, brainSeat: Faction, actionCount: Int,
                       trajectory: Trajectory, thresholds: Thresholds): Option[String] = {
        // Estimate APs: 4 factions, ~8 actions per faction per AP
        val estimatedAPs = actionCount / 32

        if (estimatedAPs < 5) return None  // Too early to judge

        val brainDoom = game.players(brainSeat).doom

        // Check doom thresholds
        if (estimatedAPs >= 5 && thresholds.doomAP5 > 0 && brainDoom < thresholds.doomAP5) {
            return Some(f"Doom too low at AP~$estimatedAPs: $brainDoom < ${thresholds.doomAP5} (threshold)")
        }

        if (estimatedAPs >= 10 && thresholds.doomAP10 > 0 && brainDoom < thresholds.doomAP10) {
            return Some(f"Doom too low at AP~$estimatedAPs: $brainDoom < ${thresholds.doomAP10} (threshold)")
        }

        if (estimatedAPs >= 15 && thresholds.doomAP15 > 0 && brainDoom < thresholds.doomAP15) {
            return Some(f"Doom too low at AP~$estimatedAPs: $brainDoom < ${thresholds.doomAP15} (threshold)")
        }

        // Check score thresholds (requires interim score calculation)
        // Calculate shaping-based interim score
        val shaping = trajectory.score(game)
        val brainShaping = shaping.getOrElse(brainSeat, 0.0)

        // Interim score: just shaping component (0-1 normalized)
        val interimScore = math.max(0.0, math.min(1.0, brainShaping))

        if (estimatedAPs >= 5 && thresholds.scoreAP5 > 0 && interimScore < thresholds.scoreAP5) {
            return Some(f"Score too low at AP~$estimatedAPs: $interimScore%.3f < ${thresholds.scoreAP5}%.3f (threshold)")
        }

        if (estimatedAPs >= 10 && thresholds.scoreAP10 > 0 && interimScore < thresholds.scoreAP10) {
            return Some(f"Score too low at AP~$estimatedAPs: $interimScore%.3f < ${thresholds.scoreAP10}%.3f (threshold)")
        }

        None
    }

    /** Write trace for early-terminated game */
    def writeKilledTrace(traceDir: String, runTag: String, iterNum: Int,
                        brainSeat: Faction, gameNum: Int, game: Game,
                        actionCount: Int, reason: String, trajectory: Trajectory): Unit = {
        val brainDoom = game.players(brainSeat).doom
        val shaping = trajectory.score(game)
        val brainShaping = shaping.getOrElse(brainSeat, 0.0)
        val interimScore = math.max(0.0, math.min(1.0, brainShaping))
        val estimatedAPs = actionCount / 32

        val pw = new java.io.PrintWriter(
            new java.io.File(s"$traceDir/arena-$runTag-iter$iterNum-${brainSeat.short.toLowerCase}-game$gameNum-KILLED.txt"))

        pw.println(f"Brain seat: ${brainSeat.short}")
        pw.println(f"Result: KILLED")
        pw.println(f"Doom: $brainDoom")
        pw.println(f"Interim Score (0-1): $interimScore%.3f")
        pw.println(f"Actions taken: $actionCount")
        pw.println(f"Estimated APs: $estimatedAPs")
        pw.println(f"Shaping breakdown: ${shaping.map { case (f, v) => f"${f.short}=$v%.2f" }.mkString(", ")}")
        pw.println(f"")
        pw.println(f"EARLY TERMINATION REASON:")
        pw.println(f"  $reason")
        pw.println(f"")
        pw.println(f"This game was killed before completion because it fell below")
        pw.println(f"quality thresholds. Training on hopeless games adds noise.")

        pw.close()
    }

    /** Rollout with early termination checks. Returns (winners, wasKilled, killReason, actionCount). */
    def rolloutWithTerminationCheck(game: Game, sit: Situation, policy: DecisionPolicy,
                                    brainSeat: Faction, trajectory: Trajectory,
                                    thresholds: Thresholds,
                                    maxDecisions: Int = 8000): ($[Faction], Boolean, Option[String], Int) = {
        var s = sit
        var decisions = 0
        var lastCheckDecisions = 0

        while (true) {
            s match {
                case Ended(winners) =>
                    return (winners, false, None, decisions)

                case Decision(faction, actions, c) =>
                    decisions += 1

                    // Check for early termination every 32 decisions (~1 AP)
                    if (decisions - lastCheckDecisions >= 32) {
                        shouldTerminate(game, brainSeat, decisions, trajectory, thresholds) match {
                            case Some(reason) =>
                                return ($(), true, Some(reason), decisions)
                            case None =>
                                lastCheckDecisions = decisions
                        }
                    }

                    if (decisions > maxDecisions) {
                        return ($(), true, Some(f"Hit decision cap at $maxDecisions"), decisions)
                    }

                    val a = policy.decide(game, faction, actions)
                    val (_, next) = game.perform(a.unwrap)
                    s = Engine.advanceToDecision(game, next)
            }
        }
        throw new IllegalStateException("unreachable")
    }
}
