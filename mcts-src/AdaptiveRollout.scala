package cws

import hrf.colmat._
import scala.collection.mutable.ArrayBuffer

/** Adaptive rollout with REAL undo: checkpoint game state, restore on bad outcome.
 *
 *  THRESHOLD CALIBRATION (from R39 iter 14 killed games):
 *  - Killed games had scores 0.055-0.212 (5-22% below thresholds)
 *  - Undo threshold: 0.10 (10 percentage point drop) = catastrophically bad
 *  - This catches major blunders without triggering on normal variance
 *
 *  ENV VARS:
 *    CW_ADAPTIVE_UNDO=true    - enable (default: false)
 *    CW_UNDO_THRESHOLD=0.10   - min score drop to undo (default: 0.10)
 *    CW_MAX_RETRIES=2         - max retries per turn (default: 2)
 */
object AdaptiveRollout {

    case class Config(
        enabled: Boolean,
        scoreDropThreshold: Double,
        maxRetriesPerTurn: Int
    )

    def loadConfig(): Config = {
        val enabled = sys.env.get("CW_ADAPTIVE_UNDO").exists(_.toLowerCase == "true")
        val threshold = sys.env.get("CW_UNDO_THRESHOLD").flatMap(s =>
            try Some(s.toDouble) catch { case _: Exception => None }
        ).getOrElse(0.10)
        val maxRetries = sys.env.get("CW_MAX_RETRIES").flatMap(s =>
            try Some(s.toInt) catch { case _: Exception => None }
        ).getOrElse(2)

        Config(enabled, threshold, maxRetries)
    }

    /** Rollout with real undo via checkpoint/restore.
     *
     *  Strategy: Play game normally, but after each brain turn measure outcome.
     *  If outcome is catastrophically bad (score dropped >threshold), restore
     *  checkpoint and block the bad action from MCTS search.
     *
     *  Returns: (winners, wasKilled, killReason, actionCount, actionList, undoCount)
     */
    def rolloutWithAdaptiveUndo(
        initialGame: Game,
        initialSit: Situation,
        policy: DecisionPolicy,
        brainSeat: Faction,
        trajectory: Trajectory,
        earlyTermThresholds: EarlyTermination.Thresholds,
        config: Config,
        maxDecisions: Int = 8000
    ): ($[Faction], Boolean, Option[String], Int, List[Action], Int) = {

        // ADAPTIVE UNDO DISABLED: did not improve R39 performance, added complexity
        // Undo system bypassed - use standard rollout with early termination only
        val (w, k, r, ac, al) = EarlyTermination.rolloutWithTerminationCheck(
            initialGame, initialSit, policy, brainSeat, trajectory, earlyTermThresholds, maxDecisions)
        return (w, k, r, ac, al, 0)  // Last value = undoCount (always 0 now)

        /* COMMENTED OUT: Adaptive undo with checkpoint/restore
        // NOTE: This code is preserved but disabled. To re-enable, uncomment this block
        // and comment out the return statement above.

        // Core loop with checkpointing
        var game = initialGame
        var s = initialSit
        var decisions = 0
        var lastCheckDecisions = 0
        val actionList = ArrayBuffer[Action]()
        var totalUndos = 0

        // Current turn checkpoint
        case class Checkpoint(
            g: Game,
            cont: Continue,
            scoreBefore: Double,
            actionIdx: Int,
            blockedActions: Set[Action],
            attemptHistory: List[(Action, Double)]  // (action, scoreAfter) for each attempt
        )
        var checkpoint: Option[Checkpoint] = None
        var retriesThisTurn = 0

        while (true) {
            s match {
                case Ended(winners) =>
                    return (winners, false, None, decisions, actionList.toList, totalUndos)

                case Decision(faction, actions, cont) =>
                    decisions += 1

                    // Early termination check every 32 decisions
                    if (decisions - lastCheckDecisions >= 32) {
                        EarlyTermination.shouldTerminate(game, brainSeat, decisions, trajectory, earlyTermThresholds) match {
                            case Some(reason) =>
                                return ($(), true, Some(reason), decisions, actionList.toList, totalUndos)
                            case None =>
                                lastCheckDecisions = decisions
                        }
                    }

                    if (decisions > maxDecisions) {
                        return ($(), true, Some(f"Hit decision cap at $maxDecisions"), decisions, actionList.toList, totalUndos)
                    }

                    if (faction == brainSeat) {
                        // BRAIN TURN: create checkpoint and decide

                        // Filter blocked actions from previous attempts
                        val blocked = checkpoint.map(_.blockedActions).getOrElse(Set.empty)
                        val availableActions = actions.filter(a => !blocked.contains(a.unwrap))

                        if (availableActions.isEmpty) {
                            // All actions blocked - should not happen, use any action
                            val a = policy.decide(game, faction, actions)
                            actionList += a.unwrap
                            val (_, next) = game.perform(a.unwrap)
                            s = Engine.advanceToDecision(game, next)
                            checkpoint = None
                            retriesThisTurn = 0
                        } else {
                            // Save checkpoint before deciding
                            if (checkpoint.isEmpty || retriesThisTurn == 0) {
                                val scoreBefore = trajectory.score(game).getOrElse(brainSeat, 0.0)
                                val (gClone, contClone) = Cloning.copy(game, cont)
                                checkpoint = Some(Checkpoint(
                                    gClone,
                                    contClone,
                                    scoreBefore,
                                    actionList.size,
                                    Set.empty,
                                    List.empty
                                ))
                                retriesThisTurn = 0
                            }

                            // Make decision
                            val a = policy.decide(game, faction, availableActions)
                            actionList += a.unwrap
                            val (_, next) = game.perform(a.unwrap)
                            s = Engine.advanceToDecision(game, next)
                        }

                    } else {
                        // OPPONENT TURN: just play, then check outcome
                        val a = policy.decide(game, faction, actions)
                        actionList += a.unwrap
                        val (_, next) = game.perform(a.unwrap)
                        s = Engine.advanceToDecision(game, next)

                        // After opponent moves, check if we're back to brain turn
                        s match {
                            case Decision(nextFaction, _, _) if nextFaction == brainSeat && checkpoint.isDefined =>
                                // Measure outcome
                                val cp = checkpoint.get
                                val scoreAfter = trajectory.score(game).getOrElse(brainSeat, 0.0)
                                val scoreDelta = scoreAfter - cp.scoreBefore
                                val attemptedAction = actionList(cp.actionIdx)
                                val newHistory = cp.attemptHistory :+ (attemptedAction, scoreAfter)

                                if (scoreDelta < -config.scoreDropThreshold && retriesThisTurn < config.maxRetriesPerTurn) {
                                    // UNDO: catastrophic outcome detected

                                    // Restore from checkpoint
                                    game = cp.g
                                    s = Engine.advanceToDecision(game, cp.cont)

                                    // Truncate action list
                                    actionList.remove(cp.actionIdx, actionList.size - cp.actionIdx)

                                    // Update checkpoint to block bad action and record attempt
                                    checkpoint = Some(cp.copy(
                                        blockedActions = cp.blockedActions + attemptedAction,
                                        attemptHistory = newHistory
                                    ))

                                    decisions = cp.actionIdx
                                    retriesThisTurn += 1
                                    totalUndos += 1

                                } else if (scoreDelta < -config.scoreDropThreshold && retriesThisTurn >= config.maxRetriesPerTurn && newHistory.nonEmpty) {
                                    // Hit max retries with still-bad outcome - pick least bad from all attempts
                                    val bestAttempt = newHistory.maxBy(_._2)  // highest score
                                    if (bestAttempt._2 > scoreAfter) {
                                        // Current attempt is worst - undo and force best attempt
                                        game = cp.g
                                        s = Engine.advanceToDecision(game, cp.cont)
                                        actionList.remove(cp.actionIdx, actionList.size - cp.actionIdx)

                                        // Force best attempt by blocking all others
                                        val allAttempted = newHistory.map(_._1).toSet
                                        val forcedBlocked = allAttempted - bestAttempt._1

                                        checkpoint = Some(cp.copy(
                                            blockedActions = forcedBlocked,
                                            attemptHistory = List.empty  // clear history for forced replay
                                        ))

                                        decisions = cp.actionIdx
                                        retriesThisTurn = 0  // reset for forced replay
                                        totalUndos += 1
                                    } else {
                                        // Current attempt is best or tied - accept it
                                        checkpoint = None
                                        retriesThisTurn = 0
                                    }
                                } else {
                                    // Outcome acceptable - clear checkpoint
                                    checkpoint = None
                                    retriesThisTurn = 0
                                }

                            case _ =>
                                // Not back to brain turn yet, continue
                        }
                    }
            }
        }
        throw new IllegalStateException("unreachable")
        */  // END OF COMMENTED OUT ADAPTIVE UNDO CODE
    }
}
