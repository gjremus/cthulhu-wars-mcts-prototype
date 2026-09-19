package cws

import hrf.colmat._
import scala.collection.mutable.ArrayBuffer

// Option A, Phase 1 — the engine harness.
//
// Everything above (MCTS search, self-play data generation, the learned
// evaluator) sits on two primitives, both defined here:
//
//   1. advanceToDecision(game, c): drive the engine through all the FORCED and
//      CHANCE nodes (Force/Then/RollD6/RollBattle/DrawES/...) until it reaches
//      either a genuine choice (Ask with >1 legal action) or GameOver. This is
//      the boundary at which a searcher / policy actually has something to decide.
//
//   2. rollout(game, c, policy): from any position, play to GameOver using a
//      pluggable DecisionPolicy at each genuine choice, and report who won. This
//      is the Monte-Carlo rollout the search and the self-play loop both need.
//
// A DecisionPolicy is the single seam every strategy plugs into: the existing
// hand-tuned bots (via BotPolicy), a uniform-random baseline, an MCTS search,
// or the learned net later. Nothing here knows which — that is the point.
//
// This mirrors Host.askFaction's dispatch but (a) separates "resolve the
// mechanical/chance steps" from "make a real decision", and (b) never hard-codes
// a bot — the decision at a real choice is delegated to the policy.

/** A position at which SOMETHING must act. Either a real choice or the end. */
sealed trait Situation
final case class Decision(faction : Faction, actions : $[Action], continue : Continue) extends Situation
final case class Ended(winners : $[Faction]) extends Situation

/** The one seam every strategy implements: pick an action at a real choice. */
trait DecisionPolicy {
    /** Called only at genuine choices (actions.num > 1). Must return one of `actions`. */
    def decide(game : Game, faction : Faction, actions : $[Action]) : Action
}

object Engine {

    // Chance resolution mirrors Host.askFaction. Kept here so the harness is
    // self-contained and so a future controllable-RNG variant can override just
    // these cases without touching decision logic.
    private def resolveChance(g : Game, c : Continue) : Option[Action] = c match {
        case Force(action)          => Some(action)
        case Then(action)           => Some(action)
        case RollD6(_, roll)        => Some(roll((1 :: 2 :: 3 :: 4 :: 5 :: 6).shuffle.first))
        case RollAgony(_, roll)     => Some(roll(AgonyDie.faces.shuffle.first))
        case RollBattle(_, n, roll) => Some(roll(1.to(n)./(_ => BattleRoll.roll())))
        case DrawES(_, 0, 0, 0, draw) => Some(draw(0, true))
        case DrawES(_, es1, es2, es3, draw) =>
            Some(draw((es1.times(1) ++ es2.times(2) ++ es3.times(3)).maxBy(_ => random()), false))
        case Ask(_, List(action))   => Some(action)  // forced: exactly one legal action
        case _                      => None            // a real Ask(>1), MultiAsk, or GameOver
    }

    /**
     * Advance the game in place through all forced/chance steps until the next
     * genuine decision or the end of the game. Returns the Situation reached.
     * MUTATES `game` (perform mutates); callers who need isolation clone first.
     */
    def advanceToDecision(game : Game, from : Continue) : Situation = {
        var c = from
        var guard = 0
        while (true) {
            guard += 1
            if (guard > 100000) throw new RuntimeException("advanceToDecision: step cap (engine loop?)")
            c match {
                case GameOver(winners) => return Ended(winners)
                // DelayedContinue is a transparent wrapper (Host recurses into it
                // without performing anything) — unwrap to its inner continue.
                case DelayedContinue(_, inner) => c = inner
                // MultiAsk: Host recurses on the head ask.
                case MultiAsk(asks) => c = asks.head
                case Ask(faction, rawActions) =>
                    // The bots never decide over raw Ask actions — they first run
                    // Explode.explode, which strips Info/Hidden/Group decorations
                    // (NeedOk, OutOfTurnRefresh, SacrificeHighPriestAllowedAction,
                    // GroupAction) and walks Soft sub-menus, yielding the real
                    // performable leaves. We must do the same before deciding.
                    val choices = Explode.explode(game, rawActions)
                    choices.num match {
                        case 0 =>
                            // No real leaf (menu of only Info/Soft that led nowhere).
                            // Fall back to performing the raw single/first action so
                            // the engine advances (mirrors Ask(_, List(a)) behaviour).
                            rawActions match {
                                case List(a) => val (_, n) = game.perform(a.unwrap); c = n
                                case _       => throw new RuntimeException(
                                    "advanceToDecision: Ask exploded to 0 choices, faction=" + faction + " raw=" + rawActions.num)
                            }
                        case 1 =>
                            val (_, n) = game.perform(choices.head.unwrap); c = n
                        case _ =>
                            return Decision(faction, choices, c)
                    }
                case _ =>
                    resolveChance(game, c) match {
                        case Some(action) =>
                            // .unwrap mirrors Host: WrappedQForcedAction and friends
                            // must be unwrapped before perform.
                            val (_, next) = game.perform(action.unwrap)
                            c = next
                        case None =>
                            throw new RuntimeException("advanceToDecision: unhandled continue " + c.getClass.getName)
                    }
            }
        }
        throw new IllegalStateException("unreachable")
    }

    /** Start a fresh game and advance to its first genuine decision. */
    def start(game : Game) : Situation = {
        val (_, c0) = game.perform(StartAction)
        advanceToDecision(game, c0)
    }

    /**
     * Play `game` from Situation `sit` to the end using `policy` at each real
     * choice. MUTATES `game`. Returns the winners. `maxDecisions` guards runaway.
     * On cap, throws (the strict default used by internal search rollouts).
     */
    def rollout(game : Game, sit : Situation, policy : DecisionPolicy, maxDecisions : Int = 20000) : $[Faction] =
        rolloutCapped(game, sit, policy, maxDecisions)._1

    /**
     * Like `rollout` but returns `(winners, hitCap)`. If the decision cap is reached
     * the game is ABANDONED (not a crash): returns the empty winner list and
     * hitCap=true, leaving `game` mutated at the cap so callers can still read its
     * doom / shaping state. This is essential for early self-play, where a weak brain
     * produces stalemate games that never reach 30 doom — those games are now useful
     * (shaped) training data instead of a run-killing exception. `throwOnCap` restores
     * the strict behaviour for internal search rollouts that must not silently stall.
     */
    def rolloutCapped(game : Game, sit : Situation, policy : DecisionPolicy,
                      maxDecisions : Int = 20000, throwOnCap : Boolean = false) : ($[Faction], Boolean) = {
        TraceLog.log(s"rolloutCapped start (maxDecisions=$maxDecisions)")
        var s = sit
        var decisions = 0
        while (true) {
            s match {
                case Ended(winners) =>
                    TraceLog.log(s"rolloutCapped ended after $decisions decisions, winners=${winners.mkString(",")}")
                    return (winners, false)
                case Decision(faction, actions, c) =>
                    decisions += 1
                    if (decisions % 50 == 0) {
                        TraceLog.log(s"rolloutCapped decision $decisions faction=$faction actions=${actions.num}")
                    }
                    if (decisions > maxDecisions) {
                        TraceLog.log(s"rolloutCapped hit decision cap at $decisions")
                        if (throwOnCap) throw new RuntimeException("rollout: decision cap")
                        return ($(), true)   // abandon: no winner, game left at cap state
                    }
                    val a = policy.decide(game, faction, actions)
                    val (_, next) = game.perform(a.unwrap)
                    s = advanceToDecision(game, next)
            }
        }
        throw new IllegalStateException("unreachable")
    }

    // ─────────────────────────────────────────────────────────────────────────
    // LOGGED VARIANTS — identical control flow to the plain engine, but they
    // ACCUMULATE (a) the serialized action string of every performed action and
    // (b) the HTML game-log lines every `perform` returns. This is exactly what
    // SimRunner does to produce a trace the Python replay engine can render
    // (build-replay.py: action-strings block + blank line + <div class='p'> log
    // block). The plain engine discards both (`val (_, next) = ...`) because
    // self-play/search only need the winners + recorded feature vectors. The
    // game MUST be constructed with logging=true for the HTML block to populate.
    //
    // `sink` receives every performed Action (for serialization by the caller,
    // which owns the Serialize instance) and its returned log lines, in order.
    // ─────────────────────────────────────────────────────────────────────────

    /** advanceToDecision that reports each performed action + its log lines to `sink`. */
    def advanceToDecisionLogged(game : Game, from : Continue,
                                sink : (Action, $[String]) => Unit) : Situation = {
        var c = from
        var guard = 0
        while (true) {
            guard += 1
            if (guard > 100000) throw new RuntimeException("advanceToDecisionLogged: step cap (engine loop?)")
            c match {
                case GameOver(winners) => return Ended(winners)
                case DelayedContinue(_, inner) => c = inner
                case MultiAsk(asks) => c = asks.head
                case Ask(faction, rawActions) =>
                    val choices = Explode.explode(game, rawActions)
                    choices.num match {
                        case 0 =>
                            rawActions match {
                                case List(a) =>
                                    val (l, n) = game.perform(a.unwrap); sink(a.unwrap, l); c = n
                                case _ => throw new RuntimeException(
                                    "advanceToDecisionLogged: Ask exploded to 0 choices, faction=" + faction + " raw=" + rawActions.num)
                            }
                        case 1 =>
                            val (l, n) = game.perform(choices.head.unwrap); sink(choices.head.unwrap, l); c = n
                        case _ =>
                            return Decision(faction, choices, c)
                    }
                case _ =>
                    resolveChance(game, c) match {
                        case Some(action) =>
                            val (l, next) = game.perform(action.unwrap); sink(action.unwrap, l); c = next
                        case None =>
                            throw new RuntimeException("advanceToDecisionLogged: unhandled continue " + c.getClass.getName)
                    }
            }
        }
        throw new IllegalStateException("unreachable")
    }

    /** Start + advance, logging every performed action (StartAction included). */
    def startLogged(game : Game, sink : (Action, $[String]) => Unit) : Situation = {
        val (l, c0) = game.perform(StartAction); sink(StartAction, l)
        advanceToDecisionLogged(game, c0, sink)
    }

    /**
     * rolloutCapped that records the full trace. Returns (winners, hitCap, actions, htmlLog)
     * where `actions` is every performed Action IN ORDER and `htmlLog` is every log line
     * IN ORDER. Caller serializes the actions (it owns the board-bound Serialize) and
     * wraps the log lines in <div class='p'>…</div> for build-replay.py.
     */
    def rolloutLogged(game : Game, sit : Situation, policy : DecisionPolicy,
                      maxDecisions : Int = 20000) : ($[Faction], Boolean, IndexedSeq[Action], IndexedSeq[String]) = {
        val actions = ArrayBuffer[Action]()
        val htmlLog = ArrayBuffer[String]()
        val sink : (Action, $[String]) => Unit = (a, ls) => { actions += a; htmlLog ++= ls.toList }

        var s = sit
        var decisions = 0
        var winners : $[Faction] = $()
        var hitCap = false
        var running = true
        while (running) {
            s match {
                case Ended(w) => winners = w; running = false
                case Decision(faction, acts, c) =>
                    decisions += 1
                    if (decisions > maxDecisions) { hitCap = true; running = false }
                    else {
                        val a = policy.decide(game, faction, acts)
                        val (l, next) = game.perform(a.unwrap); sink(a.unwrap, l)
                        s = advanceToDecisionLogged(game, next, sink)
                    }
            }
        }
        (winners, hitCap, actions.toIndexedSeq, htmlLog.toIndexedSeq)
    }
}

/** Wraps the existing hand-tuned bots as a policy (opponent model + baseline). */
object BotPolicy extends DecisionPolicy {
    def decide(game : Game, faction : Faction, actions : $[Action]) : Action =
        Host.askFaction(game, Ask(faction, actions))
}

/** Uniform-random over legal actions — the weakest baseline / sanity check. */
object RandomPolicy extends DecisionPolicy {
    def decide(game : Game, faction : Faction, actions : $[Action]) : Action =
        actions.maxBy(_ => random())
}
