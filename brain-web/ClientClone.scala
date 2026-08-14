package cws

import hrf.colmat._

// ─────────────────────────────────────────────────────────────────────────────
// BROWSER-SAFE MCTS CLONE for the client-side (Scala.js) brain port — blocker #2.
//
// The JVM self-play path clones a position with Cloning.copy(g, c): (Game, Continue),
// a REFLECTION-based deep clone (com.rits.cloning). Reflection does not exist in
// Scala.js, so that primitive cannot run in the browser. This file is the
// browser-viable replacement.
//
// FORK-BY-REPLAY (in-memory). The live engine ALREADY forks a position exactly this
// way for its own undo: it builds a fresh `new Game(board, ritualTrack, setup,
// logging, options)` and replays the recorded action list through `g.perform(a.unwrap)`
// (CthulhuWarsSolo.scala:2585-2591, performUndo). We reuse that identical primitive as
// the MCTS clone: to fork the current position, rebuild from scratch and replay the
// driver's `actions` log. The driver PREPENDS each performed action (`actions +:= a`,
// line 2773), so the chronological order to replay is `actions.reverse`.
//
// IN-MEMORY, NOT STRING ROUND-TRIP. An earlier draft replayed Serialize.write strings
// through parseAction. That is unnecessary — the driver holds the live Action objects,
// and the engine's own undo replays those objects directly — and it would be far slower
// (serialize + parse the whole log on every one of the 60-160 simulations per move).
// We replay the in-memory Actions verbatim, exactly as performUndo does.
//
// DETERMINISM (verified for base-4, see project_brain_wiring_directive_blockers_answered):
// dice use Math.random, BUT every roll OUTCOME (RollD6/RollBattle/RollAgony/DrawES) is a
// recorded action already in the log, and replay performs those recorded results verbatim
// instead of re-rolling. So replaying the log reproduces the identical state — no
// divergence, no fresh randomness. A fork is a faithful copy.
//
// CONTRACT DIFFERENCE FROM Cloning.copy — READ THIS (porter note):
//   Cloning.copy(g, c) took the live object graph and needed nothing else, because
//   reflection walks the whole graph. Fork-by-replay CANNOT read state out of `g`; it
//   must be handed board/ritualTrack/setup/options + the RECORDED ACTION LIST, so a
//   fresh Game can be built and replayed. The driver holds all of these (`board`,
//   `track`, `seating`, `setup.options`, `actions`); it stashes them in a Position on
//   MCTSPolicy.rootPos right before each decide() call, and MCTS.simulateOnce forks from
//   that Position once per simulation (matching the JVM's one-clone-per-simulation).
// ─────────────────────────────────────────────────────────────────────────────

object ClientClone {

    /** A recorded position: everything `new Game(...)` needs plus the chronological
     *  action log to replay. `recorded` is the driver's `actions.reverse` (oldest
     *  first) — the SAME list the engine's own performUndo replays. */
    case class Position(
        board       : Board,
        ritualTrack : $[Int],
        setup       : $[Faction],
        options     : $[GameOption],
        recorded    : $[Action]
    )

    /** Fork the position: build a fresh Game and replay the recorded action list.
     *  Returns the rebuilt Game and the Continue at the fork point, matching the
     *  (Game, Continue) shape of the JVM Cloning.copy it replaces. MCTS reuses the
     *  caller's rootContinue (a value-flyweight Ask) for its c0, so only the Game is
     *  consumed from this return; the Continue is returned for shape parity.
     *
     *  `logging=false`: the fork is a search scratch copy, never a UI game, so it must
     *  not emit log lines. Single-threaded browser use only. Deterministic: replays
     *  serialized roll outcomes, never re-rolls (see header). Skips isVoid actions
     *  exactly as performUndo does (CthulhuWarsSolo.scala:2588). */
    def fork(pos : Position) : (Game, Continue) = {
        val game = new Game(pos.board, pos.ritualTrack, pos.setup, false, pos.options)
        var cont : Continue = StartContinue
        pos.recorded.foreach { a =>
            if (a.isVoid.not) {
                val (_, c) = game.perform(a.unwrap)
                cont = c
            }
        }
        (game, cont)
    }
}
