package cws

// ─────────────────────────────────────────────────────────────────────────────
// BROWSER-SAFE MCTS CLONE for the client-side (Scala.js) brain port — blocker #2.
//
// The JVM self-play path clones a position with Cloning.copy(g, c): (Game, Continue),
// a REFLECTION-based deep clone (com.rits.cloning). Reflection does not exist in
// Scala.js, so that primitive cannot run in the browser. This file is the
// browser-viable replacement.
//
// FORK-BY-REPLAY. The live engine already rebuilds any position by constructing a
// fresh `new Game(board, ritualTrack, setup, logging, opts)` and replaying the
// recorded action log through `serializer.parseAction` + `game.perform` — this is
// exactly what undo / replayMenu / startGame do (CthulhuWarsSolo.scala:537, 2585,
// 3007). We reuse that same path as the clone primitive: to fork the current
// position, rebuild from scratch and replay the recorded log up to the fork point.
//
// DETERMINISM (verified for base-4, see project_brain_wiring_directive_blockers_answered):
// dice use Math.random, BUT every roll OUTCOME (RollD6/RollBattle/RollAgony/DrawES)
// is SERIALIZED into the action log as a recorded action (Serialize.scala) and the
// driver replays those recorded results verbatim (CthulhuWarsSolo.scala:563-597)
// instead of re-rolling. So replaying the log reproduces the identical state — no
// divergence, no fresh randomness. A fork is a faithful copy.
//
// CONTRACT DIFFERENCE FROM Cloning.copy — READ THIS (porter note):
//   Cloning.copy(g, c) takes the live object graph and needs nothing else, because
//   reflection walks the whole graph. Fork-by-replay CANNOT read state out of `g`;
//   it must be handed the SETUP + the RECORDED ACTION LOG, because a fresh Game is
//   built from those. The driver (CthulhuWarsSolo) already holds `var actions` —
//   thread that in. MCTS.scala:247 `val (g, c0) = Cloning.copy(rootGame, rootContinue)`
//   becomes, in the browser build, `val (g, c0) = ClientClone.fork(setup, recorded)`
//   ONCE at the root; every simulation then advances that forked Game by replaying
//   move indices exactly as the JVM path does (MCTS already never re-clones per
//   simulation — it forks once at the root per search, then mutates). So the browser
//   needs exactly ONE fork per MCTS search, which is cheap even by replay.
//
// Staged in isolation (brain-clientport/, NOT mcts-src/) so the running champion and
// the live JVM builds stay byte-identical. The porter swaps this in for Cloning.copy
// at the single root-fork call site when assembling the Scala.js /brain/ build.
// ─────────────────────────────────────────────────────────────────────────────

object ClientClone {

    /** A recorded position: everything needed to rebuild a Game from scratch.
     *  `recorded` is the serialized action log (one Serialize.write string per
     *  entry, roll outcomes included) in the exact form the driver stores. */
    case class Position(
        board       : Board,
        ritualTrack : List[Int],
        setup       : List[Faction],
        options     : List[GameOption],
        recorded    : List[String]
    )

    /** Fork the position: build a fresh Game and replay the recorded log to the end
     *  of `recorded`. Returns the rebuilt Game and the Continue at the fork point,
     *  matching the (Game, Continue) shape MCTS expects from Cloning.copy.
     *
     *  Single-threaded browser use only. Deterministic: replays serialized roll
     *  outcomes, never re-rolls (see header). */
    def fork(pos : Position) : (Game, Continue) = {
        val game = new Game(pos.board, pos.ritualTrack, pos.setup, false, pos.options)
        // `serializer` is not a global — the driver builds one per game as
        // `new Serialize(game)` (CthulhuWarsSolo.scala:545). Bind it to THIS Game so
        // parseAction resolves regions/factions/units against the freshly-built board.
        val serializer = new Serialize(game)
        var cont : Continue = game.continue   // == StartContinue on a fresh Game (Game.scala:1465)
        // Replay every recorded action verbatim. parseAction reconstructs the exact
        // Action (including RollBattle/DrawES outcomes); perform advances the Game and
        // returns the next Continue. HTML-entity unescape matches the driver's replay
        // (CthulhuWarsSolo.scala:564 `.replace("&gt;", ">")`).
        pos.recorded.foreach { line =>
            val a = serializer.parseAction(line.replace("&gt;", ">"))
            val (_, c) = game.perform(a)
            cont = c
        }
        (game, cont)
    }

    /** Convenience for the root fork inside MCTS: given the current driver setup and
     *  its live `actions` list already serialized to strings, produce a forked
     *  (Game, Continue). Keeps the MCTS call site a one-liner. */
    def fork(board : Board, ritualTrack : List[Int], setup : List[Faction],
             options : List[GameOption], recorded : List[String]) : (Game, Continue) =
        fork(Position(board, ritualTrack, setup, options, recorded))
}
