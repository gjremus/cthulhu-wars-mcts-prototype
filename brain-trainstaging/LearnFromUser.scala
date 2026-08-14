package cws

import scala.collection.mutable.ArrayBuffer

// ─────────────────────────────────────────────────────────────────────────────
// LEARN-FROM-USER — turn a saved /brain/ game (a user's real game against the
// brain) into the SAME labeled training Examples self-play produces, so the
// user's games can be folded into the training loop. Phase-2 requirement:
// "the brain should also learn from games against me."
//
// STAGED (not in mcts-src): kept in brain-trainstaging/ so it is NOT on the live
// compile path — the running champion loop and every JVM rebuild stay untouched
// until this is verified end-to-end and deliberately moved in. (An earlier draft
// sat in mcts-src and referenced a non-existent Outcome.winnersOf; that draft was
// removed to Trash. This corrected version uses the REAL winner API — the winners
// carried by the terminal GameOver continue — matching how Engine.rolloutCapped
// gets them.)
//
// KEY INSIGHT (verified 2026-08-01): a training Example needs nothing from the
// LIVE search — SelfPlay.playGame builds each Example purely from replayed GAME
// STATE: `Features.of(game, faction)` (read-only on Game), a per-move potential
// `Outcome.statePotential`, and a terminal/blended label from `Outcome.valueShaped`
// / `Outcome.progressBlended` fed by `Trajectory` (whose `observe(g)` also only
// READS the Game). A finished /brain/ game is already captured as a serialized
// action log — the exact format the replay viewer / build-replay.py save, and the
// same one CloneStepBench replays (new Game + serializer.parseAction + g.perform).
// So we regenerate labeled Examples by REPLAYING the log through a recording pass,
// identical to playGame's recorder, offline. No new capture format, no live hooks:
// the browser POSTs its finished game's action log to the training box; this turns
// it into Examples that drop straight into SelfPlay.trainIteration's buffer.
//
// VERIFIED SYMBOLS (against engine-copy/solo + mcts-src, 2026-08-01):
//   Game.perform(a): ($[String], Continue)                    Game.scala:1467
//   StartAction seeds the game (Engine.start does perform(StartAction))  Engine.scala:114
//   case class Ask(faction: Faction, actions: $[Action])      Game.scala:419
//   case class GameOver(winners: $[Faction]) extends Continue  Game.scala:445
//   new Serialize(game).parseAction(str): |[Action]           Serialize.scala:114
//   Example(features, faction){var label; var potential}      SelfPlay.scala:26
//   Outcome.statePotential / valueShaped / progressBlended     Outcome.scala
//   Trajectory(setup).observe(g) / score(g)                   Shaping.scala:38
// ─────────────────────────────────────────────────────────────────────────────

object LearnFromUser {

    /** Rebuild the game from a serialized action log and re-emit the SAME labeled
     *  Examples self-play would, one per genuine decision point. `recorded` is the
     *  game's action log (one Serialize.write string per step, roll outcomes
     *  included) exactly as the replay viewer / build-replay.py store it.
     *
     *  Mirrors SelfPlay.playGame's recording+labeling (Features.of → statePotential
     *  → Trajectory.observe → valueShaped/progressBlended); the only difference is the
     *  move at each step is the RECORDED one (what actually happened in the user's
     *  game) rather than a fresh MCTS decision — which is the whole point: learn from
     *  the moves that were really played. Returns (labeled examples, winners). */
    def examplesFrom(recorded : Seq[String]): (List[Example], $[Faction]) = {
        val g = SelfPlay.newGame()
        val serializer = new Serialize(g)
        val examples = ArrayBuffer[Example]()
        val exIdx = ArrayBuffer[Int]()
        val trajectory = new Trajectory(g.setup)
        var decisions = 0

        // Seed exactly as Engine.start does, then replay the recorded log. At every
        // genuine DECISION (an Ask with >1 legal action — same test Engine uses to
        // distinguish a real choice from a forced/chance step) record how the state
        // looks to the acting faction — the same probe playGame does — then replay the
        // recorded move. Forced/chance/info steps carry no Example, matching playGame
        // (which records only inside `decide`, called only at real decisions).
        g.perform(StartAction)
        var i = 0
        while (i < recorded.length) {
            val parsed = serializer.parseAction(recorded(i).replace("&gt;", ">")).unwrap
            g.continue match {
                case Ask(faction, actions) if actions.num > 1 =>
                    trajectory.observe(g)
                    val ex = new Example(Features.of(g, faction), faction)
                    ex.potential = Outcome.statePotential(g, faction)
                    examples += ex
                    exIdx += decisions
                    decisions += 1
                case _ =>
                    // forced move / chance resolution / info — not a decision
            }
            g.perform(parsed)
            i += 1
        }

        // Winners: the terminal GameOver carries them (same source Engine.rolloutCapped
        // trusts via Ended(winners)). If the log didn't reach GameOver (partial game),
        // treat as no-winner — the shaped labels still train, exactly like an abandoned
        // self-play stalemate.
        val winners = g.continue match {
            case GameOver(ws) => ws
            case _            => $()
        }
        val shaping = trajectory.score(g)
        val total = math.max(1, decisions)
        val exArr = examples.toArray
        var k = 0
        while (k < exArr.length) {
            val e = exArr(k)
            val terminal = Outcome.valueShaped(g, winners, e.faction, shaping.getOrElse(e.faction, 0.0))
            e.label =
                if (e.potential < 0.0) terminal
                else Outcome.progressBlended(e.potential, terminal, exIdx(k).toDouble / total)
            k += 1
        }
        (examples.toList, winners)
    }
}
