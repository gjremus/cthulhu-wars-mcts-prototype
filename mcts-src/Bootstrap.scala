package cws

import hrf.colmat._
import scala.collection.mutable.ArrayBuffer

// Path 1 — BOOTSTRAP FROM THE BOTS.
//
// The 60-iteration self-play run failed for a root reason no amount of brain size or
// reward shaping fixed: in 480 self-play games the brain NEVER SAW A WIN, so it had
// no example of finishing a game to learn from — and it settled for farming partial
// credit forever. This is the classic cold-start problem for self-play on a hard
// game, and the classic fix is to KICK-START the value net by having it learn from a
// player that already knows how to win: the hand-tuned bots.
//
// Mechanism (pure supervised learning, NO MCTS, so it's cheap and fast):
//   1. Play whole games with ALL FOUR seats controlled by the existing BotPolicy.
//      The bots reach 30 doom and WIN — so these games CONTAIN wins, the thing
//      self-play couldn't produce.
//   2. At every genuine decision, record the deciding faction's feature vector
//      (exactly as self-play does).
//   3. Label each example with the same blended win/doom/shaping value — but now the
//      labels include real 1.0 wins, so the net finally learns what a winning
//      TRAJECTORY looks like (which board states lead to victory).
//   4. Train the value net on this corpus.
//
// The result is a brain that, entering self-play/arena, already prefers states that
// the bots' winning games passed through. It is STILL faction-agnostic: we only ever
// read universal features and never copy a bot's action — we learn the VALUE of
// states the bots visited, not their move list. That value function then transfers
// to bot-less factions, which is the whole point.
//
// This is imitation of OUTCOMES, not moves: we are not cloning the bots' policy, we
// are using their competence to manufacture the won games self-play couldn't, so the
// value net has a real target. Self-play (a later phase) can then improve past them.

object Bootstrap {

    /** Play one full BOT-vs-bot game (no search), recording a shaped Example at each
     *  genuine decision. Returns (examples, winners, hitCap). */
    def playBotGame(decisionCap : Int = 8000) : ($[Example], $[Faction], Boolean) = {
        val g = SelfPlay.newGame()
        val examples = ArrayBuffer[Example]()
        val trajectory = new Trajectory(g.setup)

        val recording = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                trajectory.observe(game)
                examples += new Example(Features.of(game, faction), faction)
                BotPolicy.decide(game, faction, actions)   // the bots play — no MCTS
            }
        }

        val s0 = Engine.start(g)
        val (winners, hitCap) = Engine.rolloutCapped(g, s0, recording, decisionCap, throwOnCap = false)

        val shaping = trajectory.score(g)
        examples.foreach(e => e.label = Outcome.valueShaped(g, winners, e.faction, shaping.getOrElse(e.faction, 0.0)))
        (examples.toList, winners, hitCap)
    }

    final case class BatchResult(examples : Seq[Example], wins : Map[Faction, Int],
                                 noWinner : Int, hitCap : Int, decisions : Int)

    /** Play `nGames` bot-vs-bot games (parallel per Trainer.parallelPlay), returning
     *  all recorded examples plus a win tally so we can confirm the corpus actually
     *  contains wins (the whole reason for bootstrapping). */
    def playBatch(nGames : Int) : BatchResult = {
        def one(i : Int) : ($[Example], $[Faction], Boolean) = playBotGame()

        val results : Seq[($[Example], $[Faction], Boolean)] =
            if (Trainer.parallelPlay) {
                import scala.collection.parallel.CollectionConverters._
                (0 until nGames).par.map(one).toList
            } else {
                (0 until nGames).map(one).toList
            }

        val examples = results.flatMap(_._1)
        var wins = Map[Faction, Int]().withDefaultValue(0)
        var noWinner = 0; var hitCap = 0
        results.foreach { case (ex, winners, cap) =>
            if (cap) hitCap += 1
            if (winners.isEmpty) noWinner += 1
            else winners.foreach(f => wins = wins.updated(f, wins(f) + 1))
        }
        BatchResult(examples, wins, noWinner, hitCap, examples.size)
    }
}
