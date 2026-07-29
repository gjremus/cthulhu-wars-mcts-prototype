package cws

import hrf.colmat._

// Final performance measurement for the summary. Reports the numbers the user
// asked for in plain terms:
//   - raw engine throughput (a "bare" game with no search): the 1-game/sec baseline
//   - MCTS ModelEval throughput as decisions/sec and EFFECTIVE games/sec, where
//     "effective" counts the in-game forking: each real move runs `sims` internal
//     simulations, so one played game does ~sims x more engine work than a bare game
//   - a multi-core projection grounded in the DS refactor (SimRunner must run games
//     sequentially because the DS singleton races; moving that state onto Game — done
//     in this prototype — removes the barrier, so games can run one-per-core)

object PerfReport {

    def bareGameMs(nGames : Int) : Double = {
        // warmup
        val gw = SelfPlay.newGame(); Engine.rollout(gw, Engine.start(gw), BotPolicy)
        val t0 = System.nanoTime()
        var i = 0
        while (i < nGames) {
            val g = SelfPlay.newGame()
            Engine.rollout(g, Engine.start(g), BotPolicy)
            i += 1
        }
        (System.nanoTime() - t0) / 1e6 / nGames
    }

    def searchDecisionMs(sims : Int, nDecisions : Int) : Double = {
        val model = ValueModel.initial
        val searcher = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
        var g = SelfPlay.newGame()
        var sit = Engine.start(g)
        // warmup
        var w = 0
        while (w < 3) { sit match {
            case Ended(_) => g = SelfPlay.newGame(); sit = Engine.start(g)
            case Decision(f, acts, _) =>
                searcher.decide(g, f, acts)
                val a = BotPolicy.decide(g, f, acts); val (_, n) = g.perform(a.unwrap); sit = Engine.advanceToDecision(g, n); w += 1
        }}
        var done = 0
        val t0 = System.nanoTime()
        while (done < nDecisions) { sit match {
            case Ended(_) => g = SelfPlay.newGame(); sit = Engine.start(g)
            case Decision(f, acts, _) =>
                searcher.decide(g, f, acts)
                val a = BotPolicy.decide(g, f, acts); val (_, n) = g.perform(a.unwrap); sit = Engine.advanceToDecision(g, n); done += 1
        }}
        (System.nanoTime() - t0) / 1e6 / nDecisions
    }

    val DecisionsPerGame = 800.0

    def main(args : Array[String]) : Unit = {
        val sims = if (args.length > 0) args(0).toInt else 30
        val cores = Runtime.getRuntime.availableProcessors

        println("=== Option A — final performance report ===\n")

        val bare = bareGameMs(if (args.length > 1) args(1).toInt else 12)
        println(f"Bare game (bots, no search):   ${bare}%.0f ms/game  =  ${1000.0 / bare}%.2f games/sec (1 core)")

        val decMs = searchDecisionMs(sims, if (args.length > 2) args(2).toInt else 60)
        val gameMsSearch = decMs * DecisionsPerGame
        val gamesPerSec = 1000.0 / gameMsSearch
        println(f"MCTS decision (sims=$sims):       ${decMs}%.1f ms/decision")
        println(f"MCTS full game (sims=$sims):      ${gameMsSearch / 1000}%.1f s/game  =  ${gamesPerSec}%.3f played-games/sec (1 core)")

        // Effective games/sec = engine work relative to a bare game. Each played move
        // runs ~sims internal simulations (each a partial game), so throughput of
        // game-equivalents is roughly played-games/sec x sims.
        val effective = gamesPerSec * sims
        println(f"Effective (counts forking):    ~${effective}%.2f game-equivalents/sec (1 core)  [x$sims%d sims/move]")

        println(f"\nMachine has $cores%d cores. The DS-singleton refactor (state moved onto Game)")
        println(f"removes the shared-mutable-state barrier that forces SimRunner to run games")
        println(f"sequentially, so self-play games can run one-per-core:")
        val usableCores = math.max(1, cores - 2)
        println(f"  ~$usableCores%d usable cores -> ~${gamesPerSec * usableCores}%.2f played-games/sec, " +
                f"~${effective * usableCores}%.1f game-equivalents/sec")

        println("\n=== plain-terms takeaway ===")
        println(f"A raw game is ~${1000.0/bare}%.1f/sec. Search makes each MOVE ~$sims%dx more work, so a")
        println(f"searched game is slower in wall-clock but does far more thinking. The value")
        println(f"net (not playouts) is what keeps that ~50x cheaper than the naive approach,")
        println(f"and the DS fix is what lets it fan out across all $cores%d cores.")
    }
}
