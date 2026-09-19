package cws

import hrf.colmat._

// Option A, Phase 4 — the self-play + training loop. This is the closure that
// turns "a search + a blank evaluator" into "a bot that taught itself".
//
// The AlphaZero cycle, one iteration:
//   1. PLAY: MCTS(ModelEval(currentModel)) sits in ALL FOUR chairs and plays a
//      batch of complete games. At every genuine decision we record the deciding
//      faction's feature vector (a training INPUT). One brain, four seats — the
//      config the user asked about.
//   2. LABEL: when a game ends, every recorded example from that game is labelled
//      1.0 if its faction was among the winners, else 0.0 (the training TARGET).
//   3. TRAIN: gradient-descent the value model on those (features, label) pairs so
//      its win-probability estimate better matches what actually won.
//   4. REPEAT: the next batch plays with the improved model. Better evaluation →
//      better search → better games → better labels → better evaluation.
//
// Nothing here is faction-specific. The features are universal and the label is
// just "did this seat win", so the SAME loop produces a competent policy for a
// faction that never had a hand-written bot — which is the entire point.

/** One training example: how a state looked to the faction about to move, and
 *  (filled in when the game ends) whether that faction went on to win. */
@SerialVersionUID(1L)
final class Example(val features : Array[Double], val faction : Faction) extends Serializable {
    var label : Double = -1.0   // set at game end: 1.0 win, 0.0 loss
    // Position-quality potential Φ(s) of THIS state for THIS faction, captured live at
    // record time (Outcome.statePotential). Used by the potential-based label so the
    // value net learns per-move position quality instead of one smeared terminal label.
    // -1.0 = not captured (label falls back to pure terminal, preserving old behavior).
    var potential : Double = -1.0
}

object SelfPlay {

    // The FOUR BASE factions in a 4-player game on Earth 3.5: Great Cthulhu (GC),
    // Black Goat (BG), Yellow Sign (YS), Crawling Chaos (CC). All four are audit-
    // confirmed clone-safe (no per-game singleton vars, unlike TSExpansion), so MCTS
    // cloning is faithful and parallel self-play is safe. NO neutral units, NO neutral
    // spellbooks, NO other options — a clean base-game board. Doom track / game
    // structure are the stock RitualTrack.for4 / EarthMap4v35, unchanged.
    def fixedSeating : $[Faction] = $(GC, BG, YS, CC)
    def newGame() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, false, $())

    /** Same board/seating as newGame() but with logging ON and the MapEarth35 option
     *  set, so `perform` emits the HTML game-log lines and the Options line carries the
     *  map token build-replay.py detects (earth35). Used only for producing watchable
     *  replay traces — never inside search/self-play (logging has per-perform overhead). */
    def newGameLogged() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, true, $(MapEarth35))

    // ─── ENVIRONMENT-CONFIGURABLE GAME FACTORY (Phase 1 part 2, 2026-08-01) ────────
    //
    // The encoder can now SEE any environment (EncodingLayout caps); this is the other
    // half — CONSTRUCTING a non-base-4 game for it to read. The engine already ships the
    // pieces (EarthMap3/4v35/4v53/5/6, RitualTrack.for3/4/5, factions GC/BG/YS/CC plus
    // OW/SL/AN/WW/DS/FB/TS), so this factory just SELECTS them from env vars. It DEFAULTS
    // EXACTLY to base-4 Earth-3.5 GC/BG/YS/CC, so with no env set it builds the identical
    // game newGame() does — the running champion loop is untouched. To play a different
    // environment you set env; nothing here forces it.
    //
    //   CW_MAP      = earth33 | earth35 (default) | earth53 | earth55 | earth6
    //   CW_SEATING  = comma-separated faction codes (default "GC,BG,YS,CC"); its length
    //                 also picks the ritual track (3->for3, 4->for4, 5->for5).
    //
    // IMPORTANT — this does NOT relax the clone-safety guard. Base-4 factions are audit-
    // confirmed clone-safe; TS (TSExpansion) has per-game singleton vars that break MCTS
    // cloning, so seating TS here is REFUSED (falls back to base-4) until that leak is
    // fixed. Any faction code that isn't a known clone-safe symbol also falls back, so a
    // typo can't silently build an unclonable game. The chosen map's region count may
    // exceed the encoder's CW_MAX_REGIONS cap — that's the caller's responsibility to set
    // (a warning is printed); the encoder itself is bounds-safe either way.

    private val factionByCode : Map[String, Faction] =
        Map("GC" -> GC, "BG" -> BG, "YS" -> YS, "CC" -> CC,
            "OW" -> OW, "SL" -> SL, "AN" -> AN, "WW" -> WW, "DS" -> DS, "FB" -> FB)
        // NOTE: TS deliberately absent — not clone-safe (see project note). Add when fixed.

    private def boardByToken(tok : String) : (Board, GameOption, Int) = tok.trim.toLowerCase match {
        case "earth33"          => (EarthMap3,     MapEarth33, 13)
        case "earth53"          => (EarthMap4v53,  MapEarth53, 17)
        case "earth55"          => (EarthMap5,     MapEarth55, 21)
        case "earth35" | "" | _ => (EarthMap4v35,  MapEarth35, 17)   // base-4 default
    }

    /** Seating parsed from CW_SEATING, restricted to clone-safe factions; falls back to
     *  base-4 GC/BG/YS/CC if empty, malformed, out of the 3–5 range, or naming a
     *  non-clone-safe faction (e.g. TS). Returns (seating, ritualTrack). */
    private def envSeating() : ($[Faction], $[Int]) = {
        val raw = System.getenv("CW_SEATING")
        val parsed : Option[List[Faction]] =
            if (raw == null || raw.trim.isEmpty) None
            else {
                val codes = raw.split(",").map(_.trim.toUpperCase).filter(_.nonEmpty).toList
                val fs = codes.map(c => factionByCode.get(c))
                if (fs.forall(_.isDefined) && fs.size >= 3 && fs.size <= 5) Some(fs.map(_.get))
                else { println(s"CW_SEATING='$raw' invalid/not-clone-safe/out-of-range -> base-4 fallback"); None }
            }
        val seatList = parsed.getOrElse(List(GC, BG, YS, CC))
        val track = seatList.size match {
            case 3 => RitualTrack.for3
            case 5 => RitualTrack.for5
            case _ => RitualTrack.for4
        }
        (seatList.foldRight($[Faction]())(_ :: _), track)
    }

    /** Build a game from CW_MAP / CW_SEATING (defaults reproduce base-4 Earth-3.5 exactly).
     *  `logged` mirrors newGameLogged so replay traces work for any environment. */
    def envGame(logged : Boolean = false) : Game = {
        val (board, mapOpt, regions) = boardByToken(Option(System.getenv("CW_MAP")).getOrElse(""))
        val (seating, track) = envSeating()
        if (regions > Features.maxRegions)
            println(s"NOTE: map has $regions regions > CW_MAX_REGIONS=${Features.maxRegions}; raise the cap to encode them all (encoder stays bounds-safe).")
        val opts = if (logged) $(mapOpt) else $[GameOption]()
        new Game(board, track, seating, logged, opts)
    }

    /**
     * Play ONE full self-play game with `searcher` in every seat, recording an
     * Example at each genuine decision. Returns (examples, winners, decisionCount).
     * The searcher should already wrap the current model (ModelEval).
     */
    // Self-play games are capped well below the strict 20000 guard: a real base-4
    // game to 30 doom resolves in a couple thousand decisions, so a game still going
    // at 4000 is a weak-play stalemate. We ABANDON it (no winner) rather than burn ~5x
    // the compute churning it — and thanks to shaping it's still useful training data.
    def playGame(searcher : MCTSPolicy, decisionCap : Int = 4000) : ($[Example], $[Faction], Int) = {
        val g = newGame()
        val examples = scala.collection.mutable.ArrayBuffer[Example]()
        val trajectory = new Trajectory(g.setup)

        var decisions = 0
        val exIdx = scala.collection.mutable.ArrayBuffer[Int]()
        val recording = new DecisionPolicy {
            def decide(game : Game, faction : Faction, actions : $[Action]) : Action = {
                // Observe the live game for the shaping trajectory (gate loss/capture
                // diffs, per-turn AP power/gate snapshots), then record how the state
                // looks to THIS faction before it acts.
                trajectory.observe(game)
                val ex = new Example(Features.of(game, faction), faction)
                ex.potential = Outcome.statePotential(game, faction)  // live per-move position quality
                examples += ex
                exIdx += decisions
                decisions += 1
                searcher.decide(game, faction, actions)
            }
        }

        val s0 = Engine.start(g)
        // Capped, non-throwing: an abandoned stalemate returns no winner but leaves the
        // game state readable, so its shaped labels still train the model.
        val (winners, _) = Engine.rolloutCapped(g, s0, recording, decisionCap, throwOnCap = false)

        // PROGRESS-BLENDED per-state label (2026-07-29): blend each state's own potential
        // Φ(s_t) with the terminal value by how deep into the game it sat, so the value net
        // gets a per-move gradient instead of one terminal label smeared across every state.
        // Terminal = the same valueShaped partial-credit label (win=1.0, else doom/spellbook/
        // shaping) that gave a gradient even in no-winner games.
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
        (examples.toList, winners, examples.length)
    }

    /**
     * One training iteration over `gamesPerIter` games. Mutates `model` in place.
     * Returns per-iteration stats for reporting.
     */
    def trainIteration(model : ValueNet, sims : Int, gamesPerIter : Int,
                       lr : Double, epochs : Int) : IterStats = {
        val searcher = new MCTSPolicy(sims = sims, leaf = ModelEval(model))
        val allExamples = scala.collection.mutable.ArrayBuffer[Example]()
        var wins = Map[Faction, Int]().withDefaultValue(0)
        var noWinner = 0
        var totalDecisions = 0

        var gi = 0
        while (gi < gamesPerIter) {
            val (ex, winners, decisions) = playGame(searcher)
            allExamples ++= ex
            totalDecisions += decisions
            if (winners.isEmpty) noWinner += 1
            else winners.foreach(f => wins = wins.updated(f, wins(f) + 1))
            gi += 1
        }

        // Train: shuffle and SGD for a few epochs over the batch.
        val data = allExamples.toArray
        var beforeLoss = logLoss(model, data)
        var e = 0
        while (e < epochs) {
            val order = data.indices.toArray
            // Deterministic-ish shuffle without Math.random (index-based rotation).
            var k = 0
            while (k < order.length) {
                val ex = data(order(k))
                model.train(ex.features, ex.label, lr)
                k += 1
            }
            e += 1
        }
        val afterLoss = logLoss(model, data)

        IterStats(gamesPerIter, data.length, totalDecisions, wins, noWinner, beforeLoss, afterLoss)
    }

    /** Mean logistic loss of the model over a labelled batch (lower = better fit). */
    def logLoss(model : ValueNet, data : Array[Example]) : Double = {
        if (data.isEmpty) return 0.0
        var s = 0.0; var i = 0
        while (i < data.length) {
            val p = math.min(1.0 - 1e-9, math.max(1e-9, model.eval(data(i).features)))
            val y = data(i).label
            s += -(y * math.log(p) + (1 - y) * math.log(1 - p))
            i += 1
        }
        s / data.length
    }

    final case class IterStats(games : Int, examples : Int, decisions : Int,
                               wins : Map[Faction, Int], noWinner : Int,
                               lossBefore : Double, lossAfter : Double)
}
