package cws

import hrf.colmat._

// Option A — the terminal reward.
//
// AlphaZero's reward is win/loss/draw. That works when games END with a winner.
// Cthulhu Wars does not, under weak early self-play: a winner needs ALL spellbook
// requirements AND the doom lead; before the value model learns anything, players
// stalemate to the turn limit with nobody qualified, so the engine declares
// "humanity won" (no faction winner) EVERY game. Binary win/loss then labels every
// example 0.0 — a degenerate, gradient-free signal. The model would trivially fit
// the constant "everyone loses" and appear to "converge" while learning nothing.
//
// The standard remedy for multiplayer MCTS is a DENSE terminal reward that still
// ranks a true win highest but gives losing seats partial credit for how close they
// came — here, the doom they accumulated (the game's actual scoring objective). A
// genuine winner has the doom lead by definition, so it still scores ~1.0; the
// signal is consistent with the real rules and gives the learner a gradient from
// turn one. Faction-agnostic: it reads only universal quantities.
object Outcome {

    /** Terminal value in [0,1] for `me`, given the finished game and its winners.
     *  1.0 = a true winner; otherwise 0.5 * (my doom / leader's doom), so a loss is
     *  always worth less than a win but is scaled by how close the seat came. */
    def value(game : Game, winners : $[Faction], me : Faction) : Double = {
        if (winners.contains(me)) return 1.0
        val doomsByFaction = game.setup.map(f => game.players(f).doom.toDouble)
        val leader = math.max(1.0, doomsByFaction.max)
        val mine = game.players(me).doom.toDouble
        0.5 * (mine / leader)
    }

    // Non-winner partial credit. A true winner is ALWAYS 1.0 (the engine decides who won;
    // the reward does not second-guess it — so a WINNING bot at 32 doom + 6 SB always beats
    // a LOSING "well-behaved" bot at 29 doom + 6 SB, no matter how clean the loser played).
    //
    // REDESIGN 2026-07-29 (user directive): doom must be rewarded in exactly ONE place. It
    // now lives in the shaping score (Shaping.scoreBreakdown "doomEarned", 1 doom = 0.01 on
    // this [0,1] scale), so the old end-game doom+spellbook blend here is REMOVED — keeping
    // it would double-count doom and break the calibrated doom-equivalent ratios. The terminal
    // label for a non-winner is therefore simply the shaping score, held below a win by the
    // ceiling. shaping already encodes the full doom-equivalent picture the user specified:
    //   1 doom = 0.01, 1 SB = 8 doom, 1 ES earned = 1.66 doom, results ≫ behavior, losses
    //   subtract. A full-win-shaped game lands ≈0.78, and the ceiling guarantees a non-winner
    //   can NEVER reach 1.0 — so "results, capped below a real win" is exactly what trains.
    //
    // NonWinnerCeiling caps the whole losing band strictly below 1.0. Set to 0.90: high enough
    // that a near-win (≈0.78 shaping) reads as "almost there", but a real win (1.0) still wins.
    private val NonWinnerCeiling = 0.90

    /** Terminal label for `me`: a true WINNER is 1.0 (unconditional — the win itself
     *  guarantees the top score); every non-winner gets the doom-equivalent shaping score
     *  (`shaping` = Trajectory.score, already in [0,1]) held below 1.0 by NonWinnerCeiling,
     *  so 1.0 is unreachable without actually winning. Doom/spellbooks are counted once, in
     *  the shaping score — no separate end-game doom reward.
     *
     *  PLATEAU LEVER (b), 2026-07-29 (R12 plateaued: best-checkpoint flat 4 iters @ 3.48,
     *  doom settled ~22-23 well short of 30, arena 0/32). ROOT CAUSE: under weak self-play
     *  most games hit the decision cap with NO faction qualified, so the engine declares
     *  "humanity won" (winners empty). Every seat then gets a below-1.0 label — the net NEVER
     *  sees a "THIS is winning" target, so nothing pulls doom toward 30. DOOM-RACE TERMINAL:
     *  when there is no engine winner (the stalemate case ONLY), treat the highest-doom seat(s)
     *  as the winner for the value label (1.0) — the real-CW endgame tiebreak (most doom wins).
     *  A genuinely-decided game (winners.nonEmpty) is UNTOUCHED: its real winner is 1.0 and its
     *  real losers stay capped below 1.0, so the user's hard rule ("impossible to reach 1
     *  without winning") still holds for every game the engine actually decided. This gives
     *  high-doom seats in stalemated games the win gradient the ES/SB-gated condition denies. */
    def valueShaped(game : Game, winners : $[Faction], me : Faction, shaping : Double) : Double = {
        if (winners.contains(me)) return 1.0
        // Doom-race fallback: ONLY when the game ended with no engine winner (stalemate at cap).
        if (winners.isEmpty) {
            val doomByF = game.setup.map(f => f -> game.players(f).doom).toMap
            val topDoom = doomByF.values.max
            // Require a positive doom lead so a 0-doom stalemate doesn't hand out a spurious win.
            if (topDoom > 0 && doomByF.getOrElse(me, 0) == topDoom) return 1.0
        }
        NonWinnerCeiling * math.max(0.0, math.min(1.0, shaping))
    }

    // ─── STATE POTENTIAL Φ(s) — the immediate-reward fix (2026-07-29) ──────────────
    // The value net was trained on ONE terminal label smeared across all ~1000 states of
    // a game, so it could not tell a good move from a bad one WITHIN a game — the core
    // reason self-play never reached winning play (internal metrics climbed, arena flat,
    // doom topped out ~15-24 vs the bots' 30+). Φ(s) is a PURE FUNCTION of the live board
    // for one seat, in [0,1], built only from the universal win-condition quantities (no
    // faction named, so it transfers to bot-less factions). It measures "how good is THIS
    // position right now", independent of how the game ended.
    //
    // Weights: the two HARD win conditions (doom lead, 6 spellbooks) dominate, matching
    // valueShaped's blend; gates (the doom engine), an awakened GOO, and power round it
    // out. Doom is scored against 30 (the base-4 win threshold), capped at 1.0.
    // PLATEAU LEVER (a), 2026-07-29 (R11 flat 4 iters @ score 2.09, doom stuck 20-24, arena 0/32):
    // Φ(s) scored doom/SB/gates/GOO/power but had NO RITUAL TERM — yet the ritual is the doom
    // ENGINE (doom = valid gates per ritual). A position where the seat has ritualed is strictly
    // better (it has converted gates→doom and is on the ES/SB track), but Φ read it as identical
    // to one that hasn't. This under-valued the ritual investment PER STATE, so the value net gave
    // no positional credit for ritualing — the same gap that made "skip" beat "ritual" in shaping,
    // now fixed at the state-potential layer too. Weights rebalanced to still sum to 1.0.
    private val PotDoom   = 0.32  // doom / 30 (the doom-lead hard condition)
    private val PotSB     = 0.33  // spellbooks / 6 (the 6-spellbook hard condition)
    private val PotGates  = 0.13  // controlled gates, cap 2 (the doom-generation base)
    private val PotGOO    = 0.09  // own GOO awakened (enables Elder Signs + big rituals)
    private val PotRitual = 0.09  // rituals performed, cap 3 (the doom ENGINE — new lever a)
    private val PotPower  = 0.04  // power on hand, cap 10 (bankroll to act/ritual)

    /** Position-quality potential Φ(s) in [0,1] for `me`, read purely from the live game.
     *  Used to give EACH recorded state its own value target (see PolicyRun.selfPlayGame),
     *  so the value net learns per-move position quality instead of a single smeared
     *  terminal label. Faction-agnostic. */
    def statePotential(game : Game, me : Faction) : Double = {
        val p = game.players(me)
        val doomN  = math.min(1.0, p.doom.toDouble / 30.0)
        val sbN    = math.min(1.0, p.spellbooks.num / 6.0)
        val gatesN = math.min(1.0, p.allGates.num.toDouble / 2.0)
        val gooN   = if (p.goos.factionGOOs.nonEmpty) 1.0 else 0.0
        val ritN   = math.min(1.0, game.ritualHistory.count(_ == me).toDouble / 3.0)
        val powN   = math.min(1.0, p.power.toDouble / 10.0)
        val v = PotDoom * doomN + PotSB * sbN + PotGates * gatesN + PotGOO * gooN + PotRitual * ritN + PotPower * powN
        math.max(0.0, math.min(1.0, v))
    }

    /** Pure win/loss label (1.0 winner, 0.0 otherwise). This is the ground-truth signal
     *  that lets the value net SELF-OPTIMIZE how much weight each intermediate goal
     *  deserves: with real wins in the bot corpus, gradient descent discovers from
     *  outcomes which states (how many spellbooks, how much doom, what board shape)
     *  actually precede victory — no hand-set blend imposed. */
    def winLoss(game : Game, winners : $[Faction], me : Faction) : Double =
        if (winners.contains(me)) 1.0 else 0.0

    // ─── PROGRESS-BLENDED per-state label (2026-07-29) ─────────────────────────────
    // Blend a state's OWN potential Φ(s_t) with the game's TERMINAL label by how deep
    // into the game the state sat (progress = t / total decisions, in [0,1]):
    //
    //   label(s_t) = (1 - w(progress)) * Φ(s_t)  +  w(progress) * terminal
    //
    // Early states (progress→0) are judged mostly by their LOCAL position quality — the
    // per-move gradient the smeared terminal label never gave. Late states (progress→1)
    // are judged mostly by the ACTUAL result — so the net still learns what actually
    // won/lost. This is the discounted-return idea (credit decays toward the outcome as
    // the horizon shrinks) expressed as a supervised label, keeping the existing sigmoid
    // value net and its [0,1] target unchanged (no RL rewrite). w(progress) uses a mild
    // convex ramp so mid-game keeps meaningful local signal.
    def progressBlended(potential : Double, terminal : Double, progress : Double) : Double = {
        val pr = math.max(0.0, math.min(1.0, progress))
        val w  = pr * pr                     // convex: terminal weight grows late, local signal survives mid-game
        val v  = (1.0 - w) * potential + w * terminal
        math.max(0.0, math.min(1.0, v))
    }
}
