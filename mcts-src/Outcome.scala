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

    // Non-winner partial credit. A true winner is always 1.0. For everyone else the
    // label reflects progress toward the game's TWO HARD WIN CONDITIONS plus a smaller
    // "means-to-an-end" milestone term.
    //
    // BOTH hard conditions weighted EQUALLY (user directive 2026-07-27): you cannot win
    // without ALL 6 SPELLBOOKS, and you cannot win without the MOST DOOM — neither is
    // optional, so the reward must value them the same. The old blend gave doom 0.75
    // and buried spellbooks as 1/10 of a 0.25 shaping term (~0.025 — ~1/30th of doom),
    // which taught the brain to chase doom and neglect spellbooks, so it reached high
    // doom but never FINISHED (a win needs both). Now:
    //   - spellbook progress (x/6) and doom progress are co-equal, HardWeight each,
    //   - the ten secondary milestones (gates/power/ES/…) share the small remainder.
    // Winner still = 1.0 and the non-winner band is capped (NonWinnerCeiling), so a win
    // always dominates any loss — but the gradient now points at BOTH win conditions.
    //
    // These are only the STARTING weights for the label. The value net trained on real
    // won/lost games self-tunes its own per-feature weights from outcomes (see the
    // win-loss training mode) — this hand-set blend just seeds a sane gradient.
    // 2026-07-28 (user directive): the brain is running WAY low on spellbooks (self-play
    // SB 3.5/6 vs the bots' 5.6), so boost the SBR/spellbook term above doom. Every
    // spellbook requirement met earns a book, and a win needs ALL 6 — the co-equal blend
    // wasn't pulling the brain toward finishing them, so make the spellbook hard-condition
    // the DOMINANT reward term. Doom stays a strong secondary (you still can't win without
    // the lead), shaping shrinks to the small remainder.
    private val DoomWeight       = 0.30    // doom hard-condition (strong secondary)
    private val SpellbookWeight  = 0.55    // spellbook/SBR hard-condition (boosted, dominant)
    private val ShapingWeight    = 0.15    // secondary milestones (means to an end)
    private val NonWinnerCeiling = 0.6

    /** Terminal label combining progress toward BOTH hard win conditions (doom lead and
     *  6 spellbooks, weighted equally) with the secondary milestone shaping score.
     *  `shaping` is Trajectory.score(game) for `me`, already in [0,1]. Winner => 1.0. */
    def valueShaped(game : Game, winners : $[Faction], me : Faction, shaping : Double) : Double = {
        if (winners.contains(me)) return 1.0
        val doomsByFaction = game.setup.map(f => game.players(f).doom.toDouble)
        val leader   = math.max(1.0, doomsByFaction.max)
        val doomPart = game.players(me).doom.toDouble / leader              // toward doom-lead condition
        val sbPart   = math.min(1.0, game.players(me).spellbooks.num / 6.0) // toward 6-spellbook condition
        val blended  = DoomWeight * doomPart + SpellbookWeight * sbPart + ShapingWeight * shaping
        // Hold the whole non-winner band well below 1.0 so a loss never rivals a win.
        NonWinnerCeiling * math.max(0.0, math.min(1.0, blended))
    }

    /** Pure win/loss label (1.0 winner, 0.0 otherwise). This is the ground-truth signal
     *  that lets the value net SELF-OPTIMIZE how much weight each intermediate goal
     *  deserves: with real wins in the bot corpus, gradient descent discovers from
     *  outcomes which states (how many spellbooks, how much doom, what board shape)
     *  actually precede victory — no hand-set blend imposed. */
    def winLoss(game : Game, winners : $[Faction], me : Faction) : Double =
        if (winners.contains(me)) 1.0 else 0.0
}
