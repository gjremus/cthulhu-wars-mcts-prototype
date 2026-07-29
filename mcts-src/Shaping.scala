package cws

import hrf.colmat._
import scala.collection.mutable

// Option B — partial-credit "shaping" reward.
//
// The flat run failed for one reason: weak self-play NEVER produced a winner, so
// every training label was "nobody won" and there was no gradient to climb. The fix
// is to give each seat PARTIAL CREDIT for the strategically good things it did along
// the way, so even a game with no winner still teaches "this position was better
// than that one". The ten progress terms the user specified:
//
//   1. spellbooks earned            (more = better)
//   2. AP2 starting power           (more = better)
//   3. average AP starting power    (more = better)
//   4. end-of-AP1 gates             (more = better)
//   5. average AP-ending gates      (more = better)
//   6. own GOO awakened             (yes = better)
//   7. Elder Signs generated        (more = better)
//   8. gates lost                   (FEWER = better — inverted)
//   9. own units lost to capture    (FEWER = better — inverted)
//  10. units on map                 (more = better)
//
// Mechanism: a Trajectory observes the live game at every genuine decision (the only
// points the self-play driver stops at). It needs NO engine changes and adds NO cost
// to the MCTS search clones — it runs only in the self-play RECORDING policy. Action-
// Phase boundaries are read from g.turn / g.doomPhase (there is no phase enum); gate
// loss and captures have no engine counters, so we accumulate them by diffing
// snapshots ourselves. The per-faction shaping score in [0,1] is then blended into
// the terminal label (see Outcome.valueShaped) below a true win, so winning still
// dominates but every seat always gets a smooth signal.
//
// Faction-agnostic by construction: every term is a universal quantity read the same
// way for whichever seat is asked. No faction is ever named.

/** Rolling record of one game's trajectory, updated at each decision. */
final class Trajectory(factions : $[Faction]) {

    // Per-(faction, turn) snapshots taken during that turn's Action Phase.
    private val startPow = mutable.Map[Faction, mutable.Map[Int, Int]]()  // power at AP start
    private val endGates = mutable.Map[Faction, mutable.Map[Int, Int]]()  // controlled gates at AP end (last seen)
    private val endAband = mutable.Map[Faction, mutable.Map[Int, Int]]()  // board's abandoned gates at AP end
    // Power ENTERING the doom phase (user directive 2026-07-28) — the pre-ritual bankroll.
    // The old reward paid `avgStartPow` (power at the START of each Action Phase), which
    // PUNISHED ritualing: a ritual spends 5+ power, so a brain that rituals starts its next
    // AP poorer and scored LOWER — a direct contradiction that made "skip" beat "ritual" in
    // the doom-phase binary. Fix: reward power at the FIRST doom-phase decision of a turn
    // (fresh power has just landed in gather-power), so the brain optimizes to AFFORD the
    // ritual, and the ritual's own big reward then wins the choice. Snapshotted once/turn.
    private val preDoomPow = mutable.Map[Faction, mutable.Map[Int, Int]]()

    // Cumulative counters we must maintain ourselves (engine has none).
    private val gatesLost     = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val unitsCaptured = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val lastGates     = mutable.Map[Faction, Int]()
    private val lastCaptured  = mutable.Map[Faction, Int]()

    // Gate-EVENT counters (user directive 2026-07-28). The old `gatesLost` lumped every
    // gate departure together; the user wants the four distinct gate events scored
    // separately, each fired the INSTANT it happens (event-triggered, uncapped by later
    // recovery — same mechanism as ritualValue; delivery is the game-end label because
    // that is this brain's only reward channel). We detect them by diffing the faction's
    // controlled-gate REGION SET (not just its count) between observations, so we can
    // tell WHERE a change happened and classify it:
    //   gatesTaken       — a region newly entered my control (reward the ACT of taking).
    //   gatesAbandoned   — a region left my control and is now controlled by NOBODY
    //                      (I walked a cultist off / AbandonGateAction). PENALIZED.
    //   gatesLostToEnemy — a region left my control and another faction now controls it
    //                      (captured by an aggressor). PENALIZED (harder than abandon).
    //   gatesDefended    — a region I controlled was CONTESTED (an enemy unit shared it)
    //                      and the threat resolved while I kept control (enemy gone).
    //                      REWARDED — I held a gate against an aggressor.
    // `contestedLast` tracks which of my gates had an enemy unit present at the last
    // observation, so a defense fires once when the threat clears, not every decision.
    private val gatesTaken       = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val gatesAbandoned   = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val gatesLostToEnemy = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val gatesDefended    = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val lastGateSet      = mutable.Map[Faction, Set[Region]]()
    private val contestedLast    = mutable.Map[Faction, Set[Region]]().withDefaultValue(Set.empty)

    // Ritual-value credit (user directive 2026-07-28). The brain performs 0.0 rituals/game
    // vs the bots' ~1.9, because a ritual's payoff only shows up later as doom, so the AI
    // never feels the reward for the ACT of ritualing. Give it direct, size-scaled credit:
    // each time a faction's ritual count rises, add the gates it held at that moment — which
    // is exactly the doom that ritual produced (engine: doom = valid gates * k, Game.scala
    // ~2281). No engine edit: ritualHistory + current gate count are both readable live.
    private val lastRituals   = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val ritualValue   = mutable.Map[Faction, Double]().withDefaultValue(0.0)

    // Ability-USE incentives (user directive 2026-07-28). The brain should be nudged to
    // TRY its special tools so it can learn from using them: faction spellbooks, innate
    // faction powers, and GOO powers. The engine funnels every "use" through the once-per-*
    // cooldown sets, so we detect a use by watching a book newly ENTER any cooldown set
    // since the last observation (the sets clear at turn boundaries, so a per-turn power
    // used on turns 1 and 3 correctly counts twice). Classification is faction-agnostic:
    // a used book in f.library is a SPELLBOOK use; anything else (innate abilities, GOO-
    // granted powers) is a POWER use. These are small exploration nudges, not objectives.
    private val sbUses        = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val powerUses     = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val onCooldown    = mutable.Map[Faction, Set[Spellbook]]().withDefaultValue(Set.empty)

    // "Good ritual" pattern bonus: a ritual performed while holding 2+ gates AND with an
    // awakened GOO on the map — the shape the brain should converge toward. Counted
    // separately from raw ritual VALUE so the PATTERN itself is reinforced, not just size.
    private val goodRituals   = mutable.Map[Faction, Int]().withDefaultValue(0)

    // UNIT-LOSS pain, scaled by replacement cost (user directive 2026-07-29). Losing units
    // was BG's second-biggest mistake (after not upgrading gate keepers). A killed /
    // eliminated / sacrificed unit returns to the faction's RESERVE pool (Game.scala ~1280:
    // eliminate -> u.region = faction.reserve); a CAPTURED unit goes to a PRISON (handled by
    // the separate unitsCaptured term); a SUBMERGED Cthulhu goes to the Deep (still inPlay),
    // so it does NOT read as a loss. So we detect a true loss as a RISE in the reserve count
    // of a unit class between observations, and charge its REPLACEMENT COST — the power to
    // summon it back (UnitClass.cost: Acolyte 1, Dark Young 3, Shub-Niggurath 8, Cthulhu 4,
    // Hastur 10, ...). This scales pain by how expensive the lost piece is, exactly as
    // directed. Ancillary costs emerge for free: Shub's 2-cultist sacrifice sends those two
    // cultists to reserve, firing their own cost-1 pain each, so Shub's "true cost 10"
    // (8 + 2) accrues without any per-faction code. Board-state ancillaries (YS King-in-
    // Yellow needs a cultist in a gateless area) are left for the brain to tease out over
    // self-play, per the user's explicit allowance. Faction-agnostic: reserve + cost only.
    private val unitLossCost  = mutable.Map[Faction, Double]().withDefaultValue(0.0)
    private val lastPoolByClass = mutable.Map[Faction, Map[UnitClass, Int]]()

    // GOO-AWAKEN Action-Phase, for the AP-scaled awaken reward (user directive 2026-07-29).
    // Awakening a GOO early is worth more: AP2 is the highest-value window, AP1 ≈ AP3 (both
    // below AP2), AP4+ a flat floor. We record the TURN (this brain's AP proxy, as everywhere
    // in this file) at which the faction's first own GOO appeared on the map, then scale the
    // ownGOO reward by that turn in scoreBreakdown. -1 = not awakened.
    private val gooAwakenTurn = mutable.Map[Faction, Int]().withDefaultValue(-1)

    // ELDER SIGNS EARNED, cumulative (user directive 2026-07-29). "count earning an ES mid
    // game as a reward as well, worth about 1.66 the reward for 1 doom." An ES earned this
    // turn can be spent/converted later, so a game-end snapshot of the current ES stock would
    // MISS signs that were earned and used. We therefore accumulate the UPWARD drift of the
    // faction's total elder-sign holding (unrevealed p.es + revealed p.revealed) between
    // observations — the same event-diff mechanism as gatesTaken/rituals — so every ES ever
    // earned is credited once, whether or not it survives to the end. Faction-agnostic.
    private val esEarned = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val lastEs   = mutable.Map[Faction, Int]()

    private def inner(m : mutable.Map[Faction, mutable.Map[Int, Int]], f : Faction) : mutable.Map[Int, Int] =
        m.getOrElseUpdate(f, mutable.Map[Int, Int]())

    /** Count of faction f's own units currently sitting in ANY faction's prison. */
    private def capturedCount(g : Game, f : Faction) : Int = {
        val prisons = factions.map(_.prison)
        g.players(f).units.%(u => prisons.contains(u.region)).num
    }

    /** Called at every genuine decision on the live game (before the move is chosen). */
    def observe(g : Game) : Unit = {
        val turn = g.turn
        factions.foreach { f =>
            val p = g.players(f)
            val gatesNow = p.allGates.num
            val capNow   = capturedCount(g, f)

            // Cumulative gate losses / captures: accumulate downward/upward drift.
            lastGates.get(f).foreach(prev => if (gatesNow < prev) gatesLost(f) = gatesLost(f) + (prev - gatesNow))
            lastCaptured.get(f).foreach(prev => if (capNow > prev) unitsCaptured(f) = unitsCaptured(f) + (capNow - prev))
            lastGates(f) = gatesNow
            lastCaptured(f) = capNow

            // Gate-EVENT classification (user directive 2026-07-28). Diff the CONTROLLED
            // region set, not just the count, so each change can be classified by WHERE it
            // happened and WHO holds the region now. `p.gates` is f's controlled-gate
            // regions (allGates also folds in unit-gate regions, which we don't want here —
            // control events are about the gate-token set). Each event fires once, the
            // instant it is first observed.
            val gatesSet : Set[Region] = g.players(f).gates.toList.toSet
            lastGateSet.get(f).foreach { prev =>
                // TAKEN: regions I control now but didn't before.
                val taken = gatesSet.diff(prev)
                if (taken.nonEmpty) gatesTaken(f) = gatesTaken(f) + taken.size
                // GONE: regions I controlled before but no longer. Classify each by who
                // holds it now — another faction controlling it = lost to an aggressor;
                // nobody controlling it = I abandoned it.
                val gone = prev.diff(gatesSet)
                gone.foreach { r =>
                    val enemyControls = factions.exists(e => e != f && g.players(e).gates.has(r))
                    if (enemyControls) gatesLostToEnemy(f) = gatesLostToEnemy(f) + 1
                    else               gatesAbandoned(f)   = gatesAbandoned(f)   + 1
                }
            }
            // DEFENDED: a gate I STILL control that WAS contested last time (an enemy unit
            // shared the region) and is no longer contested (the aggressor is gone) — I
            // held it against a threat. Also refresh which of my current gates are contested
            // now, for the next observation.
            val contestedNow : Set[Region] =
                gatesSet.filter(r => factions.exists(e => e != f && g.players(e).present(r)))
            val defended = contestedLast(f).intersect(gatesSet).diff(contestedNow)
            if (defended.nonEmpty) gatesDefended(f) = gatesDefended(f) + defended.size
            contestedLast(f) = contestedNow
            lastGateSet(f)   = gatesSet

            // Ritual-value credit: when this faction's ritual count rises since we last
            // looked, credit the FULL ritual yield it would earn now — gates held (the doom
            // it produces: doom = valid gates) PLUS Elder Signs from awakened GOOs (engine:
            // es = f.goos.factionGOOs.num, Game.scala ~2283). Credit per NEW ritual so two
            // rituals count twice. This is the direct "gold star for eating the vegetable".
            val ritNow = g.ritualHistory.count(_ == f)
            val ritPrev = lastRituals(f)
            if (ritNow > ritPrev) {
                val yield1 = gatesNow.toDouble + p.goos.factionGOOs.num.toDouble  // doom + ES per ritual
                ritualValue(f) = ritualValue(f) + (ritNow - ritPrev) * yield1
                // "Good ritual" pattern: 2+ gates held AND an awakened GOO on the map —
                // the convergence target. Credit per new ritual that meets the shape.
                if (gatesNow >= 2 && p.goos.factionGOOs.nonEmpty)
                    goodRituals(f) = goodRituals(f) + (ritNow - ritPrev)
            }
            lastRituals(f) = ritNow

            // ELDER SIGNS EARNED (user directive 2026-07-29): credit each NEW elder sign the
            // instant the faction's total sign holding rises, so signs earned then spent still
            // count. Total = unrevealed (p.es) + revealed (p.revealed). Event-diff, uncapped by
            // later spending — same as gatesTaken/ritualValue.
            val esNow = p.es.num + p.revealed.num
            lastEs.get(f).foreach(prev => if (esNow > prev) esEarned(f) = esEarned(f) + (esNow - prev))
            lastEs(f) = esNow

            // UNIT-LOSS pain (user directive 2026-07-29). Count this faction's units sitting
            // in its RESERVE pool, per class, and charge replacement cost for each NEW arrival
            // since the last observation. Reserve = killed/eliminated/sacrificed (Game.scala
            // eliminate -> region = reserve); prison (capture) and Deep (submerge) are NOT the
            // reserve, so this isolates true losses. Per-class diff so a loss-then-resummon
            // (unit leaves reserve) doesn't net the pain away — the loss already happened and
            // cost real tempo. Cost = uclass.cost (universal summon power). Shub's sacrificed
            // cultists land here too, so its true replacement cost accrues automatically.
            val poolByClass : Map[UnitClass, Int] =
                p.pool.toList.groupBy(_.uclass).map { case (uc, us) => uc -> us.size }
            lastPoolByClass.get(f).foreach { prev =>
                poolByClass.foreach { case (uc, n) =>
                    val was = prev.getOrElse(uc, 0)
                    if (n > was) unitLossCost(f) = unitLossCost(f) + (n - was) * uc.cost.toDouble
                }
            }
            lastPoolByClass(f) = poolByClass

            // GOO-AWAKEN turn: record the AP (turn) the faction's first own GOO reached the
            // map, so the awaken reward can be AP-scaled. factionGOOs = own-faction GOOs in play.
            if (gooAwakenTurn(f) < 0 && p.goos.factionGOOs.nonEmpty)
                gooAwakenTurn(f) = turn

            // Ability-use detection: books that ENTERED a cooldown set since last look.
            val nowCd : Set[Spellbook] =
                (p.oncePerGame ++ p.oncePerTurn ++ p.oncePerRound ++ p.oncePerAction ++ p.oncePerBattle).toList.toSet
            val newlyUsed = nowCd.diff(onCooldown(f))
            newlyUsed.foreach { sb =>
                if (f.library.contains(sb)) sbUses(f) = sbUses(f) + 1     // faction spellbook use
                else                        powerUses(f) = powerUses(f) + 1 // innate / GOO power use
            }
            onCooldown(f) = nowCd

            // Action-Phase snapshots only (doom phase excluded).
            if (!g.doomPhase) {
                val sp = inner(startPow, f)
                if (!sp.contains(turn)) sp(turn) = p.power   // first AP decision of this turn = "AP start"
                inner(endGates, f)(turn) = gatesNow          // overwrite every AP decision => last = "AP end"
                inner(endAband, f)(turn) = g.abandonedGates.num // board's abandoned gates, snapshotted at this AP's end
            } else {
                // Pre-doom bankroll: power at the FIRST doom-phase decision of this turn
                // (fresh gather-power has landed; ritual not yet paid). This is what the
                // brain should maximize so it can AFFORD the ritual.
                val pd = inner(preDoomPow, f)
                if (!pd.contains(turn)) pd(turn) = p.power
            }
        }
    }

    // ---- getters for the intermediate-metrics report (user directive 2026-07-27) ----
    // APs a faction actually ran = number of distinct turns it was observed acting in an
    // Action Phase (the engine has no per-faction AP counter). Per-AP start-power series
    // and the ES/gate snapshots feed the arena metrics block.
    def apCount(f : Faction) : Int = startPow.getOrElse(f, mutable.Map.empty).size
    def startPowSeries(f : Faction) : Seq[Int] =
        startPow.getOrElse(f, mutable.Map.empty).toList.sortBy(_._1).map(_._2)
    def avgStartPow(f : Faction) : Double = {
        val s = startPow.getOrElse(f, mutable.Map.empty).values
        if (s.isEmpty) 0.0 else s.map(_.toDouble).sum / s.size
    }
    /** Avg gates this faction CONTROLLED at each AP end — the doom-generation base. */
    def avgEndGates(f : Faction) : Double = {
        val s = endGates.getOrElse(f, mutable.Map.empty).values
        if (s.isEmpty) 0.0 else s.map(_.toDouble).sum / s.size
    }
    /** Avg gates left ABANDONED on the board at each AP end (each feeds every faction 1 power).
     *  Board-wide, not per-faction ownership; high = gates going uncontrolled. */
    def avgEndAband(f : Faction) : Double = {
        val s = endAband.getOrElse(f, mutable.Map.empty).values
        if (s.isEmpty) 0.0 else s.map(_.toDouble).sum / s.size
    }

    def sbUseCount(f : Faction)    : Int = sbUses(f)
    def powerUseCount(f : Faction) : Int = powerUses(f)
    def goodRitualCount(f : Faction) : Int = goodRituals(f)

    def gatesTakenCount(f : Faction)       : Int = gatesTaken(f)
    def gatesAbandonedCount(f : Faction)   : Int = gatesAbandoned(f)
    def gatesLostToEnemyCount(f : Faction) : Int = gatesLostToEnemy(f)
    def gatesDefendedCount(f : Faction)    : Int = gatesDefended(f)

    /** Total replacement-cost of every unit this faction lost (killed/eliminated/sacrificed). */
    def unitLossCostTotal(f : Faction) : Double = unitLossCost(f)
    /** Total Elder Signs this faction EARNED over the game (incl. ones later spent). */
    def esEarnedTotal(f : Faction) : Int = esEarned(f)
    /** Turn (AP proxy) the faction first awakened an own GOO, or -1 if never. */
    def gooAwakenAP(f : Faction) : Int = gooAwakenTurn(f)

    // AP-scaled multiplier for the GOO-awaken reward (user directive 2026-07-29): awakening
    // in AP2 is worth the most; AP1 ≈ AP3 (both below AP2); AP4-and-beyond a flat floor.
    // "some factions deviate, but a good broad rule to start with." Returns a [0,1] factor
    // applied to the ownGOO term; 0 if the GOO was never awakened.
    private def gooAwakenFactor(ap : Int) : Double = ap match {
        case n if n <= 0 => 0.0   // never awakened
        case 2           => 1.00  // highest-value window
        case 1 | 3       => 0.70  // below AP2, equal to each other
        case _           => 0.45  // AP4+ flat floor
    }

    /** Avg power a faction held ENTERING the doom phase (the pre-ritual bankroll). */
    def avgPreDoomPow(f : Faction) : Double = {
        val s = preDoomPow.getOrElse(f, mutable.Map.empty).values
        if (s.isEmpty) 0.0 else s.map(_.toDouble).sum / s.size
    }

    private def c01(x : Double) : Double = math.max(0.0, math.min(1.0, x))
    private def mean(xs : Iterable[Int], fallback : Double) : Double =
        if (xs.isEmpty) fallback else xs.map(_.toDouble).sum / xs.size

    // ─── REWARD SCALE (user directive 2026-07-29) ──────────────────────────────────
    // The whole score lives on the [0,1] value-net label scale. Every weight below is
    // written as "how many DOOM is this thing worth" × DoomUnit, so the table is a plain
    // doom-equivalent audit: 1 doom = 0.01, so a full-win-shaped game (≈30 doom + 6 SB)
    // lands at ≈0.30 + 0.48 = ~0.78 — the "0.80 range" the user anchored, and a near-win
    // lands in the same band. RESULTS (doom, spellbooks, elder signs) carry the dominant
    // weight; every BEHAVIOR term is ≤ ~1 doom and capped, so behavior fine-tunes play
    // toward the big signals but can NEVER overturn a single result (1 SB = 8 doom beats
    // any behavior). Non-winners are hard-capped below a win by Outcome.NonWinnerCeiling,
    // so it is impossible to score 1.0 without actually winning — a faction that "lucks"
    // into a strong losing finish still outscores a well-behaved poor finisher, because
    // that luck may be winning behavior we never hand-coded.
    private val DoomUnit = 0.01   // value of 1 doom on the [0,1] label scale

    /** Named contribution of every shaping component for one faction, on the [0,1] label
     *  scale, expressed in DOOM-EQUIVALENT points (× DoomUnit). Fully rescaled 2026-07-29
     *  to the user's anchors so the reward is a transparent, auditable, LINEAR combination
     *  where RESULTS dominate BEHAVIOR:
     *
     *    RESULTS (dominant, linear in the raw quantity):
     *      doomEarned  = 1.00 doom each   → realized doom, THE objective, counted ONCE here
     *                                        (the old end-game doom blend was removed).
     *      spellbooks  = 8.00 doom each   → a spellbook is worth ~8 doom (user's ratio); a
     *                                        single SB therefore beats ANY stack of behavior.
     *      elderSigns  = 1.66 doom each   → an ES EARNED mid-game (even if later spent).
     *      ritualValue = 1.00 doom / yield→ the doom a ritual actually produced (act credit).
     *
     *    BEHAVIOR (fine-tuning nudges, each ≤ ~1 doom, capped — steer toward the big signals
     *    and away from devastating losses, but never overturn a result):
     *      ownGOO 2 doom×AP-factor, goodRitual 1, gateTaken/gateDefended 1/3 doom each,
     *      endAP1Gates 1/3, preDoomPower up to 1, unitsOnMap up to 1, sbUse/powerUse 1/3.
     *
     *    PENALTIES (the "away from devastating losses" signal, subtract):
     *      unitLoss 0.10 doom per replacement-cost point (a 1-cost unit = 0.1 doom, a Shub
     *      round-trip of 10 = 1 doom), gate lost-to-enemy 1/2 doom, gate abandoned 1/3 doom
     *      (abandon ≥ gateTaken keeps the anti-farm invariant), capture 1/3 doom per unit.
     *
     *  Orderings the user required all hold arithmetically (behavior/ES equal):
     *    27D+6SB (27+48=75) > 30D+5SB (30+40=70);  27D+5SB (67) > 26D+5SB (66);
     *    equal D+SB but fewer unit losses ⇒ smaller penalty ⇒ higher;
     *    a lucky 6-SB second place (≥0.48) outscores a well-behaved poor finisher.
     *  The whole non-winner score is hard-capped below a win by Outcome.NonWinnerCeiling,
     *  so 1.0 is UNREACHABLE without actually winning. Values are SIGNED; the scorecard
     *  printer sorts by contribution. Sub-scores read RAW quantities (no ÷cap), so the
     *  doom-equivalent ratios stay exactly linear. */
    def scoreBreakdown(g : Game, f : Faction) : Seq[(String, Double)] = {
        val p  = g.players(f)
        val eg = endGates.getOrElse(f, mutable.Map.empty[Int, Int])
        def D(doomEquiv : Double) : Double = doomEquiv * DoomUnit   // doom-equivalents → label scale

        // ── RESULTS (raw quantities, linear) ──────────────────────────────────────────
        val doom       = p.doom.toDouble                       // realized doom (the objective)
        val sbooks     = p.spellbooks.num.toDouble             // spellbooks earned (≤6)
        val esE        = esEarnedTotal(f).toDouble             // elder signs EARNED (incl. spent)
        val ritY       = ritualValue(f)                        // cumulative ritual doom-yield
        // ── BEHAVIOR (bounded nudges) ───────────────────────────────────────────────
        val ownGoo     = if (p.goos.factionGOOs.nonEmpty) gooAwakenFactor(gooAwakenAP(f)) else 0.0  // [0,1]
        val goodRit    = math.min(2.0, goodRituals(f).toDouble)            // cap 2 rituals
        val takenN     = math.min(4.0, gatesTaken(f).toDouble)            // cap 4
        val defendedN  = math.min(4.0, gatesDefended(f).toDouble)         // cap 4
        val ap1Gates   = math.min(2.0, eg.getOrElse(1, 0).toDouble)       // cap 2
        val preDoom    = c01(avgPreDoomPow(f) / 10.0)                     // [0,1], target 10 power
        val onMap      = c01(p.allInPlay.num / 12.0)                      // [0,1], target 12 units
        val sbUse      = math.min(3.0, sbUses(f).toDouble)               // cap 3
        val powUse     = math.min(3.0, powerUses(f).toDouble)            // cap 3
        // ── PENALTIES (raw, subtract) ───────────────────────────────────────────────
        val lossPts    = unitLossCostTotal(f)                            // sum of replacement costs lost
        val abandonN   = math.min(6.0, gatesAbandoned(f).toDouble)       // cap 6
        val lostN      = math.min(6.0, gatesLostToEnemy(f).toDouble)     // cap 6
        val capturedN  = math.min(6.0, unitsCaptured(f).toDouble)        // cap 6

        // Each entry: label × DoomUnit. The comment states the DOOM-EQUIVALENT weight, what
        // is rewarded, and any condition. Positives sum, penalties subtract; the whole thing
        // is clamped to [0,1] in score() and then held below a win by NonWinnerCeiling.
        List(
            // ── RESULTS — dominant, counted once, linear ───────────────────────────────
            "spellbooks"   ->  D(8.00 * sbooks),    // 8 doom / SB. Hard win condition; a single SB outweighs all behavior.
            "doomEarned"   ->  D(1.00 * doom),      // 1 doom / doom. THE objective; realized doom from EVERY path. Doom is scored ONLY here.
            "elderSigns"   ->  D(1.66 * esE),       // 1.66 doom / ES EARNED (incl. later spent). The doom accelerant from GOOs.
            "ritualValue"  ->  D(1.00 * ritY),      // 1 doom / unit of ritual yield (gates+ES the ritual made). Act-of-ritualing credit.
            // ── BEHAVIOR — bounded nudges (steer toward big signals; can't overturn a result) ──
            "ownGOO"       ->  D(2.00 * ownGoo),    // ≤2 doom, ×AP factor (AP2=1.0, AP1=AP3=0.7, AP4+=0.45). Cond: own GOO in play. Enables ES+rituals.
            "b:goodRitual" ->  D(0.50 * goodRit),   // 0.5 doom / ritual (cap 2) done WHILE 2+ gates AND awakened GOO — the convergence shape.
            "gateTaken"    ->  D(0.33 * takenN),    // 1/3 doom / gate taken (cap 4). The ACT of taking a gate (build/capture).
            "gateDefended" ->  D(0.33 * defendedN), // 1/3 doom / gate defended (cap 4). Cond: held a contested gate; threat cleared.
            "endAP1Gates"  ->  D(0.33 * ap1Gates),  // 1/3 doom / AP1-end gate (cap 2). Rewards early tempo.
            "preDoomPower" ->  D(1.00 * preDoom),   // ≤1 doom. Avg power entering doom phase /10 — saving to AFFORD the ritual.
            "unitsOnMap"   ->  D(1.00 * onMap),     // ≤1 doom. Units in play /12. Passive board presence (means to an end).
            "b:sbUse"      ->  D(0.33 * sbUse),     // 1/3 doom / spellbook use (cap 3). Exploration nudge to TRY faction books.
            "b:powerUse"   ->  D(0.33 * powUse),    // 1/3 doom / power use (cap 3). Exploration nudge to TRY innate/GOO powers.
            // ── PENALTIES — the "away from devastating losses" signal (subtract) ────────
            "p:unitLoss"   -> -D(0.10 * lossPts),   // 0.10 doom per replacement-cost pt (1-cost unit=0.1, Shub round-trip 10=1 doom).
            "p:lostGate"   -> -D(0.50 * lostN),     // 1/2 doom / gate lost to an ENEMY (cap 6). Harshest gate event.
            "p:abandon"    -> -D(0.33 * abandonN),  // 1/3 doom / gate abandoned (cap 6). ≥ gateTaken ⇒ churning never nets positive.
            "p:capture"    -> -D(0.33 * capturedN)  // 1/3 doom / own unit captured to a prison (cap 6).
        )
    }

    /** Per-faction shaping score in [0,1] at game end: the doom-equivalent reward terms
     *  minus the penalties, clamped to [0,1] (single source of truth, so the reward and its
     *  reported breakdown can never drift apart). Held below a true win by NonWinnerCeiling
     *  in Outcome.valueShaped, so 1.0 is unreachable without actually winning. */
    def score(g : Game) : Map[Faction, Double] =
        factions.map(f => f -> c01(scoreBreakdown(g, f).map(_._2).sum)).toMap
}
