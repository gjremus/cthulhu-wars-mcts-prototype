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

    /** Named contribution of every shaping component for one faction, in score order.
     *  EXPLICITLY WEIGHTED (rebalanced 2026-07-28): the earlier version weighted 11
     *  milestone terms equally (each 1/11), which let the EASY passive terms (start-power,
     *  units-on-map, spellbooks) dominate the score while the terms that actually WIN games
     *  — controlled gates and rituals — sat at the floor. Self-play then climbed the passive
     *  terms and abandoned gates ~5x the baseline (aband/AP 3.4 vs 0.65) with ~0 rituals.
     *  Fix: give gate-control + ritual terms the dominant weight, shrink the passive terms,
     *  and add the four gate-EVENT signals the user directed:
     *    + gateTaken    reward the ACT of taking a gate (build/control), anti-farm capped.
     *    + gateDefended reward holding a contested gate against an aggressor.
     *    - p:abandon    PENALTY for abandoning a gate (walked off / AbandonGateAction).
     *    - p:lostGate   PENALTY for losing a gate to an enemy (harder than abandon).
     *  Anti-farming: the per-gate ABANDON penalty >= the per-gate TAKE reward, so a
     *  take->abandon->retake cycle nets <= a single honest hold — the reward-driven analog
     *  of the free-action loop we blocked in the engine. Penalties SUBTRACT and score() then
     *  clamps to [0,1], so they can't drive the label negative but they do erase easy credit.
     *  Terms are returned with SIGNED values; the scorecard printer sorts by contribution. */
    def scoreBreakdown(g : Game, f : Faction) : Seq[(String, Double)] = {
        val p  = g.players(f)
        val sp = startPow.getOrElse(f, mutable.Map.empty[Int, Int])
        val eg = endGates.getOrElse(f, mutable.Map.empty[Int, Int])
        val powNow = p.power.toDouble

        // Normalized sub-scores in [0,1] (targets chosen so "good base-4 play" ~= 1.0).
        val sbook      = c01(p.spellbooks.num / 6.0)
        // DOOM EARNED (user directive 2026-07-29): doom is THE scoring objective (win at 30),
        // yet the shaping breakdown only rewarded its PROXIES (gates, rituals). Reward realized
        // doom directly, normalized to the 30 win threshold. This captures doom from EVERY path
        // (rituals, faction powers, Elder-Sign conversions), not just the gate/ritual proxies,
        // and it is the term the terminal label + Φ(s) already lean on — now present per-game too.
        val doomN      = c01(p.doom.toDouble / 30.0)
        // PRE-DOOM bankroll replaces the old across-AP start-power terms (which punished
        // ritualing). Target ~10 power entering doom = ritual cost (5) + a buffer to keep
        // playing. Rewarding this makes the brain SAVE for the ritual instead of hoarding.
        val preDoom    = c01(avgPreDoomPow(f) / 10.0)
        val ap1Gates   = c01(eg.getOrElse(1, 0) / 2.0)
        val avgGates   = c01(mean(eg.values, p.allGates.num.toDouble) / 2.0)   // cap-at-2 (per faction fingerprint)
        // ownGOO is now AP-SCALED (user directive 2026-07-29): awakened = 1.0 baseline, times
        // the AP factor (AP2 highest, AP1≈AP3, AP4+ flat). Rewards awakening in the strong
        // window, not just awakening at all.
        val ownGoo     = if (p.goos.factionGOOs.nonEmpty) gooAwakenFactor(gooAwakenAP(f)) else 0.0
        // UNIT-LOSS: total replacement cost lost this game, normalized. Target 20 ≈ a heavily
        // bleeding game (a Shub round-trip is 10; several Dark Young are 3 each). Penalized.
        val lossN      = c01(unitLossCostTotal(f) / 20.0)
        val eldSigns   = c01((p.es.num + p.revealed.num) / 8.0)
        val unitsKept  = 1.0 - c01(unitsCaptured(f) / 6.0)
        val onMap      = c01(p.allInPlay.num / 12.0)
        val ritVal     = c01(ritualValue(f) / 30.0)
        val goodRit    = c01(goodRituals(f) / 2.0)
        val sbUse       = c01(sbUses(f) / 3.0)
        val powUse      = c01(powerUses(f) / 3.0)
        // Gate events (cap at 4 over a game so a big-board spree can't dominate).
        val takenN     = c01(gatesTaken(f) / 4.0)
        val defendedN  = c01(gatesDefended(f) / 4.0)
        val abandonN   = c01(gatesAbandoned(f) / 4.0)
        val lostN      = c01(gatesLostToEnemy(f) / 4.0)

        // DOMINANT: the win-condition drivers get the lion's share of the weight.
        // Penalties SUBTRACT and score() clamps to [0,1]. Anti-farming is now STRICT:
        // p:abandon per-gate (0.20/4) is DOUBLE gateTaken per-gate (0.10/4), so a
        // take->abandon round-trip is net NEGATIVE — "take and hold" strictly beats
        // "take and drop". Power reward moved to pre-doom (see preDoom above), so
        // ritualing no longer costs the brain on the power term. GOO awakening boosted
        // (0.07->0.12) — it gates Elder Signs and the biggest rituals.
        // AUDIT NOTE (each term: WEIGHT × [0,1] sub-score; note says what is REWARDED + any
        // condition). Positives sum then penalties subtract, all clamped to [0,1] in score().
        // Anti-farm invariant: per-gate abandon penalty (0.20) ≥ per-gate take reward (0.10),
        // and lost-to-enemy (0.24) is harshest, so churning gates is never net-positive.
        List(
            // ── win-condition drivers (the lion's share of the weight) ──────────────────
            "doomEarned"   ->  0.18 * doomN,        // REWARD: realized doom / 30 (THE win objective). All doom paths, no condition.
            "avgEndGates"  ->  0.15 * avgGates,     // REWARD: avg AP-end controlled gates / 2 (the doom ENGINE). Proxy, trimmed since doomEarned now scores the result.
            "ritualValue"  ->  0.15 * ritVal,       // REWARD: cumulative ritual yield (gates+ES per ritual) / 30. The ACT that makes doom; fires when ritual count rises.
            "ownGOO"       ->  0.12 * ownGoo,       // REWARD: GOO awakened, ×AP factor (AP2=1.0, AP1=AP3=0.7, AP4+=0.45). Cond: own GOO in play. Enables ES + big rituals.
            "spellbooks"   ->  0.12 * sbook,        // REWARD: spellbooks / 6 (hard win condition — need all 6). No condition.
            "gateTaken"    ->  0.10 * takenN,       // REWARD: the ACT of taking a gate / 4 (capped). Fires on a region newly entering my control (build/capture).
            "b:goodRitual" ->  0.08 * goodRit,      // REWARD: rituals / 2 done WHILE holding 2+ gates AND an awakened GOO — the convergence shape. Cond: both hold at ritual time.
            "preDoomPower" ->  0.07 * preDoom,      // REWARD: avg power entering doom phase / 10 (~ritual cost + buffer). Rewards SAVING to afford the ritual, not hoarding.
            "gateDefended" ->  0.06 * defendedN,    // REWARD: gates defended / 4. Cond: a gate I held was contested by an enemy unit and the threat cleared while I kept control.
            "elderSigns"   ->  0.06 * eldSigns,     // REWARD: (Elder Signs + revealed) / 8. The doom accelerant from awakened GOOs.
            "endAP1Gates"  ->  0.05 * ap1Gates,     // REWARD: gates at END of AP1 / 2. Rewards gating up EARLY (tempo).
            "unitsKept"    ->  0.04 * unitsKept,    // REWARD: 1 − captured/6. Inverted: fewer units in enemy prisons = higher.
            "unitsOnMap"   ->  0.03 * onMap,        // REWARD: units in play / 12. Passive board presence (means to an end, small).
            "b:sbUse"      ->  0.02 * sbUse,        // NUDGE: spellbook uses / 3. Small exploration incentive to TRY faction books, not an objective.
            "b:powerUse"   ->  0.02 * powUse,       // NUDGE: innate/GOO power uses / 3. Small exploration incentive to TRY special powers.
            // ── PENALTIES (subtract) — event-detected, delivered in the game-end label ──
            "p:abandon"    -> -0.20 * abandonN,     // PENALTY: gates abandoned (walked off / AbandonGateAction, nobody controls after) / 4. 2× gateTaken (strict anti-farm).
            "p:lostGate"   -> -0.24 * lostN,        // PENALTY: gates lost to an ENEMY / 4 (an aggressor now controls it). Harshest — worse than abandon.
            "p:unitLoss"   -> -0.18 * lossN         // PENALTY: total replacement-cost of units killed/eliminated/sacrificed / 20. Scales pain by summon cost.
        )
    }

    /** Per-faction shaping score in [0,1] at game end: the weighted milestone/gate terms
     *  minus the gate penalties, clamped to [0,1] (single source of truth, so the reward
     *  and its reported breakdown can never drift apart). */
    def score(g : Game) : Map[Faction, Double] =
        factions.map(f => f -> c01(scoreBreakdown(g, f).map(_._2).sum)).toMap
}
