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

    // COST-SCALED capture pain + enemy-capture reward + GOO-loss extra (user reward-xlsx
    // 2026-08-04). Three new cost-weighted counters, all faction-agnostic:
    //   captureCost      — p:capture. Sum of the POWER REPLACEMENT COST of MY OWN units that
    //                      newly entered ANY prison since the last look (uncapped, user removed
    //                      the flat cap and made it scale by unit value — a captured Cthulhu (4)
    //                      hurts far more than a captured acolyte (1)). Prison ≠ reserve, so this
    //                      is distinct from unit-loss; a captured unit is alive but locked away.
    //   captureEnemyCost — z:slot07 b:captureEnemy. Mirror of the above for ENEMY units that
    //                      newly entered MY prison — reward for imprisoning, scaled by the value
    //                      of what I locked up.
    //   gooLostCount     — z:slot01 p:gooLost. Count of MY own GOOs that newly entered the
    //                      RESERVE (killed/eliminated) since the last look — an EXTRA flat pain
    //                      ON TOP of the per-cost unit-loss term, because losing an awakened GOO
    //                      is categorically worse than losing a cheap unit.
    private val captureCost        = mutable.Map[Faction, Double]().withDefaultValue(0.0)
    private val captureEnemyCost   = mutable.Map[Faction, Double]().withDefaultValue(0.0)
    private val gooLostCount       = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val lastMyPrisonByClass    = mutable.Map[Faction, Map[UnitClass, Int]]()
    private val lastEnemyPrisonByClass = mutable.Map[Faction, Map[UnitClass, Int]]()

    // CULTIST-UNPROTECTED penalty (z:slot02 p:cultistUnprot, user ruling 2026-08-04). A cultist
    // sitting in a region with NO same-faction monster/terror is "unprotected". Assessed ONCE
    // PER TURN (this file's AP proxy) at that turn's first observation, per cultist:
    //   • Suppressed ENTIRELY for a faction on any turn where every OTHER faction is out of power.
    //   • AP1/AP2 (turn ≤ 2): penalized ONLY if an enemy monster/terror is in the cultist's region.
    //   • AP3+ (turn ≥ 3): always penalized; a base "unit" of 1.0, escalating to 1.5 when an enemy
    //     monster/terror shares the region (so base −0.2 DEW → −0.3 DEW at the 0.20 sheet weight).
    //   • Consecutive penalized turns for the SAME cultist decay ×0.9 each turn (turn1 unit 1.0 →
    //     −0.2, turn2 0.9 → −0.18, …). Protected or unpenalized turn resets that cultist's streak.
    // unprotAccum is the summed doom-equiv-quantity (decay×escalation baked in); the sheet weight
    // 0.20 (=C18/2) multiplies it in scoreBreakdown, so editing the weight scales the whole term.
    private val unprotStreak   = mutable.Map[(Faction, UnitRef), Int]().withDefaultValue(0)
    private val unprotAccum    = mutable.Map[Faction, Double]().withDefaultValue(0.0)
    private val lastUnprotTurn = mutable.Map[Faction, Int]().withDefaultValue(-1)

    // KILL-ENEMY reward + enemy-GOO bonus (r:killEnemy z:slot03, r:enemyGooKill z:slot06,
    // user formula 2026-08-04). "Kills are always attributed to whoever did it" — the engine
    // gives us that: each faction's `battled` set is the regions it fought in this turn. So a
    // unit that DIES (newly enters its OWNER's reserve) in a region some OTHER faction battled
    // is that faction's kill. Value per the user's formula: 0.8 × the dead unit's power
    // replacement cost (80% of the value of summoning that much of your own), summed; then the
    // action cost is subtracted ONCE per (killer, turn, region) combat that produced a kill
    // (combat costs 1 power — user's "-1 power" example). enemyGooKill adds a bonus when the
    // dead unit is a GOO. lastUnitRegion tracks every unit's last-seen region so we know WHERE a
    // unit died; lastInReserve tracks who was already dead so we only fire on the transition.
    private val killEnemyValue  = mutable.Map[Faction, Double]().withDefaultValue(0.0)  // Σ 0.8×cost
    private val killActionCost  = mutable.Map[Faction, Double]().withDefaultValue(0.0)  // Σ 1 per combat-region
    private val enemyGooKills    = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val lastUnitRegion   = mutable.Map[UnitRef, Region]()
    private val lastInReserve     = mutable.Map[UnitRef, Boolean]().withDefaultValue(false)
    private val killChargedRegion = mutable.Set[(Faction, Int, Region)]()

    // AP-POWER-ORDER reward (r:apPowerOrder z:slot05, user formula 2026-08-04). AP3+ only. Reward
    // = (2·rank/playerCount − 1)·0.8, where rank is the ORDER a faction ran out of power during
    // that AP (1 = ran out first = worst; playerCount = ran out last / never = best). We record,
    // per (faction, turn), the sequence number at which the faction FIRST hit 0 power in that
    // turn's action phase; factions that never hit 0 rank last. Ranking is computed lazily at
    // score time (needs all factions' data for a turn). firstZeroSeq stores the order; zeroSeqCtr
    // is the per-turn running counter.
    private val firstZeroSeq   = mutable.Map[(Faction, Int), Int]()
    private val zeroSeqCtr     = mutable.Map[Int, Int]().withDefaultValue(0)
    private val apTurnsSeen    = mutable.Set[Int]()   // action-phase turns actually observed

    // POWER-BLOCK reward (r:powerBlock z:slot04). USER NOTE 2026-08-14 (xlsx row 22, column I):
    // "Make sure this applies to blocking other players from getting power too." PROVISIONAL
    // wiring: credit net power a faction GAINED across the game (a proxy for "gain power during
    // the AP / deny others"). Small coefficient (0.05 doom-equiv per power, cap 20). We sum
    // positive power deltas between observations (income), ignoring spends. User to finalize.
    private val powerGained    = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val lastPowerObs   = mutable.Map[Faction, Int]()

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
    // EMPTY-GATE BOOST (user ruling Q2 2026-08-04, dual-movement CONFIRMED). A +0.1 bonus ON TOP
    // of gateTaken when a faction OCCUPIES a previously-uncontrolled gate region under these
    // conditions, evaluated at the observation the take is first seen:
    //   • no ENEMY cultist occupying that gate region, AND no ENEMY GOO in the region → boost;
    //   • if an ENEMY Monster/Terror is in the region, the boost applies ONLY IF this faction
    //     ALSO has a Monster/Terror in the region at the same time (the "send a monster to
    //     protect" dual-move — observable as co-presence at the take);
    //   • ALL conditions void (no boost, ever) on any turn where every OTHER faction is out of power.
    private val emptyGateBoost   = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val gatesAbandoned   = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val gatesLostToEnemy = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val gatesDefended    = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val lastGateSet      = mutable.Map[Faction, Set[Region]]()
    // NEW 2026-08-14 (user reward-xlsx rows 26-27):
    //   r:buildGate: count gates BUILT when total controlled ≤ 3 (0.2 doom-equiv per build, cap at 3 controlled).
    //   r:EndAPGates: sum of gates held during EACH doom-phase observation (0.8 doom-equiv per gate-obs, no cap).
    private val gatesBuilt       = mutable.Map[Faction, Int]().withDefaultValue(0)
    private val doomPhaseGates   = mutable.Map[Faction, Double]().withDefaultValue(0.0)
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

            // COST-SCALED capture pain + enemy-capture reward (user reward-xlsx 2026-08-04).
            // Same event-diff mechanism as unit-loss, but keyed on PRISON regions and weighted
            // by each unit's power replacement cost (uc.cost) so value scales with what was
            // taken. Two views:
            //   MY units in ANY faction's prison  → p:capture pain (cost-weighted, uncapped).
            //   ENEMY units in MY prison          → z:slot07 b:captureEnemy reward.
            val allPrisons : Set[Region] = factions.map(_.prison).toList.toSet
            val myInPrison : Map[UnitClass, Int] =
                p.units.%(u => allPrisons.contains(u.region)).toList.groupBy(_.uclass).map { case (uc, us) => uc -> us.size }
            lastMyPrisonByClass.get(f).foreach { prev =>
                myInPrison.foreach { case (uc, n) =>
                    val was = prev.getOrElse(uc, 0)
                    if (n > was) captureCost(f) = captureCost(f) + (n - was) * uc.cost.toDouble
                }
            }
            lastMyPrisonByClass(f) = myInPrison
            // Enemy units sitting in THIS faction's own prison, per class.
            val enemyInMyPrison : Map[UnitClass, Int] =
                factions.filter(_ != f).flatMap(e => g.players(e).units.%(_.region == f.prison).toList)
                        .groupBy(_.uclass).map { case (uc, us) => uc -> us.size }
            lastEnemyPrisonByClass.get(f).foreach { prev =>
                enemyInMyPrison.foreach { case (uc, n) =>
                    val was = prev.getOrElse(uc, 0)
                    if (n > was) captureEnemyCost(f) = captureEnemyCost(f) + (n - was) * uc.cost.toDouble
                }
            }
            lastEnemyPrisonByClass(f) = enemyInMyPrison

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
                // r:buildGate NEW 2026-08-14 (xlsx row 26): count gates BUILT when total controlled ≤3.
                // "0.2 for building and controlling a new gate, capped at 3 controlled gates."
                // Each new gate taken counts IF total controlled gates at time of take is ≤ 3.
                if (taken.nonEmpty && gatesNow <= 3) {
                    val builtCount = math.min(taken.size, math.max(0, 3 - (gatesNow - taken.size)))
                    gatesBuilt(f) = gatesBuilt(f) + builtCount
                }
                // EMPTY-GATE BOOST (user Q2 ruling, dual-move confirmed). For each newly-taken
                // gate: void entirely if every OTHER faction is out of power. Else require no
                // enemy cultist AND no enemy GOO in the region; if an enemy Monster/Terror is
                // present, additionally require one of MY Monster/Terror co-present (the protect
                // dual-move). Count qualifying takes; the +0.1 bonus is applied in scoreBreakdown.
                if (taken.nonEmpty) {
                    val anyEnemyPower = factions.exists(e => e != f && g.players(e).power > 0)
                    if (anyEnemyPower) taken.foreach { r =>
                        val enemyCultistHere = factions.exists(e => e != f &&
                            g.players(e).units.exists(u => u.region == r && u.uclass.utype == Cultist))
                        val enemyGooHere = factions.exists(e => e != f &&
                            g.players(e).units.exists(u => u.region == r && u.uclass.utype == GOO))
                        val enemyMonsterHere = factions.exists(e => e != f &&
                            g.players(e).units.exists(u => u.region == r &&
                                (u.uclass.utype == Monster || u.uclass.utype == Terror)))
                        val iHaveMonsterHere = p.units.exists(u => u.region == r &&
                            (u.uclass.utype == Monster || u.uclass.utype == Terror))
                        val clean = !enemyCultistHere && !enemyGooHere
                        val ok = if (enemyMonsterHere) clean && iHaveMonsterHere else clean
                        if (ok) emptyGateBoost(f) = emptyGateBoost(f) + 1
                    }
                }
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
                    if (n > was) {
                        unitLossCost(f) = unitLossCost(f) + (n - was) * uc.cost.toDouble
                        // z:slot01 p:gooLost (user reward-xlsx 2026-08-04): a GOO newly entering
                        // reserve = an awakened GOO was killed/eliminated. Extra flat count ON TOP
                        // of the cost-weighted unit-loss pain above, since losing a GOO is
                        // categorically worse than losing a cheap piece.
                        if (uc.utype == GOO) gooLostCount(f) = gooLostCount(f) + (n - was)
                    }
                }
            }
            lastPoolByClass(f) = poolByClass

            // KILL-ENEMY attribution (r:killEnemy z:slot03 + r:enemyGooKill z:slot06, user
            // formula 2026-08-04: "kills are always attributed to whoever did it"). f is the
            // OWNER whose unit died; we credit the KILLER. A unit dies the instant it newly
            // enters its owner's RESERVE (p.pool) from the map. WHERE it died = its last on-map
            // region (lastUnitRegion). WHO killed it = every OTHER faction whose `battled` set
            // (regions it fought in this turn, engine-maintained) contains that region — in a
            // 1v1 combat the owner is excluded so only the opponent is credited; a 3-way melee
            // credits each attacker (accepted approximation). Value = 0.8 × the dead unit's
            // replacement cost (80% of the value of summoning that much yourself); a GOO also
            // bumps enemyGooKills. The combat action cost (user's "−1 power") is charged ONCE
            // per (killer, turn, region) so a single combat that kills three units still costs 1.
            // Reserve membership (not region==reserve) isolates true kills: captured units go to
            // a PRISON and submerged units to the Deep — neither is the pool — so neither counts.
            val nowReserveRefs : Set[UnitRef] = p.pool.map(_.ref).toList.toSet
            p.units.foreach { u =>
                val nowRes = nowReserveRefs.contains(u.ref)
                val wasRes = lastInReserve(u.ref)
                if (nowRes && !wasRes) {
                    lastUnitRegion.get(u.ref).filter(_.onMap).foreach { deathR =>
                        factions.foreach { e =>
                            if (e != f && g.players(e).battled.has(deathR)) {
                                killEnemyValue(e) = killEnemyValue(e) + 0.8 * u.uclass.cost.toDouble
                                if (u.uclass.utype == GOO) enemyGooKills(e) = enemyGooKills(e) + 1
                                val ck = (e, turn, deathR)
                                if (!killChargedRegion.contains(ck)) {
                                    killChargedRegion += ck
                                    killActionCost(e) = killActionCost(e) + 1.0
                                }
                            }
                        }
                    }
                }
                lastInReserve(u.ref) = nowRes
                if (u.region.onMap) lastUnitRegion(u.ref) = u.region
            }

            // POWER tracking for r:powerBlock (income) and r:apPowerOrder (exhaustion order).
            // powerGained: sum of POSITIVE power deltas between observations = income gained
            // (gather-power + ability gains); spends (negative deltas) are ignored. AP-power-out
            // ordering: during the ACTION PHASE only (not gather/doom), record the running order
            // in which each faction FIRST hits 0 power this turn — 1st to hit 0 = ran out first.
            lastPowerObs.get(f).foreach(prev => if (p.power > prev) powerGained(f) = powerGained(f) + (p.power - prev))
            lastPowerObs(f) = p.power
            if (!g.doomPhase && !g.gatherPowerPhase) {
                apTurnsSeen += turn
                if (p.power == 0 && !firstZeroSeq.contains((f, turn))) {
                    val seq = zeroSeqCtr(turn); zeroSeqCtr(turn) = seq + 1; firstZeroSeq((f, turn)) = seq
                }
            }

            // CULTIST-UNPROTECTED penalty (user ruling 2026-08-04). Assess ONCE per turn, at the
            // first observation of this turn for this faction. See field comment for the full rule.
            if (lastUnprotTurn(f) != turn) {
                lastUnprotTurn(f) = turn
                // Suppressed entirely if every OTHER faction is out of power this turn.
                val anyEnemyHasPower = factions.exists(e => e != f && g.players(e).power > 0)
                val myCultists = p.units.%(u => u.region.onMap && u.uclass.utype == Cultist).toList
                val stillUnprot = mutable.Set[(Faction, UnitRef)]()
                if (anyEnemyHasPower) {
                    myCultists.foreach { c =>
                        val r = c.region
                        val iProtect = p.units.exists(u => u.region == r &&
                                        (u.uclass.utype == Monster || u.uclass.utype == Terror))
                        if (!iProtect) {
                            val enemyThreat = factions.exists(e => e != f &&
                                g.players(e).units.exists(u => u.region == r &&
                                    (u.uclass.utype == Monster || u.uclass.utype == Terror)))
                            // AP1/AP2: only when an enemy monster/terror is in the region.
                            // AP3+: always; escalate ×1.5 when an enemy monster/terror is present.
                            val assess = if (turn <= 2) enemyThreat else true
                            if (assess) {
                                val key = (f, c.ref)
                                val streak = unprotStreak(key) + 1
                                unprotStreak(key) = streak
                                val decay  = math.pow(0.9, (streak - 1).toDouble)  // turn1 1.0, turn2 0.9, …
                                val escal  = if (turn >= 3 && enemyThreat) 1.5 else 1.0
                                unprotAccum(f) = unprotAccum(f) + decay * escal
                                stillUnprot += key
                            }
                        }
                    }
                }
                // Reset the streak for any of this faction's cultists NOT penalized this turn.
                unprotStreak.keys.filter(_._1 == f).filterNot(stillUnprot.contains).toList
                    .foreach(k => unprotStreak(k) = 0)
            }

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
                // r:EndAPGates NEW 2026-08-14 (xlsx row 27): accumulate gates held during doom phase.
                // "0.8 for each gate held during the doom phase, no cap."
                // Sum gates controlled at EACH doom-phase observation (not just first).
                doomPhaseGates(f) = doomPhaseGates(f) + gatesNow.toDouble
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
    /** Count of empty-gate takes that qualified for the +0.1 boost (user Q2 dual-move rule). */
    def emptyGateBoostCount(f : Faction)   : Int = emptyGateBoost(f)
    def gatesAbandonedCount(f : Faction)   : Int = gatesAbandoned(f)
    def gatesLostToEnemyCount(f : Faction) : Int = gatesLostToEnemy(f)
    def gatesDefendedCount(f : Faction)    : Int = gatesDefended(f)
    /** NEW 2026-08-14 r:buildGate: count of gates built when total controlled ≤ 3. */
    def gatesBuiltCount(f : Faction)       : Int = gatesBuilt(f)
    /** NEW 2026-08-14 r:EndAPGates: sum of gates held during ALL doom-phase observations. */
    def doomPhaseGatesTotal(f : Faction)   : Double = doomPhaseGates(f)

    /** Total replacement-cost of every unit this faction lost (killed/eliminated/sacrificed). */
    def unitLossCostTotal(f : Faction) : Double = unitLossCost(f)
    /** Total replacement-cost of this faction's OWN units captured to any prison (p:capture). */
    def captureCostTotal(f : Faction) : Double = captureCost(f)
    /** Total replacement-cost of ENEMY units this faction captured to its prison (b:captureEnemy). */
    def captureEnemyCostTotal(f : Faction) : Double = captureEnemyCost(f)
    /** Count of this faction's own GOOs killed/eliminated (p:gooLost, extra on top of unit-loss). */
    def gooLostTotal(f : Faction) : Int = gooLostCount(f)
    /** Decayed/escalated sum of unprotected-cultist turns (p:cultistUnprot quantity). */
    def cultistUnprotTotal(f : Faction) : Double = unprotAccum(f)
    /** Total Elder Signs this faction EARNED over the game (incl. ones later spent). */
    def esEarnedTotal(f : Faction) : Int = esEarned(f)
    /** Turn (AP proxy) the faction first awakened an own GOO, or -1 if never. */
    def gooAwakenAP(f : Faction) : Int = gooAwakenTurn(f)

    /** Net kill-enemy value for r:killEnemy: Σ 0.8×(dead enemy replacement cost) attributed to
     *  this faction, MINUS the once-per-combat action cost. Floored at 0 (a faction never scores
     *  NEGATIVE for killing — a lopsided trade just yields little; the loss side is p:unitLoss). */
    def killEnemyNet(f : Faction) : Double = math.max(0.0, killEnemyValue(f) - killActionCost(f))
    /** Count of enemy GOOs this faction killed/eliminated (r:enemyGooKill, additive to killEnemy). */
    def enemyGooKillCount(f : Faction) : Int = enemyGooKills(f)
    /** Net positive power income this faction gained over the game (r:powerBlock proxy). */
    def powerGainedTotal(f : Faction) : Int = powerGained(f)

    // AP-POWER-ORDER reward (r:apPowerOrder z:slot05, user formula 2026-08-04): AP3+ only.
    // For each action-phase turn ≥3 that we observed, rank the factions by the ORDER they ran
    // out of power (rank 1 = ran out FIRST = worst; a faction that never hit 0 ranks LAST, tied
    // at the top). reward per turn = (2·rank/playerCount − 1)·0.8, so with 4 players: last=+0.8,
    // 3rd=+0.4, 2nd=0, 1st=−0.4. Summed across AP3+ turns, then returned per faction. Computed
    // lazily here (needs all factions' exhaustion data for the turn). Faction-agnostic.
    def apPowerOrderTotal(f : Faction) : Double = {
        val pc = math.max(1, factions.size)
        apTurnsSeen.filter(_ >= 3).toList.map { t =>
            // Factions that hit 0 this turn, in order; those that never did rank after them.
            val zeroed = factions.filter(x => firstZeroSeq.contains((x, t))).sortBy(x => firstZeroSeq((x, t)))
            val never  = factions.filterNot(x => firstZeroSeq.contains((x, t)))
            val order  = zeroed.toList ++ never.toList   // index 0 = ran out first
            val idx    = order.indexOf(f)
            if (idx < 0) 0.0 else {
                val rank = idx + 1                        // 1-based; 1 = worst, playerCount = best
                (2.0 * rank / pc.toDouble - 1.0) * 0.8
            }
        }.sum
    }

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
    // HALVED 2026-08-04 (user directive: "CUT LITERALLY EVERY SCORE IN HALF … EXCEPT THE WIN").
    // Every shaping term is weight × DoomUnit, so halving this one constant (0.01 → 0.005) cuts
    // EVERY reward AND every pain in half, uniformly, in one stroke — a great LOSS now tops out
    // around ~0.39 instead of ~0.78, while the WIN label (Outcome.valueShaped = 1.0, set there,
    // NOT scaled by DoomUnit) is untouched. This replaces the old "big score then kneecap it with
    // the loss ceiling" mechanism (the ceiling is neutralized to 1.0 in Outcome): the low loss
    // score is now BUILT IN, not bolted on. Loss/win separation comes from the halved scores.
    private val DoomUnit = 0.005  // value of 1 doom on the [0,1] label scale (HALVED from 0.01)

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
        val takenN     = math.min(5.0, gatesTaken(f).toDouble)            // cap 5 (user xlsx 2026-08-04, was 4)
        val emptyBoostN= math.min(5.0, emptyGateBoost(f).toDouble)        // qualifying empty-gate takes, cap 5 (matches takenN cap)
        val defendedN  = math.min(4.0, gatesDefended(f).toDouble)         // cap 4
        val ap1Gates   = math.min(2.0, eg.getOrElse(1, 0).toDouble)       // cap 2
        val preDoom    = c01(avgPreDoomPow(f) / 10.0)                     // [0,1], target 10 power
        // unitsOnMap scaled to the faction's OWN total roster (user xlsx 2026-08-04: "made
        // scalable to faction"), replacing the flat /12. p.units = full roster (in-play +
        // reserve + prison); onMap = fraction of that roster physically in play.
        val rosterTot  = math.max(1, p.units.num)
        val onMap      = c01(p.allInPlay.num.toDouble / rosterTot.toDouble)  // [0,1], units-in-play / faction total
        val sbUse      = math.min(3.0, sbUses(f).toDouble)               // cap 3
        val powUse     = math.min(3.0, powerUses(f).toDouble)            // cap 3
        // ── PENALTIES (raw, subtract) ───────────────────────────────────────────────
        val lossPts    = unitLossCostTotal(f)                            // sum of replacement costs lost
        val abandonN   = math.min(6.0, gatesAbandoned(f).toDouble)       // cap 6
        val lostN      = math.min(6.0, gatesLostToEnemy(f).toDouble)     // cap 6
        val captPts    = captureCostTotal(f)                             // cost-weighted, UNCAPPED (user xlsx: cap removed)
        val captEnemy  = captureEnemyCostTotal(f)                        // enemy cost-weighted capture reward
        val gooLostN   = gooLostCount(f).toDouble                        // count of own GOOs killed
        val unprotQ    = cultistUnprotTotal(f)                           // decayed/escalated unprotected-cultist quantity
        // ── REWARDS from the four formerly-held slots (user formulas 2026-08-04) ────────
        val killNet    = killEnemyNet(f)                                 // Σ 0.8×dead-enemy-cost − combat action cost, ≥0
        val gooKillN   = enemyGooKillCount(f).toDouble                   // enemy GOOs killed (additive)
        val powGainN   = math.min(20.0, powerGainedTotal(f).toDouble)    // total power income, cap 20 (provisional)
        val apOrderPts = apPowerOrderTotal(f)                            // signed Σ (2·rank/players−1)·0.8 over AP3+
        // NEW 2026-08-14 (user reward-xlsx rows 26-27):
        val builtN     = gatesBuilt(f).toDouble                          // gates built when total controlled ≤ 3
        val doomGatesN = doomPhaseGates(f)                               // sum of gates held during ALL doom-phase observations

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
            "gateTaken"    ->  D(0.40 * takenN),    // 0.4 doom / gate taken (cap 5). USER-TUNED 2026-08-04 (was 0.33/cap4). The ACT of taking a gate.
            "b:emptyGate"  ->  D(0.10 * emptyBoostN),// z-slot: +0.1 doom / gate taken while UNCONTESTED (no enemy cultist/GOO; if enemy monster, only w/ my monster co-present — dual-move). Void if all enemies out of power. USER Q2 2026-08-04.
            "gateDefended" ->  D(0.33 * defendedN), // 1/3 doom / gate defended (cap 4). Cond: held a contested gate; threat cleared.
            "endAP1Gates"  ->  D(1.00 * ap1Gates),  // 1 doom / AP1-end gate (cap 2). USER-TUNED 2026-08-04 (was 0.33). Early tempo.
            "preDoomPower" ->  D(1.00 * preDoom),   // ≤1 doom. Avg power entering doom phase /10 — saving to AFFORD the ritual.
            "unitsOnMap"   ->  D(0.80 * onMap),     // ≤0.8 doom. USER-TUNED 2026-08-04 (was 1.00, /12): units-in-play / faction total roster.
            "b:sbUse"      ->  D(0.05 * sbUse),     // 0.05 doom / spellbook use (cap 3). USER-TUNED 2026-08-04 (was 0.33). Exploration nudge.
            "b:powerUse"   ->  D(0.05 * powUse),    // 0.05 doom / power use (cap 3). USER-TUNED 2026-08-04 via reward xlsx (was 0.33). Exploration nudge to TRY innate/GOO powers.
            // ── PENALTIES — the "away from devastating losses" signal (subtract) ────────
            "p:unitLoss"   -> -D(0.10 * lossPts),   // 0.10 doom per replacement-cost pt (1-cost unit=0.1, Shub round-trip 10=1 doom).
            "p:lostGate"   -> -D(0.60 * lostN),     // 0.6 doom / gate lost to an ENEMY (cap 6). USER-TUNED 2026-08-04 (was 0.50). Harshest gate event.
            "p:abandon"    -> -D(0.33 * abandonN),  // 1/3 doom / gate abandoned (cap 6). ≥ gateTaken ⇒ churning never nets positive.
            "p:capture"    -> -D(0.40 * captPts),   // 0.4 doom per replacement-cost pt of own units captured. USER-TUNED 2026-08-04 (was 0.33 flat/cap6): now cost-scaled + UNCAPPED.
            // ── RESERVED REWARD SLOTS (2026-08-02, user directive) ──────────────────────
            // A dozen pre-declared, ZERO-WEIGHT slots. Adding a new reward term changes the
            // shaping sum → shifts every training label → forces the warm-started value net to
            // re-learn its calibration (i.e. it "upsets the brain"). To make future rewards a
            // controlled, single-coefficient change instead of a restructuring of the vector,
            // these slots are reserved NOW at 0.0 — byte-identical to today (each contributes
            // exactly 0, so no current label moves), but a future reward just fills one slot's
            // multiplier. To activate slot N: replace its `D(0.0 * ...)` with the real term +
            // doom-equivalent weight, and (if it's a big signal) note the new calibration in the
            // scoreBreakdown header. Names are neutral so any faction-agnostic signal can claim one.
            "p:gooLost"      -> -D(0.20 * gooLostN),  // z:slot01. USER-DEFINED 2026-08-04: -0.2 per own GOO killed, EXTRA on top of p:unitLoss.
            "p:cultistUnprot"-> -D(0.20 * unprotQ),   // z:slot02. USER-DEFINED 2026-08-04: -0.2/turn per unprotected cultist, ×0.9 decay/turn, AP-gated, ×1.5 w/ enemy monster (AP3+), suppressed if all enemies out of power.
            "r:killEnemy"    ->  D(killNet),          // z:slot03. USER-DEFINED 2026-08-04: Σ 0.8×(dead enemy replacement cost) − 1 per combat (the "0.8×4−1=2.2 doom" formula). Attributed via `battled` region. Floored ≥0.
            "r:powerBlock"   ->  D(0.05 * powGainN),  // z:slot04. PROVISIONAL (user "still determining"): 0.05 doom per power of income gained, cap 20 income ⇒ ≤1 doom. Flagged for user to finalize.
            "r:apPowerOrder" ->  D(apOrderPts),       // z:slot05. USER-DEFINED 2026-08-04: Σ over AP3+ of (2·rank/players−1)·0.8 (4p: last=+0.8, 3rd=+0.4, 2nd=0, 1st=−0.4). rank = order power ran out (last-out best). SIGNED.
            "r:enemyGooKill" ->  D(0.20 * gooKillN),  // z:slot06. USER-DEFINED 2026-08-04: +0.2 per enemy GOO killed, ADDITIVE on top of r:killEnemy (mirrors p:gooLost −0.2). Attribution pairs w/ slot03.
            "b:captureEnemy" ->  D(0.40 * captEnemy), // z:slot07. USER-DEFINED 2026-08-04: +0.4 per replacement-cost pt of ENEMY units I imprison.
            "r:buildGate"    ->  D(0.20 * builtN),    // z:slot08. USER-DEFINED 2026-08-14 (xlsx row 26): 0.2 per gate built when total controlled ≤ 3.
            "r:EndAPGates"   ->  D(0.80 * doomGatesN),// z:slot09. USER-DEFINED 2026-08-14 (xlsx row 27): 0.8 per gate held during doom phase observations, no cap.
            "z:slot10"     ->  D(0.0),
            "z:slot11"     ->  D(0.0),
            "z:slot12"     ->  D(0.0)
        )
    }

    /** Per-faction shaping score in [0,1] at game end: the doom-equivalent reward terms
     *  minus the penalties, clamped to [0,1] (single source of truth, so the reward and its
     *  reported breakdown can never drift apart). Held below a true win by NonWinnerCeiling
     *  in Outcome.valueShaped, so 1.0 is unreachable without actually winning. */
    def score(g : Game) : Map[Faction, Double] =
        factions.map(f => f -> c01(scoreBreakdown(g, f).map(_._2).sum)).toMap
}
