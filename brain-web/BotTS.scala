package cws

import hrf.colmat._

// Tombstalker (TS) BOT: AI evaluation logic for all TS-specific actions.
object BotTS extends BotX(implicit g => new GameEvaluationTS) {
    // Testing/tracking fields removed — see Backup/pre-cleanup-code/BotTS.scala for original
    // Round 9: traceWeights moved to Bot3.traceFaction (central, parameterized by faction).
}

class GameEvaluationTS(implicit game : Game) extends GameEvaluation(TS)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val unwrapped = a.unwrap
        result ++= evalMain(unwrapped)
        result ++= evalBattle(unwrapped)
        // Round 8 (FB): score CG/Eye Opens prompts that get asked of this faction
        result ++= fbPromptedEvals(a)

        result.none |=> 0 -> "#455 none"
        true |=> (math.random() * 4).round.toInt -> "#453 random"
        // Top-2 tracking now happens in BotX.askE after compareEL sort
        result.sortBy(v => -v.weight.abs)
    }

    def evalMain(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val dh = game.deathsHead
        val tombsOnMap  = self.all(TombHerd).num
        val tombsInPool = self.pool(TombHerd).num

        // Early game: prioritise Death's Head generation and establishing presence.
        // Late game: prioritise spellbooks, Glaaki, rituals, and doom.
        val earlyGame = numSB < 3 && !have(Glaaki)
        val lateGame  = !earlyGame
        val firstAP   = game.turn == 1
        val secondAP  = game.turn == 2

        // ── Power/doom tracking for stall-opportunism strategy ────────────
        val maxEnemyPower = if (others.any) others./(_.power).max else 0
        val enemiesExhausted = others.any && others.%(_.power <= 1).num >= others.num - 1
        val allEnemiesExhausted = others.any && others.forall(_.power <= 1)
        val realDoom = self.doom + self.es./(_.value).sum
        val tsTomePenalty = game.cursedTomesOwned.get(self).|(Nil).count { case (_, fd) => fd }
        val tsAprxDoom = self.doom + (self.es.num * 5 / 3) - tsTomePenalty
        def factionAprxDoom(f : Faction) : Int = {
            val tomePen = game.cursedTomesOwned.get(f).|(Nil).count { case (_, fd) => fd }
            f.doom + (f.es.num * 5 / 3) - tomePen
        }
        val leadingFactionDoom = if (others.any) others./(f => f.doom + f.es./(_.value).sum).max else 0
        val leadingFactionAprxDoom = if (others.any) others./(factionAprxDoom).max else 0
        val doomGap = leadingFactionDoom - realDoom
        // SBR PIVOT: when total estimated doom > 30, prioritize SBR completion
        val sbrPivot = !allSB && tsAprxDoom > 30 && have(Glaaki)
        val needsGOOBattle = need(TSGlaakiBattlesGOO)
        // FINAL AP: can win this doom phase if we grab enough gates/captures
        val potentialWinDoom = realDoom + self.gates.num + (dh / 2) + (have(Glaaki).?(1).|(0))
        val canWinThisTurn = allSB && potentialWinDoom >= 28  // within striking distance of 30
        // [2026-03-31 15:05] v1.18.030: DOOM SPRINT — near 30 doom but missing SBs, fight aggressively for remaining SBs
        val doomSprint = !allSB && realDoom >= 25 && numSB >= 4
        // Predicted power next AP: gates * 2 + cultists on map
        val predictedNextPower = self.gates.num * 2 + self.cultists.num

        // Ocean gate is the key bottleneck — Glaaki requires one to awaken
        val hasOceanGate = self.gates.%(_.glyph == Ocean).any
        val needsOceanGate = !have(Glaaki) && !hasOceanGate

        // ── Cross-faction threat detection ────────────────────────────────
        // GC: Dreams eats lone cultists; Submerge makes ocean areas dangerous
        val gcHasDreams    = GC.exists && GC.has(Dreams)
        val gcHasSubmerge  = GC.exists && GC.has(Submerge)
        val gcCthulhuUp    = GC.exists && GC.goos.any
        // AN: UnholyGround+Cathedral combos are lethal to GOOs
        val anHasUG        = AN.exists && AN.has(UnholyGround)
        // YS: Hastur and KingInYellow threaten lone cultists and unguarded gates
        val ysHasturUp     = YS.exists && YS.has(Hastur)
        val ysKIYUp        = YS.exists && YS.has(KingInYellow)
        // BG: ShubNiggurath devours lone cultists; Frenzy boosts combat dice
        val bgHasShub      = BG.exists && BG.has(ShubNiggurath)
        // CC: Nyarlathotep is extremely powerful and will target Glaaki
        val ccHasNya       = CC.exists && CC.has(Nyarlathotep)
        // WW: IceAge taxes power in WW areas; polar gate rush denies TS map space
        val wwHasIceAge    = WW.exists && WW.has(IceAge)
        // SL: Tsathoggua can rapidly reach undefended gates late game
        val slHasTsatho    = SL.exists && SL.has(Tsathoggua)
        // OW: DreadCurse+YogSothoth rolls extra dice vs Glaaki
        val owHasDreadCurse = OW.exists && OW.has(DreadCurse)
        val owHasYog        = OW.exists && OW.has(YogSothoth)
        // ── Faction-specific strategy selection ─────────────────────────────
        // From 3 dominant wins: strategy adapts based on which factions are present
        // SL present → bully SL immediately (0-combat units = zero risk)
        // BG present → target BG (weak combat in early game)
        // AN present → AN as "acolyte farm" late game (keeps recruiting, easy captures)
        // CC+YS together → let them fight each other, exploit the winner
        val slEarlyAggression = SL.exists && earlyGame && SL.gates.num <= 2 && !SL.goos.any
        val anAcolyteFarm = AN.exists && !AN.goos.any  // AN without GOO = easy capture target
        val ccYSWar = CC.exists && YS.exists  // CC and YS naturally fight each other
        // ES capture pivot: once TS has GreenDecay + Glaaki, shift from gate expansion
        // toward actively harvesting Elder Signs by capturing enemy cultists
        val esPivot = have(GreenDecay) && have(Glaaki) && (lateGame || numSB >= 4)

        // ── NEW STRATEGY: Simplified TS (Apr 2026) ──────────────────────────
        // [2026-04-01] NS: AP-phase-aware strategy
        val thirdAP = game.turn == 3
        val fourthAPPlus = game.turn >= 4
        // NS1/NS2: classify opponents as aggressive or passive
        val aggressiveFactions = $(SL, GC, CC, OW, BG).%(_.exists)
        val passiveFactions = $(YS, WW, AN).%(_.exists)
        // [2026-04-03] Ice Age, Cathedral, KIY threat detection
        val iceAgeRegions = if (wwHasIceAge) WW.gates.toSet else Set[Region]()
        val topAggressivePresent = $(SL, GC, CC, OW).%(_.exists).any
        // AN Cathedral danger: AN combat > 0 + cathedral + (AN has power/HP, or SL can spend 3)
        val slCanSpend3 = SL.exists && SL.power >= 3 && (!SL.has(Lethargy) || !SL.has(AncientSorcery))
        def noEnemyGOOOrIGOO(r : Region) : Boolean =
            others./~(_.at(r)).%(u => u.uclass.utype == GOO).none

        def anCathedralDanger(r : Region) : Boolean = {
            val anCombat = if (AN.exists) AN.strength(AN.at(r), self) else 0
            val hasCathedral = game.cathedrals.contains(r)
            val anThreatActive = AN.exists && (AN.power > 0 || AN.all(HighPriest).any) || slCanSpend3
            anCombat > 0 && hasCathedral && anThreatActive
        }
        val facingAggressive = aggressiveFactions.any
        // NS3: best gate for Glaaki = most adjacent undefended enemy gates
        val glaakiGateScore : Region => Int = (r : Region) => r.near.%(n => n.enemyGate && n.foes.goos.none).num
        // NS4: weakest faction = lowest power among those with gates
        val weakestGatedFaction = others.%(_.gates.any).sortBy(_.power).headOption
        // NS10: fortress gate = own gate with most TH (non-Glaaki)
        val fortressGate = self.gates.%(g => !g.allies.goos.any).sortBy(g => -self.at(g, TombHerd).num).headOption
        val hasFortress = fortressGate.exists(g => self.at(g, TombHerd).num >= 2)
        // NS12: Glaaki missing a monster companion
        val glaakiRegion = self.all(Glaaki).headOption.map(_.region)
        val glaakiHasDT = glaakiRegion.exists(r => self.at(r, DeepTendril).any)
        val glaakiHasTH = glaakiRegion.exists(r => self.at(r, TombHerd).any)
        val glaakiMissingMonster = have(Glaaki) && (!glaakiHasDT || !glaakiHasTH)
        // NS11: power advantage for stall
        val powerAdvantage = power >= maxEnemyPower + 2
        // [2026-04-02] NS: faction tier preference for gate targeting: SL>GC>CC>BG>OW>WW>YS>AN
        // [2026-04-02] NS: faction tier — 25 per step: SL=200, GC=175, CC=150, BG=125, OW=100, WW=75, YS=50, AN=25
        // Round 8 (FB): FB inserted between BG and OW (tier ~115) — Ghatanothoa is a strong
        // GOO and FB applies steady CG pressure, but starting in early game its threat is
        // closer to mid-tier. Without this entry FB fell into `case _ => 0` and TS's bot
        // gave no preference to attacking FB-held gates.
        def factionTierBonus(r : Region) : Int = {
            others.%(ef => r.gateOf(ef) || ef.at(r).any)./(f => f match {
                case SL => 200; case GC => 175; case DS => 175; case CC => 150; case BG => 125
                case OW => 100; case WW => 75; case YS => 50; case FB => 50; case AN => 25
                case _ => 0
            }).headOption.getOrElse(0)
        }

        // Round 9: FB-awareness negative scores (see OTHER_BOTS_FB_STRATEGY.md).
        // TS exception: movement into a crater region intended for Grasping Dead
        // capture is OK. Since GraspingDead dispatches as a battle rather than a
        // plain MoveAction, the crater avoidance on MoveAction does not affect it.
        a.unwrap match {
            case MoveAction(_, u, from, to, _) =>
                if (u.uclass == Glaaki && have(Undulate)) {
                    val combatMovers = 1 + self.at(from, DeepTendril).num + self.at(from, TombHerd).num
                    fbMultiMoveAvoidance(to, combatMovers.min(3)).foreach(e => true |=> e)
                } else {
                    fbMoveAvoidance(to).foreach(e => true |=> e)
                }
                // HARD RULE (user-reported): when TS has 1 power left in the AP,
                // do NOT move ANY unit off an own gate that has only 1 cultist.
                // Moving Glaaki/DT/TH leaves the cultist defenseless (enemy
                // capture next turn → gate abandoned). Moving the cultist itself
                // abandons the gate immediately. The same unit can't be
                // replaced on the same turn with 1 power (can't afford
                // movement + recruit + gate control adjustment).
                val fromOwnGateLow = from.ownGate && power <= 1 &&
                    self.at(from).%(_.cultist).num == 1 && from != to
                fromOwnGateLow |=> -100000 -> "HARD BLOCK: don't strip last cultist from own gate at 1 power"
            case BuildGateAction(_, r) =>
                hasFBCrater(r) |=> -8000 -> "cannot build gate on FB crater"
            case RecruitAction(_, _, r) =>
                hasFBCrater(r) |=> -5000 -> "avoid recruiting at FB crater"
            case SummonAction(_, _, r) =>
                hasFBCrater(r) |=> -5000 -> "avoid summoning at FB crater"
                (fbHasCG && isFBGazeRegion(r)) |=> -6000 -> "avoid summoning into FB gaze region"
            case TSUndulateCarryAction(_, u, from, to, _) =>
                // Count only Glaaki + DT + TH (not acolytes), max 3
                val combatMovers = 1 + self.at(from, DeepTendril).num + self.at(from, TombHerd).num
                fbMultiMoveAvoidance(to, combatMovers.min(3)).foreach(e => true |=> e)
                val fromOwnGateLow = from.ownGate && power <= 1 &&
                    self.at(from).%(_.cultist).num == 1 && from != to
                fromOwnGateLow |=> -100000 -> "HARD BLOCK: don't undulate off 1-cultist own gate at 1 power"
            case FBCyclopeanGazePainUnitAction(_, _, uRef, _, _, _, _) =>
                val u = game.unit(uRef)
                val onWater = u.region.glyph == Ocean
                (u.uclass == DeepTendril && !onWater) |=> 3000 -> "TS CG: pain Tendril on land"
                (u.uclass == TombHerd && onWater) |=> 3000 -> "TS CG: pain TH on water"
                (u.uclass == DeepTendril && onWater) |=> 1000 -> "TS CG: Tendril on water (less preferred)"
                (u.uclass == TombHerd && !onWater) |=> 1000 -> "TS CG: TH on land (less preferred)"
            case _ =>
        }

        a match {

            // ── SETUP ────────────────────────────────────────────────────────
            case StartingRegionAction(_, r) =>
                // [2026-04-01] NS1: start near aggressive factions to attack them
                // NS2: against passive factions, start near ocean for Glaaki
                // NS: vs GC, prefer LAND start (GC Submerge murders ocean dwellers)
                // [DEAD] GC.exists && !r.ocean |=> 3000 -> "#4 NS: vs GC, prefer land start"
                GC.exists && r.ocean |=> -1000 -> "#9 NS: vs GC, avoid ocean start"
                !GC.exists && r.ocean |=> 2000 -> "#6 start on ocean: glaaki-ready immediately"
                r.near.%(_.ocean).any |=> 1000 -> "#10 near ocean: can reach ocean gate round 1"
                // NS1: start adjacent to highest-priority aggressive faction
                SL.exists && r.near.%(n => SL.at(n).any).any |=> 5000 -> "#1 ALWAYS start near SL"
                GC.exists && !SL.exists && r.near.%(n => GC.at(n).any).any |=> 4000 -> "#2 NS1: start near GC"
                CC.exists && !SL.exists && !GC.exists && r.near.%(n => CC.at(n).any).any |=> 3500 -> "#3 NS1: start near CC"
                // [DEAD] OW.exists && aggressiveFactions.%(f => f != OW).none && r.near.%(n => OW.at(n).any).any |=> 3000 -> "#5 NS1: start near OW"
                // [2026-04-03] Placement near BG — only when no higher-priority aggressive factions
                BG.exists && !topAggressivePresent && r.near.%(n => BG.at(n).any).any |=> 2500 -> "NS: start near BG (no SL/GC/CC/OW)"
                BG.exists && topAggressivePresent && r.near.%(n => BG.at(n).any).any |=> -600 -> "#8 start near BG: capture risk"
                // Avoid crowded starts near enemy GOOs
                // [DEAD] r.near.%(n => others./~(_.at(n)).goos.any).any |=> -236 -> "#7 near enemy goo: risky start"

            case FirstPlayerAction(_, f) =>
                // When TH are on capture targets (Death March setup), go FIRST to capture before enemies escape
                val dmCaptures = areas.%(r => self.at(r, TombHerd).any && others.exists(e => e.at(r).cultists.any && e.at(r).monsterly.none && e.at(r).goos.none))
                f == self && dmCaptures.any && have(GreenDecay) |=> 3000 -> "#598 go first: DM captures with GD"
                f == self && dmCaptures.any |=> 1500 -> "#601 go first: DM capture targets available"
                f == self && allSB |=> 500 -> "#602 play first all sb"
                f == self |=> -50 -> "#604 stall"

            case PlayDirectionAction(_, order) =>
                val tsIdx = order.indexOf(self)
                // When captures are set up, go first. Otherwise prefer later.
                val dmCaptures = areas.%(r => self.at(r, TombHerd).any && others.exists(e => e.at(r).cultists.any && e.at(r).monsterly.none && e.at(r).goos.none))
                dmCaptures.any && tsIdx == 0 |=> 3000 -> "#599 go first for DM captures"
                // [2026-04-01 17:58] v1.18.061: R8 — put weakest player last, TS plays after strong players exhaust
                val weakestFaction = others.sortBy(_.power).headOption
                val weakestIdx = weakestFaction.map(order.indexOf(_)).getOrElse(-1)
                weakestIdx == order.num - 1 |=> 2000 -> "#600 R8: weakest player last"
                // Don't play last if we need to defend a threatened gate
                val threatenedGate = self.gates.%(r => others.%(_.at(r).%(_.active).any).any).any

            // ── SPELLBOOKS — user-defined priority order ─────────────────────
            case SpellbookAction(_, sb, _) => sb match {
                case Undulate =>
                    true |=> 1200 -> "#643 Undulate: always highest SB priority"
                case GreenDecay =>
                    true |=> 1000 -> "#644 Green Decay: primary doom engine"
                case ElevenRevelations =>
                    val noAdjCaptures = !glaakiRegion.exists(r => r.near.%(n => n.foes.cultists.any && n.foes.goos.none).any)
                    (noAdjCaptures || !have(Glaaki)) |=> 900 -> "#645 Eleven Rev: no adj captures or no Glaaki"
                    true |=> 800 -> "#649 Eleven Rev: base"
                case Hecatomb =>
                    have(Glaaki) && tombsOnMap >= 4 && dh >= 2 |=> 900 -> "#646 Hecatomb: Glaaki+TH+DH"
                    true |=> 600 -> "#651 Hecatomb: base"
                case GraspingDead =>
                    val lowRelativePower = others.any && power < others./(_.power).max
                    dh >= 2 && lowRelativePower |=> 850 -> "#648 Grasping Dead: DH+low power"
                    true |=> 500 -> "#652 Grasping Dead: base"
                case Oleaginous =>
                    val hasAggressiveGOOFaction = others.%(f => f.allSB && (f == CC || f == BG || f == GC)).any
                    hasAggressiveGOOFaction |=> 900 -> "#647 Oleaginous: CC/BG/GC has allSB"
                    true |=> 500 -> "#653 Oleaginous: base"
                case ElevenRevelations =>
                    // Worth taking when TS has assets to generate value from tomes
                    // Tome VII-VIII = DH × TombHerds; IX-X = ES; XI = doom at high ritual cost
                    // Bonus: other factions are likely to actually use the tomes (first tome = XI)
                    val likelyCandidates = others.%(f => f.power >= 1 && !ofinale(f))
                    // Last resort: take it if nothing else is available
                case _ =>
                    true |=> -1000 -> "#486 unknown spellbook"
            }

            // ── RITUAL ───────────────────────────────────────────────────────
            case RitualAction(_, cost, _) =>
                // HARD RULE: NEVER ritual in first doom phase — maximize strength, not doom early
                firstAP |=> -100000 -> "#59 NEVER ritual in first doom phase"
                // Don't ritual without Glaaki early
                !have(Glaaki) && numSB < 4 && !instantDeathNow |=> -50000 -> "#592 no ritual without Glaaki early"
                // CAP: max 2 rituals. Ritual cost tracks how many have been done.
                // 1st ritual = cost 5, 2nd = 6, 3rd = 7. At cost >= 7, we've ritualed twice already.
                cost >= 7 && !allSB && !instantDeathNow && !instantDeathNext |=> -50000 -> "#593 max 2 rituals: use ES captures instead"
                // Don't ritual early (turns 1-2) — build strength first
                game.turn <= 2 |=> -10000 -> "#518 too early to ritual, build strength"
                instantDeathNow |=> 10000 -> "#583 instant death now"
                instantDeathNext && allSB && others.all(!_.allSB) |=> 10000 -> "#582 ritual if ID next and all SB"

                instantDeathNext && !allSB && others.%(_.allSB).any |=> -1000 -> "#589 dont ritual if ID next, not all SB"
                instantDeathNext && !allSB && others.all(!_.allSB) && realDoom < others./(_.aprxDoom).max |=> 900 -> "#588 ritual so ID next and nobody wins"
                allSB && realDoom + maxDoomGain >= 30 |=> 900 -> "#309 can break 30, all SB"
                !allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "#509 will break 30, not all SB"
                !allSB && self.doom + self.gates.num < 30 && realDoom <= 29 && realDoom + maxDoomGain >= 29 |=> 700 -> "#342 wont break 30, come near"

                // SB requirement: ritual satisfies TSRitualOrEnemyGate
                need(TSRitualOrEnemyGate) && self.gates.num >= 1 && cost <= power |=> 780 -> "#336 ritual for sb requirement"
                // CONTEXTUAL RITUAL (from dominant win analysis):
                // Ritual ONLY when: 3+ gates AND Glaaki AND not at power disadvantage vs GOO factions
                val powerAfterCost = power - cost
                val gooEnemyPower = if (others.%(_.goos.any).any) others.%(_.goos.any)./(_.power).max else 0
                val ritualSafe = powerAfterCost >= gooEnemyPower - 3  // within 3 power of strongest GOO enemy
                val maxDoomFromRitual = self.gates.num + (have(Glaaki).?(1).|(0))
                // [2026-03-31 12:40] v1.18.025: RITUAL EFFICIENCY FILTER — SL/YS both use doomGain/cost > 0.75
                val ritualEfficiency = if (cost > 0) maxDoomFromRitual.toDouble / cost else 99.0
                // [2026-03-31 12:45] Threshold 0.5: allows 2-gate+Glaaki (3/5=0.6), blocks 1-gate rituals (1/5=0.2)
                ritualEfficiency < 0.5 && !allSB && !instantDeathNow && !need(TSRitualOrEnemyGate) |=> -8000 -> "#591 ritual efficiency < 0.5: terrible doom/power ratio"
                // [2026-04-01 16:15] v1.18.055: R2 — relative power check: don't ritual if it drops TS below max enemy power
                val powerAfterRitual = power - cost
                val maxEnemyPwr = if (others.any) others./(_.power).max else 0
                // [2026-04-01 16:20] Reduced from -6000 to -3000 (was too aggressive, Forf +2.6)
                powerAfterRitual < maxEnemyPwr && !allSB && !instantDeathNow |=> -3000 -> "#590 ritual drops below enemy power: conserve"
                // Win condition: allSB + can break 30
                allSB && realDoom + maxDoomFromRitual >= 30 |=> 100000 -> "#113 WINNING RITUAL: break 30 with allSB"
                allSB && realDoom >= 20 |=> 3000 -> "#175 allSB close to win: ritual"
                // Standard ritual: 3+ gates + Glaaki + safe power position
                have(Glaaki) && self.gates.num >= 3 && ritualSafe && cost <= power |=> 2000 -> "#205 3 gates + Glaaki + safe: ritual"
                have(Glaaki) && self.gates.num >= 2 && ritualSafe && cost <= power && numSB >= 4 |=> 1500 -> "#238 2 gates near-allSB: ritual"
                // Unsafe ritual: would leave TS weaker than GOO enemies
                !ritualSafe && !allSB |=> -3000 -> "#502 ritual would leave TS power-weak vs GOO enemies"
                // No Glaaki or < 2 gates: don't ritual (except SB requirement)
                !have(Glaaki) && !allSB |=> -5000 -> "#510 no Glaaki: dont ritual"
                self.gates.num < 2 && !allSB |=> -3000 -> "#503 < 2 gates: dont ritual"
                // Glaaki makes each ritual worth gates+1 extra doom — this IS the win condition
                // These must beat gate-rush scores (1800) once we already have 3 gates
                have(Glaaki) && allSB && self.gates.num >= 3 && cost <= power |=> 2800 -> "#194 glaaki allSB 3gates: ritual wins games"
                have(Glaaki) && allSB && self.gates.num >= 2 && cost <= power |=> 2400 -> "#203 glaaki allSB 2gates: ritual urgently"
                have(Glaaki) && numSB >= 4 && self.gates.num >= 3 && cost <= power |=> 2824 -> "#193 glaaki 4SB 3gates: ritual"
                have(Glaaki) && numSB >= 3 && self.gates.num >= 3 && cost <= power |=> 1800 -> "#227 glaaki 3SB 3gates: ritual"
                have(Glaaki) && self.gates.num >= 3 && cost <= power |=> 1400 -> "#253 glaaki+3gates huge doom"
                have(Glaaki) && self.gates.num >= 2 && cost <= power |=> 1100 -> "#278 glaaki+2gates big doom"
                have(Glaaki) && self.gates.num >= 1 && cost <= power |=> 700  -> "#343 glaaki ritual doom machine"
                true |=> -100 -> "#463 dont ritual unless have reasons"

            // ── HECATOMB RITUAL (DH supplement) ──────────────────────────────
            // Hecatomb ritual happens during doom phase using DH — power-efficient since
            // it preserves power for the next action phase.
            // KEY: DH unused by end of doom phase is zeroed by DoomDoneAction — spend it here.
            case TSHecatombRitualCostAction(_, ritPower, dhCost) =>
                // HARD RULE: never hecatomb in first doom phase
                firstAP |=> -100000 -> "#60 NEVER hecatomb in first doom phase"
                // HARD RULE: never hecatomb without Glaaki
                // DH WILL be zeroed — spending on Hecatomb is ALWAYS better than wasting
                // Only block if no gates (can't ritual without gates)
                self.gates.num == 0 && dh > 0 |=> -50000 -> "#521 no gates: cant hecatomb"
                instantDeathNow |=> 10000 -> "#583 instant death now"
                instantDeathNext && allSB |=> 8000 -> "#584 finale ritual with dh"
                allSB && realDoom + maxDoomGain >= 30 |=> 1500 -> "#239 can break 30"
                // [RARELY] !allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "#511 will break 30 not all SB"
                // Use Hecatomb to satisfy TSRitualOrEnemyGate SB requirement
                need(TSRitualOrEnemyGate) && self.gates.num >= 1 |=> 1500 -> "#240 hecatomb ritual for sb requirement"
                // Close to winning: fire aggressively
                allSB && realDoom >= 25 && self.gates.num >= 1 |=> 5000 -> "#142 allSB close to 30: hecatomb now"
                allSB && realDoom >= 20 && self.gates.num >= 1 |=> 1600 -> "#234 allSB 20 doom: hecatomb urgently"
                // [2026-04-01 09:45] v1.18.047: DOOM SPRINT HECATOMB — at doom >= 25, aggressively use Hecatomb
                doomSprint && dh >= 3 && self.gates.num >= 1 |=> 5000 -> "#585 doom sprint: hecatomb with DH to reach 30"
                // DH stockpiled: spend it now before DoomDoneAction zeroes it
                dh >= 6 && self.gates.num >= 1 |=> 3000 -> "#176 DH stockpiled 6+: spend via hecatomb"
                have(Glaaki) && dh >= 4 && self.gates.num >= 2 |=> 2607 -> "#196 glaaki up, DH ready: hecatomb"
                have(Glaaki) && dh >= 3 && self.gates.num >= 1 |=> 1800 -> "#228 glaaki up, some DH: hecatomb"
                dh >= 4 && self.gates.num >= 1 |=> 1600 -> "#235 DH stockpiled: spend via hecatomb"
                // General cases
                allSB && lateGame && self.gates.num >= 1 |=> 1400 -> "#254 late allSB hecatomb"
                numSB >= 3 && self.gates.num >= 2 |=> 1200 -> "#264 3 SB 2 gates hecatomb"
                numSB >= 2 && self.gates.num >= 2 |=> 800 -> "#321 2 SB 2 gates hecatomb"
                numSB >= 2 && self.gates.num >= 1 |=> 700 -> "#344 2 SB 1 gate hecatomb"
                numSB >= 1 && self.gates.num >= 2 |=> 650 -> "#357 1 SB 2 gates hecatomb"
                self.gates.num >= 1 |=> 550 -> "#371 1 gate hecatomb ritual"
                // POWER-AWARE HECATOMB: always do free/cheap ritual, skip if power-starving
                val powerAfterRitual = self.power - ritPower
                // FREE ritual (DH covers full cost): ALWAYS do it (+100000)
                ritPower == 0 && self.gates.num >= 1 |=> 100000 -> "#641 FREE hecatomb: always do it"
                // Cheap ritual (1-4 power): almost always, unless severely power-starved
                ritPower >= 1 && ritPower <= 4 && self.gates.num >= 1 && powerAfterRitual >= maxEnemyPower - 3 |=> 5000 -> "#642 cheap hecatomb: good power ratio"
                ritPower >= 1 && ritPower <= 4 && self.gates.num >= 1 && powerAfterRitual < 2 |=> -1000 -> "#487 cheap hecatomb but would power-starve"
                // Expensive ritual (5+ power): only with allSB and close to winning
                ritPower >= 5 && allSB && realDoom >= 20 |=> 3000 -> "#177 expensive hecatomb for win"
                ritPower >= 5 && !allSB |=> -2000 -> "#496 expensive hecatomb not worth it"
                // Always prefer more DH spending
                dhCost > 0 |=> dhCost * 200 -> "#425 prefer more dh spending"
                self.gates.num == 0 |=> -50000 -> "#521 no gates: cant hecatomb"

            // ── DOOM PHASE ───────────────────────────────────────────────────
            case DoomDoneAction(_) =>
                true |=> 933 -> "#625 doom done"

            case LoyaltyCardDoomAction(_) =>
                // [NU-TEST 2026-04-04] Very restrictive: 2 doom is huge for TS. Only with massive surplus.
                val hasNMCard = self.loyaltyCards.of[NeutralMonsterLoyaltyCard].any
                val hasIGOOD = self.loyaltyCards.of[IGOOLoyaltyCard].any
                hasNMCard |=> -100000 -> "NU: already have NM card"
                (hasNMCard || hasIGOOD) |=> -100000 -> "NU: already have neutral unit"
                self.doom < 5 |=> 4000 -> "NU: loyalty card early (low doom)"
                self.doom < 10 |=> 3000 -> "NU: loyalty card mid-game"
                true |=> 2000 -> "NU: loyalty card base"

            case SacrificeHighPriestDoomAction(_) =>
                // [NU-TEST 2026-04-04] HP sacrifice is free (PowerNeutral) — moderate scores
                val hpDoom    = self.all(HighPriest).num
                val gateHPD   = self.gates.%(r => self.at(r, HighPriest).any).num
                (hpDoom > gateHPD) |=> 1200 -> "NU: hp doom: spare HP not on gate"
                hpDoom > 1         |=> 1000 -> "NU: hp doom: multiple HPs"
                power <= 2         |=> 1500 -> "NU: hp doom: low power, +2 useful"
                true               |=> 1100 -> "#621 hp doom: sacrifices an HP unit"

            case TSDeathMarchDoomAction(_, _) =>
                // KEY: without Hecatomb, DeathMarch zeroes ALL remaining DH.
                // With Hecatomb, DoomDoneAction zeroes DH anyway — so DeathMarch is free to use.
                // Save DH only when working toward Glaaki and Hecatomb isn't protecting it.
                val oceanReachable = areas.%(_.ocean).%(r => self.all(Acolyte)./(_.region).%(_.connected.has(r)).any || self.at(r).any).any
                val shouldSaveDH = !have(Glaaki) && !have(Hecatomb) && (hasOceanGate || (dh >= 3 && oceanReachable))
                shouldSaveDH |=> 1000 -> "#623 save all dh: deathMarch zeros dh, need it for glaaki"
                // With Hecatomb: DH is spent via hecatomb ritual anyway — use Death March freely
                have(Hecatomb) && tombsInPool > 0 |=> 844 -> "#627 hecatomb protects DH: use death march freely"
                !shouldSaveDH |=> 300 -> "#634 death march"

            case TSDeathMarchAction(_, r) =>
                // [2026-04-03] Ice Age: penalty for Death March to ice age region
                iceAgeRegions.contains(r) |=> -200 -> "NS: Ice Age region, DM avoid"
                // no-bunching check — used by ALL DM scores
                val thAlreadyAtCapture = self.at(r, TombHerd).num > 0
                // NS14: DM capture focus — place 1 TH per lone cultist, no bunching
                val loneCultistHere = others./~(_.at(r)).cultists.any && others./~(_.at(r)).monsterly.none && others./~(_.at(r)).goos.none && others./~(_.at(r)).cultists.num == 1
                // Only place if no TH already here — 1 TH per capture target
                loneCultistHere && !thAlreadyAtCapture |=> 5500 -> "#607 NS14: DM onto lone cultist for capture"
                // If TH already there, this region is covered — send to fortress instead
                loneCultistHere && thAlreadyAtCapture |=> -500 -> "#637 NS14: already have TH here, don't bunch"
                // [2026-04-01] NS15: DM fortress power — pile TH into home base gate with cultists
                val fortressCandidate = r.ownGate && r.allies.cultists.num >= 3
                fortressCandidate |=> 5000 -> "#608 NS15: DM pile TH into fortress for power"
                // F1 — DM spread via positive scores
                val thAlreadyHere = self.at(r, TombHerd).num
                val undefendedOwnGate = r.ownGate && r.allies.cultists.any && thAlreadyHere == 0 && r.allies.goos.none
                undefendedOwnGate |=> 6000 -> "#609 DM: UNDEFENDED gate gets TH first (spread)"
                val hasEnemiesHere  = others./~(_.at(r)).any
                val undefendedCultistsHere = others./~(_.at(r)).cultists.any &&
                    others./~(_.at(r)).monsterly.none && others./~(_.at(r)).goos.none
                val weakGateHere = r.enemyGate && others./~(_.at(r)).monsterly.num <= 1 &&
                    others./~(_.at(r)).goos.none && others./~(_.at(r)).cultists.num >= 2
                // [2026-04-01] NS: ALL DM capture/battle scores check for existing TH — no bunching
                // Only place if no TH already here
                have(GreenDecay) && undefendedCultistsHere && !thAlreadyAtCapture |=> 3500 -> "#611 DM+GD: TH onto undefended cultists for ES"
                have(GreenDecay) && weakGateHere && !thAlreadyAtCapture |=> 3000 -> "#614 DM+GD: TH onto weak gate for battle+capture"
                anAcolyteFarm && AN.at(r).cultists.any && AN.at(r).monsterly.none && !thAlreadyAtCapture |=> 3000 -> "#615 DM: TH onto AN acolyte farm"
                val highCombatHere = others./(e => e.strength(e.at(r), self)).sum >= 3
                need(TSTombHerdKilled) && have(GraspingDead) && highCombatHere && tombsOnMap >= 3 && !thAlreadyAtCapture |=> 2000 -> "#618 DM sacrifice: TH into combat zone for TSTombHerdKilled SB"
                have(GraspingDead) && undefendedCultistsHere && !thAlreadyAtCapture |=> 3500 -> "#612 DM: TH for GD capture setup"
                have(GraspingDead) && hasEnemiesHere && others./~(_.at(r)).goos.none && !thAlreadyAtCapture |=> 2500 -> "#616 DM: TH for GD battle setup"
                have(GraspingDead) && hasEnemiesHere && !thAlreadyAtCapture |=> 1500 -> "#620 DM: TH near enemies for GD"
                // If TH already there, don't bunch
                thAlreadyAtCapture && hasEnemiesHere |=> -500 -> "#638 NS: DM already have TH here, send to fortress"
                // [2026-03-31 16:20] v1.18.037: GATE DEFENSE PRIORITY — boosted to match offensive. 40% gate loss AP1→AP2.
                r.ownGate && r.allies.cultists.any && r.allies.monsterly.none && r.foes.active.any |=> 4000 -> "#610 DM: protect threatened gate"
                r.ownGate && r.allies.cultists.num == 1 && r.allies.monsterly.none |=> 3500 -> "#613 DM: protect lone gate keeper"
                r.ownGate && r.allies.cultists.any |=> 500 -> "#631 DM: reinforce own gate"
                // [2026-04-01 19:10] v1.18.064: TC3 — pile TH on fortress gate (non-Glaaki gate with most TH)
                val isNonGlaakiGate = r.ownGate && !r.allies.goos.any && r.allies.cultists.any
                val existingTHHere = self.at(r, TombHerd).num
                // [2026-04-01 19:15] Reduced from 4000/3000 — was competing with GDCY capture at 3500
                isNonGlaakiGate && existingTHHere >= 2 |=> 2500 -> "#617 TC3: fortress gate, pile TH"
                isNonGlaakiGate && existingTHHere >= 1 |=> 1800 -> "#619 TC3: growing fortress gate"
                r.freeGate && r.allies.cultists.any |=> 75 -> "#636 free gate with cultist"
                // Enemy areas only if no gate to defend
                earlyGame && hasEnemiesHere |=> -500 -> "#639 early: place in enemy area"
                r.enemyGate && others./~(_.at(r)).monsterly.none && others./~(_.at(r)).goos.none |=> 1000 -> "#624 threaten weak enemy gate"
                earlyGame && r.near.%(n => others./~(_.at(n)).any).any |=> 500 -> "#632 early: place near enemies"
                r.near.%(_.ownGate).any |=> 500 -> "#633 near own gate"
                r.allies.any |=> 100 -> "#635 stack with allies"
                true |=> -1200 -> "#606 any placement"

            // ── MAIN PHASE BOOKKEEPING ────────────────────────────────────────
            case PassAction(_) =>
                // NEVER pass in AP1 — always spend power (bolster 2nd gate, recruit, TH)
                firstAP |=> -100000 -> "#76 AP1: NEVER pass"
                // Post-AP1: only pass if truly nothing useful to do
                power >= 2 |=> -5000 -> "#575 dont pass with 2+ power"
                power >= 1 |=> -2000 -> "#572 dont pass with power"
                true |=> -500 -> "#470 wasting power bad"

            case MoveDoneAction(_) =>
                // debug removed
                // [2026-04-01 16:52] v1.18.056: R3a — max 1 move per turn. MoveDone = stop after 1st move.
                // This scores higher than any 2nd movement action (max ~5000 for zero-risk).
                // Edge case: AP1 gate-building sequence may need 2 moves — handled by AP1 blocks.
                // [2026-04-01 19:00] v1.18.074: R3a exception — allow serial captures during STRIKE
                // [2026-04-02] R3a: always stop after 1 move. Serial captures use CaptureAction, not movement.
                true |=> 50000 -> "#117 R3a: stop after 1 move per turn"

            case EndTurnAction(_) =>
                // FINAL AP DETECTION: if realDoom + gates + DH ≈ 30, go all-in
                val potentialDoom = realDoom + self.gates.num + (dh / 2)
                allSB && potentialDoom >= 25 |=> -100000 -> "#526 FINAL AP: go all-in for win"
                // SL PATTERN: penalty for ending after battling (battles drain power for next AP)
                self.battled.any |=> -1000 -> "#571 battled: dont end turn, keep acting"
                // [2026-03-31 17:20] v1.18.041: Stronger end-turn penalties when power available
                !firstAP && have(Glaaki) && maxEnemyPower >= 3 && power >= 2 |=> -2000 -> "#55 stall: enemies still active, keep power"
                enemiesExhausted && power >= 4 |=> -3000 -> "#504 enemies exhausted: grab gates now"
                power >= 3 |=> -2000 -> "#498 dont end with 3+ power: recruit/capture/build"
                power >= 2 |=> -1500 -> "#491 dont end with 2+ power"
                true |=> -800 -> "#570 main done"

            case NeutralMonstersAction(_, lc) =>
                val hasNMCard = self.loyaltyCards.of[NeutralMonsterLoyaltyCard].any
                hasNMCard |=> -100000 -> "NU: already have NM card"
                true |=> 1000 -> "NU: nm base"
                // 2026-05-11 v6 boost: TS combat-focused with jitter on the top three
                // (Gug/Voonith/Gnorri) so all three rotate into actual picks across runs
                // instead of Gug winning every game. 1-NM-per-faction cap still enforced.
                (lc.unit == Gug)        |=> (1900 + (math.random() * 300).toInt) -> "NU: gug combat"
                (lc.unit == Voonith)    |=> (1800 + (math.random() * 300).toInt) -> "NU: voonith combat"
                (lc.unit == Gnorri)     |=> (1700 + (math.random() * 300).toInt) -> "NU: gnorri summons"
                (lc.unit == Ghast)      |=> 1500 -> "NU: ghast"
                (lc.unit == DimensionalShamblerUnit) |=> 1400 -> "NU: shambler"
                (lc.unit == StarVampire) |=> 1300 -> "NU: star vampire"
                (lc.unit == Shantak)    |=> 1200 -> "NU: shantak"

            case SacrificeHighPriestMainAction(_) =>
                // [NU-TEST 2026-04-04] HP main sacrifice: costs 1 power, gains +2 (net +1). Moderate.
                val hpMain  = self.all(HighPriest).num
                val gateHPM = self.gates.%(r => self.at(r, HighPriest).any).num
                (hpMain > gateHPM) |=> 700 -> "NU: hp main: spare HP not guarding gate"
                hpMain > 1         |=> 500 -> "NU: hp main: multiple HPs"
                power <= 1         |=> 800 -> "NU: hp main: low power rescue"
                enemiesExhausted   |=> 1500 -> "NU: hp main: enemies out, safe to use"
                true               |=> 1300 -> "#260 hp main: sacrifices an HP unit"

            case SacrificeHighPriestAction(_, r, _) =>
                // Choose which HP to sacrifice — prefer non-gate HPs
                val acoBackup = self.at(r, Acolyte).any
                r.ownGate && !acoBackup |=> 1000 -> "#288 hp: sole gate keeper, risky"
                r.ownGate && acoBackup  |=>  100 -> "#440 hp: gate has acolyte backup"
                r.ownGate.not           |=>  700 -> "#346 hp: not at gate, safe to sacrifice"
                true                    |=>  100 -> "#441 hp: any"

            case IndependentGOOMainAction(_, lc, _) =>
                val igooOnMap = self.allInPlay.%(_.uclass.isInstanceOf[IGOO]).num
                val remaining = self.power - lc.power
                // Max 1 iGOO, except Y'Golonac is always allowed
                (igooOnMap >= 1 && lc != YgolonacCard) |=> -100000 -> "NU: max 1 iGOO (except Ygolonac)"
                remaining < 0  |=> -99999 -> "NU: cannot afford"
                !have(Glaaki) |=> -5000 -> "NU: awaken Glaaki first"
                self.gates.num < 2 |=> -3000 -> "NU: need 2+ gates first"
                remaining >= 3 |=> 2500 -> "NU: igoo comfortable power"
                remaining >= 1 |=> 2000 -> "NU: igoo affordable"
                (lc == TulzschaCard) |=> 7000 -> "NU: tulzscha top priority"
                (lc == ByatisCard)   |=> 5500 -> "NU: byatis defender"
                (lc == YgolonacCard) |=> 5000 -> "NU: ygolonac kills"
                (lc == NyogthaCard)  |=> 4000 -> "NU: nyogtha"
                (lc == DaolothCard)  |=> -3000 -> "NU: daoloth not useful"
                (lc == AbhothCard)   |=> -3000 -> "NU: abhoth not useful"
                true                 |=> 2000 -> "NU: igoo base"

            case IndependentGOOAction(_, lc, r, _) =>
                // Choose where to place the iGOO
                r.ownGate && r.allies.cultists.any |=> 621 -> "#358 igoo place: own gate with cultists"
                r.ownGate                          |=> 500 -> "#376 igoo place: own gate"
                r.allies.monsterly.any             |=> 600 -> "#360 igoo place: with other monsters"
                r.allies.cultists.any              |=> 832 -> "#319 igoo place: with cultists"
                // Byatis can't move — placement is permanent, prioritize own gate
                (lc.unit == Byatis) && r.ownGate  |=> 851 -> "#318 byatis: at own gate (immobile anchor)"
                (lc.unit == Byatis) && !r.ownGate |=> 800 -> "#322 byatis: avoid non-gate (cant move)"
                true |=> 500 -> "#377 igoo place: any valid region"

            // ── NEUTRAL UNIT: delegated to evalNeutralUnit ─────────────────────
            case _ if evalNeutralUnit(a).nonEmpty =>
                result ++= evalNeutralUnit(a)

            // ── MOVEMENT ─────────────────────────────────────────────────────
            case MoveAction(_, u, o, d, _) if u.uclass == Glaaki =>
                // GLAAKI ALONE: move to adjacent region with TS units to regroup
                val glaakiSolo = self.at(o).num == 1
                val destHasTSUnits = self.at(d, DeepTendril).num > 1 ||
                    self.at(d, TombHerd).num > 1 || self.at(d).cultists.num > 1
                (glaakiSolo && destHasTSUnits) |=> 8000 -> "TS: Glaaki alone, regroup with TS units"

                // GOO/iGOO THREAT: if enemy GOO or iGOO in TS controlled gate,
                // and Glaaki is adjacent and NOT on a controlled gate, Undulate to threatened gate
                val threatenedTSGates = self.gates.%(g =>
                    others./~(_.at(g)).%(u => u.uclass.utype == GOO).any)
                val glaakiAdjacentToThreat = !o.ownGate && threatenedTSGates.%(g =>
                    game.board.connected(o).has(g)).any
                (glaakiAdjacentToThreat && threatenedTSGates.has(d)) |=> 5000 -> "TS: Glaaki to threatened gate (GOO/iGOO)"

                // STRIKE: at 2 power drop to 6000. Also: if Glaaki already at capturable gate, don't move — capture first
                val glaakiOnCapturableGate = o.foes.cultists.any && o.foes.goos.none && o.enemyGate
                val strikeBase = if (power <= 2) 6000 else 9000
                val strikeGrab = if (power <= 2) 5000 else 8000
                // If already on capturable gate, movement drops to 0 — capture scores will win
                allEnemiesExhausted && glaakiOnCapturableGate && !d.ownGate |=> 0 -> "#456 STRIKE: already at capturable gate, capture first"
                allEnemiesExhausted && have(GreenDecay) && d.foes.cultists.any && d.foes.goos.none |=> strikeBase -> "#457 STRIKE: enemies out, GD capture"
                allEnemiesExhausted && d.enemyGate && d.foes.cultists.num <= 1 && d.foes.monsterly.none && d.foes.goos.none |=> strikeGrab -> "#458 STRIKE: enemies out, grab gate"
                // MID-AP OPPORTUNISM: grab gates when OWNER is out of power/inactive (even if others still active)
                val ownerExhausted = d.enemyGate && others.%(ef => d.gateOf(ef) && (ef.power <= 1 || !ef.active)).any
                ownerExhausted && d.foes.cultists.num <= 1 && d.foes.monsterly.none && d.foes.goos.none |=> 5000 -> "#143 opportunistic: gate owner exhausted"
                // GD capture when target faction is out of power (can't block)
                have(GreenDecay) && d.foes.cultists.any && d.foes.goos.none && others.%(e => e.at(d).any && e.power <= 1).any |=> 4000 -> "#160 opportunistic: GD capture, owner weak"
                // [2026-04-01 19:22] v1.18.065: TC4 — Undulate roaming when fortress exists
                // Only block Glaaki movement when NO fortress gate exists (< 2 TH on any non-Glaaki gate)
                val fortressExists = self.gates.%(g => !g.allies.goos.any && self.at(g, TombHerd).num >= 2).any
                // [2026-04-01 19:50] Exception: allow roaming without fortress when high-value capture available
                val highValueCapture = d.enemyGate && d.foes.cultists.any && d.foes.goos.none &&
                    others.%(ef => d.gateOf(ef) && (ef.power <= 1 || !ef.active)).any && have(GreenDecay)
                // [2026-04-02] NS: fortress check does NOT apply in AP2 — Glaaki must raid in AP2
                have(Undulate) && !secondAP && self.at(o).%(m => m.is(DeepTendril) || m.is(TombHerd)).any && !fortressExists && !highValueCapture |=> -100000 -> "#88 Glaaki: no fortress yet, stay unless high-value target"
                // HARD RULE: Glaaki should NEVER leave a controlled gate to go to a non-gate area
                // Glaaki protects gates. Moving to enemy territory without a gate to claim is wasteful.
                val glaakiGOOTarget = need(TSGlaakiBattlesGOO) && d.foes.goos.any
                val glaakiCaptureTarget = have(GreenDecay) && d.foes.cultists.any && d.foes.goos.none
                val glaakiHitAndRun = have(Oleaginous) && have(GreenDecay) && d.foes.cultists.any && d.foes.goos.none && o.ownGate && o.allies.cultists.num >= 3
                // When Oleaginous is available: Glaaki can "hit and run" — capture then retreat back
                glaakiHitAndRun |=> 3000 -> "#554 Glaaki hit-and-run: capture + oleaginous retreat"
                // Gate-leaving penalty: reduced when GD is active and capturable enemies exist ANYWHERE
                val captureTargetsExist = have(GreenDecay) && others.exists(e => e.cultists.any)
                // With GD + targets: mild penalty (allow roaming for captures)
                // [2026-04-01 16:20] REMOVED: -1000 "mild gate preference" — ineffective under compareEL (dominated by all capture scores)
                // Without GD or no targets: stronger penalty
                !captureTargetsExist && o.ownGate && !d.ownGate && !d.freeGate && !glaakiGOOTarget && !glaakiCaptureTarget && !glaakiHitAndRun && !(d.enemyGate && d.foes.cultists.num <= 1 && d.foes.monsterly.none && d.foes.goos.none) |=> -5000 -> "#512 Glaaki: prefer staying at gate"
                // Keep hard block ONLY in AP1-2 (early game gate protection is critical)
                firstAP && o.ownGate && !d.ownGate |=> -100000 -> "#61 AP1: Glaaki stays at gate"
                // ── Faction threat avoidance ───────────────────────────────────
                // AN: Cathedral+UnholyGround — extra dangerous for GOOs; avoid unless dominant
                anHasUG && game.cathedrals.contains(d) && d.allies.monsterly.none |=> -400 -> "#467 AN cathedral+UG: glaaki alone is extra vulnerable"
                anHasUG && game.cathedrals.contains(d) |=> -475 -> "#469 AN cathedral+UG: glaaki prefers not to engage"
                // CC: Nyarlathotep — very powerful, will target Glaaki
                // [2026-03-31 16:00] v1.18.035: Active-aware GOO threats — only penalize when faction is active
                ccHasNya && d.foes(Nyarlathotep).any && CC.active && d.allies.monsterly.none |=> -900 -> "#481 nyarlathotep ACTIVE: glaaki avoids without support"
                ccHasNya && d.foes(Nyarlathotep).any && !CC.active && d.foes.cultists.any |=> 2000 -> "#206 nyarlathotep INACTIVE: safe to capture nearby"
                ccHasNya && d.foes(Nyarlathotep).any |=> 180 -> "#434 nyarlathotep: glaaki cautious"
                // OW: DreadCurse+YogSothoth — rolls extra dice vs Glaaki in combat
                owHasDreadCurse && owHasYog && d.foes(YogSothoth).any && d.allies.monsterly.none |=> 800 -> "#323 OW dread curse: glaaki alone vs yog risky"
                // GC: Submerge — ocean areas very exposed to submerging Cthulhu
                gcHasSubmerge && gcCthulhuUp && d.ocean && d.allies.monsterly.none && d.allies.cultists.num <= 1 |=> 600 -> "#361 GC submerge: ocean area exposed"
                // [2026-03-31 15:08] v1.18.030: DOOM SPRINT — Glaaki fights aggressively for remaining SBs
                // [2026-03-31 15:15] Reduced from 8000/15000 — too aggressive, Glaaki abandoned gates
                doomSprint && d.foes.any && d.foes.goos.none |=> 3000 -> "#587 DOOM SPRINT: Glaaki seeks fights for SBs"
                doomSprint && d.foes.goos.any |=> 5000 -> "#586 DOOM SPRINT: Glaaki to GOO for SBs"
                // ── SB-seeking: Glaaki actively hunts GOOs for TSGlaakiBattlesGOO ──
                val needGOOFight = need(TSGlaakiBattlesGOO)
                // Move toward enemy GOOs when we need the SB
                // When TSGlaakiBattlesGOO is the LAST missing SB, it's worth EVERYTHING
                needGOOFight && numSB >= 5 && d.foes.goos.any |=> 10000 -> "#123 LAST SB: glaaki MUST fight GOO"
                // [2026-03-31 17:40] v1.18.042: GOO BATTLE ENDGAME — at doom >= 28 (incl ES), seek GOO fight for last SBs
                // Prefer exhausted/ocean/low-mobility GOOs. Full Undulate cohort = 9-10 dice.
                val totalDoom = realDoom  // already includes ES values
                val gooEndgame = !allSB && totalDoom >= 28 && needGOOFight
                val hasCohort = d.allies.%(_.is(DeepTendril)).any || d.allies.%(_.is(TombHerd)).any
                gooEndgame && d.foes.goos.any && hasCohort |=> 15000 -> "#121 GOO ENDGAME: fight GOO with cohort near 30 doom"
                gooEndgame && d.foes.goos.any |=> 8000 -> "#127 GOO ENDGAME: fight GOO alone near 30 doom"
                // Also prefer GOOs of exhausted factions
                val exhaustedGOO = d.foes.goos.any && others.%(ef => ef.at(d).goos.any && ef.power <= 1).any
                gooEndgame && exhaustedGOO |=> 5000 -> "#144 GOO ENDGAME: target exhausted faction GOO"
                needGOOFight && d.foes.goos.any && d.allies.%(_.is(DeepTendril)).any |=> 3000 -> "#178 SB: glaaki to enemy GOO with DT"
                needGOOFight && d.foes.goos.any |=> 2000 -> "#207 SB: glaaki to enemy GOO for SB"
                needGOOFight && d.near.%(n => n.foes.goos.any).any |=> 1000 -> "#289 SB: glaaki near enemy GOO"
                // [2026-03-31 15:30] v1.18.032: EARLY SB HUNT — when 3+ SBs, boost movement toward winnable fights for remaining combat SBs
                val needCombatSB = need(TSRollKill) || need(TSRoll3Pains) || need(TSTombHerdKilled)
                val earlyHunt = needCombatSB && numSB >= 3 && have(Glaaki) && d.foes.any && d.foes.goos.none
                earlyHunt && d.allies.%(_.is(TombHerd)).any |=> 3000 -> "#179 SB hunt: Glaaki to fight with TH for combat SBs"
                earlyHunt && d.allies.%(_.is(DeepTendril)).any |=> 2500 -> "#197 SB hunt: Glaaki to fight with DT for combat SBs"
                // SBR PIVOT: aprxDoom > 30, hunt nearest GOO for Glaaki battles GOO SBR
                sbrPivot && needsGOOBattle && d.foes.goos.any |=> 11000 -> "SBR PIVOT: Glaaki to GOO for battle SBR"
                sbrPivot && needsGOOBattle && d.near.%(n => n.foes.goos.any).any |=> 9000 -> "SBR PIVOT: Glaaki toward GOO (1 hop away)"
                // [2026-04-02] NS: 1-power Glaaki at home with no monsters = STAY (score 0 for leaving)
                val homeNoMonsters = o.ownGate && o.allies.monsterly.none
                power == 1 && homeNoMonsters && !d.ownGate |=> -6000 -> "#578 NS: 1pwr, no monsters at home, stay"
                // Survival mode
                power == 1 && others.%(_.power > 1).any && d.ownGate && !o.ownGate |=> 4000 -> "#550 survival: power=1, Glaaki return to gate"
                power == 1 && others.%(_.power > 1).any && !d.ownGate && !d.foes.cultists.any |=> -4000 -> "#574 survival: power=1, dont leave gate area"
                // [2026-03-31 15:20] v1.18.031: STRENGTH CHECK — avoid areas where Glaaki would be outmatched
                val destEnemyStr = others.%(e => e.at(d).any)./(e => e.strength(e.at(d), self)).sum
                val destOwnStr = self.strength(self.at(d) :+ self.all(Glaaki).head, others.head) // approximate
                destEnemyStr > 6 && d.allies.monsterly.none |=> -3000 -> "#505 strength: strong enemies, Glaaki alone"
                // [2026-04-02] NS: GOO at home gate — combat decision
                val gooAtHomeGate = d.ownGate && d.foes.goos.active.any && d.allies.cultists.any
                val homeEnemyStr = if (gooAtHomeGate) others.%(e => e.at(d).any)./(e => e.strength(e.at(d), self)).sum else 0
                val homeOwnStr = if (gooAtHomeGate) self.strength(self.at(d) ++ (if (o != d) self.all(Glaaki) else $), others.%(e => e.at(d).goos.any).headOption.getOrElse(others.head)) else 0
                val homeCombatFavorable = homeOwnStr > homeEnemyStr
                // GOO at home + favorable: return to fight (1000 above capture raid ~8000)
                gooAtHomeGate && homeCombatFavorable |=> 9000 -> "#124 NS: GOO at home, favorable — return to fight"
                // GOO at home + unfavorable: don't return, stay capturing — summon/recruit will help later
                gooAtHomeGate && !homeCombatFavorable |=> 1000 -> "#290 NS: GOO at home, unfavorable — stay away"
                // GOO near home (not on it yet)
                val gooNearOwnGate = d.ownGate && d.near.%(n => n.foes.goos.active.any).any && d.allies.cultists.any && d.allies.monsterly.none
                // [RARELY] gooNearOwnGate && homeCombatFavorable |=> 6000 -> "#137 NS: GOO approaching home, favorable — intercept"
                // [2026-04-02] NS: no GOO at home — Glaaki at capture location stays put
                val atCaptureLocation = !o.ownGate && o.foes.cultists.any && o.foes.goos.none
                val homeHasNoGOO = self.gates.forall(g => !g.foes.goos.active.any)
                // Returning home when no threat: score LOW so capture (5000) wins
                // [DEAD] atCaptureLocation && homeHasNoGOO && d.ownGate |=> 500 -> "#378 NS: no threat at home, stay and capture"
                // [2026-04-02] NS4: AP2 Glaaki Undulate raid — must beat DT summon (1400)
                // Triangle: gate adjacent to BOTH existing TS gates
                val formsTriangle = d.enemyGate && self.gates.num >= 2 && self.gates.forall(g => d.near.contains(g) || d == g)
                val destFactionBonus = factionTierBonus(d)
                // [2026-04-02] NS: AP2 prefer fewer cultists (easier to steal), post-AP2 prefer more (GD captures)
                val destCultists = d.foes.cultists.num
                val cultistTargetBonus = if (secondAP) (5 - destCultists).max(0) * 5 else destCultists * 5
                // [2026-04-02] NS4: AP2 Glaaki capture — faction pref first, then cultist pref
                // Safe = no GOO and no real combat (SL Wizards are Monster type but 0 combat)
                val destEnemyCombat = others.%(e => e.at(d).any)./(e => e.strength(e.at(d), self)).sum
                val ap2GateTarget = secondAP && d.enemyGate && d.foes.goos.none && destEnemyCombat <= 2
                ap2GateTarget && formsTriangle |=> 6000 + destFactionBonus + cultistTargetBonus -> "#82 NS4: AP2 Glaaki [UNDULATE] to triangle gate"
                ap2GateTarget |=> 4000 + destFactionBonus + cultistTargetBonus -> "#85 NS4: AP2 Glaaki [UNDULATE] to enemy gate"
                val weakestGateTarget = weakestGatedFaction.exists(wf => d.gateOf(wf) && d.foes.goos.none && (d.foes.cultists.num <= 2 || wf.power <= 1))
                secondAP && weakestGateTarget |=> 5000 + destFactionBonus + cultistTargetBonus -> "#84 NS4: AP2 Glaaki [UNDULATE] to weakest faction gate"
                // [2026-04-02] NS 5a: AP2 at 2 power — Undulate+capture if 1 enemy cultist on adjacent gate (take the gate)
                val canStealGate = d.enemyGate && d.foes.cultists.num == 1 && d.foes.monsterly.none && d.foes.goos.none
                secondAP && power >= 2 && canStealGate |=> 6000 + destFactionBonus + cultistTargetBonus -> "#90 NS5a: AP2 2pwr [UNDULATE]+capture steal gate"
                // [2026-04-02] NS 5b: AP2 at 1 power — retreat to fortress if not on capturable gate with cultist
                val onCapturableGateWithCultist = o.enemyGate && o.foes.cultists.num == 1 && o.foes.monsterly.none && o.foes.goos.none && o.allies.cultists.any
                secondAP && power == 1 && !onCapturableGateWithCultist && d.ownGate |=> 5000 -> "NS5b: AP2 1pwr [RAW MOVE] retreat to fortress"
                secondAP && power == 1 && !onCapturableGateWithCultist && !d.ownGate |=> -3000 -> "NS5b: AP2 1pwr [RAW MOVE] dont stray"
                // [2026-04-02] NS6: AP3 Glaaki returns to fortress — but NOT if it leaves origin undefended
                val originHasLoneCultist = o.ownGate && o.allies.cultists.num == 1 && o.allies.monsterly.none
                // If origin has lone cultist, recruit first or Undulate will carry — don't raw move
                thirdAP && fortressGate.exists(_ == d) && !originHasLoneCultist |=> 7000 -> "#99 NS6: AP3 Glaaki [RAW MOVE] return to fortress"
                thirdAP && d.ownGate && self.at(d, TombHerd).num >= 2 && !originHasLoneCultist |=> 6000 -> "#94 NS6: AP3 Glaaki [RAW MOVE] to TH-fortified gate"
                // If origin has lone cultist, recruit there first (4000 beats move at 3000)
                thirdAP && originHasLoneCultist |=> -2000 -> "#97 NS6: don't leave lone cultist, recruit first"
                // [2026-04-02] NS10: AP4+ Glaaki raids with faction tier preference
                val destTier = factionTierBonus(d)
                fourthAPPlus && power > 1 && have(Undulate) && have(GreenDecay) && d.foes.cultists.any && d.foes.goos.none && !anCathedralDanger(d) |=> 7000 + destTier + cultistTargetBonus -> "#134 NS10: AP4+ Glaaki [UNDULATE] GD capture raid"
                fourthAPPlus && have(Undulate) && d.enemyGate && d.foes.goos.none && d.foes.monsterly.none |=> 6000 + destTier + cultistTargetBonus -> "#138 NS10: AP4+ Glaaki [UNDULATE] raid adjacent gate"
                fourthAPPlus && power > 1 && have(Undulate) && have(GreenDecay) && d.near.%(n => n.foes.cultists.any && n.foes.goos.none).any |=> 5000 + destTier -> "#145 NS10: AP4+ Glaaki [UNDULATE] toward captures"
                fourthAPPlus && power > 1 && have(Undulate) && d.foes.any && d.foes.goos.none |=> 4000 + destTier -> "#161 NS10: AP4+ Glaaki [UNDULATE] toward enemies"
                // [2026-04-02] NS12: Glaaki return home — ONLY when threatened
                // Threat = Glaaki alone, enemy combat >= 3, enemy can act before Glaaki (has power or goes first)
                val canCaptureHere = o.foes.cultists.any && o.foes.goos.none
                val glaakiAlone = glaakiRegion.exists(r => self.at(r).%(_.monsterly).none)
                val enemyCombatHere = glaakiRegion.map(r => others.%(e => e.at(r).any)./(e => e.strength(e.at(r), self)).sum).getOrElse(0)
                val enemyCanActFirst = others.%(_.power >= 1).any
                val glaakiThreatened = glaakiAlone && enemyCombatHere >= 3 && enemyCanActFirst
                // Only return if threatened AND can't capture here
                glaakiThreatened && !canCaptureHere && d.ownGate |=> 5000 -> "#146 NS12: Glaaki [RAW MOVE] return, threatened"
                // Not threatened = don't return, go capture
                glaakiMissingMonster && !glaakiThreatened && d.ownGate |=> 1000 -> "#291 NS12: not threatened, low return priority"
                // [2026-04-02] NS: fortress ready (Glaaki+DT+2TH) — Glaaki MUST use Undulate, not raw move
                // High positive score on Undulate destinations beats raw move and summon
                val glaakiFortressReady = have(Undulate) && glaakiHasDT && glaakiRegion.exists(gr => self.at(gr, TombHerd).num >= 2)
                // When fortress ready, boost Undulate-reachable capture targets to 10000+
                // Raw move is fine IF gate stays defended (1 TH + cultist left behind)
                val gateStaysDefended = o.ownGate && o.allies.cultists.any && o.allies.%(_.is(TombHerd)).num >= 2
                // Needs to beat NS10 capture raid (7800 max) by ~1000
                glaakiFortressReady && power > 1 && have(GreenDecay) && d.foes.cultists.any && d.foes.goos.none && gateStaysDefended && !anCathedralDanger(d) |=> 8000 + destTier -> "#128 NS: fortress [UNDULATE] capture raid"
                glaakiFortressReady && d.enemyGate && d.foes.goos.none && d.foes.monsterly.none && gateStaysDefended |=> 7000 + destTier -> "#135 NS: fortress [UNDULATE] gate theft"
                // [2026-04-02] NS: low power consolidation — stop chasing, occupy empty gates toward home
                val noAdjacentCaptures = !o.near.%(n => n.foes.cultists.any && n.foes.goos.none).any
                val lowPower = power <= 3
                val glaakiNotOnGate = glaakiRegion.exists(r => !r.ownGate && !r.freeGate)
                val closerEmptyGate = d.freeGate && fortressGate.exists(fg => game.board.distance(d, fg) < game.board.distance(o, fg))
                // Not on gate + empty gate adjacent to home: Undulate group there
                lowPower && noAdjacentCaptures && glaakiNotOnGate && d.freeGate && d.near.%(_.ownGate).any |=> 5500 -> "#548 NS: low pwr [UNDULATE] to empty gate near home"
                // On gate but closer empty gate exists: Undulate closer to home
                lowPower && noAdjacentCaptures && !glaakiNotOnGate && closerEmptyGate |=> 6000 -> "#547 NS: low pwr [UNDULATE] closer to home"
                // [2026-04-02] NS: 2 power, all enemies <=1 power, spare units, empty adjacent gate
                val allEnemiesLowPower = others.forall(_.power <= 1)
                val spareUnitsAtHome = o.ownGate && o.allies.monsterly.num >= 2 && o.allies.cultists.num >= 2
                power == 2 && allEnemiesLowPower && spareUnitsAtHome && d.freeGate && d.near.%(_.ownGate).any |=> 6500 -> "#543 NS: 2pwr [UNDULATE] monster+cultist to empty adjacent gate"
                // GOO at fortress: attack immediately
                val gooAtFortress = fortressGate.exists(fg => fg.foes.goos.active.any)
                gooAtFortress && d == fortressGate.get |=> 8500 -> "#126 NS: GOO at fortress, attack immediately"
                // [2026-04-01] NS: vs GC Submerge — prefer landing on LAND, avoid ending on ocean
                gcHasSubmerge && gcCthulhuUp && d.ocean && !d.ownGate |=> -5000 -> "#513 NS: GC Submerge, avoid ocean"
                gcHasSubmerge && gcCthulhuUp && d.ocean |=> -3000 -> "#506 NS: GC Submerge, ocean risky even at gate"
                gcHasSubmerge && gcCthulhuUp && !d.ocean && d.ownGate |=> 2000 -> "#208 NS: GC Submerge, prefer land gate"
                // [2026-04-03] Ice Age: penalty for moving to ice age region
                iceAgeRegions.contains(d) |=> -300 -> "NS: Ice Age region, avoid"
                // [2026-04-03] Cathedral + AN danger: avoid regions with AN combat + cathedral
                anCathedralDanger(d) |=> -5000 -> "NS: Cathedral + AN combat danger, avoid"
                // [2026-04-03] Flee Cathedral + AN: undulate OUT if currently in dangerous region
                anCathedralDanger(o) && !anCathedralDanger(d) |=> 10000 -> "NS: FLEE Cathedral + AN danger [UNDULATE]"
                // [2026-04-03] KIY hunt: move to KIY for GOO battle SB
                val kiyAtDest = YS.exists && d.foes(KingInYellow).any
                val hasturNotThere = !d.foes(Hastur).any && (!YS.goos.any || !YS.hasAllSB)
                val glaakiHasGroup = glaakiRegion.exists(r => self.at(r).%(_.monsterly).num >= 2)
                val destCombatTotal = others.%(e => e.at(d).any)./(e => e.strength(e.at(d), self)).sum
                val noIceAtDest = !iceAgeRegions.contains(d)
                kiyAtDest && need(TSGlaakiBattlesGOO) && hasturNotThere && !anCathedralDanger(d) && glaakiHasGroup && destCombatTotal <= 7 && noIceAtDest |=> 5000 -> "NS: Glaaki [UNDULATE] to KIY for GOO SB"
                // ── Normal movement ───────────────────────────────────────────
                // Don't abandon a gate that only has a lone acolyte — summon TH there first
                // SummonAction TH at naked gate scores 1000 (> EndTurn 500) so TH gets summoned before Glaaki moves
                // Net with -900: free-gate expansion (1200-900=300) < EndTurn (500) → Glaaki stays
                //                defend-threatened-gate (1500-900=600) > EndTurn (500) → Glaaki goes to help
                val oNakedGate = o.ownGate && o.allies.cultists.any && o.allies.monsterly.none
                val oEnemyThreatAtGate = o.ownGate && (o.foes.monsterly.any || o.foes.goos.any)
                // Naked gate + enemy threat = strong reason to stay (recruit TH first)
                oNakedGate && oEnemyThreatAtGate |=> 4500 -> "#157 glaaki: stay, naked gate + enemy threat"
                oNakedGate |=> 400 -> "#392 glaaki: summon TH at naked gate before expanding"
                // Gate with monster but no cultist — don't abandon, recruit cultist first
                val oGateMonsterNoCultist = o.ownGate && o.allies.monsterly.any && o.allies.cultists.none
                oGateMonsterNoCultist |=> 3000 -> "#180 glaaki: gate has monster no cultist, recruit first"
                // Defend threatened own gate: highest non-capture priority
                // Without gate defense, Glaaki rushes to expand while enemies take existing gates
                // [2026-03-31 16:40] v1.18.039: Boosted Glaaki gate defense to match DM/TH pattern
                // [2026-03-31 16:45] Reduced from 4000 — was blocking GD captures (3500). 2500 = compete not override.
                d.ownGate && d.foes.active.any && d.allies.monsterly.none |=> 2500 -> "#198 glaaki: defend threatened own gate"
                d.ownGate && d.foes.any && self.gates.num <= 2 |=> 600 -> "#362 glaaki: protect scarce gate vs enemy"
                // ES pivot: post-GreenDecay+Glaaki, actively harvest Elder Signs
                // Multi-capture areas are especially valuable (2+ ES in one move)
                // GREEN DECAY CAPTURE: primary late-game doom engine
                // 5 cultists = 5 ES ≈ 8.3 doom for 2 power. 10x more efficient than ritual!
                esPivot && d.foes.cultists.num >= 3 && d.foes.goos.none |=> 4000 -> "#162 GD: mass capture = massive ES"
                esPivot && d.foes.cultists.num >= 2 && d.foes.goos.none |=> 3000 -> "#181 GD: multi-capture for ES"
                // Transit toward capture targets 1-2 hops away
                // [2026-04-02] NS: transit scores only when no Undulate — otherwise use Undulate carry
                esPivot && !have(Undulate) && d.near.%(n => n.foes.cultists.num >= 2 && n.foes.goos.none).any |=> 2000 -> "#209 GD: transit to multi-capture (no Undulate)"
                esPivot && !have(Undulate) && d.near.%(n => n.foes.cultists.any && n.foes.goos.none).any |=> 1500 -> "#242 GD: transit to capture (no Undulate)"
                esPivot && d.foes.cultists.any && d.foes.goos.none |=> 2000 -> "#210 GD: capture for ES"
                have(GreenDecay) && d.foes.cultists.any && d.foes.goos.none |=> 1500 -> "#243 green decay: capture for es"
                // SL early gate aggression: Glaaki goes to SL's gate ONLY after we have an ocean gate
                // (ocean gate = Glaaki awakening prereq — never divert Glaaki from that critical path)
                // [2026-04-01] NS: Glaaki grabs free/empty gates with stranded TS units via Undulate
                // This beats battle (3000) — claiming a gate is more valuable
                d.freeGate && (d.allies.cultists.any || d.allies.monsterly.any) |=> 4000 -> "#163 NS: Glaaki grab free gate with stranded TS units"
                d.freeGate |=> 1084 -> "#285 glaaki to free gate area"
                d.ownGate && d.allies.goos.none |=> 800 -> "#324 glaaki to own gate"
                d.ownGate && d.allies.cultists.any |=> 400 -> "#393 glaaki protects cultist at gate"
                d.allies.cultists.any && d.foes.none |=> 400 -> "#394 glaaki protects cultists"
                d.foes.cultists.any && d.foes.goos.none |=> 300 -> "#407 glaaki to capture"
                // UNDULATE CAPTURE COMBO: Glaaki + acolyte move to lone cultist at enemy gate → capture → take gate
                val dLoneCultistGate = d.enemyGate && d.foes.cultists.num == 1 && d.foes.monsterly.none && d.foes.goos.none
                dLoneCultistGate && have(Undulate) |=> 2000 -> "#211 glaaki: undulate capture combo, take enemy gate"
                dLoneCultistGate && have(Undulate) && others.%(ef => d.gateOf(ef) && ef.gates.num >= 2).any |=> 2500 -> "#199 glaaki: capture combo, weaken strong faction"
                // Gate theft: steal > build (1p vs 3p); GOO denial when enemy has no GOO and <=2 gates
                val dGlaakiSteal    = d.enemyGate && d.foes.goos.none && d.foes.monsterly.none
                val dGlaakiStealGOO = dGlaakiSteal && others.%(ef => d.gateOf(ef) && ef.goos.none && ef.gates.num <= 2).any
                // AP3 power-drain exploit: enemy just awakened GOO, power exhausted — Glaaki attacks their gate
                val dGlaakiDrained  = d.enemyGate && others.%(ef => d.gateOf(ef) && ef.goos.any && ef.power <= 2).any
                self.gates.num >= 2 && dGlaakiDrained && d.foes.monsterly.none |=> 1800 -> "#229 glaaki: raid drained post-GOO enemy gate"
                dGlaakiDrained && d.foes.monsterly.none                         |=> 1200 -> "#266 glaaki: attack drained enemy gate"
                self.gates.num >= 2 && dGlaakiStealGOO |=> 1000 -> "#292 glaaki gate theft: deny GOO awakening"
                self.gates.num >= 2 && dGlaakiSteal    |=> 500 -> "#379 glaaki gate theft > building (1p vs 3p)"
                dGlaakiSteal                           |=> 1200 -> "#267 glaaki: threaten weak enemy gate"
                d.enemyGate && d.foes.goos.none && d.foes.monsterly.num <= 2 |=> 600 -> "#363 glaaki: threaten lightly defended gate"
                o.ownGate && d.foes.cultists.any |=> 1200 -> "#268 go capture"
                // Glaaki to empty region = 0 (no reason to go there)
                d.foes.none && !d.ownGate && !d.freeGate |=> -500 -> "#471 glaaki to empty region: no target"
                true |=> 0 -> "#459 glaaki moves: base"

            case MoveAction(_, u, o, d, _) if u.uclass == TombHerd =>
                // [2026-04-02] NS: if Glaaki just moved (Undulate carry expected), block TH raw movement
                // This prevents Explode's skip-path from leaking movement into carry menu
                val glaakiJustMoved = have(Glaaki) && have(Undulate) && self.all(Glaaki).headOption.exists(_.tag(Moved))
                glaakiJustMoved |=> -50000 -> "#522 NS: Glaaki just moved, use Undulate carry not raw TH move"
                val enemiesAtD = others./~(_.at(d))
                val undefendedCultistsAtD = d.foes.cultists.any && d.foes.monsterly.none && d.foes.goos.none
                // HARD RULE: TH only moves to ADJACENT regions
                val isAdjacentToOrigin = o.near.contains(d)
                !isAdjacentToOrigin && !d.ownGate |=> -100000 -> "#527 TH: ONLY move to adjacent regions"
                // STRANDED TH: stay put, don't waste power
                val stranded = !o.ownGate && o.allies.cultists.none && o.allies.goos.none && o.near.%(_.ownGate).none
                stranded && !d.ownGate && d.allies.none |=> -100000 -> "#528 TH stranded: stay put"
                // HARD RULE: TH only moves for a CONCRETE REASON with conditions:
                // 1. Defend gate: ONLY if gate has lone cultist (no TH already there)
                val destNeedsDefense = d.ownGate && d.allies.cultists.any && d.allies.%(_.is(TombHerd)).none && d.allies.monsterly.none && d.foes.active.any
                // 2. Attack enemy gate: ONLY if weak target AND free acolyte available to claim gate
                val freeAcolyte = self.pool(Acolyte).any || d.near.%(n => n.allies.cultists.any && !n.ownGate).any
                val destAttackableGate = d.enemyGate && d.foes.cultists.num <= 1 && d.foes.monsterly.none && d.foes.goos.none && freeAcolyte
                // 3. Join Glaaki: ONLY if Glaaki is in combat and no TH already with Glaaki
                val destHasGlaakiCombat = d.allies.goos.any && d.foes.active.any && d.allies.%(_.is(TombHerd)).none
                // 4. Capture for ES: ONLY with Green Decay AND enemy can't block (out of power or no units to send)
                val safeCapture = have(GreenDecay) && undefendedCultistsAtD && others.%(e => e.at(d).any).forall(e => e.power <= 0)
                // 5. AP1 gate setup: move to adjacent empty region to escort acolyte for gate build
                val ap1GateSetup = firstAP && self.gates.num < 2 && !d.ownGate && !d.enemyGate && d.foes.none && o.ownGate
                // 6. ZERO-RISK ATTACK: move to region with only 0-combat units (acolytes, wizards)
                val zeroRiskTarget = d.foes.any && d.foes.monsterly.none && d.foes.goos.none
                // 7. NS1: AP1 aggression toward any aggressive faction units (not just gates)
                val ns1AggressiveTarget = firstAP && d.foes.any && d.foes.goos.none && aggressiveFactions.exists(f => f.at(d).any)
                // 8. NS1: In AP1 with aggressive factions, TH always has concrete reason to move
                val ns1MoveToward = firstAP && aggressiveFactions.any
                val hasConcreteReason = destNeedsDefense || destAttackableGate || destHasGlaakiCombat || safeCapture || ap1GateSetup || zeroRiskTarget || ns1AggressiveTarget || ns1MoveToward
                !hasConcreteReason |=> -2000 -> "#499 TH: prefer not to move without reason"
                // [2026-04-03] Ice Age: penalty for TH moving to ice age region
                iceAgeRegions.contains(d) |=> -300 -> "NS: Ice Age region, TH avoid"
                // Boost AP1 gate setup when BG is in game (acolyte needs escort)
                ap1GateSetup && BG.exists |=> 2000 -> "#35 AP1 vs BG: TH escort for gate build"
                ap1GateSetup |=> 1500 -> "#41 AP1: TH to empty region for gate setup"
                // [2026-04-02] NS1: AP1 aggression ONLY when TS doesn't have 2nd gate
                val ap1NeedsGate = firstAP && self.gates.num < 2
                ap1NeedsGate && zeroRiskTarget && d.gateOf(SL) |=> 5000 -> "#22 AP1: TH to SL gate, zero risk"
                ap1NeedsGate && zeroRiskTarget && SL.exists && d.foes.%(_.faction == SL).any |=> 4000 -> "#24 AP1: TH to SL cultists, zero risk"
                ap1NeedsGate && zeroRiskTarget |=> 2000 -> "#36 AP1: TH to zero-combat target"
                // [2026-04-02] NS1: AP1 TH targeting — faction tier + cultist bonus + gate required
                // "safe" = no GOO, enemy combat <= 2 (SL Wizards = Monster type but 0 combat)
                val destCombat = others.%(e => e.at(d).any)./(e => e.strength(e.at(d), self)).sum
                val safeTargetAtD = d.foes.goos.none && destCombat <= 2
                val weakTargetAtD = d.foes.cultists.num <= 2 && safeTargetAtD
                val strongTargetAtD = d.foes.goos.any || destCombat >= 4
                // Cultist differential: +5 per cultist fewer at dest vs origin. Only with gate.
                val originCultists = if (o.enemyGate) o.foes.cultists.num else 0
                val destCultists_th = if (d.enemyGate) d.foes.cultists.num else 0
                val cultistBonus = if (d.enemyGate) (originCultists - destCultists_th) * 5 else 0
                // Faction tier for TH targeting: SL=150, GC=125, CC=100, BG=75, OW=50, WW=25
                val thFactionTier = factionTierBonus(d)
                // SL is ALWAYS safe target (0 combat) regardless of cultist count
                val slGateAtDest = SL.exists && d.gateOf(SL) && safeTargetAtD
                val destFactionName = others.%(ef => d.gateOf(ef) || ef.at(d).any).headOption.map(_.short).getOrElse("?")
                val cultistLabel = if (cultistBonus > 0) " lower cultist" else if (cultistBonus < 0) " higher cultist" else ""
                // When TS has 2 gates and weaker gate is TH-protected: TH movement drops to 1000
                val has2Gates = self.gates.num >= 2
                val weakerGateProtected = has2Gates && self.gates.sortBy(_.allies.cultists.num).headOption.exists(g => self.at(g, TombHerd).any)
                firstAP && weakerGateProtected |=> 1000 -> "#47 NS1: 2 gates, weaker protected, TH movement reduced"
                // NS1 aggression scores — conditional on not having 2nd gate
                ap1NeedsGate && slGateAtDest |=> 6000 + cultistBonus -> ("NS1: TH to" + cultistLabel + " SL gate")
                ap1NeedsGate && d.enemyGate && safeTargetAtD && aggressiveFactions.exists(f => d.gateOf(f)) |=> 5000 + thFactionTier + cultistBonus -> ("NS1: TH to" + cultistLabel + " " + destFactionName + " gate")
                ap1NeedsGate && d.enemyGate && safeTargetAtD |=> 4500 + thFactionTier + cultistBonus -> ("NS1: TH to" + cultistLabel + " " + destFactionName + " gate")
                ap1NeedsGate && d.foes.any && !d.enemyGate && safeTargetAtD |=> 2000 + thFactionTier -> ("NS1: TH to " + destFactionName + " units (no gate)")
                // vs SL: hyper-aggressive, but only against weak positions
                earlyGame && SL.exists && d.foes.%(_.faction == SL).cultists.any && weakTargetAtD |=> 2500 -> "#200 early: TH attack weak SL position"
                earlyGame && SL.exists && d.gateOf(SL) && weakTargetAtD |=> 2000 -> "#212 early: TH to weak SL gate"
                // vs any enemy: attack weak positions for DH and gate denial
                earlyGame && weakTargetAtD && d.foes.cultists.any |=> 1800 -> "#230 early: TH to weak enemy position"
                earlyGame && d.near.%(n => n.foes.cultists.num <= 2 && n.foes.monsterly.none && n.foes.goos.none && n.foes.any).any |=> 1200 -> "#269 early: TH near weak attackable target"
                // Strongly avoid moving into strong positions — TH will die for nothing
                earlyGame && strongTargetAtD |=> -1500 -> "#492 early: dont attack strong position with lone TH"
                // ES pivot: TH aggressively seeks capturable cultists for Elder Signs
                esPivot && undefendedCultistsAtD |=> 800 -> "#325 es pivot: TH to undefended cultists for ES"
                esPivot && d.near.%(n => n.foes.cultists.any && n.foes.monsterly.none && n.foes.goos.none).any |=> 705 -> "#340 es pivot: TH near capturable cultists"
                // Green Decay opportunistic: TH moves to undefended cultists for free ES
                have(GreenDecay) && undefendedCultistsAtD && lateGame |=> 1195 -> "#274 GD opportunistic: TH to undefended cultists for ES"
                have(GreenDecay) && undefendedCultistsAtD |=> 1800 -> "#231 GD: TH to undefended cultists"
                // [2026-04-01 09:15] v1.18.045: 1-POWER SURVIVAL — TH returns to gate when power=1
                power == 1 && others.%(_.power > 1).any && d.ownGate && !o.ownGate |=> 4000 -> "#551 survival: power=1, TH return to gate"
                // [2026-03-31 16:30] v1.18.038: Boosted TH gate protection to match DM defense success
                d.ownGate && d.allies.cultists.any && d.allies.monsterly.none && d.foes.active.any |=> 4000 -> "#164 protect threatened gate urgently"
                d.ownGate && d.allies.cultists.num == 1 && d.allies.monsterly.none |=> 3500 -> "#172 protect lone gate keeper"
                // SL: Tsathoggua — Sleeper can rapidly reach undefended gates; reinforce them
                slHasTsatho && lateGame && d.ownGate && d.allies.cultists.any && d.allies.monsterly.none |=> 1800 -> "#232 SL tsatho: guard own gate late game"
                slHasTsatho && lateGame && d.ownGate && d.allies.cultists.any |=> 3036 -> "#174 SL tsatho: reinforce gate late game"
                // Ocean gate: TombHerds support cultists trying to seize ocean areas
                needsOceanGate && d.ocean && d.allies.cultists.any |=> 2611 -> "#195 support cultist at ocean area"
                needsOceanGate && d.ocean && d.foes.goos.none && d.foes.monsterly.none |=> 800 -> "#326 move TH toward ocean"
                // Proactively guard own gates — don't wait for enemies to show up
                d.ownGate && d.allies.cultists.any && d.allies.monsterly.num < 2 |=> 1500 -> "#244 reinforce own gate preemptively"
                // AP1 GOO denial: move TH toward factions with 2+ gates (close to awakening GOO)
                // Raiding their cultists in AP1 slows their awakening, giving TS AP2 window for Glaaki
                val gooDenialTargets = others.%(f => !f.goos.any && f.gates.num >= 2)
                // SL early gate aggression: TH moves toward SL gate area
                slEarlyAggression && d.gateOf(SL) && d.foes.monsterly.none && d.foes.goos.none |=> 726 -> "#339 SL early: TH to SL gate"
                slEarlyAggression && d.near.%(_.gateOf(SL)).any && d.foes.monsterly.none |=> 1225 -> "#263 SL early: TH near SL gate"
                earlyGame && gooDenialTargets.exists(f => d.foes.%(_.faction == f).any && d.foes.cultists.num <= 2) |=> 100 -> "#54 AP1: TH toward weak GOO-denial target"
                earlyGame && weakTargetAtD && enemiesAtD.any |=> 317 -> "#405 early: tomb herd toward weak enemies"
                d.ownGate && d.allies.cultists.any |=> -813 -> "#478 reinforce own gate"
                // [2026-04-02] NS: unoccupied enemy gate beats weak gate by 500
                val emptyEnemyGate = d.enemyGate && d.foes.none
                // [DEAD] emptyEnemyGate |=> 2500 -> "#201 NS: TH to unoccupied enemy gate"
                // Gate theft: steal > build (1p vs 3p)
                val dWeakEnemyGate = d.enemyGate && d.foes.cultists.any && d.foes.monsterly.none && d.foes.goos.none
                val dGOODenialTarget = dWeakEnemyGate && others.%(ef => d.gateOf(ef) && ef.goos.none && ef.gates.num <= 2).any
                // AP3 power-drain exploit: enemy just awakened GOO this AP, power exhausted — rush their gate
                val dDrainedGate = d.enemyGate && others.%(ef => d.gateOf(ef) && ef.goos.any && ef.power <= 2).any
                dDrainedGate && d.foes.monsterly.none |=> 1200 -> "#96 AP3: rush gate of power-drained post-GOO enemy"
                self.gates.num >= 2 && dGOODenialTarget         |=> 400 -> "#395 gate theft: deny faction GOO awakening"
                self.gates.num >= 2 && dWeakEnemyGate           |=> 935 -> "#307 gate theft > building (1p vs 3p)"
                dWeakEnemyGate                                   |=> 800 -> "#327 gate theft: set up capture"
                d.enemyGate && d.foes.goos.none && d.foes.monsterly.num <= 1 |=> 523 -> "#373 threaten lightly defended gate"
                // Low-power consolidation: when almost out of power, pile onto fortress gate
                self.power <= 2 && d.ownGate && d.allies.goos.any && d.allies.cultists.any |=> 581 -> "#565 low power: consolidate at Glaaki fortress"
                self.power <= 2 && d.ownGate && d.allies.monsterly.num >= 2                |=> 2285 -> "#560 low power: consolidate at fortified gate"
                // Combo assembly: TH on map multiplies Glaaki's strength; join the stack when Glaaki+DT are fighting
                // These are handled by Undulate carry, not raw TH movement
                // [2026-04-02] NS: removed unconditional -100000 — hasConcreteReason check above handles this
                // true |=> -100000 -> "TH default: dont move without concrete reason"

            case MoveAction(_, u, o, d, _) if u.uclass == DeepTendril =>
                // [2026-04-02] Block DT raw move when Glaaki just moved (carry expected)
                val glaakiJustMovedDT = have(Glaaki) && have(Undulate) && self.all(Glaaki).headOption.exists(_.tag(Moved))
                glaakiJustMovedDT |=> -50000 -> "#523 NS: Glaaki just moved, use Undulate carry not raw DT move"
                val dtToGlaaki = have(Glaaki) && d.allies.goos.any && !o.allies.goos.any
                dtToGlaaki |=> 2000 -> "#213 DT: move to join Glaaki"
                // Already with Glaaki? Don't move.
                o.allies.goos.any |=> -100000 -> "#529 DT: already with Glaaki, stay"
                // No Glaaki or Glaaki not adjacent? Don't waste power.
                !dtToGlaaki && !o.allies.goos.any |=> -100000 -> "#530 DT: no reason to move, save power"

            case MoveAction(_, u, o, d, _) if u.uclass == Acolyte =>
                // [2026-04-02] Block acolyte raw move when Glaaki just moved (carry expected)
                val glaakiJustMovedAco = have(Glaaki) && have(Undulate) && self.all(Glaaki).headOption.exists(_.tag(Moved))
                glaakiJustMovedAco |=> -50000 -> "#524 NS: Glaaki just moved, use Undulate carry not raw acolyte move"
                // [2026-04-03] Ice Age: penalty for acolyte moving to ice age region
                iceAgeRegions.contains(d) |=> -300 -> "NS: Ice Age region, acolyte avoid"
                // [2026-04-01 17:12] v1.18.057: R3b — no lone cultist to new region post-AP1
                // Exception: moving to build a gate (empty region, have power to build) is allowed
                val destHasTSMonsterOrGOO = d.allies.monsterly.any || d.allies.goos.any
                // [2026-04-01 19:52] Tightened: only allow gate-build move when adjacent to OWN gate AND safe
                val adjacentToOwnGate = d.near.%(_.ownGate).any
                val safeFromGOO = others.%(f => f.goos.any && f.power >= 2 && d.near.%(n => f.at(n).goos.any).any).none
                val movingToBuild = !d.ownGate && !d.enemyGate && d.foes.none && power >= 4 && self.gates.num < 3 && adjacentToOwnGate && safeFromGOO
                !firstAP && !d.ownGate && !destHasTSMonsterOrGOO && !movingToBuild |=> -100000 -> "#62 R3b: no lone cultist to undefended region post-AP1"
                // [2026-04-01 09:16] v1.18.045: 1-POWER SURVIVAL — acolyte returns to gate when power=1
                power == 1 && others.%(_.power > 1).any && d.ownGate && !o.ownGate |=> 3000 -> "#555 survival: power=1, acolyte to gate"
                power == 1 && others.%(_.power > 1).any && !d.ownGate && o.ownGate |=> -3000 -> "#573 survival: power=1, dont leave gate"
                // [2026-04-01 13:08] v1.18.053: F2 — old -1000 removed (beaten by +3000). WITHOUT was better. Keeping removed.
                // [2026-04-01 13:08] Also removed -1107. WITHOUT was better (+0.2% win, +0.7doom, same BW).
                // ── Faction threat avoidance / response ───────────────────────
                // GC: Dreams — lone cultist near Cthulhu risks being absorbed
                gcHasDreams && gcCthulhuUp && d.foes(Cthulhu).any |=> -1000 -> "#488 GC dreams: lone cultist near cthulhu"
                gcHasDreams && gcCthulhuUp && d.foes.goos.any && d.allies.monsterly.none |=> -900 -> "#482 GC dreams: lone cultist near GC goo"
                // BG: ShubNiggurath — lone cultist near Shub is easy prey
                bgHasShub && d.foes(ShubNiggurath).any && d.allies.monsterly.none |=> -700 -> "#475 BG shub: lone cultist near shub"
                bgHasShub && d.foes.goos.any && d.allies.monsterly.none |=> -700 -> "#476 BG shub: lone cultist near BG goo"
                // YS: Hastur/KingInYellow — dangerous for lone cultists
                ysHasturUp && d.foes(Hastur).any && d.allies.monsterly.none |=> -457 -> "#468 YS hastur: dangerous area for lone cultist"
                ysKIYUp && d.foes(KingInYellow).any && d.allies.monsterly.none |=> -731 -> "#477 YS KIY: dangerous area"
                // SL: Tsathoggua — don't leave gates undefended when Sleeper is active
                // [RARELY] slHasTsatho && o.ownGate && o.allies.cultists.num == 1 && o.allies.monsterly.none |=> -1500 -> "#493 SL tsatho: dont leave gate undefended"
                // WW: polar gate rush — intercept before WW gets their opposite-pole gate
                wwLoneCultistPolarGate(1) && WW.exists && d == game.board.starting(WW).but(game.starting(WW)).head |=> 1100 -> "#280 block WW polar gate"
                wwLoneCultistPolarGate(2) && WW.exists && d.near.%(_.==(game.board.starting(WW).but(game.starting(WW)).head)).any |=> 400 -> "#396 move toward WW polar gate to block"
                // ── AP1: ONE acolyte to get 2nd gate ──────────────────────────
                // Acolyte count at origin to prevent scatter
                val acolytesOffGate = self.all(Acolyte).num - self.gates./~(r => self.at(r, Acolyte)).num
                val thClearedArea = d.allies.%(_.is(TombHerd)).any && d.foes.cultists.num <= 1 && d.foes.monsterly.none && d.foes.goos.none
                // [2026-04-02] NS1: AP1 acolyte to empty/free gate where TH already is (HIGHEST)
                // This is how TS claims gates stolen by TH aggression
                firstAP && d.freeGate && d.allies.%(_.is(TombHerd)).any && acolytesOffGate == 0 |=> 8000 -> "#17 NS1: acolyte to free gate with TH"
                firstAP && !d.ownGate && !d.enemyGate && d.foes.none && d.allies.%(_.is(TombHerd)).any && acolytesOffGate == 0 |=> 7000 -> "#19 NS1: acolyte to empty region with TH"
                // Priority 1: TH-cleared or TH-protected area (safe, TH provides protection)
                firstAP && self.gates.num < 2 && thClearedArea |=> 3500 -> "#26 AP1: acolyte to TH-cleared area"
                firstAP && self.gates.num < 2 && d.allies.%(_.is(TombHerd)).any && !d.enemyGate && acolytesOffGate == 0 |=> 3000 -> "#27 AP1: acolyte to TH-protected area"
                // Priority 2: empty region adjacent to starting gate
                // vs BG: ONLY go to TH-protected areas (BG captures aggressively)
                val bgInGame = BG.exists
                firstAP && self.gates.num < 2 && !bgInGame && !d.ownGate && d.foes.none && o.ownGate && acolytesOffGate == 0 |=> 2500 -> "#29 AP1: acolyte to adjacent empty region"
                firstAP && self.gates.num < 2 && !bgInGame && !d.ownGate && d.foes.none && acolytesOffGate == 0 |=> 2000 -> "#37 AP1: acolyte to empty region"
                // vs BG: only block movement to areas ADJACENT to BG units (capture risk)
                // If far from BG, acolyte can move freely. BG starts in West Africa.
                val bgNearDest = BG.exists && d.near.%(n => BG.at(n).any).any
                firstAP && self.gates.num < 2 && bgNearDest && d.allies.%(_.is(TombHerd)).none && acolytesOffGate == 0 |=> -100000 -> "#63 AP1 vs BG: too close to BG, need TH escort"
                // Priority 3: any buildable area (fallback)
                firstAP && self.gates.num < 2 && !d.ownGate && !d.enemyGate && acolytesOffGate == 0 |=> 1500 -> "#42 AP1: acolyte to any buildable area"
                // Priority 5: follow TH for capture opportunity
                firstAP && self.gates.num < 2 && d.allies.%(_.is(TombHerd)).any && d.foes.cultists.num == 1 && acolytesOffGate == 0 |=> 2200 -> "#34 AP1: acolyte follows TH for capture"
                // Also allow free gate claiming (rare but possible if someone lost a gate)
                firstAP && self.gates.num < 2 && d.freeGate && acolytesOffGate == 0 |=> 2500 -> "#30 AP1: acolyte to free gate"
                // ANTI-SCATTER: max 1 acolyte off gate
                firstAP && o.ownGate && acolytesOffGate >= 1 |=> -100000 -> "#64 AP1: max 1 acolyte off gate"
                // [RARELY] firstAP && self.gates.num >= 2 && o.ownGate |=> -100000 -> "#65 AP1: 2 gates done, stay home"
                firstAP && d.enemyGate && !thClearedArea |=> -100000 -> "#66 AP1: dont move to enemy-controlled gate"
                // If acolyte is already at a buildable area, DON'T MOVE — stay and BUILD
                firstAP && !o.ownGate && !o.enemyGate && self.gates.num < 2 |=> -100000 -> "#67 AP1: stay at buildable area, BUILD"
                // POST-AP1 ACOLYTE MOVEMENT: goal-oriented only
                // GATE EVACUATION: only when enemy GOO is ON our gate AND Glaaki can't come defend
                val gooOnGate = o.ownGate && o.foes.goos.active.any
                val glaakiCanDefend = have(Glaaki) && o.near.%(_.allies.goos.any).any  // Glaaki adjacent
                val mustEvacuate = gooOnGate && !glaakiCanDefend && o.allies.monsterly.none
                val evacuateToSafety = d.ownGate && d.allies.monsterly.any
                val evacuateToTarget = d.enemyGate && d.foes.cultists.num <= 1 && d.foes.monsterly.none && d.foes.goos.none
                mustEvacuate && evacuateToSafety |=> 5000 -> "#549 evacuate: GOO on gate, retreat to defended gate"
                mustEvacuate && evacuateToTarget |=> 4000 -> "#552 evacuate: GOO on gate, retreat to capturable gate"
                mustEvacuate |=> 2000 -> "#214 evacuate: GOO on gate, flee"
                // SLEEPER PATTERN: when enemies exhausted, rush to grab gates (massive priority)
                // FINAL AP WIN: when can win this turn, grab EVERYTHING
                // [RARELY] canWinThisTurn && d.enemyGate && d.foes.cultists.num <= 2 && d.foes.goos.none |=> 100000 -> "#579 WIN: grab gate for final ritual"
                canWinThisTurn && !d.ownGate && !d.enemyGate && d.foes.none |=> 80000 -> "#580 WIN: build gate for final ritual"
                // [RARELY] !firstAP && allEnemiesExhausted && d.enemyGate && d.foes.cultists.num <= 1 && d.foes.monsterly.none && d.foes.goos.none && o.allies.cultists.num >= 3 |=> 9000 -> "#15 STRIKE: enemies exhausted, grab enemy gate"
                !firstAP && allEnemiesExhausted && d.freeGate && o.allies.cultists.num >= 3 |=> 9000 -> "#16 STRIKE: enemies exhausted, claim free gate"
                !firstAP && allEnemiesExhausted && !d.ownGate && !d.enemyGate && d.foes.none && o.allies.cultists.num >= 3 && power >= 4 |=> 8000 -> "#18 STRIKE: enemies exhausted, build new gate"
                // [2026-04-01] NS: Post-AP1 acolyte gate-building DISABLED per new strategy
                // New strategy: hold 1-2 gates, raid from fortress. No gate expansion post-AP1.
                // !firstAP && self.gates.num < 3 && !d.ownGate && !d.enemyGate && d.foes.none && o.ownGate && o.allies.cultists.num >= 4 && power >= 4 |=> 2000 -> "acolyte: move to build gate"
                // !firstAP && self.gates.num < 3 && !d.ownGate && !d.enemyGate && d.foes.none && d.near.%(_.allies.goos.any).any && o.allies.cultists.num >= 3 && power >= 4 |=> 3000 -> "acolyte: build gate near Glaaki"
                // 2. Claim a free gate
                !firstAP && d.freeGate |=> 2500 -> "#31 acolyte: claim free gate"
                // 3. Join Glaaki as meat shield — max 1, only from well-covered gate
                !firstAP && d.allies.goos.any && d.foes.active.any && d.allies.cultists.none && o.allies.cultists.num >= 4 |=> 800 -> "#50 acolyte: one meat shield for Glaaki"
                // 4. Retreat from danger
                !firstAP && !o.ownGate && o.foes.monsterly.any && d.ownGate |=> 1000 -> "#75 acolyte: retreat to gate"
                // If acolyte is at a buildable area, STAY and BUILD
                !firstAP && !o.ownGate && !o.enemyGate && self.gates.num < 3 |=> -100000 -> "#68 acolyte: stay at buildable area, BUILD"
                // CRITICAL: once at a gate, NEVER leave if you're one of the last 2 cultists
                // This prevents the "build gate then immediately abandon it" pattern
                // Keep at least 3 cultists on any gate — enough to survive 1-2 captures
                o.ownGate && o.allies.cultists.num <= 3 |=> -100000 -> "#531 acolyte: keep 3+ cultists on gate"
                // Don't wander to areas that aren't gates or buildable
                !firstAP && d.enemyGate |=> -100000 -> "#69 acolyte: dont move to enemy gate"
                // Gate keeper rules still apply
                o.ownGate && o.allies.cultists.num >= 2 && o.allies.monsterly.any |=> 704 -> "#341 leave well-covered gate"
                o.ownGate && o.allies.cultists.num >= 2 && o.allies.monsterly.none |=> -500 -> "#472 gate needs monster cover first"

            // ── OTHER UNIT MOVEMENT (Ghasts, neutral monsters, etc.) ─────────
            case MoveAction(_, u, o, d, _) =>
                // All non-core units: DON'T MOVE unless there's a concrete reason
                // Moving Ghasts/neutrals wastes power for no strategic gain
                true |=> -100000 -> "#532 non-core unit: dont waste power moving"

            // ── ATTACK ───────────────────────────────────────────────────────
            case AttackAction(_, r, f, _) if f.neutral =>
                true |=> -100000 -> "#533 dont attack neutral"

            case AttackAction(_, r, f, _) =>
                val allies    = self.at(r)
                val foes      = f.at(r)
                val enemyStr  = f.strength(foes, self)
                val ownStr    = adjustedOwnStrengthForCosmicUnity(self.strength(allies, f), allies, foes, opponent = f)

                // GOO/iGOO at TS controlled gate: battle if favorable
                val gooAtOwnGate = r.ownGate && foes.%(_.uclass.utype == GOO).any
                (gooAtOwnGate && ownStr > enemyStr) |=> 4500 -> "TS: battle GOO/iGOO at own gate (favorable)"

                // [2026-04-01 20:22] v1.18.069: CC Invisibility — polyps negate combat. Don't attack when polyps >= TS units.
                val ccInvisibility = f == CC && CC.exists && CC.has(Invisibility)
                val polypsHere = foes.%(_.uclass == FlyingPolyp).num
                ccInvisibility && polypsHere >= allies.num |=> -100000 -> "#534 CC Invisibility: polyps negate all combat"
                ccInvisibility && polypsHere >= allies.num - 1 |=> -5000 -> "#514 CC Invisibility: polyps nearly negate combat"
                // [2026-04-01 17:22] v1.18.058: R4 — CC Emissary: kills on Nya become pains. Don't attack.
                val ccEmissary = f == CC && CC.exists && CC.has(Emissary) && foes.%(_.uclass == Nyarlathotep).any
                ccEmissary && !allies.goos.any |=> -100000 -> "#535 R4: CC Emissary, cant kill Nya without GOO"
                ccEmissary && allies.goos.any && ownStr <= enemyStr |=> -5000 -> "#515 R4: CC Emissary, unfavorable vs Nya even with Glaaki"
                // ── Faction-specific combat hazards ───────────────────────────
                // AN: UnholyGround+Cathedral — fighting here is extra deadly for GOOs
                val anCathedralDanger = f == AN && anHasUG && game.cathedrals.contains(r)
                anCathedralDanger && allies.goos.any && ownStr <= enemyStr + 1 |=> 1500 -> "#245 AN cathedral+UG: glaaki extra vulnerable"
                anCathedralDanger && allies.goos.any |=> 1200 -> "#270 AN cathedral+UG: combat penalty"
                // BG: Frenzy — extra combat dice; be more conservative
                val bgFrenzy = f == BG && BG.exists && BG.has(Frenzy)
                bgFrenzy && allies.goos.any && ownStr <= enemyStr + 1 |=> 800 -> "#328 BG frenzy: risky fight for glaaki"
                // OW: DreadCurse+YogSothoth — rolls extra dice vs Glaaki
                val owDreadThreat = f == OW && owHasDreadCurse && owHasYog && foes.goos.any
                owDreadThreat && allies.goos.any && ownStr <= enemyStr |=> -3000 -> "#507 OW dread curse+yog: glaaki outgunned"
                owDreadThreat && allies.goos.any && ownStr <= enemyStr + 1 |=> 2000 -> "#215 OW dread curse: glaaki risky"
                // CC: Nyarlathotep — extremely powerful; need strong ownStr advantage
                val ccNyaThreat = f == CC && ccHasNya && foes(Nyarlathotep).any
                ccNyaThreat && allies.goos.any && ownStr < enemyStr + 2 |=> 1900 -> "#226 CC nyarlathotep: need dominant advantage"

                // BASH THE LEADER: when close to winning, attack the leading faction's gates
                val leaderFaction = others.sortBy(e => -(e.doom + e.es./(_.value).sum)).headOption
                val isLeaderGate = leaderFaction.exists(lf => r.gateOf(lf))
                val closeRace = realDoom >= 20 && doomGap <= 5  // TS is competitive, race is tight
                closeRace && isLeaderGate && ownStr >= enemyStr |=> 3000 -> "#182 bash leader: attack their gate"
                closeRace && isLeaderGate |=> 1500 -> "#246 bash leader: contest their gate"
                // LET ENEMIES FIGHT: when CC+YS are both present, deprioritize attacking either
                ccYSWar && (f == CC || f == YS) && !closeRace |=> -1500 -> "#494 let CC and YS fight each other"

                // Need battle spellbooks: TSRollKill, TSRoll3Pains, TSTombHerdKilled, TSGlaakiBattlesGOO
                val needBattleSB = need(TSRollKill) || need(TSRoll3Pains) || need(TSTombHerdKilled)
                val needGlaakiGOO = need(TSGlaakiBattlesGOO)

                // Ocean gate: critical for Glaaki — prioritise above nearly everything
                needsOceanGate && r.ocean && r.enemyGate && foes.goos.none && ownStr >= enemyStr |=> 900 -> "#310 seize ocean gate for glaaki"
                needsOceanGate && r.ocean && r.enemyGate && foes.goos.none && ownStr * 2 >= enemyStr * 3 |=> 600 -> "#364 contest ocean gate for glaaki"
                needsOceanGate && r.ocean && foes.cultists.any && foes.goos.none && ownStr >= enemyStr |=> 300 -> "#408 clear ocean for glaaki"

                // [2026-04-02] NS: enemy GOO at home gate — attack at 7000 if combat favorable
                val gooAtHomeGateAttack = r.ownGate && foes.goos.any && foes.num <= 3 && ownStr > enemyStr
                gooAtHomeGateAttack |=> 7000 -> "#136 NS: attack GOO at home gate, combat favorable"
                // [2026-04-02] NS: TH in region with undefended enemy GOO — battle at 8000
                val thVsUndefendedGOO = allies.%(_.is(TombHerd)).any && foes.goos.any && foes.monsterly.none && foes.cultists.num <= 1
                thVsUndefendedGOO |=> 8000 -> "#129 NS: TH vs undefended enemy GOO"
                // [2026-04-03] Ice Age: penalty for battles in ice age regions
                iceAgeRegions.contains(r) |=> -200 -> "NS: Ice Age region, battle penalty"
                // [2026-04-03] KIY battle: attack YS with KIY for GOO battle SB
                val kiyHere = f == YS && foes(KingInYellow).any
                val hasturNotHere = !foes(Hastur).any && (!YS.goos.any || !YS.hasAllSB)
                val glaakiGroupHere = allies.goos.any && allies.%(_.monsterly).num >= 2
                val combatHere = enemyStr
                val anCathedralDangerHere = AN.exists && AN.strength(AN.at(r), self) > 0 && game.cathedrals.contains(r) && (AN.power > 0 || AN.all(HighPriest).any || slCanSpend3)
                kiyHere && need(TSGlaakiBattlesGOO) && hasturNotHere && !anCathedralDangerHere && glaakiGroupHere && combatHere <= 7 && !iceAgeRegions.contains(r) |=> 5500 -> "NS: battle KIY for GOO SB"
                // NS1: AP1 attack at enemy gate
                val adjacentWeakerGate = r.near.%(n => n.enemyGate && n.foes.goos.none && n.foes.cultists.num < foes.cultists.num).any
                firstAP && r.enemyGate && foes.goos.none && !adjacentWeakerGate |=> 6000 -> "#20 NS1: AP1 attack here, no weaker adjacent gate"
                firstAP && r.enemyGate && foes.goos.none && aggressiveFactions.exists(ef => r.gateOf(ef)) |=> 5500 -> "#21 NS1: AP1 attack aggressive faction gate"
                // ZERO-RISK ATTACKS
                val zeroRisk = foes.monsterly.none && foes.goos.none && enemyStr == 0
                zeroRisk && earlyGame |=> 5000 -> "#147 zero-risk: attack 0-combat units freely"
                zeroRisk |=> 3000 -> "#183 zero-risk: free attacks on defenseless units"
                f == SL && zeroRisk && earlyGame |=> 2000 -> "#216 SL specific: extra aggression"
                slEarlyAggression && f == SL && r.gateOf(SL) |=> 4000 -> "#165 SL early: take their gate"
                // Fight aggressively to generate DH and satisfy spellbook requirements
                needBattleSB && allies.any && ownStr >= enemyStr |=> 300 -> "#409 need battle sb fight"
                needGlaakiGOO && allies.goos.any && foes.goos.any |=> 2000 -> "#217 glaaki vs goo for SB"
                needGlaakiGOO && numSB >= 5 && allies.goos.any && foes.goos.any |=> 10000 -> "#123 LAST SB: glaaki MUST fight GOO"
                sbrPivot && needsGOOBattle && allies.goos.any && foes.goos.any |=> 12000 -> "SBR PIVOT: battle GOO NOW (doom > 30)"
                // GOO kill escalation: killing an enemy GOO is high value; GC costs 4 power to resurrect Cthulhu
                f == GC && allies.goos.any && foes.goos.any && ownStr >= enemyStr |=> -300 -> "#466 kill GC GOO: costs 4 power to resurrect"
                allies.goos.any && foes.goos.any && ownStr > enemyStr |=> 200 -> "#427 kill enemy GOO: high value, deny power"
                allies.goos.any && foes.monsterly.any && ownStr >= foes.num |=> 150 -> "#436 glaaki fights for dh+sb"

                // Finale-stopping: scores must beat EndTurnAction ofinale score (666000) to override it
                ofinale(f) && r.ownGate && ownStr > enemyStr   |=> 700000 -> "#107 stop finalist: defend our gate"
                ofinale(f) && r.gateOf(f) && ownStr > enemyStr |=> 690000 -> "#108 stop finalist: attack their gate"
                ofinale(f) && ownStr > enemyStr                 |=> 680000 -> "#109 stop finalist: winning fight"
                ofinale(f) && ownStr >= enemyStr                |=> 660000 -> "#110 stop finalist: even fight"
                ofinale(f) && r.ownGate                         |=> 650000 -> "#111 stop finalist: defend gate"
                r.ownGate && foes.any && ownStr > enemyStr |=> 100 -> "#442 defend own gate winning"
                r.ownGate && foes.any && ownStr >= enemyStr |=> 800 -> "#329 defend own gate"
                // Fight for gates even when losing slightly — losing gate is catastrophic
                r.ownGate && foes.any && allies.cultists.any && self.gates.num <= 1 |=> 2000 -> "#218 defend last gate even if losing"
                r.ownGate && foes.any && allies.cultists.any |=> 1500 -> "#247 defend own gate gatekeeper at stake"
                r.enemyGate && ownStr > enemyStr && foes.goos.none |=> 3000 -> "#184 take enemy gate"
                // GOO denial: attacking enemy's 2nd gate when they have no GOO delays their awakening
                val denial = foes.goos.none && others.%(ef => r.gateOf(ef) && ef.goos.none && ef.gates.num <= 2).any
                denial && ownStr > enemyStr  |=> 500 -> "#380 strategic: steal 2nd gate, deny GOO power"
                denial && ownStr >= enemyStr |=> 458 -> "#390 contest: deny GOO power, roughly even"
                // AP3 power-drain exploit: enemy just awakened their GOO this AP, power exhausted.
                // TS plays late in turn order (PlayDirectionAction) to catch this window.
                // f.goos.any && f.power <= 2 = "just awakened, now vulnerable"
                f.goos.any && f.power <= 2 && r.gateOf(f) && ownStr > enemyStr  |=> 200 -> "#569 exploit: enemy drained post-GOO, steal gate"
                f.goos.any && f.power <= 2 && r.gateOf(f) && ownStr >= enemyStr |=> 2500 -> "#559 contest: enemy drained post-GOO"
                f.goos.any && f.power <= 2 && ownStr > enemyStr                 |=> 900  -> "#564 raid power-drained enemy"
                // Synergy combo bonus: Glaaki + DT(s) assembled = maximum combat efficiency
                // Each DT with Glaaki adds 2 combat strength; full combo (Glaaki+DT+TH) maximizes DH generation
                allies.goos.any && allies.%(_.is(DeepTendril)).any && allies.%(_.is(TombHerd)).any && ownStr > enemyStr |=> 3000 -> "#185 full combo Glaaki+DT+TH: max DH"
                allies.goos.any && allies.%(_.is(DeepTendril)).num >= 2 && ownStr > enemyStr |=> 2200 -> "#204 Glaaki+2DT: peak strength"
                allies.goos.any && allies.%(_.is(DeepTendril)).any && ownStr > enemyStr && foes.goos.none |=> 1400 -> "#255 Glaaki+DT combo fight"
                allies.goos.any && foes.any && ownStr > enemyStr |=> 884 -> "#316 glaaki fights winnable"
                // [2026-04-01 20:00] Fixed: positive fight scores gated by NOT terrible fight
                val terribleFight = ownStr * 2 < enemyStr
                val badGOOFight = foes.goos.any && ownStr < enemyStr
                allies.monsterly.any && foes.goos.none && ownStr > enemyStr && !terribleFight |=> 931 -> "#308 monster fight winnable"
                allies.monsterly.any && foes.goos.none && ownStr >= foes.num && !terribleFight |=> 900 -> "#311 fight for dh"

                terribleFight |=> -5000 -> "#516 very bad fight: outmatched 2:1"
                badGOOFight |=> -3000 -> "#508 bad fight vs goo: outstrength"

            // ── CAPTURE ──────────────────────────────────────────────────────
            case CaptureAction(_, r, f, _) =>
                // [2026-04-02] YS Passion: changed from block to preference on movement, not capture
                // If Glaaki is already here with YS, capture anyway — decision was made during movement
                // EARLY GAME: capture > battle — easier to take gates, less risk
                earlyGame && r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 2000 -> "#38 AP1: capture to take gate"
                earlyGame && r.enemyGate && r.controllers.num == 2 |=> 1500 -> "#43 AP1: capture toward gate control"
                // GREEN DECAY CAPTURE — faction pref first, then cultist count
                val capFactionBonus = factionTierBonus(r)
                val capCultistBonus = r.foes.cultists.num * 5
                have(GreenDecay) && have(Glaaki) && self.at(r, Glaaki).any |=> 8000 + capFactionBonus + capCultistBonus -> "#130 NS: Glaaki here, GD capture NOW"
                have(GreenDecay) |=> 5000 + capFactionBonus + capCultistBonus -> "#148 GD capture: 1.67 doom per capture"
                esPivot |=> 3000 -> "#186 es pivot: capture streak for ES"
                ofinale(f) && r.gateOf(f) |=> 650000 -> "#112 capture finalist gate"
                // Ocean gate capture: critical for Glaaki unlocking
                needsOceanGate && r.ocean && r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 600 -> "#365 capture ocean gate cultist"
                needsOceanGate && r.ocean && r.enemyGate |=> 350 -> "#404 open ocean gate"
                r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 566 -> "#370 capture and open gate"
                r.enemyGate && r.controllers.num == 2 |=> 302 -> "#406 nearly open gate"
                true |=> 400 -> "#397 capture"

            // ── BUILD / RECRUIT / SUMMON ─────────────────────────────────────
            case BuildGateAction(_, r) =>
                // [2026-04-01] NS: AP1 builds gates. Post-AP1: NO gate building per new strategy.
                firstAP && self.gates.num < 2 |=> 100000 -> "#11 AP1: BUILD 2nd gate NOW"
                // Prefer building ADJACENT to starting gate (TH can defend both)
                firstAP && r.near.%(_.ownGate).any |=> 5000 -> "#12 AP1: gate adjacent to starting gate"
                // Avoid building where enemies can immediately capture
                firstAP && r.foes.active.any |=> -50000 -> "#58 AP1: enemy here will steal gate"
                firstAP && r.near.%(n => n.foes.active.goos.any).any |=> -10000 -> "#57 AP1: enemy GOO adjacent, risky"
                // [2026-04-01] NS: Block ALL gate building post-AP1 — fortress strategy holds 1-2 gates
                !firstAP |=> -100000 -> "#70 NS: no gate building post-AP1, fortress strategy"
                // ADJACENT GATE CLUSTER: disabled per new strategy
                // val adjacentToOwnGate = r.near.%(_.ownGate).any
                // !firstAP && self.gates.num >= 2 && adjacentToOwnGate |=> 3000 -> "adjacent gate cluster: build near existing"
                // !firstAP && self.gates.num == 2 && adjacentToOwnGate |=> 2000 -> "3rd gate adjacent to cluster"
                val adjacentToOwnGate = r.near.%(_.ownGate).any
                // [2026-04-01] NS: All post-AP1 gate scores commented out — fortress strategy
                // esPivot && self.gates.num >= 3 |=> -100 -> "es pivot: deprioritize gate building, focus on ES"
                r.capturers.%(_.power > 0).any |=> -1097 -> "#490 will be captured"
                firstAP && self.gates.num == 1 |=> 5000 -> "#23 need second gate"
                // Post-AP1 gate expansion fully disabled
                // self.gates.num == 2 && (safeToExpand || glaakiCanProtect) |=> 2000 -> "3rd gate: safe or Glaaki nearby"
                // self.gates.num == 2 && !safeToExpand && !glaakiCanProtect |=> -500 -> "TC2: hold at 2 gates"
                // self.gates.num == 3 |=> 500 -> "need fourth gate"
                val safeToExpand = others.%(f => f.goos.any && f.power >= 2).none
                val glaakiCanProtect = have(Glaaki) && r.near.%(_.allies.goos.any).any
                // [2026-04-01 17:50] v1.18.060: R7 — gate must have monster/GOO protection or be near one
                val hasProtection = r.allies.monsterly.any || r.allies.goos.any || r.near.%(_.allies.goos.any).any
                !firstAP && !hasProtection && r.foes.active.any |=> -5000 -> "#56 R7: no protection, enemies active, dont build"
                // Glaaki guards this area — safe to build
                // [RARELY] have(Glaaki) && self.gates.num < 4 && r.allies.goos.any |=> 300 -> "#410 glaaki guards: build gate"
                // Ocean gate is especially valuable for Glaaki
                needsOceanGate && r.ocean |=> 150 -> "#437 build ocean gate for glaaki"
                hasOceanGate && r.ocean && !have(Glaaki) |=> -200 -> "#464 second ocean gate value"
                // Faction threats at build location
                // GC: Submerge — ocean gates are very vulnerable when Cthulhu can submerge
                // [RARELY] gcHasSubmerge && gcCthulhuUp && r.ocean && r.allies.monsterly.none |=> -2800 -> "#501 GC submerge: ocean gate unprotected"
                // WW: IceAge — TS pays extra power for actions in WW gate areas
                wwHasIceAge && WW.gates.contains(r) |=> -2200 -> "#500 WW ice age: actions in WW area cost extra"
                power >= 3 + maxEnemyPower |=> 2000 -> "#219 safe to build"
                // [DEAD] r.allies.goos.any |=> 297 -> "goo protects"
                r.allies.monsterly.any |=> 1113 -> "#277 monster protects"
                // [RARELY] r.foes.goos.active.any |=> 46 -> "#449 enemy goo here"

            case RecruitAction(_, Acolyte, r) =>
                // [2026-04-01] NS: After Glaaki killed — recruit to ocean gate with TH to re-awaken
                val glaakiDead = !have(Glaaki) && !firstAP && !secondAP
                val oceanGateWithTH = r.ownGate && r.ocean && r.allies.monsterly.any && r.allies.cultists.none
                val oceanFreeWithTH = !r.ownGate && r.ocean && r.allies.monsterly.any && r.allies.cultists.none
                glaakiDead && oceanGateWithTH |=> 50000 -> "#118 NS: recruit to ocean gate with TH, re-awaken Glaaki"
                glaakiDead && oceanFreeWithTH |=> 40000 -> "#120 NS: recruit to ocean with TH, claim gate then awaken"
                // Recruit to empty gate with GOO/monster when low power
                val emptyOwnGate = r.ownGate && r.allies.cultists.none && (r.allies.monsterly.any || r.allies.goos.any)
                (power < 3 && emptyOwnGate && self.pool.cultists.any) |=> 8100 -> "TS: recruit to empty gate (low power, GOO/monster there)"
                // [2026-04-01 20:12] v1.18.068: BG Avatar defense — keep 3+ cultists on gates
                val bgAvatarThreat = BG.exists && bgHasShub && BG.power >= 1
                r.ownGate && r.allies.cultists.num <= 2 && bgAvatarThreat |=> 4000 -> "#166 BG Avatar: need 3+ cultists on gate"
                r.capturers.%(_.power > 0).any |=> -900 -> "#483 dont recruit to be captured"
                // [2026-04-02] Post-gate-wipe rebuild: recruit at 5000 until 6 cultists on map
                val recruitGateWipe = have(Glaaki) && self.gates.num <= 1 && self.cultists.num < 6
                recruitGateWipe && r.allies.goos.any |=> 5000 -> "#102 NS: gate wipe rebuild, recruit cultist"
                // [2026-04-02] Low-power consolidation: recruit at 6500 when 1 power, no captures, no closer gate
                val recruitNoAdjCaptures = !r.near.%(n => n.foes.cultists.any && n.foes.goos.none).any
                val recruitNoCloserGate = !glaakiRegion.exists(gr => gr.near.%(n => n.freeGate && fortressGate.exists(fg => game.board.distance(n, fg) < game.board.distance(gr, fg))).any)
                power == 1 && recruitNoAdjCaptures && recruitNoCloserGate && r.allies.goos.any |=> 6500 + r.allies.cultists.num * 100 -> "#544 NS: 1pwr consolidate, recruit cultist"
                // STALL VALUE
                !firstAP && have(Glaaki) && maxEnemyPower >= 3 |=> 1500 -> "#44 recruit to stall: enemies still active"
                // Recruit at own gate: PRIORITY when gate is under threat or low on defenders
                r.ownGate && r.allies.cultists.num <= 1 && r.foes.active.any |=> 3000 -> "#187 URGENT: recruit at threatened gate"
                r.ownGate && r.allies.cultists.num <= 2 |=> 2000 -> "#220 recruit: gate needs more defenders"
                r.ownGate && r.allies.cultists.num < 4 |=> 1000 -> "#293 recruit at gate for defense"
                // [DEAD] r.freeGate |=> 700 -> "free gate"
                // [DEAD] r.allies.goos.any && r.allies.cultists.none |=> 500 -> "goo protects"
                self.pool.cultists.num >= power |=> -5000 -> "#517 recover cultists"

            case SummonAction(_, TombHerd, r) =>
                // EMERGENCY: lone acolyte gatekeeper is about to be captured — summon TH NOW
                // This overrides the earlyGame penalty; losing a gate is catastrophic
                val immediateCaptureRisk = r.ownGate && r.allies.cultists.num == 1 &&
                    r.allies.monsterly.none && (r.foes.monsterly.any || r.foes.goos.any)
                immediateCaptureRisk |=> 1000 -> "#294 emergency TH: imminent capture of lone gatekeeper"
                // [2026-04-02] NS: BG GOO defense — bolster gates when Shub is on board + BG has power
                // Conditions: gate has 1 cultist, no Glaaki, and 0-1 monsters
                val bgGOOThreat = bgHasShub && BG.goos.any && BG.power >= 1
                val gateNeedsMonster = r.ownGate && r.allies.monsterly.num <= 1 && r.allies.cultists.num == 1 && !r.allies.goos.any
                bgGOOThreat && gateNeedsMonster |=> 8000 -> "#132 NS: BG GOO active, bolster gate with TH"
                // PRE-GLAAKI: when Glaaki awakening is approaching (DH >= 3), guard gates first
                // Threshold lowered from 4→3: TS can awaken at DH=3 with power, leaving gates naked
                !have(Glaaki) && hasOceanGate && dh >= 3 && r.ocean && r.ownGate && r.allies.monsterly.none |=> 1165 -> "#276 guard ocean gate before Glaaki awakens"
                !have(Glaaki) && hasOceanGate && dh >= 3 && r.ownGate && r.allies.monsterly.none |=> 700 -> "#349 guard gate before Glaaki awakens"
                // POST-GLAAKI DEADLOCK FIX: Glaaki gets -900 oNakedGate penalty when at a naked gate.
                // The early-game -800 penalty prevents TH summon at gate (1000-800=200<EndTurn500).
                // Solution: when Glaaki is physically AT a naked gate, TH summon overrides early penalty (1400-800=600>500).
                // [DEAD] have(Glaaki) && r.ownGate && r.allies.goos.any && r.allies.monsterly.none && r.allies.cultists.any |=> 600 -> "TH summon: unlock Glaaki from naked gate deadlock"
                // ── AP1: 1 TH only, then 2nd gate ──────────────────────────
                firstAP && tombsOnMap == 0 |=> 1200 -> "#13 AP1: first TH"
                // [2026-04-01 16:30] TC1 removed — aggression tested negative. Standard gate-first.
                firstAP && tombsOnMap >= 1 && self.gates.num < 2 |=> -100000 -> "#71 AP1: NO more TH until 2nd gate"
                firstAP && self.gates.num >= 2 && tombsOnMap < 2 |=> 800 -> "#51 AP1: 2 gates, one more TH ok"
                // [2026-04-01 18:15] R6 — summon/place TH at undefended gate, not same gate as other TH
                val rUndefGate = r.ownGate && r.allies.monsterly.none && r.allies.goos.none && r.allies.cultists.any
                // R6 tested at 3000 — v1.18.071 showed slight regression. Keeping disabled pending further analysis.
                // rUndefGate |=> 3000 -> "R6: TH to undefended gate"
                // [2026-04-01 17:42] v1.18.059: R6 — summon/place TH at undefended gate, not same gate
                val rUndefendedGate = r.ownGate && r.allies.monsterly.none && r.allies.goos.none && r.allies.cultists.any
                rUndefendedGate |=> 5000 -> "#150 R6: TH to undefended gate (not same gate as other TH)"
                // ── POST-AP1: NEVER summon TH — gain through Death March ────
                // Exceptions: save lone gate cultist, protect Glaaki, or NS7 AP3 summon chain
                val loneGateKeeperThreat = r.ownGate && r.allies.cultists.num == 1 && r.allies.monsterly.none && r.foes.active.any
                val glaakiStrongThreat = have(Glaaki) && r.allies.goos.any && r.allies.%(_.is(TombHerd)).none &&
                    r.foes.active.any && others.exists(e => e.at(r).any && e.strength(e.at(r), self) >= 4)
                !firstAP && loneGateKeeperThreat |=> 2000 -> "#39 post-AP1: TH save lone gate cultist"
                !firstAP && glaakiStrongThreat |=> 1500 -> "#45 post-AP1: TH protect Glaaki"
                // [2026-04-02] NS7: AP3-5 summon TH at Glaaki ONLY until fortress minimum (Glaaki+DT+2TH)
                val thAtGlaakiGate = glaakiRegion.map(gr => self.at(gr, TombHerd).num).getOrElse(0)
                val fortressReady = thAtGlaakiGate >= 2 && glaakiHasDT
                (thirdAP || fourthAPPlus || game.turn == 5) && have(Glaaki) && r.allies.goos.any && self.all(DeepTendril).any && !fortressReady |=> 2500 -> "#95 NS7: AP3 summon TH at Glaaki after DT"
                // [2026-04-02] Post-gate-wipe rebuild: summon TH at 5000 until fortress rebuilt
                val gateWipeRecovery = have(Glaaki) && self.gates.num <= 1 && (tombsOnMap < 2 || !glaakiHasDT)
                gateWipeRecovery && r.allies.goos.any |=> 5000 -> "#103 NS: gate wipe rebuild, summon TH"
                // [2026-04-02] Low-power consolidation: summon TH at 6500 when 2 power, no captures, no closer gate
                val noAdjacentCapturesForSummon = !r.near.%(n => n.foes.cultists.any && n.foes.goos.none).any
                val noCloserEmptyGate = !glaakiRegion.exists(gr => gr.near.%(n => n.freeGate && fortressGate.exists(fg => game.board.distance(n, fg) < game.board.distance(gr, fg))).any)
                power == 2 && noAdjacentCapturesForSummon && noCloserEmptyGate && r.allies.goos.any |=> 6500 + self.at(r, TombHerd).num * 100 + r.allies.cultists.num * 50 -> "#545 NS: 2pwr consolidate, summon TH"
                // NS12: summon replacement monster
                fourthAPPlus && glaakiMissingMonster && r.allies.goos.any |=> 2500 -> "#105 NS12: summon replacement monster at Glaaki"
                // Post-AP1 TH block — with exceptions for rebuild/consolidation
                val allowTHSummon = loneGateKeeperThreat || glaakiStrongThreat || gateWipeRecovery ||
                    ((thirdAP || fourthAPPlus || game.turn == 5) && have(Glaaki) && r.allies.goos.any && self.all(DeepTendril).any && !fortressReady) ||
                    (fourthAPPlus && glaakiMissingMonster && r.allies.goos.any) ||
                    (power == 2 && noAdjacentCapturesForSummon && noCloserEmptyGate && r.allies.goos.any)
                !firstAP && !allowTHSummon |=> -100000 -> "#72 post-AP1: NO TH summon use Death March"
                // [DEAD] r.allies.monsterly.num >= 3 |=> 500 -> "already crowded"

            case SummonAction(_, DeepTendril, r) =>
                // NEVER summon without Glaaki
                !have(Glaaki) |=> -100000 -> "#536 DT: NEVER summon without Glaaki"
                // NEVER summon more than 1 DT in AP2 — save power for gates
                secondAP && self.all(DeepTendril).num >= 1 |=> -100000 -> "#89 AP2: max 1 DT, save power for gates"
                // First DT: summon at Glaaki's location, need 2+ gates (or AP3 per NS7)
                have(Glaaki) && self.all(DeepTendril).num == 0 && self.gates.num >= 2 |=> 1400 -> "#256 first DT for glaaki with 2 gates"
                // [RARELY] have(Glaaki) && self.all(DeepTendril).num == 0 && self.gates.num < 2 && !thirdAP |=> -100000 -> "#98 DT: need 2 gates first"
                // NS7: AP3 summon DT FIRST (before TH)
                thirdAP && have(Glaaki) && self.all(DeepTendril).num == 0 |=> 3000 -> "#93 NS7: AP3 summon DT first"
                // [2026-04-02] Post-gate-wipe rebuild: summon DT at 5000
                val dtGateWipeRecovery = have(Glaaki) && self.gates.num <= 1 && self.all(DeepTendril).num == 0
                dtGateWipeRecovery && r.allies.goos.any |=> 5000 -> "#104 NS: gate wipe rebuild, summon DT"
                // [2026-04-02] Low-power consolidation: summon DT at 6500 when 3 power, no captures, no closer gate
                val dtNoAdjCaptures = !r.near.%(n => n.foes.cultists.any && n.foes.goos.none).any
                val dtNoCloserGate = !glaakiRegion.exists(gr => gr.near.%(n => n.freeGate && fortressGate.exists(fg => game.board.distance(n, fg) < game.board.distance(gr, fg))).any)
                power == 3 && dtNoAdjCaptures && dtNoCloserGate && self.all(DeepTendril).num == 0 && r.allies.goos.any |=> 6500 + (r.allies.%(_.is(DeepTendril)).none.?(100).|(0)) -> "#546 NS: 3pwr consolidate, summon DT"
                // Second DT: only in AP3+
                !firstAP && !secondAP && have(Glaaki) && self.all(DeepTendril).num == 1 |=> 800 -> "#52 AP3: second DT for combat"
                // Max 2 DTs
                self.all(DeepTendril).num >= 2 |=> -100000 -> "#537 max 2 DTs"
                // Must summon at Glaaki's location
                r.allies.goos.any |=> 600 -> "#367 summon DT at glaaki"
                !r.allies.goos.any |=> -100000 -> "#538 DT: MUST summon at Glaaki location"

            // ── AWAKEN GLA'AKI ────────────────────────────────────────────────
            case AwakenMainAction(_, Glaaki, _) =>
                // [2026-04-01] NS: Glaaki killed mid-game — re-awaken ASAP (top priority)
                val glaakiWasKilled = !have(Glaaki) && !firstAP && !secondAP
                glaakiWasKilled |=> 50000 -> "#119 NS: re-awaken Glaaki ASAP after death"
                // AP1 GLAAKI: ONLY when GC has a lone cultist adjacent to our ocean gate
                val gcAdjacentTarget = GC.exists && !BG.exists && self.gates.%(_.glyph == Ocean)./~(_.near).%(n => GC.at(n).cultists.num == 1 && n.foes.monsterly.none && n.foes.goos.none).any
                firstAP && !gcAdjacentTarget |=> -1000000 -> "#14 AP1: build gate instead"
                // [2026-04-03] NS: vs YS, awaken Glaaki urgently — KIY is a GOO SB target
                YS.exists && !have(Glaaki) |=> 8000 -> "NS: vs YS, awaken Glaaki for KIY hunt"
                // AP2: ABSOLUTE PRIORITY — Glaaki must awaken NOW
                // [DEAD] secondAP && !have(Glaaki) && self.gates.%(_.glyph == Ocean).any |=> 5000 -> "#79 AP2: MUST awaken glaaki"
                // [DEAD] secondAP && !have(Glaaki) |=> 3000 -> "#86 AP2: glaaki is critical"
                // DH >= 7: Glaaki costs 0 power (after the 1-power floor) — ALWAYS awaken immediately
                dh >= 7 && !have(Glaaki) |=> 100000 -> "#114 DH >= 7: FREE Glaaki, awaken NOW"
                // [DEAD] dh >= 4 && !have(Glaaki) |=> 5000 -> "#151 DH >= 4: very cheap Glaaki"
                // Standard: awaken with 2 gates and ocean
                // [DEAD] self.gates.num >= 2 && self.gates.%(_.glyph == Ocean).any |=> 2996 -> "#192 2 gates + ocean: awaken glaaki now"
                // [DEAD] self.gates.%(_.glyph == Ocean).any |=> 1800 -> "#233 ocean gate: awaken glaaki"
                // [DEAD] !have(Glaaki) |=> 1095 -> "#283 need glaaki"
                // [DEAD] dh >= 2 && self.gates.%(_.glyph == Ocean).any |=> 1564 -> "#237 dh: cheap glaaki"

            case TSAwakenGlaakiChooseCostAction(_, gPower, dhCost) =>
                // ABSOLUTE BLOCK: no Glaaki in AP1 unless GC adjacent lone cultist
                val gcAdjacentCost = GC.exists && !BG.exists && self.gates.%(_.glyph == Ocean)./~(_.near).%(n => GC.at(n).cultists.num == 1 && n.foes.monsterly.none && n.foes.goos.none).any
                firstAP && !gcAdjacentCost |=> -1000000 -> "#73 AP1 COST: no GC target"
                // AP2: MUST awaken — replicate from AwakenMainAction
                secondAP && !have(Glaaki) |=> 100000 -> "#77 AP2 COST: MUST awaken glaaki"
                // DH >= 7: free Glaaki
                dh >= 7 && !have(Glaaki) |=> 100000 -> "#115 COST: DH >= 7 free glaaki"
                val remaining = self.power - gPower
                dhCost > 0 |=> dhCost * 150 -> "#439 prefer dh toward glaaki"
                // Glaaki is the engine; awaken ASAP at any stage
                remaining >= 3 |=> 1300 -> "#399 awaken glaaki plenty left"
                remaining >= 2 |=> 1500 -> "#257 awaken glaaki 2 left"
                remaining >= 1 && active.none |=> 1200 -> "#422 awaken glaaki last action"
                remaining >= 1 |=> 1000 -> "#295 awaken glaaki 1 left"
                remaining == 0 && active.none |=> 700 -> "#350 awaken glaaki no power left no others"
                remaining == 0 |=> 400 -> "#398 awaken glaaki spending all power"
                remaining < 0 |=> -86 -> "#462 cannot afford glaaki"

            case TSAwakenGlaakiPayAction(_, r, gPower, dhCost) =>
                // ABSOLUTE BLOCK: no Glaaki in AP1 unless GC adjacent lone cultist
                val gcAdjacentPay = GC.exists && !BG.exists && self.gates.%(_.glyph == Ocean)./~(_.near).%(n => GC.at(n).cultists.num == 1 && n.foes.monsterly.none && n.foes.goos.none).any
                // [RARELY] firstAP && !gcAdjacentPay |=> -1000000 -> "#74 AP1 PAY: no GC target, build gate"
                // AP2: MUST awaken
                secondAP && !have(Glaaki) |=> 100000 -> "#78 AP2 PAY: MUST awaken glaaki"
                // DH >= 7: free Glaaki
                dh >= 7 && !have(Glaaki) |=> 100000 -> "#116 PAY: DH >= 7 free glaaki"
                val remaining = self.power - gPower
                dhCost > 0 |=> dhCost * 150 -> "#439 prefer dh toward glaaki"
                remaining >= 3 |=> 400 -> "#399 awaken glaaki plenty left"
                remaining >= 2 |=> 1400 -> "#257 awaken glaaki 2 left"
                remaining >= 1 && active.none |=> 212 -> "#422 awaken glaaki last action"
                remaining >= 1 |=> 1400 -> "#295 awaken glaaki 1 left"
                remaining == 0 && active.none |=> 1200 -> "#350 awaken glaaki no power left no others"
                remaining == 0 |=> 1100 -> "#398 awaken glaaki spending all power"
                // [2026-04-02] NS3: prefer gate with adjacent undefended enemy gates + spare cultists
                val adjEnemyGates = glaakiGateScore(r)
                val spareCultistsHere = r.allies.cultists.num >= 2  // 2+ cultists = can bring 1 with Undulate
                // Undefended gates + spare cultists is the ideal (can Undulate carry)
                !firstAP && adjEnemyGates >= 2 && spareCultistsHere |=> 4000 -> "#25 NS3: 2+ adj enemy gates + spare cultists"
                !firstAP && adjEnemyGates >= 1 && spareCultistsHere |=> 3000 -> "#28 NS3: 1 adj enemy gate + spare cultists"
                // Undefended gates without spare cultists — still good but worse
                !firstAP && adjEnemyGates >= 2 |=> 2500 -> "#33 NS3: 2+ adj enemy gates, no spare cultists"
                !firstAP && adjEnemyGates >= 1 |=> 1500 -> "#46 NS3: 1 adj enemy gate, no spare cultists"
                r.ownGate |=> 1050 -> "#286 awaken at own gate"
                r.allies.cultists.any |=> 950 -> "#306 awaken with cultists"
                r.allies.monsterly.any |=> 900 -> "#312 awaken with monsters"
                r.near.%(_.ownGate).any |=> 800 -> "#330 awaken near own gate"
                r.foes.goos.active.any |=> -600 -> "#474 dont awaken into enemy goo"

            // ── ELEVEN REVELATIONS ────────────────────────────────────────────
            case TSElevenRevelationsMainAction(_) =>
                // [2026-04-04] tsTomesOnCard = # given away. Next tome = given + 1.
                val tomeNum    = game.tsTomesOnCard + 1
                val goodTargets = others.%(f => f.power >= 1 && !ofinale(f))
                // Tome XI (last given now): doom = ritualCost-5; at ritual cost 7+ that's 2+ doom for 1 power
                val xiValueTome   = tomeNum == 11 && game.ritualCost >= 7
                // Tomes IX-X: TS gains 1 ES (~1.67 doom) — always high value
                val esTome        = tomeNum >= 9 && tomeNum <= 10
                // Tomes VII-VIII: TS gains DH × TombHerds — great with many TH on map
                val dhBonusTomes  = tomeNum >= 7 && tomeNum <= 8 && tombsOnMap >= 3
                val dhTome        = tomeNum >= 7 && tomeNum <= 8
                // Tomes V-VI: 1 doom each; tomes III-IV: 1 DT; tomes I-II: 1 TH
                // Will any eligible target actually use the tome? If not, TS benefit never fires
                val likelyTomeUsers = goodTargets.%(f => BotCursedTome.willLikelyUseTome(f, tomeNum))
                // Veto: finalist may win because of the power boost
                // [2026-04-01] NS9: AP3 stuck at 1 power — give tomes to stall
                // [DEAD] thirdAP && power == 1 && goodTargets.any |=> 3000 -> "#101 NS9: AP3 at 1 power, give tomes to stall"
                // TC7: Stall with tomes when low power and enemies still active
                // Base: ElevenRevelations costs 1 power; need the tome value to justify it

            case TSElevenRevelationsAction(_, f) =>
                result ++= BotCursedTome.scoreGiveTomeTo(f)

            // ── SHEPHERD OF THE CRYPT ─────────────────────────────────────────
            case TSShepherdGatherMainAction(_) =>
                // Free power per Tomb-Herd is critical — enables Glaaki, rituals, expansion

            case TSShepherdGatherAction(_, r, _) =>
                // Prefer the area with the most Tomb-Herds
                self.at(r, TombHerd).num > 0 |=> self.at(r, TombHerd).num * 300 -> "#597 tomb herds here"

            // ── GRASPING DEAD ─────────────────────────────────────────────────
            case GraspingDeadMainAction(_) =>
                val tombsNearEnemies = areas.%(r => self.at(r, TombHerd).any && others.exists(_.at(r).any))
                // AP1 strategy: deny factions close to awakening their GOO by raiding their cultists.
                // GC needs 6p (2 gates), CC needs 10p (3+ gates), BG needs 4p (1-2 gates).
                // Hitting them in AP1 slows their awakening to AP3, letting TS awaken Glaaki in AP2
                // unopposed — and then exploit their post-awakening power drain in AP3.
                val gooDenialTargets = others.%(f => !f.goos.any && f.gates.num >= 2)
                val tombsNearDenialTargets = tombsNearEnemies.%(r => gooDenialTargets.exists(_.at(r).any))
                // Stall leaders who are close to winning
                tombsNearEnemies.any && others.%(ofinale).any |=> 2000 -> "#299 GD: stall finalist"
                // GD IS THE CORE ENGINE: free multi-area battles + DH + Green Decay captures
                // When DH can pay: GD is FREE (0 power cost). Use it EVERY chance.
                // Even at 1 power cost, GD with multiple battles is extremely efficient
                // [2026-04-01] NS11: when TS has 2+ more power, focus on 0-power GrspD with DH
                // [DEAD] fourthAPPlus && powerAdvantage && dh >= 1 && tombsNearEnemies.any |=> 6000 -> "#139 NS11: GD stall with DH, power advantage"
                // [DEAD] fourthAPPlus && dh >= 1 && tombsNearEnemies.any |=> 4500 -> "#158 NS11: AP4+ GD with DH to outlast"
                // Stall value

            case GraspingDeadPayPowerAction(_) =>
                dh < 2 |=> 1000 -> "#296 not enough dh, pay power"
                true |=> 1000 -> "#297 default pay power"

            case GraspingDeadPayDHAction(_) =>
                dh >= 4 |=> 1000 -> "#298 flush with dh"
                dh >= 2 |=> 900 -> "#314 have enough dh"
                dh < 2 |=> -1000000 -> "#540 no dh"

            case GraspingDeadBattleAction(_, r, f) =>
                val foes     = f.at(r)
                val allies   = self.at(r)
                val ownStr   = self.strength(allies, f)
                val enemyStr = f.strength(foes, self)
                val undefended = foes.cultists.any && foes.monsterly.none && foes.goos.none
                // GrspD Glaaki-adjacent tested negative (Cap -0.17). Removed.
                // Undefended cultists: free DH + capture (especially with Green Decay → ES)
                have(GreenDecay) && undefended |=> 800 -> "#331 GD: undefended cultists → ES via green decay"
                undefended      |=> 700 -> "#353 GD: undefended cultists → easy DH + capture"
                // Top priority: stall finalists, ideally at their gate
                ofinale(f) && r.gateOf(f) && ownStr >= enemyStr |=> 500 -> "#384 GD: stall finalist at their gate"
                ofinale(f) && ownStr >= enemyStr                 |=> 1000 -> "#299 GD: stall finalist"
                ofinale(f)                                       |=> 800  -> "#332 GD: contest finalist even if risky"
                // Gate fights remove cultists = DH + pressure
                r.gateOf(f) && ownStr >= enemyStr |=> 700 -> "#354 GD: gate fight for DH"
                r.gateOf(f)                       |=> 600  -> "#368 GD: threaten gate"
                // Winnable fights generate DH
                ownStr > enemyStr && foes.goos.none  |=> 900 -> "#315 GD: winning fight for DH"
                ownStr >= enemyStr                   |=> 400 -> "#400 GD: even fight for DH"

            // ── CURSED TOMES ──────────────────────────────────────────────────
            // TS normally doesn't hold cursed tomes (they're the issuer, not target)
            // but in edge cases a tome might end up back with TS
            case TSUseTomeAction(_, n) =>
                result ++= BotCursedTome.scoreUseTome(self, n)

            case TSSkipRemoveTomeAction(_) =>
                // Face-down tomes at game end LOSE doom — we want to remove them, not keep them
                // [DEAD] true |=> 200 -> "#428 keeping tomes loses doom at end"

            case TSRemoveTomeAction(_, _) =>
                // Removing face-down tomes prevents doom loss; always do this
                // [DEAD] true |=> 1598 -> "#236 remove tome prevents doom loss"

            // [2026-04-04] Tome unit placement — TS chooses which gate
            case TSPlaceTomeUnitAction(_, uc, r, _) =>
                // Prefer gates that need defense, or Glaaki's gate for Undulate combo
                r.allies.goos.any |=> 3000 -> "tome place: at Glaaki gate for Undulate"
                r.allies.monsterly.none && r.allies.cultists.any |=> 2500 -> "tome place: undefended gate"
                r.foes.active.any |=> 1500 -> "tome place: threatened gate"
                r.allies.cultists.num >= 3 |=> 2000 -> "tome place: fortress gate"
                true |=> 1000 -> "tome place: any gate"

            // ── UNDULATE (carry chain) ────────────────────────────────────────
            // Combo: Glaaki + DT + TombHerd in one area = up to 4 SBs from a single battle
            case TSUndulateCarryAction(_, u, _, to, _) =>
                val needBattleSB  = need(TSRollKill) || need(TSRoll3Pains) || need(TSTombHerdKilled)
                val needGlaakiGOO = need(TSGlaakiBattlesGOO)
                val undefendedDest = to.foes.cultists.any && to.foes.monsterly.none && to.foes.goos.none

                // [2026-04-02] NS4: AP2 carry cultist with Glaaki to enemy gate — beats DT summon (1400)
                // Triangle bonus: gate adjacent to both existing TS gates
                val carryTriangle = to.enemyGate && self.gates.num >= 2 && self.gates.forall(g => to.near.contains(g) || to == g)
                secondAP && u.cultist && to.enemyGate && to.foes.goos.none && carryTriangle |=> 8000 -> "#80 NS4: AP2 [UNDULATE] cultist to triangle gate"
                secondAP && u.cultist && to.enemyGate && to.foes.goos.none |=> 6000 -> "#83 NS4: AP2 [UNDULATE] cultist to raid"
                secondAP && tombsOnMap >= 3 && u.uclass == TombHerd && to.enemyGate && to.foes.goos.none |=> 7000 -> "#81 NS4: AP2 [UNDULATE] TH (3+ TH on map)"
                // NS4: AP2 always carry a cultist when Glaaki moves — even to non-gate destinations
                secondAP && u.cultist |=> 3000 -> "#87 NS4: AP2 [UNDULATE] always bring cultist"
                // [2026-04-02] NS: fortress-ready carry — must beat raw Glaaki move (10000)
                val carryFortressReady = have(Undulate) && glaakiHasDT && glaakiRegion.exists(gr => self.at(gr, TombHerd).num >= 2)
                // Needs to beat raw Glaaki fortress move (8800 max) by ~1000
                // [DEAD] carryFortressReady && have(GreenDecay) && to.foes.cultists.any && to.foes.goos.none |=> 9000 -> "#125 NS: fortress [UNDULATE] GD capture raid"
                carryFortressReady && to.enemyGate && to.foes.goos.none && to.foes.monsterly.none |=> 8000 -> "#135 NS: fortress [UNDULATE] gate theft"
                // AP4+ carry (fortress may not be ready yet)
                // [DEAD] fourthAPPlus && have(GreenDecay) && to.foes.cultists.any && to.foes.goos.none |=> 5500 -> "#141 NS10: AP4+ [UNDULATE] GD capture raid"
                // Carry Glaaki to an enemy GOO area — satisfies TSGlaakiBattlesGOO
                // Carry DT/TombHerd to Glaaki's area — assembles the combo
                // When combat is imminent (enemies at destination), boost: combo is about to fire
                have(Glaaki) && to.allies.goos.any && to.foes.active.any && u.uclass == DeepTendril |=> 400 -> "#401 carry DT to Glaaki combat area: combo fires now"
                have(Glaaki) && to.allies.goos.any && to.foes.active.any && u.uclass == TombHerd    |=> 200 -> "#429 carry TH to Glaaki combat area: combo fires now"
                have(Glaaki) && to.allies.goos.any && u.uclass == DeepTendril |=> 200 -> "#430 carry DT to glaaki: combo"
                have(Glaaki) && to.allies.goos.any && u.uclass == TombHerd    |=> 1000 -> "#300 carry TombHerd to glaaki: combo"
                // Carry Glaaki/DT into area with undefended cultists for Oleaginous+GreenDecay harvest
                have(GreenDecay) && have(Oleaginous) && u.uclass == DeepTendril && undefendedDest |=> 400 -> "#567 carry DT: oleaginous retreat + green decay harvest"
                // [2026-04-01] NS8: AP3 stuck at 2 power — Undulate DT+TH to weakest adjacent gate
                thirdAP && power <= 2 && tombsInPool == 0 && to.enemyGate && to.foes.goos.none && to.foes.monsterly.none |=> 5000 -> "#100 NS8: AP3 stuck at 2p, Undulate to weak gate"
                // Carry any unit into a fight where Glaaki is already present
                have(Glaaki) && to.allies.goos.any && to.foes.any |=> 300 -> "#413 carry to glaaki combat area"
                // [2026-04-01 09:55] v1.18.048: GATE THEFT CARRY — carry to enemy gate with lone cultist for capture+steal
                val gateTheftDest = to.enemyGate && to.foes.cultists.num == 1 && to.foes.monsterly.none && to.foes.goos.none
                gateTheftDest && have(GreenDecay) |=> 6000 -> "#140 gate theft carry: GDCY capture + steal gate"
                gateTheftDest |=> 4500 -> "#159 gate theft carry: capture + steal gate"
                // GREEN DECAY CARRY: carry to capture targets is high value (1.67 doom per capture)
                have(GreenDecay) && undefendedDest |=> 4000 -> "#169 GDCY carry: capture for ES"
                have(GreenDecay) && to.foes.cultists.num >= 2 && to.foes.goos.none |=> 5000 -> "#153 GD carry: multi-capture for ES"
                undefendedDest |=> 172 -> "#435 carry to undefended cultists: DH + capture"
                // Neutral monster carry scoring: Gug > cultist, 2-power below TH, 3-power below DT (except Voonith)
                (u.uclass == Gug) |=> 3500 -> "NU: carry Gug (above cultist)"
                (u.uclass == StarVampire) |=> 3200 -> "NU: carry Star Vampire (above cultist)"
                (u.uclass == Voonith) |=> 250 -> "NU: carry Voonith (above DT level)"
                (u.uclass == Ghast || u.uclass == DimensionalShamblerUnit || u.uclass == Gnorri) |=> 150 -> "NU: carry 2-power NM (below TH)"
                // Gate-stay rule: don't carry units away from own gate UNLESS good reason
                val fromOwnGate = self.gates.%(g => self.at(g).%(_ == u).any).any
                val udGDTarget = have(GreenDecay) && to.foes.cultists.any && to.foes.goos.none
                val udGOOTarget = need(TSGlaakiBattlesGOO) && to.foes.goos.any
                // [2026-04-02] NS: allow carry whenever gate keeps 1+ monster + 1+ cultist
                val gateRegion = self.gates.%(g => self.at(g).%(_ == u).any).headOption
                val gateKeepsDefense = gateRegion.exists(g => {
                    val remainingMonsters = self.at(g).%(_.monsterly).num - (if (u.monsterly) 1 else 0)
                    val remainingCultists = self.at(g).%(_.cultist).num - (if (u.cultist) 1 else 0)
                    remainingMonsters >= 1 && remainingCultists >= 1
                })
                // Gate with only 1 cultist: abandon is OK, prioritize Glaaki having units
                val gateHasOneCultist = gateRegion.exists(g => self.at(g).%(_.cultist).num == 1)
                val udFortressRaid = carryFortressReady && (to.foes.any || to.enemyGate) && gateKeepsDefense
                val canCarrySafely = gateKeepsDefense || gateHasOneCultist || !fromOwnGate || secondAP
                // debug removed
                fromOwnGate && !to.ownGate && !allEnemiesExhausted && !udGDTarget && !udGOOTarget && !gateTheftDest && !udFortressRaid && !canCarrySafely |=> -100000 -> "#539 undulate: stay at gate unless target"
                // Carry to own gate (consolidation)
                to.ownGate && to.allies.cultists.any |=> 1500 -> "#249 carry to own gate"
                // Carry to winnable fight for SB requirements
                needBattleSB && to.foes.any && to.foes.goos.none && to.allies.monsterly.any && !fromOwnGate |=> 2000 -> "#223 carry to fight for SB"
                // Carry to fight only if not abandoning a gate
                !fromOwnGate && to.foes.any && to.allies.monsterly.num > 1 |=> 1500 -> "#250 carry to fight"
                // [2026-04-03] NS: Undulate carry MUST beat ALL leaked movement scores
                // Leaked moves from Explode Skip path can score 8000+. Carry must be higher.
                u.is(DeepTendril) |=> 12000 -> "NS: [UNDULATE] carry DT first"
                u.is(TombHerd) |=> 11000 -> "NS: [UNDULATE] carry TH second"
                u.cultist |=> 10000 -> "NS: [UNDULATE] carry cultist last"
                true |=> -880 -> "#479 undulate: skip unless good reason"

            case TSUndulateSkipAction(_) =>
                true |=> -1500 -> "#495 skip undulate if no better"

            // ── ES / GENERAL ──────────────────────────────────────────────────
            case RevealESAction(_, es, false, _) if self.es != es =>
                true |=> -10000 -> "#519 better reveal all"

            case RevealESAction(_, _, _, _) =>
                allSB && realDoom >= 30 |=> 1000 -> "#301 reveal and win"
                // SBR PIVOT: reveal when TS aprxDoom > all others to end the game
                (allSB && tsAprxDoom > leadingFactionAprxDoom) |=> 10000 -> "SBR PIVOT: reveal ES, TS leads all factions"
                true |=> 300 -> "#414 dont reveal"
                // [DEAD] canRitual |=> 1195 -> "#275 ritual first"

            case ControlGateAction(_, r, u, _) =>
                // [NU-TEST 2026-04-04] Fixed infinite gate control swap loop with HPs
                // ORIGINAL: true |=> 1000000 -> "#106 always" (caused infinite HP/Acolyte oscillation)
                // Round 8 Bug 69: the previous "fix" was incomplete and still produced
                // an HP↔Acolyte oscillation that hit the 7000-turn safety guard. The
                // problem was that "keep acolyte on gate" was scored -5000 but "remain
                // calm" was scored -1000000, so when Acolyte was already on the gate,
                // the bot picked "swap to HP" (score -5000) over "remain calm" (-1000000)
                // because -5000 > -1000000. Then HP was on gate, so it picked "swap to
                // Acolyte" (+5000), then "swap to HP" again, forever. The fix is to
                // make BOTH no-op swap directions score -1000000 (i.e. tie with "remain
                // calm"), so the bot can never pick a swap that just reverses the
                // previous swap. The +5000 for "swap HP off gate" is preserved as the
                // ONE legitimate swap (HP→Acolyte is a one-time setup move, not a
                // recurring swap).
                r.allies.%(_.onGate).foreach { c =>
                    c.uclass == u.uclass |=> -1000000 -> "#541 remain calm: no swap needed"
                    // Prefer keeping Acolyte on gate (HP is more valuable to sacrifice)
                    c.uclass == HighPriest && u.uclass == Acolyte |=> 5000 -> "NU-TEST: swap HP off gate, acolyte controls"
                    // Don't swap Acolyte off for HP — match "remain calm" so the bot
                    // never picks a reverse-swap over a no-op (Bug 69).
                    c.uclass == Acolyte && u.uclass == HighPriest |=> -1000000 -> "NU-TEST: keep acolyte on gate"
                }
                true |=> 0 -> "#106 gate control base"

            case AbandonGateAction(_, _, _) =>
                true |=> -1000000 -> "#542 never"

            case ThousandFormsAskAction(f, r, offers, _, _, _, p) =>
                r < p + offers./(_.n).sum |=> -6*6*6*6*6*6 -> "dont overpay"
                p == -1 && power >= f.power + r && !f.allSB |=> (4*4*4*4*4*4 * Math.random()).round.toInt -> "refuse pay"
                p == 0 |=> (3*3*3*3*3*3 * Math.random()).round.toInt -> "pay 0"
                p == 1 |=> (2*3*3*3*3*3 * Math.random()).round.toInt -> "pay 1"
                p == 2 |=> (2*2*3*3*3*3 * Math.random()).round.toInt -> "pay 2"
                p == 3 |=> (2*2*2*3*3*3 * Math.random()).round.toInt -> "pay 3"
                p == 4 |=> (2*2*2*2*3*3 * Math.random()).round.toInt -> "pay 4"
                p == 5 |=> (2*2*2*2*2*3 * Math.random()).round.toInt -> "pay 5"
                p == 6 |=> (2*2*2*2*2*2 * Math.random()).round.toInt -> "pay 6"

            case GhrothAskAction(_, _, _, _, _, _, n) =>
                n == -1 |=> 200 -> "#431 refuse"
                n == 0 |=> 0 -> "#460 wait"

            case GhrothTargetAction(_, c, f, _) =>
                c.friends.cultists.none && c.region.capturers.%(!_.blind(f)).any |=> 1000 -> "#302 will be captured anyway"
                c.gateKeeper |=> -900 -> "#484 gate keeper"
                c.friends.cultists.none && c.region.capturers.any |=> 800 -> "#334 can be captured"
                // [RARELY] c.friends.cultists.any && c.region.capturers.any |=> 700 -> "#355 many can be captured"
                c.friends.goos.any |=> -500 -> "#473 goo huggers"

            case GiveWorstMonsterAskAction(_, _, uc, r, _) =>
                result = eval(SummonAction(self, uc, r))

            case GiveBestMonsterAskAction(_, _, uc, r, _) =>
                result = eval(SummonAction(self, uc, r))

            case _ =>
        }

        result
    }

    def evalNeutralUnit(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $
        implicit class condToEvalNU(val bool : Boolean) {
            def |=>(e : (Int, String)) { result +:= Evaluation(e._1, e._2) }
        }
        a.unwrap match {
            case RecruitAction(_, HighPriest, r) =>
                (game.turn <= 2) |=> -5000 -> "NU: dont recruit HP early"
                r.ownGate |=> 1000 -> "NU: recruit HP at gate"
                true |=> 500 -> "NU: recruit HP"
            case SacrificeHighPriestOutOfTurnMainAction(_) =>
                true |=> 800 -> "NU: HP out-of-turn"
            case LoyaltyCardSummonAction(_, uc, r) =>
                r.ownGate && self.at(r).goos.any |=> 2000 -> "NU: NM at Glaaki"
                r.ownGate |=> 1000 -> "NU: NM at gate"
                true |=> 500 -> "NU: NM place"
            case FreeSummonAction(_, _, r, _) =>
                r.ownGate |=> 1200 -> "NU: free summon gate"
                true |=> 800 -> "NU: free summon"
            case ShantakCarryCultistAction(_, o, ur, r) =>
                r.ownGate |=> 1500 -> "NU: shantak to gate"
                true |=> 800 -> "NU: shantak"
            case ShamblerDeployMainAction(_, _) =>
                true |=> 1500 -> "NU: deploy shambler"
            case ShamblerDeployAction(_, r, _) =>
                r.foes.cultists.any && r.foes.goos.none |=> 2000 -> "NU: shambler capture"
                r.ownGate |=> 1500 -> "NU: shambler gate"
                true |=> 800 -> "NU: shambler"
            case ShamblerSummonMainAction(_) =>
                true |=> 1000 -> "NU: summon shambler"
            case ShamblerSummonAction(_) =>
                true |=> 1000 -> "NU: summon shambler"
            case GodOfForgetfulnessMainAction(_, _, _) =>
                true |=> 1200 -> "NU: byatis pull"
            case GodOfForgetfulnessAction(_, d, r) =>
                r.foes.cultists.any |=> 1000 -> "NU: byatis pull"
                true |=> 500 -> "NU: byatis"
            case FilthMainAction(_, _) =>
                true |=> 800 -> "NU: filth"
            case FilthAction(_, r) =>
                r.enemyGate |=> 1200 -> "NU: filth gate"
                true |=> 600 -> "NU: filth"
            case NightmareWebMainAction(_, _) =>
                true |=> 1000 -> "NU: web"
            case NightmareWebAction(_, r) =>
                r.ownGate |=> 1500 -> "NU: web gate"
                true |=> 600 -> "NU: web"
            case TulzschaGivePowerMainAction(_) =>
                !self.hasAllSB |=> 1500 -> "NU: tulzscha SB"
                true |=> 500 -> "NU: tulzscha"
            case TulzschaGivePowerAction(_) =>
                true |=> 800 -> "NU: tulzscha"
            case CeremonyOfAnnihilationChoiceAction(_) =>
                self.power <= 2 |=> 1500 -> "NU: ceremony"
                true |=> 800 -> "NU: ceremony"
            case SummonAction(_, uc, r) if uc == StarVampire || uc == Gug =>
                r.ownGate |=> (if (uc == StarVampire) 9000 else 8500) -> "NU: summon above TH/DT"
                true |=> (if (uc == StarVampire) 5000 else 4500) -> "NU: summon"
            case SummonAction(_, uc, r) if uc.isInstanceOf[NeutralMonster] =>
                r.ownGate |=> 500 -> "NU: NM summon"
                true |=> 300 -> "NU: NM"
            case EliminateNoWayAction(_, u) if u.uclass == Ygolonac =>
                true |=> 6000 -> "NU: Ygolonac kill"
            case DemandSacrificeKillsArePainsAction(_) =>
                true |=> 500 -> "demand sacrifice: prefer pains over kills"

            case AvatarReplacementAction(_, _, r, o, u) =>
                val monstersHere = self.at(r).monsterly.num
                (monstersHere > 1 && u.uclass == TombHerd) |=> 5000 -> "TS avatar: give TH (>1 monster)"
                (monstersHere > 1 && u.uclass == DeepTendril) |=> 4000 -> "TS avatar: give DT (>1 monster, no TH)"
                (monstersHere <= 1 && u.cultist) |=> 5000 -> "TS avatar: give cultist (only 1 monster)"
                (monstersHere <= 1 && u.monsterly) |=> -3000 -> "TS avatar: keep monster (only 1)"
            case _ =>
        }
        result
    }

    def evalBattle(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val tombsOnMap = self.all(TombHerd).num

        // ── BATTLE ───────────────────────────────────────────────────────────
        if (game.battle.any) {
            if (game.battle./~(_.sides).has(self).not) {
                a match {
                    case _ =>
                }
            }
            else {
                implicit val battle = game.battle.get

                val allies   = self.forces
                val enemies  = self.opponent.forces

                def elim(u : UnitFigure) {
                    // [2026-04-01] NS: Kill priority: acolyte > TH > DT > Glaaki
                    // Exception: if TH kill SB needed and multiple TH, sacrifice 1 TH before acolyte
                    u.is(TombHerd) && need(TSTombHerdKilled) && tombsOnMap > 1 |=> 1200 -> "#273 sacrifice TH for SB: TSTombHerdKilled"
                    u.is(Acolyte)     |=> 700 -> "#356 elim acolyte"
                    u.is(TombHerd)    |=> 300 -> "#415 elim tombherd"
                    u.is(DeepTendril) |=> 100 -> "#444 elim deep tendril"
                    u.is(Glaaki)      |=>  50 -> "#448 elim glaaki last"
                }

                def retreat(u : UnitFigure) {
                    // [RARELY] u.gateKeeper && allies./(battle.canAssignPains).sum > 2 |=> -1000 -> "#489 retr gate keeper"
                    // Acolytes and cheap monsters retreat; Glaaki fights on
                    u.is(Acolyte)     |=> 800 -> "#335 retr acolyte"
                    u.is(TombHerd)    |=> 400 -> "#402 retr tombherd"
                    u.is(DeepTendril) |=> 200 -> "#432 retr deep tendril"
                    u.is(Glaaki)      |=> -200 -> "#465 dont retr glaaki"
                }

                a match {
                    case OleaginousRetreatAction(_, _, r) =>
                        // Glaaki/DeepTendril with Oleaginous: retreat to somewhere useful
                        // Green Decay + Oleaginous: retreat into capture zones for ES (avg 1.67 doom each)
                        // GREEN DECAY COMBO: retreat INTO cultist-rich region for mass capture
                        have(GreenDecay) && r.foes.cultists.num >= 3 && r.foes.goos.none |=> 5000 -> "oleaginous+GD: mass capture 3+ cultists"
                        have(GreenDecay) && r.foes.cultists.num >= 2 && r.foes.goos.none |=> 3000 -> "oleaginous+GD: capture 2 cultists"
                        have(GreenDecay) && r.foes.cultists.any && r.foes.goos.none |=> 2000 -> "#224 oleaginous+GD: capture for ES"
                        r.foes.cultists.any && r.foes.goos.none |=> 500 -> "#385 oleaginous to capture"
                        // Retreat to own gate is OK but not priority
                        r.ownGate |=> 500 -> "#386 oleaginous to own gate"
                        r.near.%(_.ownGate).any |=> 300 -> "#416 oleaginous near own gate"

                    case DevourAction(_, u) =>
                        elim(u)

                    case AssignKillAction(_, _, _, u) =>
                        elim(u)

                    case AssignPainAction(_, _, _, u) =>
                        retreat(u)

                    case EliminateNoWayAction(_, u) =>
                        elim(u)

                    case RetreatUnitAction(_, u, r) =>
                        // NEVER retreat cultist to area with enemy monster (will be captured)
                        u.cultist && r.foes.monsterly.any |=> -5000 -> "never retreat cultist to enemy monster"
                        u.cultist && r.foes.goos.any |=> -5000 -> "never retreat cultist to enemy GOO"
                        // Retreat cultist to safety (own gate, allies, Glaaki)
                        u.cultist && r.ownGate |=> 2000 -> "cultist to own gate"
                        u.cultist && r.allies.goos.any |=> 3000 -> "#191 cultist to glaaki"
                        u.cultist && r.allies.monsterly.any |=> 1500 -> "cultist to monsters"
                        u.cultist && r.foes.none && r.freeGate |=> 2500 -> "cultist to free gate"
                        // [2026-04-02] NS: TH pain retreat — home base (own gate with cultists) is top priority
                        val homeBase = r.ownGate && r.allies.cultists.any
                        val retreatCultistBonus = if (r.foes.cultists.any) (5 - r.foes.cultists.num).max(0) * 5 else 0
                        // With Undulate just earned: home base wins strongly (can carry on next move)
                        u.is(TombHerd) && homeBase && have(Undulate) |=> 5000 -> "NS: TH pain [TO HOME] Undulate ready"
                        u.is(TombHerd) && homeBase |=> 4000 -> "NS: TH pain [TO HOME]"
                        // Without Undulate: prefer enemy gates adjacent to own cultists, fewest cultists
                        val vulnerableEnemyGate = r.enemyGate && r.foes.cultists.num <= 2 && r.foes.monsterly.none && r.foes.goos.none
                        val adjacentToOwnCultists = r.near.%(n => n.ownGate || n.allies.cultists.any).any
                        u.is(TombHerd) && !have(Undulate) && vulnerableEnemyGate && adjacentToOwnCultists |=> 3500 + retreatCultistBonus -> "NS: TH pain [TO ENEMY GATE] near own cultists"
                        u.is(TombHerd) && vulnerableEnemyGate && adjacentToOwnCultists |=> 3000 + retreatCultistBonus -> "#556 TH retreats to vulnerable enemy gate near cultists"
                        // Don't retreat to enemy gate NOT adjacent to own cultists
                        u.is(TombHerd) && vulnerableEnemyGate && !adjacentToOwnCultists |=> 500 -> "#387 TH to enemy gate but no cultists nearby"
                        u.is(TombHerd) && r.ownGate && r.allies.cultists.num == 1 |=> 3000 -> "#557 TH retreats to defend lone gatekeeper"
                        // Glaaki retreats
                        u.is(Glaaki) && vulnerableEnemyGate && have(GreenDecay) |=> 4000 -> "#553 glaaki retreats to GD capture target"
                        u.is(Glaaki) && vulnerableEnemyGate |=> 3000 -> "#558 glaaki retreats to vulnerable gate"
                        u.is(Glaaki) && r.ownGate |=> 2000 -> "#561 glaaki retreats to own gate"
                        u.monsterly && r.allies.%(_.capturable).any && r.foes.goos.none |=> 1000 -> "#303 monster prevent capture"
                        u.monsterly && r.allies.goos.any |=> 500 -> "#388 monster to goo"
                        u.monsterly && r.ownGate |=> 1000 -> "#304 monster to own gate"
                        u.monsterly && r.freeGate |=> 300 -> "#417 monster to free gate"

                        u.goo && r.ownGate |=> 400 -> "#403 goo to own gate"
                        u.goo && r.freeGate |=> 200 -> "#433 goo to free gate"

                        if (u.goo)
                            result ++= eval(MoveAction(u.faction, u, u.region, r, 0))

                        true |=> r.connected.distinct.num -> "#461 reachable regions"

                    case _ =>
                }
            }
        }

        result
    }
}
