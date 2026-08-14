package cws

import hrf.colmat._

import html._


// Tombstalker (TS) UNITS: TombHerd (monster, placed via Death March from Death's Head),
// DeepTendril (monster, combat bonus in ocean and with Gla'aki), Gla'aki (GOO, combat = Deep Tendrils in play)
case object TombHerd extends FactionUnitClass(TS, "Tomb-Herd", Monster, 2)
case object DeepTendril extends FactionUnitClass(TS, "Deep Tendril", Monster, 3)
case object Glaaki extends FactionUnitClass(TS, "Gla'aki", GOO, 7)

// Tombstalker (TS) SPELLBOOKS: Death March (unique ability), Eleven Revelations, Oleaginous,
// Grasping Dead, Hecatomb, Green Decay, Undulate
case object DeathMarch extends FactionSpellbook(TS, "Death March")
case object ElevenRevelations extends FactionSpellbook(TS, "Eleven Revelations")
case object Oleaginous extends FactionSpellbook(TS, "Oleaginous") with BattleSpellbook
case object GraspingDead extends FactionSpellbook(TS, "Grasping Dead")
case object Hecatomb extends FactionSpellbook(TS, "Hecatomb")
case object GreenDecay extends FactionSpellbook(TS, "Green Decay")
case object Undulate extends FactionSpellbook(TS, "Undulate")

// Tombstalker (TS) SPELLBOOK REQUIREMENTS: conditions checked by TSExpansion.triggers()
case object TSAwakenGlaaki extends Requirement("Awaken Gla'aki")
case object TSTombHerdKilled extends Requirement("Tomb-Herd Killed")
case object TSRollKill extends Requirement("Roll a Kill")
case object TSRoll3Pains extends Requirement("Roll 3 Pains")
case object TSGlaakiBattlesGOO extends Requirement("Gla'aki battles GOO")
case object TSRitualOrEnemyGate extends Requirement("Ritual OR Control Enemy Gate")


// Tombstalker (TS) FACTION OBJECT: defines faction identity, unit roster,
// awaken costs (Gla'aki uses Death's Head to supplement power), and combat strength formula
case object TS extends Faction { f =>
    def name = "Tombstalker"
    def short = "TS"
    def style = "ts"

    override def abilities = $(DeathMarch)
    override def library = $(ElevenRevelations, Oleaginous, GraspingDead, Hecatomb, GreenDecay, Undulate)
    override def requirements(options : $[GameOption]) = $(TSAwakenGlaaki, TSTombHerdKilled, TSRollKill, TSRoll3Pains, TSGlaakiBattlesGOO, TSRitualOrEnemyGate)

    val allUnits =
        1.times(Glaaki) ++
        3.times(DeepTendril) ++
        6.times(TombHerd) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case Glaaki => (f.gates.has(r) && f.gates.exists(_.glyph == Ocean)).?(max(1, 7 - game.deathsHead))
        case _ => None
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        val arena = game.battle./(_.arena)
        // During actual battle, use arena; during bot evaluation (no battle), infer from units' location
        val onOcean      = arena./(_.glyph == Ocean).|(units.%(_.region.glyph == Ocean).any)
        val glaakiInArea = arena./(r => f.at(r, Glaaki).not(Zeroed).any).|(units.goos.any)
        val tombHerdCount    = units(TombHerd).not(Zeroed).num
        val deepTendrilCount = units(DeepTendril).not(Zeroed).num
        val glaakiCount      = units(Glaaki).not(Zeroed).num
        // Gla'aki: 2 combat per Deep Tendril in play anywhere on the map (not just in this battle)
        val deepTendrilsInPlay = f.onMap(DeepTendril).not(Zeroed).num

        (tombHerdCount > 0).?(3).|(0) +
        deepTendrilCount * (1 + glaakiInArea.??(1) + onOcean.??(1)) +
        glaakiCount * deepTendrilsInPlay * 2 +
        neutralStrength(units, opponent)
    }
}


// Tombstalker (TS) ACTION CLASSES: data types for all TS-specific game actions.

// DEATH MARCH DOOM ACTIONS
case class TSDeathMarchDoomAction(self : Faction, dh : Int) extends OptionFactionAction("Death March: place " + TombHerd.styled(TS) + " (" + dh.toString.styled("kill") + " Death's Head remaining)") with DoomQuestion with Soft with PowerNeutral
case class TSDeathMarchAction(self : Faction, r : Region) extends BaseFactionAction("Place " + TombHerd.styled(TS) + " with Death's Head in", r) with PowerNeutral
case class TSDeathMarchDoneAction(self : Faction) extends BaseFactionAction(None, "Done".styled("power")) with PowerNeutral

// HECATOMB DOOM ACTION (ritual with DH supplement: pick power/DH combo)
case class TSHecatombRitualCostAction(self : Faction, power : Int, dh : Int) extends OptionFactionAction(
    "Ritual of Annihilation".styled("doom") + ": " + power.power + (dh > 0).?(" + " + dh.toString.styled("kill") + " Death's Head").|(s"")
) with DoomQuestion

// SHEPHERD OF THE CRYPT (Gather Power Phase)
case class TSShepherdGatherMainAction(self : Faction) extends OptionFactionAction("Shepherd of the Crypt: gain " + 1.power + " per " + TombHerd.styled(TS)) with MainQuestion with PowerNeutral
case class TSShepherdGatherPhaseAction(self : Faction, remaining : $[Region]) extends ForcedAction with PowerNeutral
// [2026-04-01 18:08] Show actual power gain per region
case class TSShepherdGatherAction(self : Faction, r : Region, remaining : $[Region]) extends BaseFactionAction(
    implicit g => "Gain " + self.at(r, TombHerd).num.power + " from " + self.at(r, TombHerd).num + " " + TombHerd.styled(TS) + " in", r) with PowerNeutral

// ELEVEN REVELATIONS (Give a Tome to enemy, cost 1)
case class TSElevenRevelationsMainAction(self : Faction) extends OptionFactionAction(ElevenRevelations) with MainQuestion with Soft
case class TSElevenRevelationsAction(self : Faction, f : Faction) extends BaseFactionAction("Give topmost " + "Cursed Tome".styled(TS) + " to", f)

// TOME ACTIONS (for any faction that holds a face-up tome)
case class TSUseTomeAction(self : Faction, tomeNum : Int) extends OptionFactionAction("Cursed Tome".styled(TS) + " Vol. " + tomeNumToRoman(tomeNum)) with MainQuestion with PowerNeutral

// CURSED TOMES - ritual removal (optional, for any faction with face-down tomes)
case class TSRemoveTomeAction(self : Faction, tomeNum : Int) extends OptionFactionAction("Remove face-down " + "Cursed Tome".styled(TS) + " Vol. " + tomeNumToRoman(tomeNum) + " from game") with PowerNeutral {
    def question(implicit game : Game) = "Ritual of Annihilation"
}
case class TSSkipRemoveTomeAction(self : Faction) extends OptionFactionAction("Keep face-down " + "Cursed Tomes".styled(TS)) with Soft with PowerNeutral {
    def question(implicit game : Game) = "Ritual of Annihilation"
}

// [2026-04-04] TOME UNIT PLACEMENT — TS chooses which gate to place TH/Tendril from tomes 1-4
case class TSPlaceTomeUnitAction(self : Faction, uc : UnitClass, r : Region, tomeNum : Int) extends BaseFactionAction("Place " + uc.styled(TS) + " from Tome " + tomeNumToRoman(tomeNum) + " at", r)

// UNDULATE (carry chain: moved unit can carry lesser-cost units for free)
case class TSUndulateCarryPhaseAction(self : Faction, from : Region, to : Region, carrierCost : Int) extends ForcedAction with PowerNeutral
case class TSUndulateCarryAction(self : Faction, u : UnitRef, from : Region, to : Region, newCarrierCost : Int) extends BaseFactionAction(
    (g : Game) => "Undulate: carry " + g.unit(u).full + " (cost " + u.uclass.cost + ") for free from " + from + " to",
    to
) with PowerNeutral
// [2026-04-03] Removed Soft — prevents Explode from following Skip into movement menu, which leaked MoveActions into carry decisions
case class TSUndulateSkipAction(self : Faction) extends BaseFactionAction(None, "Skip " + "Undulate".styled(TS) + " carry") with PowerNeutral

// AWAKEN GLA'AKI (two-step: pick cost, then pick region)
case class TSAwakenGlaakiChooseCostAction(self : Faction, power : Int, dh : Int) extends BaseFactionAction(
    "Awaken " + Glaaki.styled(TS) + ": " + power.power + (dh > 0).?(" + " + dh.toString.styled("kill") + " Death's Head").|(""),
    (dh > 0).?(s"($power power + $dh DH = 6)").|(s"$power power")) with Soft
case class TSAwakenGlaakiChooseRegionAction(self : Faction, power : Int, dh : Int) extends ForcedAction
case class TSAwakenGlaakiPayAction(self : Faction, r : Region, power : Int, dh : Int) extends BaseFactionAction(
    implicit g => "Awaken " + Glaaki.styled(TS) + " in",
    implicit g => r + self.iced(r)
)

// GRASPING DEAD
case class GraspingDeadMainAction(self : Faction) extends OptionFactionAction(GraspingDead) with MainQuestion with Soft
case class GraspingDeadPayDHAction(self : Faction) extends BaseFactionAction(GraspingDead, "Spend 2 Death's Head".styled("kill") + " instead of " + 1.power)
case class GraspingDeadPayPowerAction(self : Faction) extends BaseFactionAction(GraspingDead, "Spend " + 1.power)
case class GraspingDeadChooseRegionAction(self : Faction) extends ForcedAction with PowerNeutral
case class GraspingDeadBattleAction(self : Faction, r : Region, f : Faction) extends BaseFactionAction(GraspingDead.toString + " battle " + f + " with " + TombHerd.styled(TS) + " in", r)


// Tombstalker (TS) EXPANSION OBJECT: manages all TS-specific game state (Death March, Hecatomb,
// Grasping Dead, Shepherd, Eleven Revelations, Undulate, tomes) and action dispatch
object TSExpansion extends Expansion {
    // Sentinel: prevents Shepherd of the Crypt from firing twice in one power-gather phase
    var shepherdDoneThisGather : Boolean = false
    // [2026-04-03] Pure DH hecatomb tracking
    var pureDHRitualsDone : Int = 0              // count of pure DH rituals performed
    var pureDHMarkerIndices : $[Int] = $         // ritual marker index at time of each pure DH ritual

    // Regions remaining for Grasping Dead chain (reset after all regions battled)
    var graspingDeadRemaining : $[Region] = $()
    // Regions already fought this GD activation (to avoid re-fighting)
    var graspingDeadFought : $[Region] = $()
    // Whether a GD activation is currently in progress
    var graspingDeadActive : Boolean = false

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        // Death March: increment Death's Head when enemy unit dies in battle
        if (u.faction != TS && u.region.onMap && game.battle.any) {
            game.deathsHead += 1
            if (game.factions.has(TS))
                TS.log("Death March: Death's Head is now", game.deathsHead.toString.styled("kill"))
        }

        // TSTombHerdKilled requirement
        if (u.uclass == TombHerd && game.battle.any && game.factions.has(TS))
            TS.satisfy(TSTombHerdKilled, "Tomb-Herd killed in battle")
    }

    override def triggers()(implicit game : Game) {
        // TSRitualOrEnemyGate: check if TS has performed a ritual OR controls an enemy start area gate
        if (game.factions.has(TS))
            TS.satisfyIf(TSRitualOrEnemyGate, "Ritual or Control enemy start gate",
                game.ritualHistory.has(TS) ||
                game.factions.but(TS).exists(f => game.starting.contains(f) && TS.gates.has(game.starting(f))))
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {

        // UNDULATE (carry chain after a TS move)
        case MovedAction(self, u, from, to) if self == TS && self.has(Undulate) =>
            val carrierCost = u.uclass.cost
            val eligible = self.at(from).not(Moved).%(v => v.uclass.cost > 0 && v.uclass.cost < carrierCost)
            // debug removed
            if (eligible.any)
                Force(TSUndulateCarryPhaseAction(self, from, to, carrierCost))
            else
                MoveContinueAction(self, true)

        case TSUndulateCarryPhaseAction(self, from, to, carrierCost) =>
            // Round 8: respect canBeMoved (matches Arctic Wind / Beyond One pattern).
            // Byatis has canBeMoved = false (see IGOOs.scala line 41) and must NOT be
            // carried by Undulate, just like other movement abilities exclude it.
            val eligible = self.at(from).not(Moved).%(v => v.uclass.cost > 0 && v.uclass.cost < carrierCost && v.uclass.canBeMoved(v))
            if (eligible.any) {
                implicit val asking = Asking(self)
                eligible.sortBy(v => -v.uclass.cost).foreach { v =>
                    + TSUndulateCarryAction(self, v.ref, from, to, v.uclass.cost)
                }
                + TSUndulateSkipAction(self)
                asking
            } else
                MoveContinueAction(self, true)

        case TSUndulateCarryAction(self, u, from, to, newCarrierCost) =>
            u.region = to
            u.add(Moved)
            u.add(MovedForFree)
            self.log("Undulate: carried", u, "for free from", from, "to", to)
            Force(TSUndulateCarryPhaseAction(self, from, to, newCarrierCost))

        case TSUndulateSkipAction(self) =>
            MoveContinueAction(self, true)

        // SHEPHERD OF THE CRYPT (Gather Power Phase)
        case PowerGatherAction(_) if game.factions.has(TS) =>
            TSExpansion.shepherdDoneThisGather = false
            UnknownContinue

        case AfterPowerGatherAction =>
            if (game.factions.has(TS) && TS.has(Glaaki) && TS.onMap(TombHerd).any && !TSExpansion.shepherdDoneThisGather) {
                val regions = areas.%(r => TS.at(r, TombHerd).any)
                if (regions.any) {
                    TSExpansion.shepherdDoneThisGather = true
                    Force(TSShepherdGatherPhaseAction(TS, regions))
                } else
                    UnknownContinue
            }
            else
                UnknownContinue

        case TSShepherdGatherPhaseAction(self, remaining) =>
            if (remaining.any) {
                implicit val asking = Asking(self)
                remaining.foreach { r => + TSShepherdGatherAction(self, r, remaining.but(r)) }
                asking
            }
            else
                Force(AfterPowerGatherAction)  // re-dispatch; guard now false → Game.scala handler runs

        case TSShepherdGatherAction(self, r, remaining) =>
            val n = self.at(r, TombHerd).num
            self.power += n
            self.log("Shepherd of the Crypt: gained", n.power, "from", n, TombHerd.styled(TS), "in", r)
            Force(AfterPowerGatherAction)  // done; sentinel already set so Game.scala handler runs

        // DOOM PHASE
        case DoomAction(f) if f == TS =>
            // MANDATORY: Death March must place ALL TH from pool before any other doom options
            if (game.deathsHead > 0 && f.pool(TombHerd).any && areas.any)
                Force(TSDeathMarchDoomAction(f, game.deathsHead))
            else {
                implicit val asking = Asking(f)

                // Ritual options: Hecatomb DH combos only when all Tomb-Herds are on map (pool empty)
                if (f.has(Hecatomb) && game.deathsHead > 0 && f.pool(TombHerd).none && f.gates.any && f.acted.not) {
                    val cost = f.has(Herald).?(5).|(game.ritualCost)
                    val minPower = max(0, cost - game.deathsHead)
                    val maxPower = min(cost, f.power)
                    (minPower to maxPower).foreach(p => + TSHecatombRitualCostAction(f, p, cost - p))
                } else {
                    game.rituals(f)
                }

                game.reveals(f)

                game.highPriests(f)

                game.hires(f)

                + DoomDoneAction(f)

                asking
            }

        case TSDeathMarchDoomAction(self, _) =>
            // MANDATORY: must place TH, no cancel option
            Ask(self).each(areas)(r => TSDeathMarchAction(self, r))

        case TSDeathMarchAction(self, r) =>
            game.deathsHead -= 1
            self.place(TombHerd, r)
            self.log("Death March: placed", TombHerd.styled(TS), "in", r, "(Death's Head now", game.deathsHead.toString.styled("kill") + ")")

            // If Hecatomb is available and DH > 0, can spend remainder toward ritual after placing all Tomb-Herds
            // Continue placing if DH remains and Tomb-Herds remain in pool
            if (game.deathsHead > 0 && self.pool(TombHerd).any)
                Force(TSDeathMarchDoomAction(self, game.deathsHead))
            else
                Force(TSDeathMarchDoneAction(self))

        case TSDeathMarchDoneAction(self) =>
            // When Hecatomb is active, DH persists until DoomDoneAction (player may use it toward ritual)
            if (!self.has(Hecatomb))
                game.deathsHead = 0
            CheckSpellbooksAction(DoomAction(self))

        case TSHecatombRitualCostAction(self, power, dh) =>
            game.deathsHead -= dh
            if (dh > 0)
                self.log("Hecatomb: using", dh.toString.styled("kill"), "Death's Head toward Ritual of Annihilation")

            if (power == 0) {
                // [2026-04-03] PURE DH HECATOMB: copies standard ritual logic from Game.scala
                // EXCEPT: does not advance ritualMarker, does not add to ritualHistory
                val brood = self.enemies.%(_.has(TheBrood))
                val gates = self.allGates
                val valid = gates.%!(r => brood.exists(_.at(r)(Filth).any))
                val doom = valid.num
                val es = self.goos.factionGOOs.num + self.has(Consecration).??($(0, 1, 1, 1, 2)(game.cathedrals.num))

                // Exactly match standard ritual: power, doom, log, ES, acted
                // power already 0, no deduction needed
                self.doom += doom
                game.appendLog($(CthulhuWarsSolo.DottedLine))
                self.log("performed Hecatomb ritual (pure DH, no track advance)", "and gained", doom.doom, (es > 0).??("and " + es.es))
                self.takeES(es)
                self.acted = true

                // Do NOT add to ritualHistory (no standard glyph placement)
                // Do NOT advance ritualMarker

                // Track pure-DH ritual for overlay glyph placement
                // Store the ritual marker index at time of ritual — overlay will calculate position
                TSExpansion.pureDHRitualsDone += 1
                TSExpansion.pureDHMarkerIndices :+= game.ritualMarker

                game.triggers()
                game.showROAT()

                self.satisfy(PerformRitual, "Perform Ritual of Annihilation (Hecatomb pure DH)")

                // Continue doom phase — exactly as standard ritual does for TS (no tome penalty)
                CheckSpellbooksAction(DoomAction(self))
            }
            else
                Force(RitualAction(self, power, 1))

        // MAIN ACTIONS
        case MainAction(f) if f == TS && f.active.not =>
            UnknownContinue

        case MainAction(f) if f == TS && f.acted =>
            UnknownContinue

        case MainAction(f) if f == TS =>
            implicit val asking = Asking(f)

            game.moves(f)

            game.captures(f)

            game.recruits(f)

            game.battles(f)

            game.controls(f)

            game.builds(f)

            game.summons(f)

            game.awakens(f)

            game.independents(f)

            // ELEVEN REVELATIONS: give topmost tome to enemy (cost 1)
            // [2026-04-04] tsTomesOnCard now tracks # given away (0=none, 11=all)
            if (f.can(ElevenRevelations) && game.tsTomesOnCard < 11 && f.enemies.any && f.power >= 1)
                + TSElevenRevelationsMainAction(f)

            // GRASPING DEAD: battle with only Tomb-Herds
            if (f.has(GraspingDead) && f.onMap(TombHerd).any &&
                areas.%(r => f.at(r, TombHerd).any && f.enemies.exists(_.at(r).any)).any &&
                (f.power >= 1 || game.deathsHead >= 2))
                + GraspingDeadMainAction(f)

            game.neutralSpellbooks(f)

            game.libraryActions(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            asking

        // AWAKEN GLA'AKI — step 1: pick cost combination
        case AwakenMainAction(self, Glaaki, locations) if self == TS =>
            // Compute the range of valid power values across all eligible regions
            // [2026-04-01 18:12] Fixed: was max(1,...) which blocked 0-power awakening with 6+ DH
            // [2026-05-08] Glaaki cost bumped from 6 to 7 — DH threshold for free awakening shifts to 7.
            val minPower = max(0, 7 - game.deathsHead)
            val maxPower = locations./(r => min(7, self.power - self.taxIn(r))).maxOr(0)
            Ask(self)
                .list((minPower to maxPower)./(power => TSAwakenGlaakiChooseCostAction(self, power, 7 - power)))
                .cancel

        // AWAKEN GLA'AKI — step 2: pick region
        case TSAwakenGlaakiChooseCostAction(self, power, dh) =>
            Force(TSAwakenGlaakiChooseRegionAction(self, power, dh))

        case TSAwakenGlaakiChooseRegionAction(self, power, dh) =>
            val eligible = areas.%(r => self.gates.has(r) && self.gates.exists(_.glyph == Ocean) && self.affords(power)(r) && self.pool(Glaaki).any)
            Ask(self).list(eligible./(r => TSAwakenGlaakiPayAction(self, r, power, dh))).cancel

        case TSAwakenGlaakiPayAction(self, r, power, dh) =>
            self.power -= power
            self.payTax(r)
            game.deathsHead -= dh
            self.place(Glaaki, r)
            self.log("awakened", Glaaki.styled(TS), "in", r, "for", power.power + (dh > 0).?(" + " + dh.toString.styled("kill") + " Death's Head").|(""))
            self.satisfy(TSAwakenGlaaki, "Awaken Gla'aki")
            EndAction(self)

        // ELEVEN REVELATIONS
        case TSElevenRevelationsMainAction(self) =>
            Ask(self).each(self.enemies)(f => TSElevenRevelationsAction(self, f)).cancel

        case TSElevenRevelationsAction(self, f) =>
            self.power -= 1
            // [2026-04-04] Reversed: give tomes I→XI (ascending). tsTomesOnCard tracks # given away.
            game.tsTomesOnCard += 1
            val tomeNum = game.tsTomesOnCard
            // Add face-up tome (false = face-up) to target faction
            val existing = game.cursedTomesOwned.get(f).|(Nil)
            game.cursedTomesOwned = game.cursedTomesOwned + (f -> (existing :+ (tomeNum, false)))
            self.log("gave", "Cursed Tome".styled(TS), "Volume", tomeNumToRoman(tomeNum), "to", f)
            f.log("received", "Cursed Tome".styled(TS), "Volume", tomeNumToRoman(tomeNum), "from", self)
            EndAction(self)

        // TOME USAGE
        case TSUseTomeAction(self, tomeNum) =>
            // Flip tome face-down
            val existing = game.cursedTomesOwned.get(self).|(Nil)
            val updated = existing.map { case (n, faceDown) => if (n == tomeNum && !faceDown) (n, true) else (n, faceDown) }
            game.cursedTomesOwned = game.cursedTomesOwned + (self -> updated)

            // Self gets 1 power regardless of tome number
            self.power += 1
            self.log("used", "Cursed Tome".styled(TS), "Volume", tomeNumToRoman(tomeNum), "and gained", 1.power)

            // Apply tome effect
            val ts = TS
            tomeNum match {
                case 1 | 2 =>
                    // [2026-04-04] TS chooses which gate to place Tomb-Herd
                    val gates = ts.gates.onMap.distinct
                    if (ts.pool(TombHerd).any && gates.any)
                        Ask(ts).each(gates)(r => TSPlaceTomeUnitAction(ts, TombHerd, r, tomeNum))
                    else
                        Force(EndAction(self))
                case 3 | 4 =>
                    // [2026-04-04] TS chooses which gate to place Deep Tendril
                    val gates = ts.gates.onMap.distinct
                    if (ts.pool(DeepTendril).any && gates.any)
                        Ask(ts).each(gates)(r => TSPlaceTomeUnitAction(ts, DeepTendril, r, tomeNum))
                    else
                        Force(EndAction(self))
                case 5 | 6 =>
                    ts.doom += 1
                    ts.log("gained", 1.doom, "from Tome", tomeNumToRoman(tomeNum))
                    EndAction(self)
                case 7 | 8 =>
                    val dhGain = ts.onMap(TombHerd).num
                    game.deathsHead += dhGain
                    ts.log("gained", dhGain.toString.styled("kill"), "Death's Head from Tome", tomeNumToRoman(tomeNum))
                    EndAction(self)
                case 9 | 10 =>
                    ts.takeES(1)
                    ts.log("gained", 1.es, "from Tome", tomeNumToRoman(tomeNum))
                    EndAction(self)
                case 11 =>
                    val doomGain = max(0, game.ritualCost - 5)
                    ts.doom += doomGain
                    ts.log("gained", doomGain.doom, "from Tome XI (Ritual Cost", game.ritualCost.toString, "minus 5)")
                    EndAction(self)
                case _ =>
                    EndAction(self)
            }

        // [2026-04-04] Handle TS's choice of gate for tome unit placement
        case TSPlaceTomeUnitAction(self, uc, r, tomeNum) =>
            self.place(uc, r)
            self.log("placed", uc.styled(TS), "at", r, "from Tome", tomeNumToRoman(tomeNum))
            EndAction(self)

        // CURSED TOME - ritual removal
        case TSRemoveTomeAction(self, tomeNum) =>
            val existing = game.cursedTomesOwned.get(self).|(Nil)
            game.cursedTomesOwned = game.cursedTomesOwned + (self -> existing.filterNot { case (n, fd) => n == tomeNum && fd })
            self.log("removed face-down", "Cursed Tome".styled(TS), "Volume", tomeNumToRoman(tomeNum), "from game")
            CheckSpellbooksAction(DoomAction(self))

        case TSSkipRemoveTomeAction(self) =>
            CheckSpellbooksAction(DoomAction(self))

        // GRASPING DEAD
        case GraspingDeadMainAction(self) =>
            implicit val asking = Asking(self)
            if (self.power >= 1)
                + GraspingDeadPayPowerAction(self)
            if (game.deathsHead >= 2)
                + GraspingDeadPayDHAction(self)
            asking

        case GraspingDeadPayPowerAction(self) =>
            self.power -= 1
            TSExpansion.graspingDeadRemaining = areas.%(r => self.at(r, TombHerd).any && self.enemies.exists(_.at(r).any))
            TSExpansion.graspingDeadFought = $()
            TSExpansion.graspingDeadActive = true
            Force(GraspingDeadChooseRegionAction(self))

        case GraspingDeadPayDHAction(self) =>
            game.deathsHead -= 2
            self.log("spent 2 Death's Head for", GraspingDead)
            TSExpansion.graspingDeadRemaining = areas.%(r => self.at(r, TombHerd).any && self.enemies.exists(_.at(r).any))
            TSExpansion.graspingDeadFought = $()
            TSExpansion.graspingDeadActive = true
            Force(GraspingDeadChooseRegionAction(self))

        case GraspingDeadChooseRegionAction(self) =>
            // Add any newly eligible regions (e.g., TombHerds pained into regions with enemies)
            val alreadyScheduled = TSExpansion.graspingDeadRemaining ++ TSExpansion.graspingDeadFought
            val newRegions = areas.%(r => self.at(r, TombHerd).any && self.enemies.exists(_.at(r).any) && !alreadyScheduled.has(r))
            if (newRegions.any)
                TSExpansion.graspingDeadRemaining ++= newRegions
            // Filter to regions still valid (TombHerd present + enemy present) from the remaining list
            val eligible = TSExpansion.graspingDeadRemaining.%(r => self.at(r, TombHerd).any && self.enemies.exists(_.at(r).any))
            if (eligible.any) {
                TSExpansion.graspingDeadRemaining = eligible
                val variants = eligible./~(r => self.enemies.%(_.at(r).any)./(f => r -> f))
                implicit val asking = Asking(self)
                variants.foreach { case (r, f) => + GraspingDeadBattleAction(self, r, f) }
                asking
            } else {
                TSExpansion.graspingDeadRemaining = $()
                TSExpansion.graspingDeadFought = $()
                TSExpansion.graspingDeadActive = false
                EndAction(self)
            }

        case GraspingDeadBattleAction(self, r, f) =>
            self.log("Grasping Dead: battle with", self.at(r, TombHerd).num, TombHerd.styled(TS), "in", r)
            TSExpansion.graspingDeadRemaining = TSExpansion.graspingDeadRemaining.but(r)
            TSExpansion.graspingDeadFought :+= r
            game.queue = $(new Battle(r, self, f, |(GraspingDead))) ++ game.queue
            ProceedBattlesAction

        case AfterAction(self) if self == TS && TSExpansion.graspingDeadActive =>
            // GD chain still active — check for more regions (including pained TombHerds in new regions)
            Force(GraspingDeadChooseRegionAction(self))

        // ...
        case _ => UnknownContinue
    }
}
