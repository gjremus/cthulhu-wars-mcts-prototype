package cws

import hrf.colmat._

import html._


// ============================================================================
// Firstborn (FB) UNITS: Desiccated (monster, created by Writhe from Acolytes),
// Revenant of K'Naa (monster, combat = number of Desiccated in play),
// Ghatanothoa (GOO, combat = player's current Power)
// ============================================================================
case object Desiccated extends FactionUnitClass(FB, "Desiccated", Monster, 2)
case object RevenantOfKnaa extends FactionUnitClass(FB, "Revenant of K'Naa", Monster, 3)
case object Ghatanothoa extends FactionUnitClass(FB, "Ghatanothoa", GOO, 6)

// Firstborn (FB) Crater building: placed by Devil's Mark, destroys any non-Yog gate in its region
case object Crater extends FactionUnitClass(FB, "Crater", Building, 0) {
    override def canMove(u : UnitFigure)(implicit game : Game) = false
    override def canBattle(u : UnitFigure)(implicit game : Game) = false
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
}


// ============================================================================
// Firstborn (FB) SPELLBOOKS: Writhe (unique ability), Augury, Carnage,
// The Eye Opens, Cyclopean Gaze, Devil's Mark, Call of the Faithful
// FB spellbooks can be flipped facedown by Infernal Pact / Carnage and restored on awakening
// ============================================================================
case object Writhe extends FactionSpellbook(FB, "Writhe")
case object Augury extends FactionSpellbook(FB, "Augury")
case object Carnage extends FactionSpellbook(FB, "Carnage") with BattleSpellbook
case object TheEyeOpens extends FactionSpellbook(FB, "The Eye Opens")
case object CyclopeanGaze extends FactionSpellbook(FB, "Cyclopean Gaze")
case object DevilsMark extends FactionSpellbook(FB, "Devil's Mark")
case object CallOfTheFaithful extends FactionSpellbook(FB, "Call of the Faithful")


// ============================================================================
// Firstborn (FB) SPELLBOOK REQUIREMENTS: conditions checked by FBExpansion.triggers()
// to unlock spellbook slots (6 requirements for 6 spellbooks)
// ============================================================================
case object FBNoAcolytesInStart extends Requirement("No Acolytes in Start Area")
case object FBAwakenGhatanothoa extends Requirement("Awaken Ghatanothoa")
case object FBTwoFacedownSpellbooks extends Requirement("2 Facedown Spellbooks")
case object FBSecondAwakening extends Requirement("2nd Ghatanothoa Awakening")
case object FBMostDoomOrMoreGates extends Requirement("Most Doom or More Gates than 1stP")
case object FBThirdAwakening extends Requirement("3rd Ghatanothoa Awakening")


// ============================================================================
// Firstborn (FB) FACTION OBJECT: defines faction identity, unit roster,
// summon/awaken costs, combat strength calculation, and High Priest restriction
// ============================================================================
case object FB extends Faction { f =>
    def name = "Firstborn"
    def short = "FB"
    def style = "fb"

    override def abilities = $(Writhe)
    override def library = $(Augury, Carnage, TheEyeOpens, CyclopeanGaze, DevilsMark, CallOfTheFaithful)
    override def requirements(options : $[GameOption]) = $(FBNoAcolytesInStart, FBAwakenGhatanothoa, FBTwoFacedownSpellbooks, FBSecondAwakening, FBMostDoomOrMoreGates, FBThirdAwakening)

    // Crater is NOT in allUnits — it's tracked as a region list in game.fbCraters
    // (same pattern as Ancients' Cathedral, which uses game.cathedrals)
    val allUnits =
        1.times(Ghatanothoa) ++
        2.times(RevenantOfKnaa) ++
        6.times(Desiccated) ++
        6.times(Acolyte)

    override def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case _ => u.cost
    }

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case Ghatanothoa => |(max(1, 11 - game.ritualCost))
        case _ => None
    }

    // Firstborn cannot recruit High Priests - handled in Game.scala recruits()

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        if (units.none) return 0
        // Bug fix Round 4: Revenant combat = (number of Desiccated on map) per Revenant.
        // Previously this used `f.onMap(Desiccated).num`, which includes Zeroed units (units
        // that have been eliminated mid-battle but not yet removed). Mirroring the TS Glaaki
        // pattern (`f.onMap(DeepTendril).not(Zeroed).num`), we exclude Zeroed Desiccated so
        // a Desiccated killed earlier in the same battle no longer contributes to Revenant combat.
        val desiccatedInPlay = f.onMap(Desiccated).not(Zeroed).num
        val arena = game.battle./(_.arena)
        val onLand = arena./(_.glyph != Ocean).|(units.head.region.glyph != Ocean)

        units(Desiccated).not(Zeroed).num * (onLand.?(1).|(0)) +
        units(RevenantOfKnaa).not(Zeroed).num * desiccatedInPlay +
        // Bug fix: Ghatanothoa combat is power BEFORE the battle began (snapshot taken
        // at battle start in Battle.scala). f.power decreases during a battle as costs are
        // paid, which would otherwise reduce his combat unfairly.
        units(Ghatanothoa).not(Zeroed).num * (if (game.battle.any && f == FB) game.fbPowerAtBattleStart else f.power) +
        neutralStrength(units, opponent)
    }
}


// ============================================================================
// Firstborn (FB) ACTION CLASSES: data types for all FB-specific game actions.
// Each case class represents one step in an action sequence.
// ============================================================================

// ── WRITHE ACTIONS ── Roll dice equal to Power; Kills eliminate own units
// (Acolytes become Desiccated), Pains relocate own units anywhere
case class FBWritheMainAction(self : Faction) extends OptionFactionAction(implicit g => {
    val ipDiscount = min(g.fbInfernalPactDiscount, 2)
    val effectiveCost = 2 - ipDiscount
    Writhe.toString + " (Cost " + effectiveCost.power + ")" + (ipDiscount > 0).??(" (" + "IP discounted".styled(FB.style) + ")")
}) with MainQuestion
case class FBWritheRollAction(self : Faction, numDice : Int) extends ForcedAction
case class FBWritheRollResultAction(self : Faction, numDice : Int, rolls : $[BattleRoll]) extends ForcedAction
// Re-entry into the post-roll menu without re-logging the roll line. Used by
// FBWritheUndoAllAction so the dice-roll log isn't duplicated each rollback.
case class FBWritheRollResultMenuAction(self : Faction, numDice : Int, rolls : $[BattleRoll]) extends ForcedAction
case class FBWritheRerollAction(self : Faction, rolls : $[BattleRoll]) extends OptionFactionAction("Reroll ALL dice with " + Writhe.styled(FB)) with PowerNeutral { def question(implicit game : Game) = Writhe.styled(FB) }
case class FBWritheKeepAction(self : Faction, rolls : $[BattleRoll]) extends OptionFactionAction("Keep current " + Writhe.styled(FB) + " results") with PowerNeutral { def question(implicit game : Game) = Writhe.styled(FB) }
case class FBWritheApplyAction(self : Faction, rolls : $[BattleRoll]) extends ForcedAction with PowerNeutral
case class FBWritheUndoAllAction(self : Faction, rolls : $[BattleRoll], numDice : Int) extends ForcedAction with PowerNeutral
case class FBWritheUndoLastKillAction(self : Faction, remainingKills : Int, remainingPains : Int) extends ForcedAction with PowerNeutral
case class FBWritheUndoLastPainAction(self : Faction, remainingPains : Int, chosen : $[UnitRef]) extends ForcedAction with PowerNeutral
case class FBWritheUndoLastMoveAction(self : Faction, uRef : UnitRef, remaining : $[UnitRef]) extends ForcedAction with PowerNeutral

// Augury replacement during Writhe
case class FBWritheAuguryCancelAction(self : Faction, rolls : $[BattleRoll]) extends OptionFactionAction("Skip " + Augury.styled(FB)) with PowerNeutral { def question(implicit game : Game) = Writhe.styled(FB) }
case class FBWritheAuguryReplaceAction(self : Faction, rolls : $[BattleRoll], n : Int) extends OptionFactionAction(
    "Replace " + n + " Miss" + (n > 1).?("es").|(("")) + " with " + n + " Kill" + (n > 1).?("s").|(("")) + " with " + Augury.styled(FB)
) with PowerNeutral { def question(implicit game : Game) = Writhe.styled(FB) }

// Kill assignment from Writhe - menu: "Writhe: Choose units to eliminate", options show unit + region
case class FBWritheAssignKillAction(self : Faction, remainingKills : Int, remainingPains : Int, rolls : $[BattleRoll]) extends ForcedAction with PowerNeutral
case class FBWritheKillUnitAction(self : Faction, u : UnitRef, remainingKills : Int, remainingPains : Int) extends BaseFactionAction(
    Writhe.styled(FB) + ": Choose units to eliminate", implicit g => g.unit(u).full + " in " + g.unit(u).region
) with PowerNeutral
// Pain assignment from Writhe - Phase 1: "Writhe: Choose units to pain", options show unit + region
case class FBWritheAssignPainAction(self : Faction, remainingPains : Int) extends ForcedAction with PowerNeutral
case class FBWritheChoosePainUnitAction(self : Faction, u : UnitRef, remainingPains : Int, chosen : $[UnitRef]) extends BaseFactionAction(
    Writhe.styled(FB) + ": Choose units to pain", implicit g => g.unit(u).full + " in " + g.unit(u).region
) with PowerNeutral
// Pain assignment from Writhe - Phase 2: "Writhe all units to:" with region list
case class FBWritheMoveAllAction(self : Faction, chosen : $[UnitRef]) extends ForcedAction with PowerNeutral
case class FBWritheMoveAllToRegionAction(self : Faction, r : Region, chosen : $[UnitRef]) extends BaseFactionAction(
    Writhe.styled(FB) + " all units to:", r
) with PowerNeutral
case class FBWritheMoveSeparatelyAction(self : Faction, chosen : $[UnitRef]) extends OptionFactionAction(
    Writhe.styled(FB) + " units separately"
) with PowerNeutral { def question(implicit game : Game) = Writhe.styled(FB) + " all units to:" }
// Pain assignment from Writhe - Phase 3: "Writhe [unit] to:" with region list
case class FBWritheMoveOneAction(self : Faction, u : UnitRef, remaining : $[UnitRef]) extends ForcedAction with PowerNeutral
case class FBWritheMoveOneToRegionAction(self : Faction, u : UnitRef, r : Region, remaining : $[UnitRef]) extends BaseFactionAction(
    implicit g => Writhe.styled(FB) + " " + g.unit(u).full + " in " + g.unit(u).region + " to:", r
) with PowerNeutral
// Bug fix Round 6: "join" variant shown at top of region list when paining separately.
// Displays "Region - join UnitName" so the player can quickly send the next unit to the same region.
case class FBWritheMoveOneJoinAction(self : Faction, u : UnitRef, r : Region, remaining : $[UnitRef], joinUnit : String) extends BaseFactionAction(
    implicit g => Writhe.styled(FB) + " " + g.unit(u).full + " in " + g.unit(u).region + " to:",
    implicit g => r.toString + " - join " + joinUnit.styled(FB)
) with PowerNeutral

// ── INFERNAL PACT ACTIONS ── Ghatanothoa's innate: flip faceup spellbooks to reduce action costs by 1 each
case class FBInfernalPactMainAction(self : Faction) extends OptionFactionAction("Discount with " + "Infernal Pact".styled(FB)) with MainQuestion with Soft with PowerNeutral
// Widened from FactionSpellbook to Spellbook so IGOO spellbooks (NeutralSpellbook) can
// also be flipped facedown via Infernal Pact (Round 8, Bug 40).
case class FBInfernalPactChooseAction(self : Faction, sb : Spellbook) extends BaseFactionAction("Flip facedown", sb.name.styled(FB)) with PowerNeutral
case class FBInfernalPactDoneAction(self : Faction) extends OptionFactionAction("Done".styled("power")) with Soft with PowerNeutral { def question(implicit game : Game) = "Infernal Pact" }
// Bug fix: Infernal Pact cancel actions are Hard, not Soft. They mutate state (flip spellbooks back up,
// restore faction power, reset game.fbInfernalPact* vars). Soft would prevent undo from replaying them.
case class FBInfernalPactCancelAction(self : Faction) extends OptionFactionAction("Cancel " + "Infernal Pact".styled(FB)) with PowerNeutral { def question(implicit game : Game) = "Infernal Pact" }
case class FBInfernalPactCancelMainAction(self : Faction) extends OptionFactionAction("Cancel " + "Infernal Pact".styled(FB)) with MainQuestion with PowerNeutral
// Bug fix Round 4 (Bug 4): doom-phase variants of Infernal Pact. Discount abilities should be
// available in every phase where they apply. The doom-phase variants flip spellbooks and add
// power exactly like the main-action versions, but Done/Cancel return to DoomAction(f) instead
// of MainAction(f) so the player stays in the doom phase after activating the discount.
case class FBInfernalPactDoomMainAction(self : Faction) extends OptionFactionAction("Discount with " + "Infernal Pact".styled(FB)) with DoomQuestion with Soft with PowerNeutral
// Widened from FactionSpellbook to Spellbook for IGOO support (Round 8, Bug 40)
case class FBInfernalPactDoomChooseAction(self : Faction, sb : Spellbook) extends BaseFactionAction("Flip facedown", sb.name.styled(FB)) with PowerNeutral
case class FBInfernalPactDoomDoneAction(self : Faction) extends OptionFactionAction("Done".styled("power")) with Soft with PowerNeutral { def question(implicit game : Game) = "Infernal Pact" }
case class FBInfernalPactDoomCancelAction(self : Faction) extends OptionFactionAction("Cancel " + "Infernal Pact".styled(FB)) with PowerNeutral { def question(implicit game : Game) = "Infernal Pact" }
case class FBInfernalPactCancelDoomAction(self : Faction) extends OptionFactionAction("Cancel " + "Infernal Pact".styled(FB)) with DoomQuestion with PowerNeutral
// Round 8 Bug 72: resume actions used as the `then` for CheckSpellbooksAction after
// a flip satisfies FBTwoFacedownSpellbooks. CheckSpellbooksAction's `then` field is
// typed as ForcedAction, but FBInfernalPactMainAction / FBInfernalPactDoomMainAction
// are OptionFactionAction with Soft (menu-entry actions). These tiny wrappers bridge
// the type gap: they're ForcedAction (so CheckSpellbooksAction accepts them) and
// Soft (pure navigation — their handler just Force-re-enters the corresponding
// IP sub-menu).
case class FBInfernalPactResumeAction(self : Faction) extends ForcedAction with Soft
case class FBInfernalPactDoomResumeAction(self : Faction) extends ForcedAction with Soft

// ── AWAKEN GHATANOTHOA ── Cost is 11 minus current Ritual cost; placed in start area, no gate needed
case class FBAwakenGhatanothoaAction(self : Faction, cost : Int) extends OptionFactionAction(implicit g => {
    val ipDiscount = min(g.fbInfernalPactDiscount, cost)
    val effectiveCost = cost - ipDiscount
    "Awaken " + Ghatanothoa.styled(FB) + " for " + effectiveCost.power + " in Start Area" +
        (ipDiscount > 0).??(" (" + "IP discounted".styled(FB.style) + ")")
}) with MainQuestion

// ── THE EYE OPENS ACTIONS ── Cost 1: eliminate enemy cultists in areas with Desiccated, gain power
// Round 8 Bug 42: was `with Soft` but the handler mutates state (deducts power via
// consumeDiscount + power -= cost). Soft excludes the action from undo replay, so undoing
// across The Eye Opens corrupted state (lost power changes, eliminated units lost track).
// Now Hard — undo will correctly reverse the power deduction and elimination chain.
// FB helper case classes used as fields in FB actions and Writhe state. These
// follow the named-case-class pattern (like Offer, AzathothOffer, UnitRef in
// base) so the serializer's existing per-type handling works. Bare tuples DO
// NOT serialize correctly through Serialize.write (falls to the catch-all that
// emits only the class name); the cases below are added explicitly to the
// writer in Serialize.scala.
case class FBEyeOpensTarget(region : Region, faction : Faction, unit : UnitRef)
case class FBCyclopeanGazeSource(region : Region, unit : UnitClass)
case class FBWritheKillEntry(unit : UnitRef, region : Region, origClass : UnitClass, replacement : |[UnitRef])
case class FBWrithePainEntry(unit : UnitRef, fromRegion : Region, toRegion : Region)

case class FBTheEyeOpensMainAction(self : Faction) extends OptionFactionAction(implicit g => {
    val ipDiscount = min(g.fbInfernalPactDiscount, 1)
    val effectiveCost = 1 - ipDiscount
    TheEyeOpens.styled(FB) + " (" + effectiveCost.power + ")" + (ipDiscount > 0).??(" (" + "IP discounted".styled(FB.style) + ")")
}) with MainQuestion
case class FBTheEyeOpensRegionAction(self : Faction, r : Region, pending : $[FBEyeOpensTarget]) extends BaseFactionAction(
    implicit g => TheEyeOpens.styled(FB) + ": Choose", r) {
    override def question(implicit game : Game) = TheEyeOpens.styled(FB) + ": Choose Region"
}
case class FBTheEyeOpensFactionAction(self : Faction, r : Region, f : Faction, pending : $[FBEyeOpensTarget]) extends BaseFactionAction(
    implicit g => "Target", f.full) {
    override def question(implicit game : Game) = TheEyeOpens.styled(FB) + ": Choose target faction"
}
case class FBTheEyeOpensChooseCultistAction(self : Faction, f : Faction, r : Region, u : UnitRef, pending : $[FBEyeOpensTarget]) extends BaseFactionAction(
    TheEyeOpens.styled(FB) + ": eliminate", implicit g => g.unit(u).full
)
case class FBTheEyeOpensLoopAction(self : Faction, pending : $[FBEyeOpensTarget]) extends ForcedAction
case class FBTheEyeOpensCancelAction(self : Faction) extends ForcedAction
case class FBTheEyeOpensCommitAction(self : Faction, pending : $[FBEyeOpensTarget]) extends ForcedAction

// ── CYCLOPEAN GAZE ACTIONS ── Ongoing: after opponent actions/battles, pain enemy units in areas with Revenants/Ghatanothoa
// Bug fix Round 4: each Revenant and each Ghatanothoa is its own pain source — sourcesPending tracks
// the queue of (region, sourceUnitClass) tuples so each pain logs the originating unit type separately.
// Replaces the old `regions : $[(Region, Int)]` shape which only tracked total pains per region.
// `fromBattle` indicates this chain was triggered from a post-battle hook (Battle.scala) rather
// than from the AfterAction expansion handler — when true the chain ends with FBCyclopeanGazeBattleDoneAction
// which Battle.scala catches to resume battle flow via proceed().
case class FBCyclopeanGazePhaseAction(self : Faction, actor : Faction, sourcesPending : $[FBCyclopeanGazeSource], fromBattle : Boolean) extends ForcedAction with PowerNeutral
case class FBCyclopeanGazeAssignPainAction(self : Faction, actor : Faction, r : Region, sourceUnit : UnitClass, sourcesPending : $[FBCyclopeanGazeSource], fromBattle : Boolean) extends ForcedAction with PowerNeutral
// Firstborn (FB): Cyclopean Gaze — actor chooses which of their units to pain.
// Round 8 Bug 57: the painted faction (NOT FB) picks which of their units to pain.
// `self` is set to the painted faction so:
//   - The Ask in the dispatcher (FBCyclopeanGazeAssignPainAction) targets the painted
//     faction → menu border colored in painted faction's color.
//   - The action's `self.style` matches the menu styling.
// Title format includes the painted faction name styled in their color so it's
// visually obvious WHO is being asked to choose.
case class FBCyclopeanGazePainUnitAction(self : Faction, actor : Faction, u : UnitRef, r : Region, sourceUnit : UnitClass, sourcesPending : $[FBCyclopeanGazeSource], fromBattle : Boolean) extends BaseFactionAction(
    implicit g => CyclopeanGaze.styled(FB) + " - " + sourceUnit.styled(FB) + ": " + self.name.styled(self) + " choose unit to pain",
    implicit g => g.unit(u).full + " in " + r
) with PowerNeutral
// Round 8 Bug 61: FIRSTBORN chooses the destination region (the painter directs the pain).
// `self` is FB (set by the dispatcher in FBCyclopeanGazePainUnitAction), so the menu border
// is in FB's color and the title is attributed to FB. The title shows the unit being
// retreated and FB as the chooser.
case class FBCyclopeanGazeDestinationAction(self : Faction, u : UnitRef, r : Region, sourceUnit : UnitClass, sourcesPending : $[FBCyclopeanGazeSource], actor : Faction, fromBattle : Boolean) extends BaseFactionAction(
    implicit g => CyclopeanGaze.styled(FB) + " - " + sourceUnit.styled(FB) + ": " + FB.name.styled(FB) + " retreat " + g.unit(u).full + " to", r
) with PowerNeutral
// Round 8 Bug 51: when CG pain has no legal destinations, the painted faction's owner
// chooses which of their units in the region to eliminate (rather than auto-killing the
// FB-selected unit). This is a "soak" choice — the painted faction can preserve their
// most valuable unit by sacrificing a cheaper one.
// Round 8 Bug 57: title format updated for consistency with the unit-pick menu.
// `self` should be the painted faction so the menu border is in their color (set by
// the dispatcher in FBCyclopeanGazePainUnitAction's no-destinations branch).
case class FBCyclopeanGazeKillChoiceAction(self : Faction, painedFaction : Faction, killRef : UnitRef, r : Region, sourceUnit : UnitClass, sourcesPending : $[FBCyclopeanGazeSource], actor : Faction, fromBattle : Boolean) extends BaseFactionAction(
    implicit g => CyclopeanGaze.styled(FB) + " - " + sourceUnit.styled(FB) + ": " + painedFaction.name.styled(painedFaction) + " choose unit to eliminate (no retreat)",
    implicit g => g.unit(killRef).full
) with PowerNeutral
// Marker action: emitted by FBCyclopeanGazePhaseAction when all battle-mode pains are done.
// Battle.scala catches this in its action dispatcher and calls proceed() to resume battle flow.
case class FBCyclopeanGazeBattleDoneAction(self : Faction) extends ForcedAction with PowerNeutral

// ── DEVIL'S MARK ACTIONS ── Doom phase: place Crater on controlled gate (land only), gain ES, destroy gates
// Bug fix: Devil's Mark must NOT be Soft. Soft actions are not recorded in the undo log,
// and making the menu opener Soft caused a 300-game regression (doom 20→12, rituals 3.8→1.7).
// Reverted to Hard. The Cancel-button bug is fixed via a different mechanism below.
case class FBDevilsMarkDoomAction(self : Faction) extends OptionFactionAction("Devil's Mark".styled(FB)) with DoomQuestion with PowerNeutral
case class FBDevilsMarkChooseRegionAction(self : Faction, regions : $[Region]) extends ForcedAction with PowerNeutral
// Round 9 Fix 4: dynamic label shows the power bonus preview when the target
// region has a faction glyph. Crater placement in a glyph region grants power
// equal to (current crater count + 1) per FBDevilsMarkPlaceCraterAction handler.
case class FBDevilsMarkPlaceCraterAction(self : Faction, r : Region) extends BaseFactionAction(
    "Devil's Mark".styled(FB) + ": place Crater in",
    implicit g => {
        // A region has a faction glyph if EITHER:
        //   (a) it's the printed-on-map starting region of a base faction
        //       (GC/CC/YS/BG/SL/WW — printed regardless of who's playing), OR
        //   (b) it's the chosen starting region of an in-game dynamic-start
        //       faction (OW, FB, TS, DS, AN — they place their glyph there).
        val printedGlyphFactions = $(GC, CC, YS, BG, SL, WW)
        val hasPrintedGlyph = printedGlyphFactions.exists(f => g.board.starting(f).has(r))
        val hasDynamicGlyph = g.factions.exists(f => g.starting.get(f).has(r))
        val hasGlyph = hasPrintedGlyph || hasDynamicGlyph
        if (hasGlyph) {
            val preview = g.fbCraters.num + 1 // crater count AFTER placing this one
            r.toString + " with faction glyph +" + preview.toString + " power"
        }
        else
            r.toString
    }
) with PowerNeutral

// Custom cancel for the DM region picker: Soft so the continue cache is
// not updated when it fires, returns Force(DoomAction(self)) so the player
// is taken back to the doom-phase menu. The built-in `.cancel` on the Ask
// DSL returns the cached prior state — but because FBDevilsMarkDoomAction
// itself is Hard (must be Hard to record state-mutating crater placement
// for undo), that cached state is the region picker itself, so the built-in
// cancel loops.
case class FBDevilsMarkDoomCancelAction(self : Faction) extends OptionFactionAction("Cancel " + "Devil's Mark".styled(FB)) with DoomQuestion with Soft with PowerNeutral

// ── CALL OF THE FAITHFUL ACTIONS ── Free action: place Acolyte from pool in area with Ghatanothoa/Revenant
case class FBCallOfTheFaithfulMainAction(self : Faction) extends OptionFactionAction(CallOfTheFaithful) with MainQuestion with Soft with PowerNeutral
case class FBCallOfTheFaithfulAction(self : Faction, r : Region) extends BaseFactionAction(
    CallOfTheFaithful.styled(FB) + ": place Acolyte in", r
) with PowerNeutral

// ── CARNAGE ACTIONS ── Post-battle: if both sides lost units, pay 1 power or flip spellbook for ES
case class FBCarnagePostBattleAction(self : Faction) extends ForcedAction with PowerNeutral
case class FBCarnagePayPowerAction(self : Faction) extends OptionFactionAction("Pay " + 1.power + " for an " + "Elder Sign".styled("es") + " with " + Carnage.styled(FB)) with PowerNeutral { def question(implicit game : Game) = "Carnage" }
case class FBCarnageFlipSpellbookAction(self : Faction) extends OptionFactionAction("Flip a Spellbook facedown for an " + "Elder Sign".styled("es") + " with " + Carnage.styled(FB)) with PowerNeutral { def question(implicit game : Game) = "Carnage" }
case class FBCarnageChooseSpellbookAction(self : Faction, sb : FactionSpellbook) extends BaseFactionAction(Carnage.styled(FB) + ": flip facedown", sb.name.styled(FB)) with PowerNeutral
// Round 8 Bug 67: FBCarnageCancelAction is NOT Soft. Its perform falls through
// FBExpansion (UnknownContinue) to base Game's battle.perform → proceed(), which
// advances the battle past PostBattlePhase to BattleEnd and sets game.battle = None.
// Soft actions are required to be pure menu-navigation with NO state mutation.
// Marking cancel as Soft caused Explode to perform it during action enumeration,
// clearing battle as a side effect, and the subsequent real perform of
// FBCarnageChooseSpellbookAction then crashed in base Game.perform with a
// MatchError because `case action if battle.any` no longer matched.
case class FBCarnageCancelAction(self : Faction) extends OptionFactionAction("Cancel") with PowerNeutral { def question(implicit game : Game) = "Carnage" }

// ── AUGURY BATTLE ACTIONS ── Replace Miss dice with stored Kill results from the Augury spellbook
case class FBAuguryBattleAction(self : Faction, rolls : $[BattleRoll]) extends ForcedAction with PowerNeutral
case class FBAuguryBattleReplaceAction(self : Faction, n : Int) extends OptionFactionAction(
    "Replace " + n + " Miss" + (n > 1).?("es").|(("")) + " with " + n + " Kill" + (n > 1).?("s").|(("")) + " with " + Augury.styled(FB)
) with PowerNeutral { def question(implicit game : Game) = "Augury" }
// Round 8 Bug 67: NOT Soft, same reason as FBCarnageCancelAction. Battle.scala
// dispatches FBAuguryBattleCancelAction to `jump(AssignDefenderKills)`, which
// mutates the battle phase. Soft actions must be pure menu navigation.
case class FBAuguryBattleCancelAction(self : Faction) extends OptionFactionAction("Skip " + Augury.styled(FB)) with PowerNeutral { def question(implicit game : Game) = "Augury" }


// ============================================================================
// Firstborn (FB) EXPANSION OBJECT: manages all FB-specific game state (awakenings,
// augury kills, infernal pact discount, craters, writhe tracking) and dispatches
// all FB action handlers. Registered via Game.expansions for the FB faction.
// ============================================================================
object FBExpansion extends Expansion {
    // NOTE: All mutable FB state has been moved to Game.scala (fbAuguryKills, fbGhatnothoaAwakenings,
    // fbCraters, fbInfernalPactDiscount, fbInfernalPactPowerAdded, fbInfernalPactStartPower,
    // fbInfernalPactFlipped, fbDevilsMarkUsedThisDoom, fbWritheUsedUnits, fbWritheRerolled)
    // so that the undo mechanism (which creates a new Game and replays recorded actions)
    // properly resets state. NEVER store per-game mutable state as singleton vars here.

    // Gate destruction from Crater ability
    def destroyGatesInCraterRegions()(implicit game : Game) {
        game.fbCraters.foreach { r =>
            // Find all gates in this region (normal gates, chaos gates, etc.)
            val gatesHere = game.factions.%(f => f.gates.has(r))
            gatesHere.foreach { f =>
                // Don't destroy Yog-Sothoth gate (handled separately if ever relevant)
                f.gates = f.gates.but(r)
                f.log("Gate in", r, "destroyed by", "Crater".styled(FB))
            }
        }
    }

    // Bug fix Round 3: helper for FBMostDoomOrMoreGates spellbook requirement.
    // Previously this check only ran at EndTurnAction (end of FB's action phase turn),
    // so doom earned in the doom phase (or gates taken during doom-phase rituals) weren't
    // counted until FB's next action turn — delaying satisfaction by a whole round.
    // This helper is now called from: (1) EndTurnAction, (2) after doom is distributed
    // at DoomPhaseAction, (3) after DoomDoneAction for FB.
    // Uses visible doom only (doom track), NOT unrevealed Elder Signs — must STRICTLY
    // have more doom than every enemy (no ties).
    def checkMostDoomOrGates()(implicit game : Game) {
        if (game.factions.has(FB) && FB.needs(FBMostDoomOrMoreGates)) {
            val fbDoom = FB.doom
            val mostEnemyDoom = game.factions.but(FB)./(_.doom).maxOr(0)
            val hasMostDoom = fbDoom > mostEnemyDoom && fbDoom > 0
            val firstPlayer = game.first
            val hasMoreGates = FB.gates.num > firstPlayer.gates.num
            if (hasMostDoom || hasMoreGates)
                FB.satisfy(FBMostDoomOrMoreGates, (hasMostDoom).?("Most Doom").|("More Gates than First Player"))
        }
    }

    // Check if a gate was just placed/moved to a crater region
    // Crater ability: destroy any gate in a crater region (both faction and global lists)
    def checkCraterDestroysGate(r : Region)(implicit game : Game) {
        if (game.fbCraters.has(r)) {
            game.factions.%(f => f.gates.has(r)).foreach { f =>
                f.gates = f.gates.but(r)
                f.log("Gate in", r, "immediately destroyed by", "Crater".styled(FB))
            }
            game.gates = game.gates.but(r)
            // Round 8 Bug 74: clear `onGate` on any units in this region so
            // menu rendering doesn't still label them as "on the gate" after
            // the gate is destroyed. Same reasoning as the fix in
            // FBDevilsMarkPlaceCraterAction — the gate no longer exists, so
            // nobody can be "on" it.
            game.factions.foreach { f =>
                f.at(r).%(_.onGate).foreach(_.onGate = false)
            }
        }
    }

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        // Nothing special on eliminate for FB
    }

    override def triggers()(implicit game : Game) {
        if (game.factions.has(FB)) {
            // Check: No Acolytes in Start Area
            // Follows the OW SBR pattern (e.g., EightGates) — checked in triggers() so it
            // fires on any action by any faction, not just FB's turn.
            val startArea = game.starting.get(FB)
            startArea.foreach { start =>
                FB.satisfyIf(FBNoAcolytesInStart, "No Acolytes in Start Area",
                    FB.at(start, Acolyte).none && FB.at(start, HighPriest).none)
            }

            // Round 8 bug fix (Bug 41): Most Doom or More Gates than First Player.
            // Previously checked only at EndTurnAction (end of FB's turn). Now follows
            // the OW SBR pattern — checked here in triggers() so it fires on any action
            // by any faction (e.g., another faction destroys a gate, performs a ritual
            // changing doom totals, etc.). The condition is a STRICT "more than" — ties
            // do not satisfy the requirement (matches the spellbook card text).
            checkMostDoomOrGates()

            // Check: Awaken Ghatanothoa (1st time)
            FB.satisfyIf(FBAwakenGhatanothoa, "Awaken Ghatanothoa",
                game.fbGhatnothoaAwakenings >= 1)

            // Check: 2nd Ghatanothoa Awakening
            FB.satisfyIf(FBSecondAwakening, "2nd Ghatanothoa Awakening",
                game.fbGhatnothoaAwakenings >= 2)

            // Check: 3rd Ghatanothoa Awakening
            FB.satisfyIf(FBThirdAwakening, "3rd Ghatanothoa Awakening",
                game.fbGhatnothoaAwakenings >= 3)
        }
    }

    // Flip a spellbook facedown (for Infernal Pact or Carnage)
    // Flip a spellbook facedown (disable it). Accepts both FactionSpellbook and
    // NeutralSpellbook (IGOO spellbooks) — Round 8 Bug 40 widened from FactionSpellbook.
    def flipSpellbookDown(sb : Spellbook)(implicit game : Game) {
        FB.oncePerGame :+= sb
        FB.log(sb.name.styled(FB), "flipped facedown")
    }

    // Flip a spellbook faceup (restore from facedown). Widened to Spellbook for IGOO support.
    def flipSpellbookUp(sb : Spellbook)(implicit game : Game) {
        FB.oncePerGame = FB.oncePerGame.but(sb)
        FB.log(sb.name.styled(FB), "flipped faceup")
    }

    // Get currently faceup (active) FB faction spellbooks
    def faceUpSpellbooks(implicit game : Game) : $[FactionSpellbook] =
        FB.spellbooks.%(sb => sb.isInstanceOf[FactionSpellbook]).map(_.asInstanceOf[FactionSpellbook]).%(sb => !FB.oncePerGame.has(sb))

    // Get currently facedown FB faction spellbooks
    def faceDownSpellbooks(implicit game : Game) : $[FactionSpellbook] =
        FB.spellbooks.%(sb => sb.isInstanceOf[FactionSpellbook]).map(_.asInstanceOf[FactionSpellbook]).%(sb => FB.oncePerGame.has(sb))

    // Round 8 Bug 40: IGOO spellbooks that FB has earned and are currently faceup.
    // These are NeutralSpellbook instances stored in FB.upgrades, not FB.spellbooks.
    // Flipping them facedown adds them to oncePerGame; their effects are gated by
    // f.has(sb) which checks upgrades, so we need separate tracking.
    def faceUpIGOOSpellbooks(implicit game : Game) : $[NeutralSpellbook] =
        FB.upgrades.%(_.isInstanceOf[NeutralSpellbook]).map(_.asInstanceOf[NeutralSpellbook]).%(sb => !FB.oncePerGame.has(sb))

    // IGOO spellbooks that FB has earned but are currently facedown
    def faceDownIGOOSpellbooks(implicit game : Game) : $[NeutralSpellbook] =
        FB.upgrades.%(_.isInstanceOf[NeutralSpellbook]).map(_.asInstanceOf[NeutralSpellbook]).%(sb => FB.oncePerGame.has(sb))

    // Count facedown spellbooks (faction + IGOO combined, for FBTwoFacedownSpellbooks requirement)
    def faceDownCount(implicit game : Game) : Int = faceDownSpellbooks.num + faceDownIGOOSpellbooks.num

    // Effective cost after Infernal Pact discount
    def effectiveCost(baseCost : Int)(implicit game : Game) : Int = max(0, baseCost - game.fbInfernalPactDiscount)

    // Round 8 Bug 75: Infernal Pact discount is now a SEPARATE pool from power.
    // consumeDiscount returns the amount of discount used (0..baseCost). Callers
    // deduct the REMAINDER from real power: `self.power -= (baseCost - discountUsed)`.
    // This is the "separate pool" contract: discount is spent first, real power
    // covers the rest.
    def consumeDiscount(baseCost : Int)(implicit game : Game) : Int = {
        val used = min(game.fbInfernalPactDiscount, baseCost)
        game.fbInfernalPactDiscount -= used
        if (used > 0)
            FB.log("Infernal Pact".styled(FB), "discounted", used.power)
        used
    }


    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {

        // Firstborn (FB): reset per-turn flags at start of each gather power phase
        case PowerGatherAction(_) if game.factions.has(FB) =>
            game.fbDevilsMarkUsedThisDoom = false
            UnknownContinue

        // ── DOOM PHASE ──
        case DoomAction(f) if f == FB =>
            implicit val asking = Asking(f)

            // Round 8 Bug 75 (doom phase): transient IP boost around offer checks.
            // Same pattern as the main-phase handlers. game.rituals(f) checks
            // `f.power >= cost` directly and doesn't know about the IP discount
            // pool, so without this boost an IP discount cannot unlock a ritual
            // the faction couldn't otherwise afford (e.g. 9 power + 1 discount
            // should unlock a 10-power ritual). Restored before return.
            val ipBoost = game.fbInfernalPactDiscount
            if (ipBoost > 0)
                f.power += ipBoost

            game.rituals(f)

            // 2026-05-12 v4: Infernal Pact entirely removed from the doom phase per
            // user direction. IP is now an action-phase-only mechanic; the IP
            // discount cannot be used to reduce Ritual of Annihilation cost. The
            // doom-phase entry points (FBInfernalPactDoomMainAction +
            // FBInfernalPactCancelDoomAction) are no longer offered here. The
            // case classes and handlers are kept for backward compatibility with
            // existing replay logs but are unreachable in new games.
            // Devil's Mark is unaffected and remains a doom-phase ability.

            // Firstborn (FB): Devil's Mark — place crater at controlled land gate (once per doom phase)
            if (f.has(DevilsMark) && !FB.oncePerGame.has(DevilsMark) && !game.fbDevilsMarkUsedThisDoom) {
                val landGates = f.gates.%(r => r.glyph != Ocean)
                if (landGates.any)
                    + FBDevilsMarkDoomAction(f)
            }

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            + DoomDoneAction(f)

            if (ipBoost > 0)
                f.power -= ipBoost

            asking

        // ── DEVIL'S MARK ──
        case FBDevilsMarkDoomAction(self) =>
            val landGates = self.gates.%(r => r.glyph != Ocean)
            implicit val asking = Asking(self)
            landGates.foreach(r => + FBDevilsMarkPlaceCraterAction(self, r))
            + FBDevilsMarkDoomCancelAction(self)
            asking

        case FBDevilsMarkDoomCancelAction(self) =>
            // Just return to the doom-phase menu; no state changes. Soft
            // means this action is not added to the continue cache, so the
            // DoomAction(self) Force takes the player back cleanly.
            Force(DoomAction(self))

        case FBDevilsMarkPlaceCraterAction(self, r) =>
            // Mark Devil's Mark as used this doom phase
            game.fbDevilsMarkUsedThisDoom = true

            // Bug fix: log crater placement FIRST, then gate destruction
            // (matches the order users expect to read events in the log)
            // Place crater — tracked as region list, NOT as unit figure
            // (same pattern as Ancients' Cathedral using game.cathedrals)
            game.fbCraters :+= r
            self.log("placed", "Crater".styled(FB), "in", r, "with", "Devil's Mark".styled(FB))

            // Round 8 Bug 74: clear `onGate` on any units at this region BEFORE
            // removing the gate. When the gate is destroyed by the crater, no
            // unit is "on the gate" anymore — the gate doesn't exist. Without
            // this update, the Acolyte (or HP) that controlled the destroyed
            // gate still has `u.onGate = true`, and menus render it as
            // "Acolyte (on the gate)" which is wrong (the gate is gone).
            // Iterate all factions to be safe — normally only FB has onGate
            // units at its own controlled gate, but this is cheap.
            // Because FBDevilsMarkPlaceCraterAction is Hard (not Soft), undo
            // replay reconstructs the pre-action state, so the `onGate` reset
            // is reversed correctly.
            game.factions.foreach { f =>
                f.at(r).%(_.onGate).foreach(_.onGate = false)
            }

            // Remove gate from both faction gates AND global game gates
            self.gates = self.gates.but(r)
            game.gates = game.gates.but(r)
            self.log("Gate in", r, "destroyed by", "Crater".styled(FB))

            // Elder sign for destroyed gate
            self.takeES(1)
            self.log("gained", 1.es, "from", "Devil's Mark".styled(FB))

            // A region has a faction glyph if EITHER:
            //   (a) it's the printed-on-map starting region of a base faction
            //       (GC/CC/YS/BG/SL/WW — printed regardless of who's playing).
            //       WW has 2 (one ocean, one land); only land matters here since
            //       Devil's Mark places craters on land only.
            //   (b) it's the chosen starting region of an in-game dynamic-start
            //       faction (OW, FB, TS, DS, AN — they place their glyph there).
            val printedGlyphFactions = $(GC, CC, YS, BG, SL, WW)
            val hasPrintedGlyph = printedGlyphFactions.exists(f => game.board.starting(f).has(r))
            val hasDynamicGlyph = game.factions.exists(f => game.starting.get(f).has(r))
            val hasGlyph = hasPrintedGlyph || hasDynamicGlyph
            if (hasGlyph) {
                val craterCount = game.fbCraters.num
                self.power += craterCount
                self.log("gained", craterCount.power, "from", "Devil's Mark".styled(FB), "(" + craterCount + " Craters, glyph region)")
            }

            // Destroy any other gates in this region (from Crater ability)
            checkCraterDestroysGate(r)

            CheckSpellbooksAction(DoomAction(self))

        // ── MAIN ACTIONS ──
        case MainAction(f) if f == FB && f.active.not =>
            UnknownContinue

        // Round 8: post-acted FB main menu. Call of the Faithful is an UNLIMITED
        // action — it must be available BEFORE and AFTER any standard main action
        // during the action phase. Mirrors BG's post-acted handler pattern
        // (FactionBG.scala:133-147) which injects `game.summons(f)` for BG's
        // unlimited summons. Without this override, FB's post-acted handler would
        // fall through to Game.scala's generic `case MainAction(f) if f.acted`
        // (Game.scala:2278) which only offers controls / battles (if allSB) /
        // reveals / endTurn — CoF would be inaccessible after FB's main action.
        //
        // The CoF gate check is identical to the pre-acted handler's (line 576)
        // so the action is offered symmetrically. Because the handler re-renders
        // on every MainAction re-entry, CoF becomes immediately available in the
        // same action-phase turn where it's earned via CheckSpellbooksAction.
        case MainAction(f) if f == FB && f.acted =>
            implicit val asking = Asking(f)

            // Round 8 Bug 75: transient IP boost — same pattern as the pre-acted
            // handler. Post-acted unlimited actions (like CoF) don't cost power,
            // but battles-if-allSB and gate control might, and future unlimited
            // actions could. The boost ensures all offer checks in this block see
            // f.power as "real + discount". Restored before return.
            val ipBoost = game.fbInfernalPactDiscount
            if (ipBoost > 0)
                f.power += ipBoost

            game.controls(f)

            if (f.hasAllSB)
                game.battles(f)

            // Post-main IP: when FB has all 6 spellbooks and unlimited battles
            // are available, FB can flip SBs to discount the battle cost. The
            // pre-main IP session's flips are now committed (via EndAction
            // commit hook), so any cancel here only reverses post-main flips.
            if (f.hasAllSB && faceUpSpellbooks.any && game.fbInfernalPactDiscount == game.fbInfernalPactCommittedDiscount) {
                // Only show "enter new IP session" when there's no pending
                // uncommitted state — prevents double-entry confusion.
                + FBInfernalPactMainAction(f)
            }
            // Cancel only available if there's post-commit discount to cancel
            if (game.fbInfernalPactDiscount > game.fbInfernalPactCommittedDiscount)
                + FBInfernalPactCancelMainAction(f)

            // Call of the Faithful (Unlimited, Cost 0) — available post-action
            if (f.has(CallOfTheFaithful) && !FB.oncePerGame.has(CallOfTheFaithful) && f.pool(Acolyte).any) {
                val eligible = areas.%(r => (f.at(r, Ghatanothoa).any || f.at(r, RevenantOfKnaa).any) && f.at(r, Acolyte).none)
                if (eligible.any)
                    + FBCallOfTheFaithfulMainAction(f)
            }

            game.reveals(f)

            game.endTurn(f)(true)

            if (ipBoost > 0)
                f.power -= ipBoost

            asking

        case MainAction(f) if f == FB =>
            implicit val asking = Asking(f)

            // Cancel Infernal Pact from main menu
            if (game.fbInfernalPactDiscount > 0)
                + FBInfernalPactCancelMainAction(f)

            // Infernal Pact: discount actions by flipping spellbooks
            // Bug fix Round 4 (Bug 5): only show when there's at least one power-costing action
            // available. After f.acted is true (faction has spent its action this turn), only
            // Soft / PowerNeutral options remain, so Infernal Pact discount cannot be applied
            // and the menu entry would be a dead end. Suppress it in that state.
            // 2026-05-11: also suppress if FB cancelled an IP session this turn —
            // prevents Cancel→IPMain→Cancel watchdog loop when wantIP score keeps
            // pulling bot in but every flip is blocked by per-SB scoring.
            if (f.has(Ghatanothoa) && f.onMap(Ghatanothoa).any && faceUpSpellbooks.any && !f.acted &&
                !game.fbInfernalPactCancelledThisTurn)
                + FBInfernalPactMainAction(f)

            // ── Round 8 Bug 75: Transient power boost for offer checks ──
            // IP discount is a separate pool; game.moves/captures/recruits/etc.
            // check f.power >= cost directly and don't know about the discount.
            // Temporarily add the discount to f.power so offer checks see effective
            // purchasing power, then restore at the end of the block. The boost is
            // scoped entirely within this synchronous menu-building block — no
            // other handler runs in between, so the boost can't leak.
            val ipBoost = game.fbInfernalPactDiscount
            if (ipBoost > 0)
                f.power += ipBoost

            game.moves(f)

            game.captures(f)

            game.recruits(f)

            game.battles(f)

            game.controls(f)

            game.builds(f)

            game.summons(f)

            // Awaken Ghatanothoa - special: no gate required, appears in start area.
            // Inside the transient-boost scope so f.power already reflects effective power.
            val awakenCost = max(1, 11 - game.ritualCost)
            if (f.pool(Ghatanothoa).any && f.power >= awakenCost)
                + FBAwakenGhatanothoaAction(f, awakenCost)

            game.independents(f)

            // Writhe (Cost 2) — inside boost scope, so f.power already includes discount
            if (f.power >= 2)
                + FBWritheMainAction(f)

            // The Eye Opens (Cost 1, +1 in Ice Age region)
            if (f.has(TheEyeOpens) && !FB.oncePerGame.has(TheEyeOpens)) {
                if (f.power >= 1) {
                    val eligible = areas.%(r => f.at(r, Desiccated).any &&
                        f.enemies.exists(_.at(r).%(_.uclass.utype == Cultist).any) &&
                        // Ice Age regions cost 2 total — check affordability
                        (f.power >= 2 || !game.factions.exists(wf => wf.iceAge.contains(r))))
                    if (eligible.any)
                        + FBTheEyeOpensMainAction(f)
                }
            }

            // Call of the Faithful (Unlimited, Cost 0)
            if (f.has(CallOfTheFaithful) && !FB.oncePerGame.has(CallOfTheFaithful) && f.pool(Acolyte).any) {
                val eligible = areas.%(r => (f.at(r, Ghatanothoa).any || f.at(r, RevenantOfKnaa).any) && f.at(r, Acolyte).none)
                if (eligible.any)
                    + FBCallOfTheFaithfulMainAction(f)
            }

            game.neutralSpellbooks(f)

            game.libraryActions(f)

            // No High Priests for Firstborn
            // game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            // Round 8 Bug 75: restore the transient IP boost. The offer-building
            // block above saw f.power = real + discount so game.*(f) and the
            // FB-specific checks worked. After the menu is built, real power is
            // restored so that subsequent handlers (when the user picks an action)
            // see the pre-boost value. When they DO pick a cost-bearing action,
            // FBExpansion's action intercepts re-apply the boost right before the
            // base handler deducts cost — guaranteeing the discount is spent
            // before real power.
            if (ipBoost > 0)
                f.power -= ipBoost

            asking

        // ── AWAKEN GHATANOTHOA ──
        case FBAwakenGhatanothoaAction(self, cost) =>
            // Round 8 Bug 75: pay from discount first, then power.
            val discountUsed = consumeDiscount(cost)
            self.power -= (cost - discountUsed)
            val startArea = game.starting(FB)
            self.place(Ghatanothoa, startArea)
            game.fbGhatnothoaAwakenings += 1
            self.log("awakened", Ghatanothoa.styled(FB), "in", startArea, "for", cost.power)

            // Flip all facedown spellbooks faceup (faction + IGOO spellbooks).
            // Round 8 Bug 40: also flip IGOO spellbooks back up when Ghatanothoa is
            // re-awakened. This re-enables their powers but does NOT re-earn them —
            // only spellbooks already in FB.upgrades are affected.
            faceDownSpellbooks.foreach { sb =>
                flipSpellbookUp(sb)
            }
            faceDownIGOOSpellbooks.foreach { sb =>
                flipSpellbookUp(sb)
            }

            game.triggers()
            // Check spellbook requirements (awakening is a requirement)
            CheckSpellbooksAction(EndAction(self))

        // ── WRITHE ──
        // Writhe can be used by any faction that has it (FB natively, or via SL Ancient
        // Sorcery borrowing). Non-FB factions have 0 Desiccated in pool, so killed
        // Acolytes are simply eliminated (line ~664 checks pool(Desiccated).any).
        // Augury integration is gated by self.has(Augury), which is false for non-FB.
        // Infernal Pact discount is gated by game.fbInfernalPactDiscount, which is 0
        // for non-FB. All three cases are safe — no special non-FB handling needed.
        case FBWritheMainAction(self) =>
            // Round 8 Bug 75: Writhe dice count is based on REAL power BEFORE spending.
            // Previously, flipping spellbooks added fake power to `self.power`, so the
            // "power before spending" figure included the fake discount. Now discount
            // is a separate pool: capture `self.power` first (real power, untouched by
            // discount), then pay from discount + power, then roll dice based on the
            // captured real-power-before-spending figure.
            val writheCost = 2
            val numDice = self.power  // capture REAL power before spending
            val discountUsed = consumeDiscount(writheCost)
            self.power -= (writheCost - discountUsed)
            game.fbWritheRerolled = false
            game.fbWritheUsedUnits = $
            game.fbWritheKillLog = $
            game.fbWrithePainLog = $
            // Bug fix Round 6: reset Writhe "join" tracking at start of each Writhe activation
            game.fbWritheLastPainRegion = None
            game.fbWritheLastPainedUnit = ""
            self.log("used", Writhe.styled(FB), "rolling", numDice, "dice")
            // Use RollBattle continuation so dice are rolled by the game engine
            // and stored in replay log (consistent on undo, with dice animation)
            RollBattle(self, Writhe.styled(FB), numDice, rolls => FBWritheRollResultAction(self, numDice, rolls))

        // FBWritheRollAction is kept for reroll dispatch only
        case FBWritheRollAction(self, numDice) =>
            RollBattle(self, Writhe.styled(FB), numDice, rolls => FBWritheRollResultAction(self, numDice, rolls))

        case FBWritheRollResultAction(self, numDice, rolls) =>
            val kills = rolls.count(_ == Kill)
            val pains = rolls.count(_ == Pain)
            val misses = rolls.count(_ == Miss)
            self.log("rolled", kills, Kill, pains, Pain, misses, "Miss" + (misses != 1).?("es").|(("")))
            Force(FBWritheRollResultMenuAction(self, numDice, rolls))

        case FBWritheRollResultMenuAction(self, numDice, rolls) =>
            game.fbWritheRolls = rolls
            game.fbWritheNumDice = numDice
            val kills = rolls.count(_ == Kill)
            val pains = rolls.count(_ == Pain)
            val misses = rolls.count(_ == Miss)

            // Offer reroll if not yet rerolled
            if (!game.fbWritheRerolled) {
                implicit val asking = Asking(self)
                + FBWritheRerollAction(self, rolls)
                + FBWritheKeepAction(self, rolls)

                // Augury: offer to replace misses with kills before deciding reroll
                if (self.has(Augury) && !FB.oncePerGame.has(Augury) && game.fbAuguryKills > 0 && misses > 0) {
                    val maxReplace = min(misses, game.fbAuguryKills)
                    (1 to maxReplace).foreach { n =>
                        + FBWritheAuguryReplaceAction(self, rolls, n)
                    }
                }

                asking
            } else {
                // After reroll, offer Augury then apply
                if (self.has(Augury) && !FB.oncePerGame.has(Augury) && game.fbAuguryKills > 0 && misses > 0) {
                    implicit val asking = Asking(self)
                    val maxReplace = min(misses, game.fbAuguryKills)
                    (1 to maxReplace).foreach { n =>
                        + FBWritheAuguryReplaceAction(self, rolls, n)
                    }
                    + FBWritheAuguryCancelAction(self, rolls)
                    asking
                } else
                    Force(FBWritheApplyAction(self, rolls))
            }

        case FBWritheRerollAction(self, oldRolls) =>
            game.fbWritheRerolled = true
            self.log("rerolled ALL dice with", Writhe.styled(FB))
            Force(FBWritheRollAction(self, oldRolls.num))  // re-roll same number of dice

        case FBWritheKeepAction(self, rolls) =>
            Force(FBWritheApplyAction(self, rolls))

        case FBWritheAuguryReplaceAction(self, rolls, n) =>
            game.fbAuguryKills -= n
            // Replace n misses with kills
            var newRolls = rolls
            var replaced = 0
            newRolls = newRolls.map { r =>
                if (r == Miss && replaced < n) {
                    replaced += 1
                    Kill
                } else r
            }
            self.log(Augury.styled(FB) + ": replaced", n, "Miss" + (n > 1).?("es").|(("")), "with", n, "Kill" + (n > 1).?("s").|(("")))
            Force(FBWritheApplyAction(self, newRolls))

        case FBWritheAuguryCancelAction(self, rolls) =>
            Force(FBWritheApplyAction(self, rolls))

        case FBWritheUndoLastKillAction(self, remainingKills, remainingPains) =>
            // Reverse the last kill transformation
            if (game.fbWritheKillLog.any) {
                val FBWritheKillEntry(origRef, origRegion, origClass, replacementRef) = game.fbWritheKillLog.last
                game.fbWritheKillLog = game.fbWritheKillLog.dropRight(1)
                // Remove replacement Desiccated if one was placed
                replacementRef.foreach { rRef =>
                    val replacement = game.unit(rRef)
                    replacement.region = self.reserve
                    replacement.health = Alive
                }
                // Restore original unit
                val orig = game.unit(origRef)
                orig.region = origRegion
                orig.health = Alive
                orig.state = $
                game.fbWritheUsedUnits = game.fbWritheUsedUnits.but(origRef)
                self.log(Writhe.styled(FB) + ": undid last kill")
            }
            Force(FBWritheAssignKillAction(self, remainingKills + 1, remainingPains, $))

        case FBWritheUndoLastPainAction(self, remainingPains, chosen) =>
            // Remove last pain selection — just pop from chosen list
            if (chosen.any) {
                val newChosen = chosen.dropRight(1)
                self.log(Writhe.styled(FB) + ": undid last pain selection")
                Force(FBWritheAssignPainAction(self, remainingPains + 1))
            } else
                Force(FBWritheAssignPainAction(self, remainingPains))

        case FBWritheUndoLastMoveAction(self, uRef, remaining) =>
            // Reverse the last pain movement
            if (game.fbWrithePainLog.any) {
                val FBWrithePainEntry(movedRef, fromRegion, toRegion) = game.fbWrithePainLog.last
                game.fbWrithePainLog = game.fbWrithePainLog.dropRight(1)
                val movedUnit = game.unit(movedRef)
                movedUnit.region = fromRegion
                movedUnit.onGate = false
                game.fbWritheUsedUnits = game.fbWritheUsedUnits.but(movedRef)
                self.log(Writhe.styled(FB) + ": undid last region selection")
                // Re-offer region choice for the unit that was just un-moved
                Force(FBWritheMoveOneAction(self, movedRef, uRef +: remaining))
            } else
                Force(FBWritheMoveOneAction(self, uRef, remaining))

        case FBWritheUndoAllAction(self, rolls, numDice) =>
            // Reverse all pain movements (in reverse order)
            game.fbWrithePainLog.reverse.foreach { case FBWrithePainEntry(uRef, fromRegion, toRegion) =>
                val u = game.unit(uRef)
                u.region = fromRegion
                u.onGate = false
            }
            // Reverse all kills (in reverse order)
            game.fbWritheKillLog.reverse.foreach { case FBWritheKillEntry(origRef, origRegion, origClass, replacementRef) =>
                // If a replacement was placed (Acolyte → Desiccated), remove it
                replacementRef.foreach { rRef =>
                    val replacement = game.unit(rRef)
                    replacement.region = self.reserve
                    replacement.health = Alive
                }
                // Restore the original unit
                val orig = game.unit(origRef)
                orig.region = origRegion
                orig.health = Alive
                orig.state = $
            }
            // Reset writhe state — restore reroll flag to what it was before kills/pains
            game.fbWritheUsedUnits = $
            game.fbWritheKillLog = $
            game.fbWrithePainLog = $
            game.fbWritheRerolled = game.fbWritheHadRerolled
            game.fbWritheLastPainRegion = None
            game.fbWritheLastPainedUnit = ""
            self.log(Writhe.styled(FB) + ": undid all choices, back to dice roll")
            Force(FBWritheRollResultMenuAction(self, numDice, rolls))

        case FBWritheApplyAction(self, rolls) =>
            game.fbWritheHadRerolled = game.fbWritheRerolled
            val kills = rolls.count(_ == Kill)
            val pains = rolls.count(_ == Pain)
            if (kills > 0)
                Force(FBWritheAssignKillAction(self, kills, pains, rolls))
            else if (pains > 0)
                Force(FBWritheAssignPainAction(self, pains))
            else
                EndAction(self)

        case FBWritheAssignKillAction(self, remainingKills, remainingPains, rolls) =>
            if (remainingKills <= 0) {
                if (remainingPains > 0)
                    Force(FBWritheAssignPainAction(self, remainingPains))
                else
                    EndAction(self)
            } else {
                // Sort by region name, then by unit cost descending
                val units = self.units.%(u => u.region.onMap && u.uclass != Crater && !game.fbWritheUsedUnits.has(u.ref))
                    .sortBy(u => (u.region.toString, -u.uclass.cost))
                if (units.none)
                    EndAction(self)
                else {
                    val ask = Ask(self).each(units)(u => FBWritheKillUnitAction(self, u.ref, remainingKills, remainingPains))
                    if (game.fbWritheKillLog.any)
                        ask.add(FBWritheUndoLastKillAction(self, remainingKills, remainingPains).as("Undo last elimination")(Writhe.styled(FB)))
                            .add(FBWritheUndoAllAction(self, game.fbWritheRolls, game.fbWritheNumDice).as("Undo all back to dice roll")(Writhe.styled(FB)))
                    else
                        ask.add(FBWritheUndoAllAction(self, game.fbWritheRolls, game.fbWritheNumDice).as("Undo all back to dice roll")(Writhe.styled(FB)))
                }
            }

        case FBWritheKillUnitAction(self, uRef, remainingKills, remainingPains) =>
            val u = game.unit(uRef)
            val r = u.region
            val origClass = u.uclass
            game.fbWritheUsedUnits :+= uRef
            if (u.uclass == Acolyte && self.pool(Desiccated).any) {
                // Acolyte killed by Writhe: replace with Desiccated
                game.eliminate(u)
                self.place(Desiccated, r)
                val placedDesc = self.at(r, Desiccated).last
                game.fbWritheUsedUnits = game.fbWritheUsedUnits.but(placedDesc.ref)
                game.fbWritheKillLog :+= FBWritheKillEntry(uRef, r, origClass, |(placedDesc.ref))
                self.log(Writhe.styled(FB) + ": Acolyte replaced with", Desiccated.styled(FB), "in", r)
            } else {
                game.eliminate(u)
                game.fbWritheKillLog :+= FBWritheKillEntry(uRef, r, origClass, None)
                self.log(Writhe.styled(FB) + ": eliminated", u.uclass.styled(self), "in", r)
            }
            Force(FBWritheAssignKillAction(self, remainingKills - 1, remainingPains, $))

        // ── WRITHE PAIN: Phase 1 - Choose units ──
        case FBWritheAssignPainAction(self, remainingPains) =>
            if (remainingPains <= 0)
                EndAction(self)
            else {
                // Sort by region name, then by unit cost descending
                val units = self.units.%(u => u.region.onMap && u.uclass != Crater && !game.fbWritheUsedUnits.has(u.ref))
                    .sortBy(u => (u.region.toString, -u.uclass.cost))
                if (units.none)
                    EndAction(self)
                else if (remainingPains >= units.num) {
                    // More pains than units — auto-select all, skip choose menu
                    val chosen = units./(_.ref)
                    Force(FBWritheMoveAllAction(self, chosen))
                } else {
                    // Player chooses which units to pain
                    Ask(self).each(units)(u => FBWritheChoosePainUnitAction(self, u.ref, remainingPains, $))
                        .add(FBWritheUndoAllAction(self, game.fbWritheRolls, game.fbWritheNumDice).as("Undo all back to dice roll")(Writhe.styled(FB)))
                }
            }

        case FBWritheChoosePainUnitAction(self, uRef, remainingPains, chosen) =>
            val newChosen = chosen :+ uRef
            val remaining = remainingPains - 1
            if (remaining <= 0) {
                // All pains assigned, move to relocation phase
                Force(FBWritheMoveAllAction(self, newChosen))
            } else {
                // Sort by region name, then by unit cost descending
                val units = self.units.%(u => u.region.onMap && u.uclass != Crater && !game.fbWritheUsedUnits.has(u.ref) && !newChosen.has(u.ref))
                    .sortBy(u => (u.region.toString, -u.uclass.cost))
                if (units.none) {
                    // No more units to choose, move with what we have
                    Force(FBWritheMoveAllAction(self, newChosen))
                } else {
                    Ask(self).each(units)(u => FBWritheChoosePainUnitAction(self, u.ref, remaining, newChosen))
                        .when(newChosen.any)(FBWritheUndoLastPainAction(self, remaining, newChosen).as("Undo last pain selection")(Writhe.styled(FB)))
                        .add(FBWritheUndoAllAction(self, game.fbWritheRolls, game.fbWritheNumDice).as("Undo all back to dice roll")(Writhe.styled(FB)))
                }
            }

        // ── WRITHE PAIN: Phase 2 - Move all to one region or separately ──
        case FBWritheMoveAllAction(self, chosen) =>
            if (chosen.num == 1) {
                // Single unit: go straight to "Writhe [unit] to:" menu, no "separately" option
                Force(FBWritheMoveOneAction(self, chosen.head, $))
            } else {
                implicit val asking = Asking(self)
                + FBWritheMoveSeparatelyAction(self, chosen)
                areas.foreach { r =>
                    + FBWritheMoveAllToRegionAction(self, r, chosen)
                }
                + (FBWritheUndoAllAction(self, game.fbWritheRolls, game.fbWritheNumDice).as("Undo all back to dice roll")(Writhe.styled(FB)))
                asking
            }

        case FBWritheMoveAllToRegionAction(self, r, chosen) =>
            chosen.foreach { uRef =>
                val u = game.unit(uRef)
                val from = u.region
                game.fbWrithePainLog :+= FBWrithePainEntry(uRef, from, r)
                u.region = r
                u.onGate = false
                game.fbWritheUsedUnits :+= uRef
                self.log(Writhe.styled(FB) + ": relocated", u.uclass.styled(self), "from", from, "to", r)
            }
            EndAction(self)

        case FBWritheMoveSeparatelyAction(self, chosen) =>
            // Sort by cost descending
            val sorted = chosen.sortBy(uRef => -uRef.uclass.cost)
            if (sorted.any)
                Force(FBWritheMoveOneAction(self, sorted.head, sorted.tail))
            else
                EndAction(self)

        // ── WRITHE PAIN: Phase 3 - Move each unit separately ──
        case FBWritheMoveOneAction(self, uRef, remaining) =>
            val u = game.unit(uRef)
            // Round 8: per user request, show ALL regions where FB has any unit as
            // "join <unit>" options at the top of the menu. This includes:
            //   - the region the current unit is being pained FROM (its source),
            //   - any region where previously-pained units now sit (was the only
            //     case the old code handled — `fbWritheLastPainRegion`),
            //   - any region that already had FB presence before Writhe started.
            // Below those, the remaining regions follow as normal "writhe to" entries.
            // Earlier-round behavior only listed `fbWritheLastPainRegion` as a join
            // hint; this expansion gives the player a one-click way to keep clusters
            // together regardless of which region the cluster is in.
            implicit val asking = Asking(self)
            val fbRegionRefs = areas.%(r => self.at(r).any).sortBy(_.toString)
            fbRegionRefs.foreach { r =>
                // Pick a representative FB unit in this region for the "join <X>"
                // label. Prefer a unit other than the one being pained (so the label
                // names a unit that will still be there after the move). Fall back to
                // the unit being pained itself if it's the only FB unit in the source.
                val candidates = self.at(r).%!(_.ref == uRef)
                val rep = candidates.sortBy(-_.uclass.cost).headOption
                val label = rep./(_.uclass.name).|(u.uclass.name)
                + FBWritheMoveOneJoinAction(self, uRef, r, remaining, label)
            }
            areas.%!(fbRegionRefs.has).foreach(r => + FBWritheMoveOneToRegionAction(self, uRef, r, remaining))
            if (game.fbWrithePainLog.any) + (FBWritheUndoLastMoveAction(self, uRef, remaining).as("Undo last region selection")(Writhe.styled(FB)))
            + (FBWritheUndoAllAction(self, game.fbWritheRolls, game.fbWritheNumDice).as("Undo all back to dice roll")(Writhe.styled(FB)))
            asking

        case FBWritheMoveOneToRegionAction(self, uRef, r, remaining) =>
            val u = game.unit(uRef)
            val from = u.region
            game.fbWrithePainLog :+= FBWrithePainEntry(uRef, from, r)
            u.region = r
            u.onGate = false
            // Bug fix Round 6: store destination for "join" hint on next unit's region list
            game.fbWritheLastPainRegion = |(r)
            game.fbWritheLastPainedUnit = u.uclass.name
            self.log(Writhe.styled(FB) + ": relocated", u.uclass.styled(self), "from", from, "to", r)
            if (remaining.any)
                Force(FBWritheMoveOneAction(self, remaining.head, remaining.tail))
            else
                EndAction(self)

        // Bug fix Round 6: "join" variant dispatches identically to normal region move
        case FBWritheMoveOneJoinAction(self, uRef, r, remaining, _) =>
            val u = game.unit(uRef)
            val from = u.region
            game.fbWrithePainLog :+= FBWrithePainEntry(uRef, from, r)
            u.region = r
            u.onGate = false
            game.fbWritheLastPainRegion = |(r)
            game.fbWritheLastPainedUnit = u.uclass.name
            self.log(Writhe.styled(FB) + ": relocated", u.uclass.styled(self), "from", from, "to", r)
            if (remaining.any)
                Force(FBWritheMoveOneAction(self, remaining.head, remaining.tail))
            else
                EndAction(self)

        // ── INFERNAL PACT ──
        case FBInfernalPactMainAction(self) =>
            // Round 8 Bug 40: offer both faction spellbooks and IGOO spellbooks for flipping
            val available : $[Spellbook] = faceUpSpellbooks ++ faceUpIGOOSpellbooks
            if (available.any) {
                implicit val asking = Asking(self)
                available.foreach { sb =>
                    + FBInfernalPactChooseAction(self, sb)
                }
                + FBInfernalPactDoneAction(self)
                + FBInfernalPactCancelAction(self)
                asking
            } else
                Force(MainAction(self))

        case FBInfernalPactChooseAction(self, sb) =>
            // Round 8 Bug 75: IP discount is now a SEPARATE counter — NOT added to
            // self.power. This stops Writhe from reading inflated power when dice
            // are rolled. Flipping a spellbook only increments `fbInfernalPactDiscount`.
            // Spending it happens via the "transient boost" intercepts on cost-bearing
            // actions (which pre-add the consumed discount to power right before the
            // base handler deducts cost, so net power = original - (cost - consumed)).
            //
            // Snapshot spellbooks + unfulfilled on the first flip so Cancel (Bug 72)
            // can still revert any spellbook awarded mid-session.
            if (game.fbInfernalPactStartPower < 0) {
                game.fbInfernalPactStartPower = self.power
                game.fbInfernalPactSpellbooksBeforeSession = self.spellbooks
                game.fbInfernalPactUnfulfilledBeforeSession = self.unfulfilled
            }
            flipSpellbookDown(sb)
            game.fbInfernalPactDiscount += 1
            game.fbInfernalPactFlipped :+= sb
            self.log("Infernal Pact".styled(FB) + ": flipped", sb.name.styled(FB), "facedown, discount now", game.fbInfernalPactDiscount.power)
            // 2026-05-12 v4: FBTwoFacedownSpellbooks SBR no longer satisfied
            // immediately on the 2nd facedown flip. Moved to EndAction(FB) so the
            // player can't flip 2, earn a new SB, and immediately flip that new SB
            // in the SAME action. The SBR now fires after the action completes —
            // CheckSpellbooksAction at the natural AfterAction → PreMainAction
            // transition (Game.scala line 2573) handles the SB pick BEFORE any
            // unlimited actions are offered, so the 6th-SBR-grants-unlimited
            // scenario still lets FB IP-discount the unlimited battle.
            CheckSpellbooksAction(FBInfernalPactResumeAction(self))

        case FBInfernalPactResumeAction(self) =>
            Force(FBInfernalPactMainAction(self))

        case FBInfernalPactDoneAction(self) =>
            // Done - return to main action menu with discount active
            Force(MainAction(self))

        case FBInfernalPactCancelAction(self) =>
            // Undo flips from the CURRENT (post-commit) session only. Any
            // flips committed at main-action time stay flipped, and the
            // committed discount floor is preserved.
            game.fbInfernalPactFlipped.foreach { sb =>
                flipSpellbookUp(sb)
            }
            if (game.fbInfernalPactStartPower >= 0) {
                self.spellbooks = game.fbInfernalPactSpellbooksBeforeSession
                self.unfulfilled = game.fbInfernalPactUnfulfilledBeforeSession
            }
            // Reset discount to the committed floor (not zero).
            game.fbInfernalPactDiscount = game.fbInfernalPactCommittedDiscount
            game.fbInfernalPactStartPower = -1
            game.fbInfernalPactFlipped = $
            game.fbInfernalPactSpellbooksBeforeSession = $
            game.fbInfernalPactUnfulfilledBeforeSession = $
            // 2026-05-11: block IPMain re-offers this turn — see Game.scala
            // var declaration for rationale.
            game.fbInfernalPactCancelledThisTurn = true
            self.log("Cancelled", "Infernal Pact".styled(FB))
            Force(MainAction(self))

        // (doom-phase cancels use committed floor too)

        case FBInfernalPactCancelMainAction(self) =>
            // Same logic as FBInfernalPactCancelAction. Round 8 Bug 75: no power
            // refund (discount never touched power).
            game.fbInfernalPactFlipped.foreach { sb =>
                flipSpellbookUp(sb)
            }
            if (game.fbInfernalPactStartPower >= 0) {
                self.spellbooks = game.fbInfernalPactSpellbooksBeforeSession
                self.unfulfilled = game.fbInfernalPactUnfulfilledBeforeSession
            }
            game.fbInfernalPactDiscount = game.fbInfernalPactCommittedDiscount
            game.fbInfernalPactStartPower = -1
            game.fbInfernalPactFlipped = $
            game.fbInfernalPactSpellbooksBeforeSession = $
            game.fbInfernalPactUnfulfilledBeforeSession = $
            self.log("Cancelled", "Infernal Pact".styled(FB))
            Force(MainAction(self))

        // (doom-phase cancels use committed floor too)

        // Bug fix Round 4 (Bug 4): doom-phase Infernal Pact handlers. Mirror the main-action
        // versions exactly except for the return path — Done/Cancel route to DoomAction(self)
        // so the player stays in the doom phase. State mutations (flip spellbooks, bump power,
        // update game.fbInfernalPact* vars) are identical so the existing end-of-turn cleanup
        // in EndTurnAction works for both phases without modification.
        case FBInfernalPactDoomMainAction(self) =>
            // Round 8 Bug 40: offer both faction spellbooks and IGOO spellbooks for flipping
            val available : $[Spellbook] = faceUpSpellbooks ++ faceUpIGOOSpellbooks
            if (available.any) {
                implicit val asking = Asking(self)
                available.foreach { sb =>
                    + FBInfernalPactDoomChooseAction(self, sb)
                }
                + FBInfernalPactDoomDoneAction(self)
                + FBInfernalPactDoomCancelAction(self)
                asking
            } else
                Force(DoomAction(self))

        case FBInfernalPactDoomChooseAction(self, sb) =>
            // Round 8 Bug 75: same separate-pool treatment as the main-action variant.
            // Flipping only increments the discount counter; power is untouched.
            if (game.fbInfernalPactStartPower < 0) {
                game.fbInfernalPactStartPower = self.power
                game.fbInfernalPactSpellbooksBeforeSession = self.spellbooks
                game.fbInfernalPactUnfulfilledBeforeSession = self.unfulfilled
            }
            flipSpellbookDown(sb)
            game.fbInfernalPactDiscount += 1
            game.fbInfernalPactFlipped :+= sb
            self.log("Infernal Pact".styled(FB) + ": flipped", sb.name.styled(FB), "facedown, discount now", game.fbInfernalPactDiscount.power)
            if (self.needs(FBTwoFacedownSpellbooks) && faceDownCount >= 2)
                self.satisfy(FBTwoFacedownSpellbooks, "2 Facedown Spellbooks")
            CheckSpellbooksAction(FBInfernalPactDoomResumeAction(self))

        case FBInfernalPactDoomResumeAction(self) =>
            Force(FBInfernalPactDoomMainAction(self))

        case FBInfernalPactDoomDoneAction(self) =>
            // Done - return to doom action menu with discount active
            Force(DoomAction(self))

        case FBInfernalPactDoomCancelAction(self) =>
            // Revert only current session flips; preserve committed floor.
            game.fbInfernalPactFlipped.foreach { sb =>
                flipSpellbookUp(sb)
            }
            if (game.fbInfernalPactStartPower >= 0) {
                self.spellbooks = game.fbInfernalPactSpellbooksBeforeSession
                self.unfulfilled = game.fbInfernalPactUnfulfilledBeforeSession
            }
            game.fbInfernalPactDiscount = game.fbInfernalPactCommittedDiscount
            game.fbInfernalPactStartPower = -1
            game.fbInfernalPactFlipped = $
            game.fbInfernalPactSpellbooksBeforeSession = $
            game.fbInfernalPactUnfulfilledBeforeSession = $
            self.log("Cancelled", "Infernal Pact".styled(FB))
            Force(DoomAction(self))

        case FBInfernalPactCancelDoomAction(self) =>
            // Revert only current session flips; preserve committed floor.
            game.fbInfernalPactFlipped.foreach { sb =>
                flipSpellbookUp(sb)
            }
            if (game.fbInfernalPactStartPower >= 0) {
                self.spellbooks = game.fbInfernalPactSpellbooksBeforeSession
                self.unfulfilled = game.fbInfernalPactUnfulfilledBeforeSession
            }
            game.fbInfernalPactDiscount = game.fbInfernalPactCommittedDiscount
            game.fbInfernalPactStartPower = -1
            game.fbInfernalPactFlipped = $
            game.fbInfernalPactSpellbooksBeforeSession = $
            game.fbInfernalPactUnfulfilledBeforeSession = $
            self.log("Cancelled", "Infernal Pact".styled(FB))
            Force(DoomAction(self))

        // ── END OF MAIN ACTION ──
        // When FB's main action completes (EndAction / AfterAction reached), two
        // things must happen:
        //   1. Commit any IP flips from the pre-main session as non-cancelable
        //      (a subsequent post-main IP cancel should NOT undo them). Only
        //      runs if flips actually occurred.
        //   2. 2026-05-12 v4: check FBTwoFacedownSpellbooks SBR. Moved from
        //      the immediate post-flip path to here so that flipping 2 SBs and
        //      earning the 5th/6th doesn't let FB immediately flip that newly
        //      earned SB in the SAME action. The SB pick fires naturally at
        //      Game.scala:2573 (AfterAction → CheckSpellbooks → PreMainAction),
        //      which is BEFORE unlimited actions get offered — so if this is
        //      FB's final SBR and unlimited combat unlocks, FB can IP-discount
        //      the unlimited battle using the freshly-earned 6th SB.
        case EndAction(self) if self == FB =>
            if (game.fbInfernalPactFlipped.any || game.fbInfernalPactCommittedDiscount != game.fbInfernalPactDiscount) {
                game.fbInfernalPactCommittedDiscount = game.fbInfernalPactDiscount
                game.fbInfernalPactFlipped = $
                game.fbInfernalPactStartPower = -1
                game.fbInfernalPactSpellbooksBeforeSession = $
                game.fbInfernalPactUnfulfilledBeforeSession = $
            }
            if (self.needs(FBTwoFacedownSpellbooks) && faceDownCount >= 2)
                self.satisfy(FBTwoFacedownSpellbooks, "2 Facedown Spellbooks")
            UnknownContinue

        // ── END TURN: reset Infernal Pact and check gate requirement ──
        case EndTurnAction(self) if self == FB =>
            // Round 8 Bug 75: Infernal Pact discount is a separate pool from power.
            // No power clamp is needed — power was never boosted. Just zero the
            // session state. Any unused discount is simply discarded.
            game.fbInfernalPactDiscount = 0
            game.fbInfernalPactCommittedDiscount = 0
            game.fbInfernalPactStartPower = -1
            game.fbInfernalPactFlipped = $
            game.fbInfernalPactSpellbooksBeforeSession = $
            game.fbInfernalPactUnfulfilledBeforeSession = $
            // 2026-05-11: reset the cancel-loop guard so next turn FB can use IP again.
            game.fbInfernalPactCancelledThisTurn = false
            // Check Most Doom OR More Gates at end of turn (not mid-turn)
            // Bug fix Round 3: delegate to FBExpansion.checkMostDoomOrGates so the exact same
            // condition is also applied from DoomPhaseAction and DoomDoneAction (see Game.scala).
            FBExpansion.checkMostDoomOrGates()

            // Check 2 Facedown Spellbooks at end of turn (uses CURRENT total facedown count)
            if (FB.needs(FBTwoFacedownSpellbooks) && faceDownCount >= 2)
                FB.satisfy(FBTwoFacedownSpellbooks, "2 Facedown Spellbooks")

            UnknownContinue

        // ── THE EYE OPENS ──
        case FBTheEyeOpensMainAction(self) =>
            // Pay 1 power (once for the whole action)
            val eyeCost = 1
            val discountUsed = consumeDiscount(eyeCost)
            self.power -= (eyeCost - discountUsed)
            Force(FBTheEyeOpensLoopAction(self, $))

        case FBTheEyeOpensLoopAction(self, pending) =>
            // Find eligible regions: have desiccated + enemy cultist, not already targeted
            val doneRegions = pending./(_.region)
            val eligible = areas.%(r => !doneRegions.has(r) && self.at(r, Desiccated).any &&
                self.enemies.exists(_.at(r).%(_.uclass.utype == Cultist).any))
            if (eligible.none)
                Force(FBTheEyeOpensCommitAction(self, pending))
            else
                Ask(self).each(eligible)(r =>
                    FBTheEyeOpensRegionAction(self, r, pending))
                .add(FBTheEyeOpensCancelAction(self).as("Cancel")(TheEyeOpens.styled(FB) + ": Choose Region"))

        case FBTheEyeOpensRegionAction(self, r, pending) =>
            // Find enemy factions with cultists in this region
            val enemyFactions = self.enemies.%(e => e.at(r).%(_.uclass.utype == Cultist).any)
            if (enemyFactions.num == 1) {
                val f = enemyFactions.head
                val cultists = f.at(r).%(_.uclass.utype == Cultist)
                if (cultists.num == 1) {
                    Force(FBTheEyeOpensLoopAction(self, pending :+ FBEyeOpensTarget(r, f, cultists.head.ref)))
                } else {
                    Ask(f).each(cultists)(u => FBTheEyeOpensChooseCultistAction(self, f, r, u.ref, pending))
                }
            } else {
                Ask(self).each(enemyFactions)(f =>
                    FBTheEyeOpensFactionAction(self, r, f, pending))
                .add(FBTheEyeOpensCancelAction(self).as("Cancel")(TheEyeOpens.styled(FB) + ": Choose target faction"))
            }

        case FBTheEyeOpensFactionAction(self, r, f, pending) =>
            val cultists = f.at(r).%(_.uclass.utype == Cultist)
            if (cultists.num == 1) {
                Force(FBTheEyeOpensLoopAction(self, pending :+ FBEyeOpensTarget(r, f, cultists.head.ref)))
            } else {
                Ask(f).each(cultists)(u => FBTheEyeOpensChooseCultistAction(self, f, r, u.ref, pending))
            }

        case FBTheEyeOpensChooseCultistAction(self, f, r, uRef, pending) =>
            Force(FBTheEyeOpensLoopAction(self, pending :+ FBEyeOpensTarget(r, f, uRef)))

        case FBTheEyeOpensCommitAction(self, pending) =>
            // Execute all pending eliminations
            pending.foreach { case FBEyeOpensTarget(r, f, uRef) =>
                val u = game.unit(uRef)
                game.eliminate(u)
                val d = self.at(r, Desiccated).head
                game.eliminate(d)
                self.log(TheEyeOpens.styled(FB) + ": eliminated", u.uclass.styled(f), "and", Desiccated.styled(FB), "in", r)
                // Ice Age: costs 1 extra power in Ice Age region
                val iceAgeExtra = game.factions.exists(wf => wf.iceAge.contains(r))
                if (iceAgeExtra) {
                    self.power -= 1
                    self.log(TheEyeOpens.styled(FB), "costs extra", 1.power, "due to", IceAge)
                }
                self.power += 1
                self.log(TheEyeOpens.styled(FB) + ": gained", 1.power)
            }
            EndAction(self)

        case FBTheEyeOpensCancelAction(self) =>
            // Cancel — refund the 1 power cost, no eliminations happened
            self.power += 1
            EndAction(self)

        // ── CALL OF THE FAITHFUL ──
        case FBCallOfTheFaithfulMainAction(self) =>
            val eligible = areas.%(r => (self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any) && self.at(r, Acolyte).none)
            Ask(self).each(eligible)(r => FBCallOfTheFaithfulAction(self, r)).cancel

        case FBCallOfTheFaithfulAction(self, r) =>
            // Snapshot the pool acolyte BEFORE placing so we have a reliable
            // reference (self.at(r, Acolyte).last was fragile when the menu
            // re-renders and multiple acolytes are at r).
            val placed = self.pool(Acolyte).first
            self.place(Acolyte, r)
            // If the region has a gate and no faction has a keeper on it, the
            // placed Acolyte should IMMEDIATELY claim the gate — matching the
            // hand-play rule "placing a cultist on a free gate claims that
            // gate". Mirror ControlGateAction: set onGate, add region to
            // self.gates, clear from abandoned. Without this, the game is
            // left in a state where gate exists, acolyte is in the region,
            // but nothing is on the gate tile, which triggers the
            // AdjustGateControl menu next cycle even when GateDiplomacy is
            // off. ALSO: unconditionally run checkGatesGained at the end so
            // any missed edge case (e.g., the placed acolyte's onGate was
            // reset by a subsequent step) gets caught by the standard
            // game-level gate-gain sweep.
            val gateHere = game.gates.has(r)
            val noGateKeeper = game.factions.%(_.at(r).%(_.onGate).any).none
            if (gateHere && noGateKeeper) {
                self.at(r).foreach(_.onGate = false)
                placed.onGate = true
                if (!self.gates.has(r)) {
                    self.gates :+= r
                    self.abandoned :-= r
                    game.factions.foreach(_.abandoned :-= r)
                }
                self.log(CallOfTheFaithful.styled(FB) + ": placed Acolyte and took control of the gate in", r)
            }
            else
                self.log(CallOfTheFaithful.styled(FB) + ": placed Acolyte in", r)
            // Backstop: if the standard checkGatesGained sweep would have
            // claimed the gate, run it now so state is consistent even if
            // the conditions above didn't fire (defensive layer).
            game.checkGatesGained(self)
            // Round 8 bug fix (Bug 39): Call of the Faithful is an unlimited action —
            // it does not consume the turn or count as the faction's action. Return to
            // MainAction so the player can continue taking actions (same pattern as
            // gate diplomacy's AdjustGateControlAction).
            //
            // Note on Soft vs Hard: only the MAIN ACTION class (FBCallOfTheFaithfulMainAction,
            // the menu entry) has `with Soft` because it's pure navigation. This action class
            // (FBCallOfTheFaithfulAction) does NOT have `with Soft` — it's Hard because it
            // mutates state (places an Acolyte). Hard means it IS recorded in the undo log,
            // so undo correctly reverses the Acolyte placement. PowerNeutral just means it
            // doesn't deduct power.
            Force(MainAction(self))

        // ── CYCLOPEAN GAZE ──
        // Bug fix: only trigger when an action MOVED/PLACED/CREATED enemy units in FB ghato/rev regions,
        // not merely when units are PRESENT. Snapshot enemy counts at PreMainAction (start of action phase
        // turn) and compare against the post-action count. Restricting the snapshot hook to PreMainAction
        // ensures Cyclopean Gaze only fires during the action phase, never during the doom phase.
        case PreMainAction(f) if f != FB && game.factions.has(FB) && FB.has(CyclopeanGaze) && !FB.oncePerGame.has(CyclopeanGaze) =>
            // Clear stale entries from doom phase (Death March, etc.)
            game.fbCyclopeanGazeActionRegions = $
            val gazeRegions = areas.%(r => FB.at(r, RevenantOfKnaa).any || FB.at(r, Ghatanothoa).any)
            game.fbCyclopeanGazeSnapshot = (for {
                r <- gazeRegions
                ef <- game.factions.but(FB)
            } yield (ef, r) -> ef.at(r).%(_.uclass.utype != Building).num).toMap
            UnknownContinue

        // ── CYCLOPEAN GAZE EDGE CASES: ICE AGE, LETHARGY, DREAD CURSE ──
        // All three are zero-delta actions (no unit movement) that should trigger CG.
        // Each owning expansion records its target region in a game var because it
        // handles the action before FBExpansion in the dispatch order:
        //   Ice Age → game.fbCyclopeanGazeIceAgeRegion (set in FactionWW.scala)
        //   Lethargy → game.fbCyclopeanGazeLethargyRegion (set in FactionSL.scala)
        //   Dread Curse → game.fbCyclopeanGazeDreadCurseRegion (set in FactionOW.scala)
        // The AfterAction handler below checks all three vars.

        // ── CYCLOPEAN GAZE EDGE CASE: LETHARGY ──
        // Round 8 bug fix (Bug 36): Lethargy region is now recorded in SLExpansion's
        // LethargyMainAction handler (FactionSL.scala) because SLExpansion runs before
        // FBExpansion in the dispatch order. See game.fbCyclopeanGazeLethargyRegion.

        // ── CYCLOPEAN GAZE EDGE CASE: DREAD CURSE OF AZATHOTH ──
        // Round 8 bug fix (Bug 37): Dread Curse region is now recorded in OWExpansion's
        // DreadCurseAction handler (FactionOW.scala) because OWExpansion runs before
        // FBExpansion in the dispatch order. See game.fbCyclopeanGazeDreadCurseRegion.

        case AfterAction(actor) if actor != FB && game.factions.has(FB) && FB.has(CyclopeanGaze) && !FB.oncePerGame.has(CyclopeanGaze) =>
            // Bug fix Round 4 (Bugs 1 & 2): build per-source pain queue.
            // Each Revenant in a gaze region contributes 1 pain; each Ghatanothoa contributes 1 pain.
            // Each pain is logged with the originating source unit class (Revenant or Ghatanothoa),
            // and the per-source iteration replaces the old "total pains per region" model.
            // Battle hook (see Battle.scala PostBattlePhase) bypasses this AfterAction path entirely.
            //
            // Round 8 bug fix (Bug 33 - Ice Age): also check fbCyclopeanGazeIceAgeRegion.
            // IceAgeAction doesn't move units so the snapshot delta is always 0, but if
            // WW placed Ice Age in a gaze region where WW has non-Building units, CG fires.
            //
            // Round 8 bug fix (Bug 34 - double-counting): after CG pains are applied, the
            // chain returns here via Then(AfterAction(actor)). If 2+ enemy units arrived but
            // only 1 source pain was dealt, the delta is still positive and CG would re-fire.
            // Fix: update the snapshot to current counts BEFORE checking deltas, so that
            // pains already dealt are reflected and the re-check finds delta = 0.
            //
            val gazeRegions = areas.%(r => FB.at(r, RevenantOfKnaa).any || FB.at(r, Ghatanothoa).any)

            // Check for zero-delta edge cases: actions that target a region but don't move
            // units (snapshot delta = 0). Each action handler appends its target region to
            // game.fbCyclopeanGazeActionRegions; we check the list here, fire CG if applicable,
            // and clear it.
            //
            // Tracked zero-delta actions (Bugs 33, 36, 37, 54):
            //   - WW IceAgeAction
            //   - SL LethargyMainAction
            //   - OW DreadCurseAction
            //   - Game.scala BuildGateAction (any faction)
            //   - AN BuildCathedralAction
            val edgeCaseRegions : $[Region] = game.fbCyclopeanGazeActionRegions.distinct
            game.fbCyclopeanGazeActionRegions = $

            // Standard snapshot delta check for movement/summoning/placement actions.
            //
            // Round 8 bug fix (Bug 35 - Avatar false positive): only check regions that
            // WERE gaze regions at PreMainAction snapshot time. If a region is a gaze
            // region NOW but has no snapshot entry, it means a Revenant/Ghatanothoa was
            // moved there mid-action (e.g. by BG Avatar swapping the Revenant to Shub's
            // origin). The enemy units in that region were already present — they didn't
            // arrive this action. Using getOrElse(0) would make all existing units look
            // like "new arrivals" and falsely trigger CG. Skip these new gaze regions.
            //
            // Round 8 bug fix (Bug 47 - GoF): check ALL non-FB factions' deltas, not just
            // the actor's. Byatis God of Forgetfulness moves enemy cultists belonging to
            // factions OTHER than the actor (Byatis owner) into the gaze region. The
            // actor's count doesn't change, but other factions' counts do. We now iterate
            // every non-FB faction and trigger CG if any of them had units arrive in a
            // gaze region. Snapshots are updated for all factions (not just actor) to
            // prevent re-entry double-counting.
            val moveTriggeredRegions : $[Region] = gazeRegions.%{ r =>
                // Check all non-FB factions for arrivals in this gaze region
                var anyArrived = false
                game.factions.but(FB).foreach { ef =>
                    if (game.fbCyclopeanGazeSnapshot.contains((ef, r))) {
                        val current = ef.at(r).%(_.uclass.utype != Building).num
                        val before = game.fbCyclopeanGazeSnapshot((ef, r))
                        // Round 8 fix: update snapshot to current count so re-entry via
                        // Then(AfterAction) won't re-fire for the same arrivals
                        game.fbCyclopeanGazeSnapshot += (ef, r) -> current
                        if (current > before)
                            anyArrived = true
                    }
                }
                anyArrived
            }

            // Bug fix 2026-04-16 (CG double-trigger): a move into a gaze region registers
            // BOTH a delta (moveTriggeredRegions) AND gets appended to
            // fbCyclopeanGazeActionRegions via the Region setter in Game.scala:220-226.
            // Without dedup, edgeCaseSources + moveSources would each produce 1 source
            // per Rev/Ghato for the SAME region event, causing CG to trigger twice for
            // one move. Fix: compute the union of trigger regions (dedup), then emit
            // sources once per unique region.
            val triggerRegions : $[Region] = (moveTriggeredRegions ++
                edgeCaseRegions.%(r => gazeRegions.has(r) && actor.at(r).%(_.uclass.utype != Building).any)).distinct

            val sources : $[FBCyclopeanGazeSource] = triggerRegions./~ { r =>
                val sourceUnits : $[UnitClass] =
                    FB.at(r, RevenantOfKnaa).num.times(RevenantOfKnaa : UnitClass) ++
                    FB.at(r, Ghatanothoa).num.times(Ghatanothoa : UnitClass)
                // Each source (Revenant/Ghatanothoa) causes exactly 1 pain regardless
                // of how many enemy units arrived — do NOT cap at the arrival count
                sourceUnits./(u => FBCyclopeanGazeSource(r, u))
            }
            // Two-pass CG: store sources for Game.scala to fire AFTER triggers()/SBRs
            if (sources.any) {
                game.fbCyclopeanGazePendingSources = sources
                game.fbCyclopeanGazePendingActor = Some(actor)
            }
            UnknownContinue

        case FBCyclopeanGazePhaseAction(self, actor, sourcesPending, fromBattle) =>
            // Bug fix Round 4: pop one (region, sourceUnit) per step and dispatch a single pain.
            // Round 8 Bug 58: only the actor's units are valid CG targets (CG triggers
            // against the faction that took the action ending in a gaze region). Skip
            // sources whose region has no actor units.
            if (sourcesPending.any) {
                val FBCyclopeanGazeSource(r, srcUnit) = sourcesPending.head
                val rest = sourcesPending.tail
                val units = actor.at(r).%(u => u.uclass.utype != Building)
                if (units.any)
                    Force(FBCyclopeanGazeAssignPainAction(self, actor, r, srcUnit, rest, fromBattle))
                else
                    Force(FBCyclopeanGazePhaseAction(self, actor, rest, fromBattle))
            } else {
                // No more sources to process. Battle-mode hands control back to Battle.scala
                // via the marker action; action-phase mode re-fires AfterAction. The AfterAction
                // handler updates the snapshot before checking deltas (Round 8 fix), so the
                // re-entry is a no-op and normal flow resumes.
                if (fromBattle)
                    Force(FBCyclopeanGazeBattleDoneAction(self))
                else
                    Then(AfterAction(actor))
            }

        case FBCyclopeanGazeAssignPainAction(self, actor, r, sourceUnit, sourcesPending, fromBattle) =>
            // Round 8 Bug 58: CG only triggers against the FACTION THAT TOOK THE ACTION
            // (the actor). Other factions' units in the same gaze region (e.g., a previously-
            // deployed Dimensional Shambler from another faction) are NOT valid CG targets.
            // Per the user: "CG should only trigger against the faction that took an action
            // ending in a CG region." If the actor has no non-Building units in the region,
            // no CG fires for this source — skip to the next source.
            //
            // Round 8 Bug 57: the painted faction (= the actor) chooses which of their units
            // gets pained. Ask(actor) → menu border in actor's color. Action's self = actor
            // → menu title styled in actor's color. The painted faction's name appears in
            // the title.
            val units = actor.at(r).%(u => u.uclass.utype != Building)
            if (units.any)
                Ask(actor).each(units)(u => FBCyclopeanGazePainUnitAction(actor, actor, u.ref, r, sourceUnit, sourcesPending, fromBattle))
            else
                // Actor has no paintable units in this region — skip this source
                Force(FBCyclopeanGazePhaseAction(self, actor, sourcesPending, fromBattle))

        case FBCyclopeanGazePainUnitAction(self, actor, uRef, r, sourceUnit, sourcesPending, fromBattle) =>
            // Round 8 Bug 46: CG pains follow standard pain rules — destination region
            // cannot contain FB (the painer's faction) units (Buildings excepted).
            // Round 8 Bug 48: if no legal destinations exist, the unit is ELIMINATED
            // (auto-kill), matching standard battle pain rules (see Battle.scala line ~470
            // EliminateNoWayAction).
            // Round 8 Bug 51: if the painted faction has multiple non-Building units in
            // the region, prompt them to choose which to eliminate (a "soak" choice).
            // Round 8 Bug 61: FIRSTBORN chooses the destination region (not the painted
            // faction). This matches CW pain rules where the painter directs the pain
            // motion. Use Ask(FB) and pass FB as the destination action's `self` so the
            // menu is bordered in FB's color and the title is attributed to FB.
            val u = game.unit(uRef)
            val destinations = game.board.connectedForRetreat(u.region).%(_.glyph.onMap).%(d => FB.at(d).%(_.uclass.utype != Building).none)
            if (destinations.any)
                Ask(FB).each(destinations)(dest => FBCyclopeanGazeDestinationAction(FB, uRef, dest, sourceUnit, sourcesPending, actor, fromBattle))
            else {
                // No legal destinations — the painted faction must lose a unit. If they
                // have multiple non-Building units in the region, let them choose which.
                val painedFaction = u.faction
                val killCandidates = painedFaction.at(r).%(_.uclass.utype != Building)
                if (killCandidates.num <= 1) {
                    // Only the FB-selected unit (or no other choices) — eliminate it directly
                    val from = u.region
                    game.eliminate(u)
                    self.log(CyclopeanGaze.styled(FB) + " - " + sourceUnit.styled(FB) + ": " + u.uclass.styled(painedFaction) + " in " + from + " had nowhere to retreat and was eliminated")
                    Force(FBCyclopeanGazePhaseAction(self, actor, sourcesPending, fromBattle))
                } else {
                    // Multiple units — painted faction chooses which to lose
                    Ask(painedFaction).each(killCandidates)(k => FBCyclopeanGazeKillChoiceAction(self, painedFaction, k.ref, r, sourceUnit, sourcesPending, actor, fromBattle))
                }
            }

        case FBCyclopeanGazeKillChoiceAction(self, painedFaction, killRef, r, sourceUnit, sourcesPending, actor, fromBattle) =>
            // Round 8 Bug 51: painted faction's "soak" choice — eliminate the chosen unit
            val k = game.unit(killRef)
            val ucName = k.uclass.styled(painedFaction)
            game.eliminate(k)
            self.log(CyclopeanGaze.styled(FB) + " - " + sourceUnit.styled(FB) + ": " + ucName + " in " + r + " had nowhere to retreat and was eliminated")
            Force(FBCyclopeanGazePhaseAction(self, actor, sourcesPending, fromBattle))

        case FBCyclopeanGazeDestinationAction(self, uRef, dest, sourceUnit, sourcesPending, actor, fromBattle) =>
            // Bug fix Round 4: log the source unit class so the player sees which Revenant/Ghatanothoa
            // caused the pain. After applying, continue with remaining sources via FBCyclopeanGazePhaseAction.
            val u = game.unit(uRef)
            val from = u.region
            game.fbSuppressCGForPlacement = true
            u.region = dest
            game.fbSuppressCGForPlacement = false
            u.onGate = false
            self.log(CyclopeanGaze.styled(FB) + " - " + sourceUnit.styled(FB) + ": pained", u.uclass.styled(u.faction), "from", from, "to", dest)
            Force(FBCyclopeanGazePhaseAction(self, actor, sourcesPending, fromBattle))

        // Bug fix Round 4: marker that battle-mode Cyclopean Gaze is finished. Battle.scala's
        // action dispatcher catches this and calls proceed(); the FB expansion just acknowledges it.
        case FBCyclopeanGazeBattleDoneAction(self) =>
            UnknownContinue

        // ── CARNAGE (handled in Battle.scala post-battle phase) ──
        case FBCarnagePostBattleAction(self) =>
            implicit val asking = Asking(self)
            if (self.power >= 1)
                + FBCarnagePayPowerAction(self)
            if (faceUpSpellbooks.any)
                + FBCarnageFlipSpellbookAction(self)
            + FBCarnageCancelAction(self)
            asking

        case FBCarnagePayPowerAction(self) =>
            self.power -= 1
            self.takeES(1)
            self.log(Carnage.styled(FB) + ": paid", 1.power, "for", 1.es)
            UnknownContinue

        case FBCarnageFlipSpellbookAction(self) =>
            val available = faceUpSpellbooks
            Ask(self).each(available)(sb => FBCarnageChooseSpellbookAction(self, sb))

        case FBCarnageChooseSpellbookAction(self, sb) =>
            flipSpellbookDown(sb)
            self.takeES(1)
            self.log(Carnage.styled(FB) + ": flipped", sb.name.styled(FB), "facedown for", 1.es)
            UnknownContinue

        case FBCarnageCancelAction(self) =>
            UnknownContinue

        // ── AUGURY IN BATTLE (handled in Battle.scala) ──
        // These are dispatched from Battle.scala after dice are rolled
        case FBAuguryBattleReplaceAction(self, n) =>
            game.fbAuguryKills -= n
            self.log(Augury.styled(FB) + ": replaced", n, "Miss" + (n > 1).?("es").|(("")), "with Kills in battle")
            UnknownContinue

        case FBAuguryBattleCancelAction(self) =>
            UnknownContinue

        // ── Round 8 Bug 75: FB cost-bearing action intercepts (transient boost) ──
        // IP discount is a separate pool from power. Shared cost-bearing actions
        // in Game.scala (RitualAction, BuildGateAction, RecruitAction, SummonAction,
        // MoveAction, AttackAction) do `f.power -= cost` directly and don't know
        // about the discount pool. To make FB pay from discount FIRST, we intercept
        // each action in FBExpansion, consume discount up to `cost`, pre-add the
        // consumed amount to `f.power` as a transient boost, and return
        // UnknownContinue so the base handler's `f.power -= cost` runs. Net result:
        //     f.power = original + consumed - cost = original - (cost - consumed)
        // i.e. FB paid `cost - consumed` from real power, with `consumed` coming
        // from the discount pool.
        //
        // The guard `game.fbInfernalPactDiscount > 0` is the only condition —
        // when there's no discount the base handler runs unchanged. Each
        // intercept logs the consumed amount for traceability.
        //
        // Generic helper captured locally (can't be a class method because it
        // needs to appear inside the case-match flow).
        case RitualAction(f, cost, k) if f == FB && game.fbInfernalPactDiscount > 0 =>
            val consumed = min(game.fbInfernalPactDiscount, cost)
            game.fbInfernalPactDiscount -= consumed
            f.power += consumed
            FB.log("Infernal Pact".styled(FB), "discounted", consumed.power, "on ritual")
            UnknownContinue

        case BuildGateAction(f, r) if f == FB && game.fbInfernalPactDiscount > 0 =>
            val baseCost = 3 - f.has(UmrAtTawil).??(1)
            val consumed = min(game.fbInfernalPactDiscount, baseCost)
            game.fbInfernalPactDiscount -= consumed
            f.power += consumed
            FB.log("Infernal Pact".styled(FB), "discounted", consumed.power, "on build gate")
            UnknownContinue

        case RecruitAction(f, uc, r) if f == FB && game.fbInfernalPactDiscount > 0 =>
            val baseCost = f.recruitCost(uc, r)
            val consumed = min(game.fbInfernalPactDiscount, baseCost)
            game.fbInfernalPactDiscount -= consumed
            f.power += consumed
            FB.log("Infernal Pact".styled(FB), "discounted", consumed.power, "on recruit")
            UnknownContinue

        case SummonAction(f, uc, r) if f == FB && game.fbInfernalPactDiscount > 0 =>
            val baseCost = f.summonCost(uc, r)
            val consumed = min(game.fbInfernalPactDiscount, baseCost)
            game.fbInfernalPactDiscount -= consumed
            f.power += consumed
            FB.log("Infernal Pact".styled(FB), "discounted", consumed.power, "on summon")
            UnknownContinue

        case MoveAction(f, u, from, to, cost) if f == FB && game.fbInfernalPactDiscount > 0 =>
            val consumed = min(game.fbInfernalPactDiscount, cost)
            game.fbInfernalPactDiscount -= consumed
            f.power += consumed
            if (consumed > 0)
                FB.log("Infernal Pact".styled(FB), "discounted", consumed.power, "on move")
            UnknownContinue

        case AttackAction(f, r, fe, effect) if f == FB && game.fbInfernalPactDiscount > 0 && effect.has(FromBelow).not =>
            // Attack costs 1 power (skipped when FromBelow effect is in play)
            val consumed = min(game.fbInfernalPactDiscount, 1)
            game.fbInfernalPactDiscount -= consumed
            f.power += consumed
            FB.log("Infernal Pact".styled(FB), "discounted", consumed.power, "on attack")
            UnknownContinue

        case CaptureAction(f, r, fe, effect) if f == FB && game.fbInfernalPactDiscount > 0 && effect.has(FromBelow).not =>
            // Capture costs 1 power (+ tax). Base cost is 1 power; the tax is
            // region-specific and handled inside the base handler via payTax.
            val consumed = min(game.fbInfernalPactDiscount, 1)
            game.fbInfernalPactDiscount -= consumed
            f.power += consumed
            FB.log("Infernal Pact".styled(FB), "discounted", consumed.power, "on capture")
            UnknownContinue

        // ── Round 8 Bug 73: end-of-doom-phase IP cleanup for FB ──
        // DoomDoneAction in Game.scala does not clear FB's IP session state.
        // Without this intercept, any remaining discount / flipped-spellbook
        // tracking leaks into the next action phase, and the pre-acted FB main
        // menu still sees `fbInfernalPactDiscount > 0` and offers Cancel —
        // which then incorrectly refunds power and unflips spellbooks even
        // though the discount was already spent on a ritual. This mirrors the
        // cleanup block in EndTurnAction so doom-phase and action-phase IP
        // sessions are cleared identically. Power is clamped to the pre-IP
        // snapshot via the same formula as EndTurnAction.
        case DoomDoneAction(f) if f == FB =>
            // Round 8 Bug 75: separate-pool rework. No power clamp needed.
            game.fbInfernalPactDiscount = 0
            game.fbInfernalPactStartPower = -1
            game.fbInfernalPactFlipped = $
            game.fbInfernalPactSpellbooksBeforeSession = $
            game.fbInfernalPactUnfulfilledBeforeSession = $
            UnknownContinue

        // ...
        case _ => UnknownContinue
    }
}
