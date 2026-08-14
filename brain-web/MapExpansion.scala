package cws

import hrf.colmat._
import html._

// ══════════════════════════════════════════════════════════════════════════════
// LIBRARY AT CELAENO — Map Expansion
// Implements Library-specific mechanics: Silence Tokens, Custodian, Librarian,
// Library Tomes (4 map spellbooks), and Agony Die.
// ══════════════════════════════════════════════════════════════════════════════

// ── MAP UNITS ──
case object TheCustodian extends MapUnitClass("Custodian")
case object TheLibrarian extends MapUnitClass("Librarian")

// ── NEUTRAL MAP FACTION ──
case object LibraryFaction extends NeutralFaction {
    def name = "Library"
    def short = "LB"
    def style = "lb"

    override def abilities = $
    override def library = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = $(TheCustodian, TheLibrarian)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = 0
}

// ── LIBRARY TOMES (Map Spellbooks) ──
// Four tomes, one per Special Collection room:
//   1. Barrier of Naach-Tith (always active, never flips) — battle gate
//   2. Guardian under the Lake (flippable) — move units between Archways
//   3. Larvae of the Outer Gods (flippable) — gain ES if opponent has more Power
//   4. Yr and the Nhhngr (flippable) — place Monster or gain Power if opponent has more Doom
sealed trait LibraryTome extends Record {
    def name : String
    def region(implicit game : Game) : Region
    def elem = name.styled("lb")
    override def toString = name
}
case object TomeBarrier extends LibraryTome {
    def name = "Barrier of Naach-Tith"
    def region(implicit game : Game) = LibraryCelaeno55.BarrierOfNaachTith
}
case object TomeGuardian extends LibraryTome {
    def name = "Guardian under the Lake"
    def region(implicit game : Game) = LibraryCelaeno55.GuardianUnderLake
}
case object TomeLarvae extends LibraryTome {
    def name = "Larvae of the Outer Gods"
    def region(implicit game : Game) = LibraryCelaeno55.LarvaeOfOuterGods
}
case object TomeYr extends LibraryTome {
    def name = "Yr and the Nhhngr"
    def region(implicit game : Game) = LibraryCelaeno55.YrAndTheNhhngr
}

// ── AGONY DIE ──
// Custom d6: faces 1, 2, 2, 3, 3, 4
object AgonyDie {
    val faces = $(1, 2, 2, 3, 3, 4)
    def roll() : Int = faces.shuffle.first
}

// ── CUSTODIAN / LIBRARIAN starting positions (black circles on L floor bitmaps) ──
// In joined vertical image coordinates. Librarian = left circle, Custodian = right circle.
object LibraryUnitPlacement {
    // (cx, cy) in joined vertical image space, diameter ~175px
    val librarianPos5L = (653, 1910)
    val custodianPos5L = (1164, 1910)
    val librarianPos3L = (642, 1959)
    val custodianPos3L = (1092, 1959)
    val unitSize = 150 // larger than monsters (~80) but smaller than GOOs (~160+)
}

// ── CONNECTION GLYPH HELPER ──
// Archway glyph: only when archway is the ONLY means of connection (not in base adjacency).
// Stairwell glyph: any base adjacency between regions on different floors (upper vs lower).
object ConnectionGlyph {
    def apply(from : Region, to : Region)(implicit game : Game) : String = {
        if (!game.board.isLibraryMap) return ""
        val base = game.board.connectedForRetreat(from) // = baseConnected on Library maps
        val inBase = base.contains(to)

        // Determine if regions are on different floors
        val upperRegions = LibraryCelaeno55.upperRegions
        val fromUpper = upperRegions.contains(from)
        val toUpper = upperRegions.contains(to)
        val crossFloor = fromUpper != toUpper

        // Stairwell: base adjacency across floors
        if (inBase && crossFloor) {
            val letter = StairwellLetters.letterFor(from, to).getOrElse("A")
            return s"""<img src="${Overlays.imageSource("stairwell-" + letter)}" style="height:1.4em;vertical-align:middle;margin-right:2px"/>"""
        }

        // Archway: only if NOT in base adjacency (archway is the sole connection)
        if (!inBase && game.board.archways.contains(from) && game.board.archways.contains(to))
            return s"""<img src="${Overlays.imageSource("archway-glyph")}" style="height:1.2em;vertical-align:middle;margin-right:2px"/>"""

        ""
    }
}

// ── STAIRWELL LETTER MAPPINGS ──
// Stairwell letters are assigned to physical positions. Different regions connect
// through them depending on which map halves (3U/5U, 3L/5L) are used.
// The letter is determined by the PAIR of cross-floor regions, not individual regions.
object StairwellLetters {
    import LibraryCelaeno55._
    // Cross-floor pairs → letter. Order doesn't matter within the pair.
    val pairs : $[((Region, Region), String)] = $(
        // A: top-left stairwell
        ((Byakhiary, ChamberOfSngac), "A"),
        ((YrAndTheNhhngr, ChamberOfSngac), "A"),  // 3U variant (no Byakhiary)
        // B: center stairwell
        ((Fountain, Oubliette), "B"),
        // C: top-right stairwell
        ((Horrorium, BlueHall), "C"),
        ((GuardianUnderLake, BlueHall), "C"),  // 3U variant (no Horrorium)
        // D: left-mid stairwell
        ((BarrierOfNaachTith, ChamberOfApkallu), "D"),  // 3L variant
        ((BarrierOfNaachTith, ScorchedChamber), "D"),    // 5L variant
        // E: center-bottom stairwell
        ((LakeOfHaliOverlook, Hyperquarium), "E"),
        // F: right-bottom stairwell
        ((LarvaeOfOuterGods, TheCrawlingOnes), "F")
    )

    def letterFor(from : Region, to : Region) : |[String] = {
        pairs.find { case ((a, b), _) => (a == from && b == to) || (a == to && b == from) }.map(_._2)
    }
}

// ── TOME PLACEMENT on map (pixel coordinates in vertical placement bitmap) ──
// Each tome occupies a black square on the 5U/3U tome bitmaps.
// Center and size are in the 1791x3584 vertical image coordinate space.
object TomePlacement {
    // 5U tome positions (in vertical placement bitmap coords)
    val positions5U : $[(LibraryTome, Int, Int, Int, Int)] = $(
        (TomeYr,       292,  749, 234, 234),
        (TomeGuardian, 1402, 774, 234, 234),
        (TomeLarvae,   1349, 1312, 231, 232),
        (TomeBarrier,  303,  1417, 238, 236)
    )
    // 3U tome positions (different layout)
    val positions3U : $[(LibraryTome, Int, Int, Int, Int)] = $(
        (TomeYr,       302,  552, 204, 196),
        (TomeGuardian, 1409, 537, 206, 200),
        (TomeLarvae,   1425, 1235, 206, 200),
        (TomeBarrier,  300,  1353, 208, 198)
    )
    // Pick positions based on whether the board has 5U or 3U upper floor
    def positions(implicit game : Game) : $[(LibraryTome, Int, Int, Int, Int)] = {
        val has5U = game.board.regions.exists(_.name == "Byakhiary")
        if (has5U) positions5U else positions3U
    }
}

// ── ACTIONS ──

// Custodian activation
case class SpendOnCustodianAction(self : Faction) extends OptionFactionAction(implicit g => "Activate " + "Custodian".styled("lb")) with MainQuestion
case class CustodianMoveAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Move " + "Custodian".styled("lb") + " to", r)
case class CustodianStayAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Custodian".styled("lb") + " stays in", implicit g => r.toString + " " + "(+1 to Agony roll)".styled("power"))
case class CustodianAssignAgonyAction(self : Faction, r : Region, remaining : Int, assigned : Map[Faction, Int]) extends ForcedAction
case class CustodianAssignToFactionAction(self : Faction, r : Region, remaining : Int, assigned : Map[Faction, Int], target : Faction) extends BaseFactionAction(implicit g => "Assign 1 " + "Agony".styled("lb") + " to", implicit g => target.full + " (" + remaining + " remaining)")
case class CustodianResolveAgonyAction(self : Faction, r : Region, assigned : Map[Faction, Int]) extends ForcedAction
case class CustodianMoveToOublietteAction(self : Faction, r : Region, target : Faction, uRef : UnitRef, remaining : Map[Faction, Int]) extends BaseFactionAction(implicit g => target.full + " moves", implicit g => g.unit(uRef).full + " to " + "Oubliette".styled("lb"))

// Librarian activation
case class SpendOnLibrarianAction(self : Faction) extends OptionFactionAction(implicit g => "Activate " + "Librarian".styled("lb")) with MainQuestion
case class LibrarianMoveAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Move " + "Librarian".styled("lb") + " to", r)
case class LibrarianStayAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Librarian".styled("lb") + " stays in", implicit g => r.toString + " " + "(+1 to Agony roll)".styled("power"))
// Librarian agony distribution: activator picks a faction, then picks how many of
// the remaining agony to assign to them, repeating until all agony is allocated.
// `total` is the original agony rolled (for "Reset agony assignments").
// `remaining` is the unassigned amount; `assigned` maps faction → count assigned;
// `order` preserves the activator's selection sequence so resolution walks in
// the same order the activator chose.
case class LibrarianAssignAgonyAction(self : Faction, r : Region, total : Int, remaining : Int, assigned : Map[Faction, Int], order : $[Faction]) extends ForcedAction
case class LibrarianAssignToFactionAction(self : Faction, r : Region, total : Int, remaining : Int, assigned : Map[Faction, Int], order : $[Faction], target : Faction) extends BaseFactionAction(
    implicit g => self.full + " — choose a faction to assign up to " + remaining + " " + "Agony".styled("lb") + " to",
    implicit g => {
        val sofar = assigned.getOrElse(target, 0)
        if (sofar > 0) target.full + " (" + sofar + " assigned)" else target.full
    }
)
case class LibrarianAssignAmountAction(self : Faction, r : Region, total : Int, remaining : Int, assigned : Map[Faction, Int], order : $[Faction], target : Faction, amount : Int) extends BaseFactionAction(
    implicit g => self.full + " — assign " + "Agony".styled("lb") + " to " + target.full + " (up to " + remaining + ")",
    implicit g => amount.toString + " " + "Agony".styled("lb")
)
case class LibrarianAssignCancelAction(self : Faction, r : Region, total : Int, remaining : Int, assigned : Map[Faction, Int], order : $[Faction]) extends BaseFactionAction(
    implicit g => self.full + " — assign " + "Agony".styled("lb"),
    implicit g => "Cancel"
)
case class LibrarianResetAgonyAction(self : Faction, r : Region, total : Int) extends BaseFactionAction(
    implicit g => self.full + " — choose a faction to assign up to " + total + " " + "Agony".styled("lb") + " to",
    implicit g => "Reset agony assignments"
)
case class LibrarianResolveAgonyAction(self : Faction, assigned : Map[Faction, Int], order : $[Faction]) extends ForcedAction
case class LibrarianSatisfyAgonyAction(target : Faction, agony : Int, remaining : Map[Faction, Int], activator : Faction, order : $[Faction]) extends ForcedAction
case class LibrarianEliminateUnitMainAction(self : Faction, agony : Int, remaining : Map[Faction, Int], activator : Faction, order : $[Faction]) extends OptionFactionAction("Eliminate a unit (1 " + "Agony".styled("lb") + ")") {
    def question(implicit game : Game) = self.full + " " + "Satisfy Agony".styled("lb") + " — " + agony + " remaining"
}
case class LibrarianEliminateRegionAction(self : Faction, r : Region, agony : Int, remaining : Map[Faction, Int], activator : Faction, order : $[Faction], eliminated : $[UnitRef]) extends ForcedAction
case class LibrarianEliminateUnitAction(self : Faction, uRef : UnitRef, r : Region, agony : Int, remaining : Map[Faction, Int], activator : Faction, order : $[Faction], eliminated : $[UnitRef]) extends BaseFactionAction(implicit g => "Eliminate", implicit g => g.unit(uRef).full)
case class LibrarianEliminateDoneAction(self : Faction, agony : Int, remaining : Map[Faction, Int], activator : Faction, order : $[Faction], eliminated : $[UnitRef]) extends ForcedAction
case class LibrarianReturnTomeMainAction(self : Faction, agony : Int, remaining : Map[Faction, Int], activator : Faction, order : $[Faction]) extends OptionFactionAction("Return " + "Overdue Tome".styled("lb") + " (1 " + "Agony".styled("lb") + ")") {
    def question(implicit game : Game) = self.full + " " + "Satisfy Agony".styled("lb") + " — " + agony + " remaining"
}
case class LibrarianReturnTomeAction(self : Faction, tome : LibraryTome, agony : Int, remaining : Map[Faction, Int], activator : Faction, order : $[Faction]) extends BaseFactionAction(implicit g => "Return", implicit g => tome.elem)
case class LibrarianLoseDoomAction(self : Faction, agony : Int, remaining : Map[Faction, Int], activator : Faction, order : $[Faction]) extends OptionFactionAction("Lose 1 " + "Doom".styled("doom") + " (1 " + "Agony".styled("lb") + ")") {
    def question(implicit game : Game) = self.full + " " + "Satisfy Agony".styled("lb") + " — " + agony + " remaining"
}

// Barrier of Naach-Tith battle gate
case class BarrierCheckAction(attacker : Faction, defender : Faction, then : Action) extends ForcedAction
case class BarrierReleaseCultistFactionAction(self : Faction, captiveFaction : Faction, then : Action) extends ForcedAction
case class BarrierDiscardESAction(self : Faction, then : Action) extends ForcedAction
case class BarrierDiscardTokenAction(self : Faction, then : Action) extends ForcedAction
case class BarrierBlockedAction(self : Faction) extends ForcedAction

// Tome flip via Silence Token
case class SpendToFlipTomeAction(self : Faction, tome : LibraryTome) extends BaseFactionAction(implicit g => "Flip " + tome.elem + " face-up", "(" + "Silence Token".styled("lb") + ")") {
    override def question(implicit game : Game) = "Spend " + "Silence Token".styled("lb")
}

// Tome usage actions
case class UseTomeGuardianMainAction(self : Faction) extends OptionFactionAction(implicit g => "Use " + TomeGuardian.elem + " (" + "1 Power".styled("power") + ")") with MainQuestion with Soft
case class UseTomeGuardianRelocateAction(self : Faction, source : Region, target : Faction) extends BaseFactionAction(implicit g => "Relocate " + target.full + " units in", source) {
    override def question(implicit game : Game) = "Relocate units with " + TomeGuardian.elem
}
case class UseTomeGuardianDestAction(self : Faction, source : Region, target : Faction, dest : Region) extends BaseFactionAction(implicit g => "Move to", dest) {
    override def question(implicit game : Game) = "Choose destination for " + target.full + " units from " + source
}

case class UseTomeLarvaeAction(self : Faction) extends OptionFactionAction(implicit g => "Use " + TomeLarvae.elem + " (" + "1 Power".styled("power") + ")") with MainQuestion

case class UseTomeYrMainAction(self : Faction) extends OptionFactionAction(implicit g => "Use " + TomeYr.elem + " (" + "1 Power".styled("power") + ")") with MainQuestion with Soft
case class UseTomeYrMonsterAction(self : Faction) extends OptionFactionAction(implicit g => "Place a Monster at a Gate") with Soft {
    def question(implicit game : Game) = "Use " + TomeYr.elem
}
case class UseTomeYrMonsterChooseAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Place " + uc.styled(self) + " in", r)
case class UseTomeYrPowerAction(self : Faction) extends OptionFactionAction(implicit g => "Gain " + "2 Power".styled("power") + " (net " + "+1".styled("power") + ")") {
    def question(implicit game : Game) = "Use " + TomeYr.elem
}

// Flippable tome flip-up actions (unlimited, no power cost) — shared by Guardian, Larvae, Yr
case class FlipTomeReleaseCultistMainAction(self : Faction, tome : LibraryTome) extends OptionFactionAction(implicit g => "Release a Captured Cultist to flip " + tome.elem + " face-up") with MainQuestion with Soft {
    override def question(implicit game : Game) = "Flip " + tome.elem + " face-up"
}
case class FlipTomeReleaseCultistAction(self : Faction, tome : LibraryTome, uRef : UnitRef) extends ForcedAction
case class FlipTomeDiscardESAction(self : Faction, tome : LibraryTome) extends OptionFactionAction(implicit g => "Discard an " + "Elder Sign".styled("es") + " to flip " + tome.elem + " face-up") with MainQuestion
case class FlipTomeDiscardTokenAction(self : Faction, tome : LibraryTome) extends OptionFactionAction(implicit g => "Discard a " + "Silence Token".styled("lb") + " to flip " + tome.elem + " face-up") with MainQuestion


// ── EXPANSION ──
object LibraryExpansion extends Expansion {

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {


        // ── CUSTODIAN ACTIVATION ──
        case SpendOnCustodianAction(self) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            self.log("spent a", "Silence Token".styled("lb"), "to activate the", "Custodian".styled("lb"))

            val currentRegion = game.custodianRegion

            val stayActions = currentRegion./(r => CustodianStayAction(self, r)).$
            val moveActions = game.board.regions.%(_.glyph.onMap).%(r => !currentRegion.has(r))./( r =>
                CustodianMoveAction(self, r))
            Ask(self).each(stayActions ++ moveActions)(identity)

        case CustodianMoveAction(self, r) =>
            game.custodianRegion = |(r)
            self.log("moved", "Custodian".styled("lb"), "to", r)
            RollAgony(_ => "Roll Agony die for Custodian", x => { log("Agony Die".styled("lb"), "rolled", x.toString.styled("power")); CustodianAssignAgonyAction(self, r, x, Map()) })

        case CustodianStayAction(self, r) =>
            self.log("Custodian".styled("lb"), "stays in", r, "(+1 to Agony roll)")
            RollAgony(_ => "Roll Agony die for Custodian (+1 for staying)", x => { log("Agony Die".styled("lb"), "rolled", x.toString.styled("power"), "+ 1 for staying =", (x + 1).toString.styled("power"), "total"); CustodianAssignAgonyAction(self, r, x + 1, Map()) })

        // ── CUSTODIAN AGONY DISTRIBUTION ──
        case CustodianAssignAgonyAction(self, r, remaining, assigned) =>
            val present = factions.%(_.at(r).%(u => u.uclass.utype != MapUnit).any)
            val totalUnitsPresent = present./~(_.at(r).%(u => u.uclass.utype != MapUnit)).num

            if (remaining <= 0 || totalUnitsPresent == 0)
                Force(CustodianResolveAgonyAction(self, r, assigned))
            else
                Ask(self).each(present)(f =>
                    CustodianAssignToFactionAction(self, r, remaining, assigned, f))

        case CustodianAssignToFactionAction(self, r, remaining, assigned, target) =>
            val newAssigned = assigned.updated(target, assigned.getOrElse(target, 0) + 1)
            Force(CustodianAssignAgonyAction(self, r, remaining - 1, newAssigned))

        // ── CUSTODIAN AGONY RESOLUTION — units to Oubliette ──
        case CustodianResolveAgonyAction(self, r, assigned) =>
            val next = assigned.find(_._2 > 0)
            next match {
                case None => EndAction(self)
                case Some((target, agony)) =>
                    val unitsInRegion = target.at(r).%(u => u.uclass.utype != MapUnit)
                    if (unitsInRegion.none)
                        Force(CustodianResolveAgonyAction(self, r, assigned - target))
                    else
                        Ask(target).each(unitsInRegion)(u =>
                            CustodianMoveToOublietteAction(self, r, target, u.ref, assigned.updated(target, agony - 1)))
            }

        case CustodianMoveToOublietteAction(self, r, target, uRef, remaining) =>
            val u = game.unit(uRef)
            val oubliette = game.board.regions.%(_.name == "Oubliette").head
            u.region = oubliette
            u.onGate = false
            target.log(u.uclass.styled(target), "moved to", oubliette, "(" + "Custodian".styled("lb") + ")")
            Force(CustodianResolveAgonyAction(self, r, remaining))

        // ── LIBRARIAN ACTIVATION ──
        case SpendOnLibrarianAction(self) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            self.log("spent a", "Silence Token".styled("lb"), "to activate the", "Librarian".styled("lb"))

            val currentRegion = game.librarianRegion

            val overdueHolders = factions.but(self).%(f =>
                game.tomeOverdue.exists { case (tome, overdue) => overdue && game.tomeHolders.get(tome).flatten.has(f) })
            val validRegions = game.board.regions.%(_.glyph.onMap).%(r =>
                overdueHolders.exists(_.at(r).%(u => u.uclass.utype != MapUnit).any))

            val stayActions = currentRegion./(r => LibrarianStayAction(self, r)).$
            val moveActions = validRegions.%(r => !currentRegion.has(r))./( r =>
                LibrarianMoveAction(self, r))
            val allActions = stayActions ++ moveActions

            // Offer guard at Game.scala:1622 only checks that *some* faction has an
            // overdue tome; it does not check that the holder has non-MapUnit units
            // on the map for the Librarian to actually visit. If the bot picks
            // SpendOnLibrarianAction and no valid move/stay action exists, `Ask` would
            // produce an empty action list and crash BotX.askE with `head of empty list`.
            // End the action cleanly in that case.
            if (allActions.isEmpty)
                EndAction(self)
            else
                Ask(self).each(allActions)(identity)

        case LibrarianMoveAction(self, r) =>
            game.librarianRegion = |(r)
            self.log("moved", "Librarian".styled("lb"), "to", r)
            RollAgony(_ => "Roll Agony die for Librarian", x => { log("Agony Die".styled("lb"), "rolled", x.toString.styled("power")); LibrarianAssignAgonyAction(self, r, x, x, Map(), $) })

        case LibrarianStayAction(self, r) =>
            self.log("Librarian".styled("lb"), "stays in", r, "(+1 to Agony roll)")
            RollAgony(_ => "Roll Agony die for Librarian (+1 for staying)", x => { val total = x + 1; log("Agony Die".styled("lb"), "rolled", x.toString.styled("power"), "+ 1 for staying =", total.toString.styled("power"), "total"); LibrarianAssignAgonyAction(self, r, total, total, Map(), $) })

        // ── LIBRARIAN AGONY DISTRIBUTION ──
        // Step 1: pick a faction (or reset). Eligible = ANY other faction with
        // units in the librarian's region — overdue-tome status is NOT a filter
        // (the activator may target any faction with units; that target then
        // satisfies the agony however they can — eliminate units, lose doom, OR
        // return an overdue tome if they hold one). If only one faction is
        // eligible AND nothing is assigned yet, skip the chooser.
        case LibrarianAssignAgonyAction(self, r, total, remaining, assigned, order) =>
            val eligibleTargets = factions.but(self).%(f =>
                f.at(r).%(u => u.uclass.utype != MapUnit).any)

            if (remaining <= 0 || eligibleTargets.none)
                Force(LibrarianResolveAgonyAction(self, assigned, order))
            else if (eligibleTargets.num == 1 && assigned.isEmpty)
                Force(LibrarianAssignAmountAction(self, r, total, remaining, assigned, order, eligibleTargets.head, remaining))
            else {
                val base = Ask(self).each(eligibleTargets)(f =>
                    LibrarianAssignToFactionAction(self, r, total, remaining, assigned, order, f))
                if (assigned.values.sum > 0)
                    base.add(LibrarianResetAgonyAction(self, r, total))
                else
                    base
            }

        // Step 2: pick how many agony to assign to the chosen target. Cancel
        // returns to step 1 with no change.
        case LibrarianAssignToFactionAction(self, r, total, remaining, assigned, order, target) =>
            Ask(self).each((1 to remaining).toList)(n =>
                LibrarianAssignAmountAction(self, r, total, remaining, assigned, order, target, n)
            ).add(LibrarianAssignCancelAction(self, r, total, remaining, assigned, order))

        case LibrarianAssignAmountAction(self, r, total, remaining, assigned, order, target, amount) =>
            val newAssigned = assigned.updated(target, assigned.getOrElse(target, 0) + amount)
            val newOrder = if (order.has(target)) order else order :+ target
            self.log("assigned", amount.toString.styled("power"), "Agony".styled("lb"), "to", target.full)
            Force(LibrarianAssignAgonyAction(self, r, total, remaining - amount, newAssigned, newOrder))

        case LibrarianAssignCancelAction(self, r, total, remaining, assigned, order) =>
            Force(LibrarianAssignAgonyAction(self, r, total, remaining, assigned, order))

        case LibrarianResetAgonyAction(self, r, total) =>
            self.log("reset", "Agony".styled("lb"), "assignments")
            Force(LibrarianAssignAgonyAction(self, r, total, total, Map(), $))

        // ── LIBRARIAN AGONY RESOLUTION — victim chooses ──
        // Walk factions in the activator's selection order.
        case LibrarianResolveAgonyAction(self, assigned, order) =>
            order.find(f => assigned.getOrElse(f, 0) > 0) match {
                case None => EndAction(self)
                case Some(target) =>
                    val agony = assigned(target)
                    Force(LibrarianSatisfyAgonyAction(target, agony, assigned.updated(target, 0), self, order))
            }

        case LibrarianSatisfyAgonyAction(target, agony, remaining, activator, order) =>
            if (agony <= 0)
                Force(LibrarianResolveAgonyAction(activator, remaining, order))
            else {
                val hasUnits = target.allInPlay.%(u => u.uclass.utype != MapUnit).any
                val hasDoom = target.doom > 0
                val hasOverdueTomes = game.tomeOverdue.exists { case (tome, overdue) =>
                    overdue && game.tomeHolders.get(tome).flatten.has(target) }

                if (!hasUnits && !hasDoom && !hasOverdueTomes)
                    Force(LibrarianSatisfyAgonyAction(target, 0, remaining, activator, order))
                else
                    Ask(target)
                        .when(hasUnits)(LibrarianEliminateUnitMainAction(target, agony, remaining, activator, order))
                        .when(hasOverdueTomes)(LibrarianReturnTomeMainAction(target, agony, remaining, activator, order))
                        .when(hasDoom)(LibrarianLoseDoomAction(target, agony, remaining, activator, order))
            }

        case LibrarianEliminateUnitMainAction(self, agony, remaining, activator, order) =>
            // Step 1: choose a single region to eliminate units from
            val regions = game.board.regions.%(r => self.at(r).%(u => u.uclass.utype != MapUnit).any)
            Ask(self).each(regions)(r =>
                LibrarianEliminateRegionAction(self, r, agony, remaining, activator, order, $)
                    .as(r)("Choose a region to eliminate units from"))

        case LibrarianEliminateRegionAction(self, r, agony, remaining, activator, order, eliminated) =>
            // Step 2: choose units in the selected region (with cancel/done)
            val units = self.at(r).%(u => u.uclass.utype != MapUnit && !eliminated.has(u.ref))
            if (units.none || agony <= 0)
                Force(LibrarianEliminateDoneAction(self, agony, remaining, activator, order, eliminated))
            else {
                val q = self.full + " " + "Satisfy Agony".styled("lb") + " - eliminate units in " + r
                Ask(self).each(units)(u =>
                    LibrarianEliminateUnitAction(self, u.ref, r, agony, remaining, activator, order, eliminated)
                ).add(LibrarianEliminateDoneAction(self, agony, remaining, activator, order, eliminated).as("Done")(q))
            }

        case LibrarianEliminateUnitAction(self, uRef, r, agony, remaining, activator, order, eliminated) =>
            // Don't eliminate yet - just mark as selected, reduce agony counter
            Force(LibrarianEliminateRegionAction(self, r, agony - 1, remaining, activator, order, eliminated :+ uRef))

        case LibrarianEliminateDoneAction(self, agony, remaining, activator, order, eliminated) =>
            // Now actually eliminate all selected units
            eliminated.foreach { ref =>
                val u = game.unit(ref)
                val r = u.region
                game.eliminate(u)
                self.log("eliminated", u.uclass.styled(self), "in", r, "to satisfy", "Agony".styled("lb"))
            }
            // 2026-05-11 loop-break: if the bot reaches Done with no eliminations,
            // it picked into Eliminate path but then refused every unit (Bot3 scoring
            // can rate every available unit more negatively than the empty-Done -100).
            // Without this guard, SatisfyAgony re-offers Eliminate, same outcome,
            // and the game enters a 7000-action `ControlGate`/`Eliminate` loop that
            // the SimRunner watchdog has to kill. Treat empty-Done as "waive the
            // current agony" so the satisfy loop exits.
            val newAgony = if (eliminated.isEmpty) 0 else agony
            Force(LibrarianSatisfyAgonyAction(self, newAgony, remaining, activator, order))


        case LibrarianReturnTomeMainAction(self, agony, remaining, activator, order) =>
            val overdueTomes = $(TomeBarrier, TomeGuardian, TomeLarvae, TomeYr).%(tome =>
                game.tomeOverdue.getOrElse(tome, false) && game.tomeHolders.get(tome).flatten.has(self))
            if (overdueTomes.num == 1)
                Force(LibrarianReturnTomeAction(self, overdueTomes.head, agony, remaining, activator, order))
            else
                Ask(self).each(overdueTomes)(t =>
                    LibrarianReturnTomeAction(self, t, agony, remaining, activator, order))

        case LibrarianReturnTomeAction(self, tome, agony, remaining, activator, order) =>
            game.tomeHolders = game.tomeHolders + (tome -> None)
            game.tomeOverdue = game.tomeOverdue + (tome -> false)
            self.log("returned", tome.elem, "to satisfy", "Agony".styled("lb"))
            Force(LibrarianSatisfyAgonyAction(self, agony - 1, remaining, activator, order))

        case LibrarianLoseDoomAction(self, agony, remaining, activator, order) =>
            self.doom -= 1
            self.log("lost", 1.doom, "to satisfy", "Agony".styled("lb"))
            Force(LibrarianSatisfyAgonyAction(self, agony - 1, remaining, activator, order))

        // ── TOME FLIP VIA SILENCE TOKEN ──
        case SpendToFlipTomeAction(self, tome) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            game.tomeFaceUp = game.tomeFaceUp + (tome -> true)
            self.log("spent a", "Silence Token".styled("lb"), "to flip", tome.elem, "face-up")
            EndAction(self)

        // ── BARRIER OF NAACH-TITH — battle gate cost ──
        case BarrierCheckAction(attacker, defender, then) =>
            // Factions whose cultists the attacker holds captive
            val captiveFactions = factions.%(f => attacker.at(f.prison).%(_.uclass.utype == Cultist).any)
            val hasES = attacker.es.any
            val hasToken = game.board.isLibraryMap && game.silenceTokens.getOrElse(attacker, 0) > 0

            if (captiveFactions.none && !hasES && !hasToken) {
                // No payments possible — auto-block
                attacker.log("attempted to battle.", "Battle was blocked by", TomeBarrier.elem)
                EndAction(attacker)
            }
            else {
                val q = "Battle blocked by " + TomeBarrier.elem + ", pay to override"
                Ask(attacker)
                    .each(captiveFactions)(f =>
                        BarrierReleaseCultistFactionAction(attacker, f, then)
                            .as(f.full + " cultist")(q))
                    .when(hasES)(BarrierDiscardESAction(attacker, then)
                        .as("Discard an " + "Elder Sign".styled("es"))(q))
                    .when(hasToken)(BarrierDiscardTokenAction(attacker, then)
                        .as("Discard a " + "Silence Token".styled("lb"))(q))
                    .add(BarrierBlockedAction(attacker).as("None")(q))
                    .cancel
            }

        case BarrierReleaseCultistFactionAction(self, captiveFaction, then) =>
            val u = self.at(captiveFaction.prison).%(_.uclass.utype == Cultist).head
            u.region = captiveFaction.reserve
            u.onGate = false
            log(captiveFaction.full, "cultist was returned")
            Force(then)

        case BarrierDiscardESAction(self, then) =>
            val es = self.es.head
            self.es = self.es.but(es)
            log("Elder Sign".styled("es"), "was discarded")
            Force(then)

        case BarrierDiscardTokenAction(self, then) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            log("Silence Token".styled("lb"), "was discarded")
            Force(then)

        case BarrierBlockedAction(self) =>
            self.log("attempted to battle.", "Battle was blocked by", TomeBarrier.elem)
            EndAction(self)

        // ── GUARDIAN UNDER THE LAKE — move enemy units between Archway regions ──
        case UseTomeGuardianMainAction(self) =>
            val arches = game.board.regions.%(game.board.archways.contains)
            // Show one entry per (region, enemy faction) combo where enemy has units
            val options = arches./~{ r =>
                factions.but(self).%(_.at(r).%(u => u.uclass.utype != MapUnit).any)./(f =>
                    UseTomeGuardianRelocateAction(self, r, f))
            }
            Ask(self).each(options)(identity).cancel

        case UseTomeGuardianRelocateAction(self, source, target) =>
            // Show all OTHER archway regions as destinations, with cancel
            val arches = game.board.regions.%(game.board.archways.contains).but(source)
            Ask(self).each(arches)(dest =>
                UseTomeGuardianDestAction(self, source, target, dest)).cancel

        case UseTomeGuardianDestAction(self, source, target, dest) =>
            self.power -= 1
            game.tomeFaceUp = game.tomeFaceUp + (TomeGuardian -> false)
            target.at(source).%(u => u.uclass.utype != MapUnit).foreach { u =>
                u.region = dest
                u.onGate = false
            }
            self.log("used", TomeGuardian.elem, "to move", target.full, "units from", source, "to", dest)
            EndAction(self)

        // ── LARVAE OF THE OUTER GODS — gain ES ──
        case UseTomeLarvaeAction(self) =>
            self.power -= 1
            game.tomeFaceUp = game.tomeFaceUp + (TomeLarvae -> false)
            if (factions.but(self).exists(_.power > self.power)) {
                self.takeES(1)
                self.log("used", TomeLarvae.elem, "and gained", 1.es)
            } else {
                self.log("used", TomeLarvae.elem, "(no opponent has more Power — no ES gained)")
            }
            EndAction(self)

        // ── YR AND THE NHHNGR ──
        case UseTomeYrMainAction(self) =>
            Ask(self)
                .when(self.pool.%(_.uclass.utype == Monster).any && self.gates.onMap.any)(
                    UseTomeYrMonsterAction(self))
                .add(UseTomeYrPowerAction(self))
                .cancel

        case UseTomeYrMonsterAction(self) =>
            val monsters = self.pool.%(_.uclass.utype == Monster)./(_.uclass).distinct
            val gates = self.gates.%(_.glyph.onMap)
            Ask(self).each(monsters./~(uc => gates./(r => UseTomeYrMonsterChooseAction(self, uc, r))))(identity).cancel

        case UseTomeYrMonsterChooseAction(self, uc, r) =>
            self.power -= 1
            game.tomeFaceUp = game.tomeFaceUp + (TomeYr -> false)
            val u = self.pool.%(_.uclass == uc).head
            u.region = r
            self.log("used", TomeYr.elem, "to place", uc.styled(self), "in", r)
            EndAction(self)

        case UseTomeYrPowerAction(self) =>
            self.power -= 1
            game.tomeFaceUp = game.tomeFaceUp + (TomeYr -> false)
            self.power += 2 // net +1
            self.log("used", TomeYr.elem, "and gained", 2.power)
            EndAction(self)

        // ── FLIPPABLE TOME FLIP-UP (unlimited actions, shared by Guardian/Larvae/Yr) ──
        case FlipTomeReleaseCultistMainAction(self, tome) =>
            val captured = factions./~(f => f.at(f.prison).%(_.uclass.utype == Cultist))
            Ask(self).each(captured)(u =>
                FlipTomeReleaseCultistAction(self, tome, u.ref)
                    .as(u.full, "of", u.faction)("Release a Captured Cultist")).cancel

        case FlipTomeReleaseCultistAction(self, tome, uRef) =>
            val u = game.unit(uRef)
            val owner = u.faction
            u.region = owner.reserve
            u.onGate = false
            game.tomeFaceUp = game.tomeFaceUp + (tome -> true)
            self.log("released", u.uclass.styled(owner), "to flip", tome.elem, "face-up")
            Force(MainAction(self))

        case FlipTomeDiscardESAction(self, tome) =>
            val es = self.es.head
            self.es = self.es.but(es)
            game.tomeFaceUp = game.tomeFaceUp + (tome -> true)
            self.log("discarded", es.short, "to flip", tome.elem, "face-up")
            Force(MainAction(self))

        case FlipTomeDiscardTokenAction(self, tome) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            game.tomeFaceUp = game.tomeFaceUp + (tome -> true)
            self.log("discarded a", "Silence Token".styled("lb"), "to flip", tome.elem, "face-up")
            Force(MainAction(self))

        case _ => UnknownContinue
    }

    def checkTomeAcquisition()(implicit game : Game) {
        $(TomeBarrier, TomeGuardian, TomeLarvae, TomeYr).foreach { tome =>
            if (game.board.regions.has(tome.region)) {
                val gateOwner = factions.find(_.gates.has(tome.region))
                val currentHolder = game.tomeHolders.getOrElse(tome, None)
                gateOwner match {
                    // Initial acquisition: tome is unheld and a faction now controls
                    // its gate. They take the tome.
                    case Some(f) if currentHolder.isEmpty =>
                        game.tomeHolders = game.tomeHolders + (tome -> |(f))
                        game.tomeOverdue = game.tomeOverdue + (tome -> false)
                        log(f, "acquired", tome.elem)
                    // Holder still controls the tome's gate — clear overdue if set.
                    case Some(f) if currentHolder.has(f) && game.tomeOverdue.getOrElse(tome, false) =>
                        game.tomeOverdue = game.tomeOverdue + (tome -> false)
                        log(f, tome.elem, "is no longer", "Overdue".styled("lb"))
                    // Holder no longer controls the gate (uncontrolled OR another
                    // faction controls it). Tome stays with the original holder —
                    // it is only released via the Librarian "return cursed tome"
                    // action — but it is now Overdue so the Librarian can target it.
                    case _ if currentHolder.isDefined && !gateOwner.has(currentHolder.get) =>
                        if (!game.tomeOverdue.getOrElse(tome, false)) {
                            game.tomeOverdue = game.tomeOverdue + (tome -> true)
                            log(currentHolder.get, tome.elem, "is now", "Overdue".styled("lb"))
                        }
                    case _ =>
                }
            }
        }
    }

    override def afterAction()(implicit game : Game) {
        if (!game.board.isLibraryMap) return
        checkTomeAcquisition()
    }

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (u.uclass == TheCustodian) game.custodianRegion = None
        if (u.uclass == TheLibrarian) game.librarianRegion = None
    }
}
