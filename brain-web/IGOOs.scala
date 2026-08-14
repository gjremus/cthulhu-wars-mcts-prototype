package cws

import hrf.colmat._

import html._

// IGOOs
case object ByatisCard extends IGOOLoyaltyCard(ByatisIcon, Byatis, power = 4, combat = 4)
case object AbhothCard extends IGOOLoyaltyCard(AbhothIcon, Abhoth, power = 4)
case object DaolothCard extends IGOOLoyaltyCard(DaolothIcon, Daoloth, power = 6)
case object NyogthaCard extends IGOOLoyaltyCard(NyogthaIcon, Nyogtha, power = 6, quantity = 2, combat = 4)

case object ByatisIcon extends UnitClass(Byatis.name + " Icon", Token, 0)
case object AbhothIcon extends UnitClass(Abhoth.name + " Icon", Token, 0)
case object DaolothIcon extends UnitClass(Daoloth.name + " Icon", Token, 0)
case object NyogthaIcon extends UnitClass(Nyogtha.name + " Icon", Token, 0)

// ── TULZSCHA ──
// Cost 4 to awaken, combat 1. Tulzscha's ongoing ability "Undying Flame" fires once per
// turn at Gather Power: if any enemy has more Doom, ES (face-down only), or Power than
// the Tulzscha owner, the owner gains +1 of whichever they're behind on (each checked
// independently). Spellbook requirement: give each enemy 2 Power. Spellbook "Ceremony of
// Annihilation": during the doom phase, the owner may take the current ritual cost as Power
// instead of performing a normal ritual (no Doom or ES gained, but ritual track advances).
case object TulzschaCard extends IGOOLoyaltyCard(TulzschaIcon, Tulzscha, power = 4, combat = 1)
case object TulzschaIcon extends UnitClass(Tulzscha.name + " Icon", Token, 0)

// ── Y'GOLONAC ──
// Cost 2 to awaken, combat 1. Y'Golonac's battle ability "Orifices" triggers when Y'Golonac
// is killed in battle: before elimination, the owner may choose an enemy Monster, Terror, or
// Cultist in the same battle. That unit is eliminated and Y'Golonac is placed under the
// target's faction's control with the loyalty card and spellbook transferred. Spellbook
// "The Revelations" (ongoing): during the doom phase, every enemy faction gains 1 Elder Sign.
// The spellbook requirement is satisfied automatically when a faction receives Y'Golonac
// (either by awakening or via Orifices transfer).
case object YgolonacCard extends IGOOLoyaltyCard(YgolonacIcon, Ygolonac, power = 2, combat = 1)
case object YgolonacIcon extends UnitClass(Ygolonac.name + " Icon", Token, 0)

case object Byatis extends UnitClass("Byatis", GOO, 4) with IGOO {
    override def canMove(u : UnitFigure)(implicit game : Game) = false
    override def canBeMoved(u : UnitFigure)(implicit game : Game) = false
}

case object Abhoth extends UnitClass("Abhoth", GOO, 4) with IGOO
case object Daoloth extends UnitClass("Daoloth", GOO, 6) with IGOO
case object Nyogtha extends UnitClass("Nyogtha", GOO, 6) with IGOO
case object Tulzscha extends UnitClass("Tulzscha", GOO, 4) with IGOO
case object Ygolonac extends UnitClass("Y'Golonac", GOO, 2) with IGOO

case object Filth extends UnitClass("Filth", Monster, 1) {
    override def canMove(u : UnitFigure)(implicit game : Game) = false
    override def canBattle(u : UnitFigure)(implicit game : Game) = false
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canBeSummoned(f : Faction)(implicit game : Game) = f.has(Fertility)
}

// Byatis
case object ToadOfBerkeley extends NeutralSpellbook("Toad of Berkeley")
case object GodOfForgetfulness extends NeutralSpellbook("God of Forgetfulness")

// Abhoth
case object LostAbhoth extends NeutralSpellbook("Lost Abhoth")
case object TheBrood extends NeutralSpellbook("The Brood")

// Daoloth
case object CosmicUnity extends NeutralSpellbook("Cosmic Unity") with BattleSpellbook
case object Interdimensional extends NeutralSpellbook("Interdimensional")

// Nyogtha
case object FromBelow extends NeutralSpellbook("From Below")
case object NyogthaPrimed extends NeutralSpellbook("Nyogtha Primed")
case object NyogthaMourning extends NeutralSpellbook("Nyogtha Mourning")

case object NightmareWeb extends NeutralSpellbook("Nightmare Web")

// Tulzscha spellbook: during the doom phase, the owner may take the current ritual cost
// as Power instead of performing a normal ritual (no Doom or ES, but ritual track advances)
case object CeremonyOfAnnihilation extends NeutralSpellbook("Ceremony of Annihilation")

// Y'Golonac spellbook: during the doom phase, every enemy faction gains 1 Elder Sign.
// Auto-satisfied when a faction receives Y'Golonac (via awakening or Orifices transfer).
case object TheRevelations extends NeutralSpellbook("The Revelations")


trait NeutralFaction extends Faction

case object NeutralAbhoth extends NeutralFaction {
    def name = "Neutral Abhoth"
    def short = "NA"
    def style = "nt"

    override def abilities = $
    override def library = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = 12.times(Filth)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = 0
}


case class IndependentGOOMainAction(self : Faction, lc : IGOOLoyaltyCard, l : $[Region]) extends OptionFactionAction(g => {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}", false""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        "Awaken " + lc.name.styled("nt") +
    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with MainQuestion with Soft
case class IndependentGOOAction(self : Faction, lc : LoyaltyCard, r : Region, cost : Int) extends BaseFactionAction(g => "Awaken " + lc.unit.styled(self) + g.forNPowerWithTax(r, self, cost) + " in", implicit g => r + self.iced(r))

case class GodOfForgetfulnessMainAction(self : Faction, d : Region, l : $[Region]) extends OptionFactionAction("God of Forgetfulness".styled(self)) with MainQuestion with Soft
case class GodOfForgetfulnessAction(self : Faction, d : Region, r : Region) extends BaseFactionAction(g => "Move all enemy Cultists to " + Byatis.styled(self) + " " + g.forNPowerWithTax(d, self, 1) + " from", r)

case class FilthMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Place " + Filth.styled(self)) with MainQuestion with Soft
case class FilthAction(self : Faction, r : Region) extends BaseFactionAction(g => "Place " + Filth.styled(self) + " " + g.forNPowerWithTax(r, self, 1) + " in", r)

case class NightmareWebMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Awaken " + Nyogtha.styled(self) + " with " + NightmareWeb.styled(self)) with MainQuestion with Soft
case class NightmareWebAction(self : Faction, r : Region) extends BaseFactionAction(g => "Awaken " + Nyogtha.styled(self) + g.forNPowerWithTax(r, self, 2) + " in", implicit g => r + self.iced(r))

// Y'Golonac Orifices: when Y'Golonac is killed in battle, this action lets the owner
// choose an enemy unit to replace. The target is eliminated, Y'Golonac transfers to the
// target's faction, and the loyalty card + spellbook transfer with it.
case class YgolonacOrificesAction(self : Faction, target : UnitRef) extends ForcedAction

// Tulzscha spellbook requirement: the owner voluntarily gives each enemy faction 2 Power.
// Once done, the owner gains the Ceremony of Annihilation spellbook.
case class TulzschaGivePowerMainAction(self : Faction) extends OptionFactionAction("Give each enemy 2 Power (Tulzscha SBR)".styled(self)) with MainQuestion with Soft
case class TulzschaGivePowerAction(self : Faction) extends BaseFactionAction(g => "Give each enemy 2 Power for " + Tulzscha.styled(self) + " Spellbook Requirement", g => "Give 2 Power")

// Ceremony of Annihilation: doom-phase option. Instead of a normal ritual, the owner
// earns Power equal to the current ritual cost. No Doom or Elder Signs are gained, but
// the ritual track still advances. Offered in the DoomAction handler alongside normal rituals.
case class CeremonyOfAnnihilationChoiceAction(self : Faction) extends OptionFactionAction(
    g => "Use " + CeremonyOfAnnihilation.styled(self) + " (earn " + g.ritualCost.power + ", no Doom/ES)"
) with DoomQuestion


object IGOOsExpansion extends Expansion {
    def checkAbhothSpellbook()(implicit game : Game) {
        factions.foreach { f =>
            if (f.has(Abhoth) && f.upgrades.has(TheBrood).not) {
                val monsters = f.units.monsters.inPlay

                if (monsters./(_.uclass).distinct.num >= 4 || monsters.num >= 8) {
                    f.upgrades :+= TheBrood
                    f.log("gained", TheBrood.styled(f), "for", Abhoth.styled(f))
                }
            }
        }
    }

    def checkInterdimensional()(implicit game : Game) {
        factions.foreach { f =>
            // Round 8 Bug 40: check facedown state — IGOO spellbooks flipped via
            // FB Infernal Pact are disabled (tracked in oncePerGame)
            if (f.has(Interdimensional) && !f.oncePerGame.has(Interdimensional)) {
                val r = f.goo(Daoloth).region

                if (game.lastDaolothRegion.has(r).not) {
                    game.lastDaolothRegion = |(r)

                    if (r.onMap && game.gates.has(r).not) {
                        game.gates :+= r
                        log(Daoloth.styled(f), "placed a Gate in", r, "with", Interdimensional.styled(f))
                    }
                }
            }
        }
    }

    // ── TULZSCHA: UNDYING FLAME (ongoing ability) ──
    // Fires once per turn during Gather Power (called via triggers()). Compares the
    // Tulzscha owner's Doom, Elder Signs, and Power against all enemies. For each
    // category where any enemy is strictly ahead, the owner gains +1 of that resource.
    // Guard: tulzschaFlameTurn tracks the last turn this fired. triggers() is called
    // from multiple places (gate checks, power gather, ritual), but the guard ensures
    // Undying Flame only activates once per turn — at the first triggers() call after
    // the turn counter increments (which happens in PowerGatherAction).
    def checkTulzschaUndyingFlame()(implicit game : Game) {
        if (game.tulzschaFlameTurn >= game.turn) return
        game.tulzschaFlameTurn = game.turn
        factions.foreach { f =>
            if (f.has(Tulzscha)) {
                val others = factions.but(f)
                // Doom: if any enemy has strictly more doom, gain +1 doom
                if (others.exists(_.doom > f.doom)) {
                    f.doom += 1
                    f.log(Tulzscha.styled(f), "gained 1 Doom from", "Undying Flame".styled(f))
                }
                // Elder Signs: compare only face-down (unspent) ES via es.num.
                // Bug fix: was (o.es.num + o.revealed.num) which counted revealed ES that
                // had already been converted to Doom, inflating both sides' totals.
                if (others.exists(o => o.es.num > f.es.num)) {
                    f.takeES(1)
                    f.log(Tulzscha.styled(f), "gained 1 Elder Sign from", "Undying Flame".styled(f))
                }
                // Power: if any enemy has strictly more power, gain +1 power
                if (others.exists(_.power > f.power)) {
                    f.power += 1
                    f.log(Tulzscha.styled(f), "gained 1 Power from", "Undying Flame".styled(f))
                }
            }
        }
    }

    override def triggers()(implicit game : Game) {
        checkAbhothSpellbook()
        checkInterdimensional()
        checkTulzschaUndyingFlame()
    }

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        val f = u.faction

        u.uclass @@ {
            case Byatis =>
                f.units :-= u
                f.upgrades :-= GodOfForgetfulness

                f.loyaltyCards :-= ByatisCard
                game.loyaltyCards :+= ByatisCard

            case Abhoth =>
                f.units :-= u
                f.upgrades :-= TheBrood

                f.loyaltyCards :-= AbhothCard
                game.loyaltyCards :+= AbhothCard

                f.oncePerAction :+= LostAbhoth

            case Daoloth =>
                f.units :-= u
                f.upgrades :-= CosmicUnity
                f.upgrades :-= Interdimensional

                f.loyaltyCards :-= DaolothCard
                game.loyaltyCards :+= DaolothCard

                game.lastDaolothRegion = None

            case Nyogtha if f.all(Nyogtha).but(u).any =>
                f.oncePerAction :+= NyogthaMourning

            case Nyogtha =>
                f.units = f.units.%!(_.uclass == Nyogtha)
                f.upgrades :-= FromBelow
                f.upgrades :-= NightmareWeb
                f.loyaltyCards :-= NyogthaCard
                game.loyaltyCards :+= NyogthaCard


            // Tulzscha eliminated: remove unit, revoke Ceremony of Annihilation spellbook,
            // return loyalty card to the available pool so another faction can awaken it later
            case Tulzscha =>
                f.units :-= u
                f.upgrades :-= CeremonyOfAnnihilation
                f.loyaltyCards :-= TulzschaCard
                game.loyaltyCards :+= TulzschaCard

            // Y'Golonac eliminated: remove unit, revoke The Revelations spellbook,
            // return loyalty card to pool. Guard on loyaltyCards.has because Orifices
            // can transfer the card to another faction — if the card already moved,
            // don't try to return it from the original owner.
            case Ygolonac =>
                f.units :-= u
                f.upgrades :-= TheRevelations
                if (f.loyaltyCards.has(YgolonacCard)) {
                    f.loyaltyCards :-= YgolonacCard
                    game.loyaltyCards :+= YgolonacCard
                }

            case _ =>
        }

        // 2026-05-11 §20 root-cause fix: if the case above removed `u` from
        // `f.units` (every iGOO case does — except the "another Nyogtha
        // remains" guard), the active battle's Side.forces may still hold a
        // reference to the now-orphaned UnitFigure. The next pain/retreat
        // Ask iterates Side.forces and emits AssignPainAction/RetreatUnitAction
        // options whose UnitRefs no longer resolve via `game.unit(ur)`. The
        // UI then crashes with None.get when rendering the action's
        // description (e.g. `AssignPainAction.option(game)` →
        // `unitRefFullToDesc(ur)` → `game.unit(ur).full`).
        //
        // Mirror what `Battle.eliminate` already does (it always calls
        // `exempt(u)` first), but for the `game.eliminate(u)` paths that
        // bypass `Battle.eliminate` — most notably DS's
        // `CosmicRulerSacrificeAction` and any other handler that eliminates
        // a unit via `game.eliminate` while a battle is in progress.
        if (!f.units.contains(u)) {
            game.battle.foreach { b =>
                b.attackers.forces = b.attackers.forces.but(u)
                b.defenders.forces = b.defenders.forces.but(u)
            }
        }
    }

    override def afterAction()(implicit game : Game) {
        factions.%(_.oncePerAction.has(LostAbhoth)).foreach { f =>
            NeutralAbhoth.units = f.units(Filth)./(u => new UnitFigure(NeutralAbhoth, u.uclass, u.index, (u.region == f.reserve).?(NeutralAbhoth.reserve).|(u.region), u.onGate, u.state, u.health))

            f.units = f.units.not(Filth)
        }

        factions.%(_.oncePerAction.has(NyogthaPrimed)).%!(_.oncePerAction.has(NyogthaMourning)).foreach { f =>
            if (f.upgrades.has(NightmareWeb).not) {
                f.upgrades :+= NightmareWeb

                f.log("gained", NightmareWeb.styled(f), "for", Nyogtha.styled(f))
            }
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        case IndependentGOOMainAction(self, lc, l) =>
            Ask(self).each(l)(r => IndependentGOOAction(self, lc, r, lc.power)).cancel

        case IndependentGOOAction(self, lc, r, _) =>
            self.loyaltyCards :+= lc
            game.loyaltyCards :-= lc

            self.power -= lc.power

            self.log("awakened", lc.unit.name.styled("nt"), "in", r, "for", lc.power.power)

            self.units :+= new UnitFigure(self, lc.unit, 1, r)

            lc.unit match {
                case Abhoth =>
                    if (game.neutrals.contains(NeutralAbhoth).not)
                        game.neutrals += NeutralAbhoth -> new Player(NeutralAbhoth)

                    self.units ++= NeutralAbhoth.units./(u => new UnitFigure(self, u.uclass, u.index, (u.region == NeutralAbhoth.reserve).?(self.reserve).|(u.region), u.onGate, u.state, u.health))

                    NeutralAbhoth.units = $

                case Daoloth =>
                    self.upgrades :+= CosmicUnity

                case Tulzscha =>

                case Nyogtha =>
                    self.units :+= new UnitFigure(self, lc.unit, 2, r)

                    self.upgrades :+= FromBelow

                case _ =>
            }

            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal)

                self.takeES(1)
            }

            EndAction(self)

        // BYATIS
        case GodOfForgetfulnessMainAction(self, d, l) =>
            Ask(self).each(l)(r => GodOfForgetfulnessAction(self, d, r)).cancel

        case GodOfForgetfulnessAction(self, d, r) =>
            self.power -= 1
            self.payTax(r)

            self.enemies.foreach { f =>
                f.at(r).cultists.foreach { u =>
                    u.region = d
                    u.onGate = false
                }
            }

            log(Byatis.styled(self), "used", GodOfForgetfulness.name.styled("nt"), "to move all enemy cultist from", r, "to", d)
            EndAction(self)

        // ABHOTH
        case FilthMainAction(self, l) =>
            Ask(self).each(l)(r => FilthAction(self, r)).cancel

        case FilthAction(self, r) =>
            self.power -= 1
            self.payTax(r)

            self.place(Filth, r)
            log(Abhoth.styled(self), "placed", Filth.styled(self), "in", r)

            EndAction(self)

        // DAOLOTH — Round 8 Bug 49 (FB CG ordering): when Daoloth moves to a new region
        // and the owner has Interdimensional, place the gate IMMEDIATELY here in the
        // MovedAction hook (before EndAction → AfterAction). This ensures the gate
        // exists before FB Cyclopean Gaze fires in AfterAction. Otherwise CG would
        // pain Daoloth out of the destination region BEFORE checkInterdimensional runs
        // (which only fires in the base AfterAction handler via checkGatesGained →
        // triggers()), causing the gate to be placed in the WRONG region (the pained-to
        // destination, not the originally intended move destination).
        case MovedAction(self, u, o, r) if u.uclass == Daoloth && self.has(Interdimensional) =>
            checkInterdimensional()
            MoveContinueAction(self, true)

        // NYOGTHA
        case MovedAction(self, u, o, r) if u.uclass == Nyogtha =>
            self.all(Nyogtha).but(u).not(Moved).%(_.region.onMap).single./(n => MoveSelectAction(self, n, n.region, 0)).|(MoveContinueAction(self, true))

        case NightmareWebMainAction(self, regions) =>
            Ask(self).each(regions)(r => NightmareWebAction(self, r)).cancel

        case NightmareWebAction(self, r) =>
            self.power -= 2
            self.payTax(r)

            val ny = self.pool.one(Nyogtha)

            ny.region = r

            self.log("awakened", Nyogtha.styled(self), "in", r, "with", NightmareWeb.styled(self))

            EndAction(self)

        // ── Y'GOLONAC: ORIFICES (battle ability) ──
        // Triggered from Battle.scala YgolonacOrificesPhase when Y'Golonac is killed.
        // The owner chooses an enemy Monster, Terror, or Cultist in the same battle.
        // That unit is eliminated, and Y'Golonac is placed under the victim's faction's
        // control. The loyalty card and spellbook (The Revelations) transfer with it.
        // If the receiving faction didn't already have The Revelations, they gain it
        // automatically (satisfies the spellbook requirement). After the transfer,
        // battle resumes at EliminatePhase to process the original Y'Golonac kill.
        case YgolonacOrificesAction(self, target) =>
            val targetFaction = target.faction
            val region = target.region

            // Eliminate the replaced unit (exempt from battle forces first so it's
            // removed cleanly without double-processing in EliminatePhase)
            val targetFigure = game.unit(target)
            game.battle.foreach(_.exempt(targetFigure))
            game.eliminate(targetFigure)

            // Place Y'Golonac in the target's region under the new owner
            val newYg = new UnitFigure(targetFaction, Ygolonac, 1, region)
            targetFaction.units :+= newYg

            // Transfer loyalty card from old owner to new owner
            self.loyaltyCards :-= YgolonacCard
            targetFaction.loyaltyCards :+= YgolonacCard

            // Transfer spellbook if the old owner already earned it
            if (self.upgrades.has(TheRevelations)) {
                self.upgrades :-= TheRevelations
                targetFaction.upgrades :+= TheRevelations
            }

            // Receiving Y'Golonac via Orifices automatically satisfies the SBR
            if (targetFaction.upgrades.has(TheRevelations).not) {
                targetFaction.upgrades :+= TheRevelations
                targetFaction.log("gained", TheRevelations.styled(targetFaction), "for receiving", Ygolonac.styled(targetFaction))
            }

            self.log(Ygolonac.styled(self), "used Orifices:", Ygolonac, "now belongs to", targetFaction)

            // Resume battle at EliminatePhase to process Y'Golonac's original kill
            BattleProceedAction(EliminatePhase)


        // ── CEREMONY OF ANNIHILATION (Tulzscha spellbook, doom-phase action) ──
        // Instead of a normal ritual, the owner gains Power equal to the current ritual
        // cost. No Doom or Elder Signs are awarded. The ritual track still advances (so
        // future rituals cost more). Counts as "performing the ritual" for spellbook
        // requirements (PerformRitual). Offered in the DoomAction handler when the owner
        // has CeremonyOfAnnihilation and hasn't already acted this doom phase.
        case CeremonyOfAnnihilationChoiceAction(self) =>
            val earned = game.ritualCost
            self.power += earned
            if (game.ritualTrack(game.ritualMarker) != 999)
                game.ritualMarker += 1
            game.showROAT()
            self.acted = true
            self.satisfy(PerformRitual, "Perform Ritual of Annihilation")
            self.log("used", CeremonyOfAnnihilation.styled(self), "and earned", earned.power, "(no Doom or Elder Signs)")
            CheckSpellbooksAction(DoomAction(self))

        // ── TULZSCHA SPELLBOOK REQUIREMENT ──
        // The owner voluntarily gives each enemy faction 2 Power. Once done, the owner
        // receives the Ceremony of Annihilation spellbook. Offered in the MainAction
        // handler when the owner has Tulzscha in play but hasn't earned the spellbook yet.
        case TulzschaGivePowerMainAction(self) =>
            Ask(self).add(TulzschaGivePowerAction(self)).cancel

        case TulzschaGivePowerAction(self) =>
            self.enemies.foreach { f =>
                f.power += 2
                log(f, "gained 2 Power from", Tulzscha.styled(self), "Spellbook Requirement")
            }

            if (self.upgrades.has(CeremonyOfAnnihilation).not) {
                self.upgrades :+= CeremonyOfAnnihilation
                self.log("gained", CeremonyOfAnnihilation.styled(self), "for", Tulzscha.styled(self))
            }

            EndAction(self)

        // ...
        case _ => UnknownContinue
    }
}
