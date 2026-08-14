package cws

import hrf.colmat._

// ══════════════════════════════════════════════════════════════════════════════
// BOT MAP SCORING — Library at Celaeno
// Shared map-specific scoring for all bot factions on Library maps.
// Called from Bot3.eval() for Library-specific action scoring.
// ══════════════════════════════════════════════════════════════════════════════

object BotMaps {
    def eval(actions : $[Action], self : Faction)(implicit game : Game) : $[(Action, Int, String)] = {
        if (!game.board.isLibraryMap)
            return $

        val others = game.factions.%(_ != self)
        val tomeRegions = $(LibraryCelaeno55.YrAndTheNhhngr, LibraryCelaeno55.GuardianUnderLake,
            LibraryCelaeno55.BarrierOfNaachTith, LibraryCelaeno55.LarvaeOfOuterGods)
        val hasSilenceToken = game.silenceTokens.getOrElse(self, 0) > 0
        val barrierHolder = game.tomeHolders.getOrElse(TomeBarrier, None)
        val barrierActive = barrierHolder.isDefined && !game.tomeOverdue.getOrElse(TomeBarrier, false)

        actions./~{ action => action match {

            // ── GATE TARGETING: boost tome regions as capture targets ──
            case a : FactionAction if a.self == self =>
                // General: factions that capture gates should prefer tome regions
                tomeRegions.%(r => others.exists(_.gates.has(r))).any.$(
                    (action, 200, "tome region gate is high-value target"))

            // ── MOVEMENT: penalize moving into custodian/librarian occupied area ──
            case MoveAction(f, _, _, to, _) if f == self =>
                val hasCustodian = game.custodianRegion.has(to)
                val hasLibrarian = game.librarianRegion.has(to)
                if (hasCustodian || hasLibrarian) $((action, -3000, "avoid custodian/librarian region"))
                else $

            // ── GATE DIPLOMACY: hard block abandon when custodian/librarian blocks re-control ──
            case AbandonGateAction(f, r, _) if f == self =>
                val blocked = game.custodianRegion.has(r) || game.librarianRegion.has(r)
                if (blocked) $((action, -1000000, "BLOCK abandon: custodian/librarian prevents re-control"))
                else $

            // ── STARTING REGION: prefer regions adjacent to tome regions ──
            case StartingRegionAction(f, r) if f == self =>
                val adjToTome = tomeRegions.exists(tr => game.board.connected(r).has(tr))
                if (adjToTome) $((action, 100, "starting: adjacent to tome region"))
                else $

            // ── OW BEYOND ONE: boost tome regions in AP1 ──
            case BeyondOneAction(f, _, _, r) if f == self && self == OW =>
                val isAP1 = game.ritualMarker == 0
                val isTomeRegion = tomeRegions.%(_ == r).any
                if (isAP1 && isTomeRegion) $((action, 100, "OW beyond one: tome region in AP1"))
                else $

            // ── CUSTODIAN/LIBRARIAN ACTIVATION: urgent when blocking own gate control ──
            case SpendOnCustodianAction(f) if f == self =>
                // Urgent if custodian is in a region with own monster-protected cultist
                val cusRegion = game.custodianRegion
                val urgent = cusRegion.exists(r => self.at(r).%(_.canControlGate).any &&
                    self.at(r).%(u => u.uclass.utype == Monster || u.uclass.utype == Terror || u.uclass.utype == GOO).any)
                if (urgent) $((action, 9500, "URGENT: custodian blocking own monster-protected cultist"))
                else $

            case SpendOnLibrarianAction(f) if f == self =>
                val libRegion = game.librarianRegion
                val urgent = libRegion.exists(r => self.at(r).%(_.canControlGate).any &&
                    self.at(r).%(u => u.uclass.utype == Monster || u.uclass.utype == Terror || u.uclass.utype == GOO).any)
                // Librarian: also conditioned on valid target region (not own gate or own GOO)
                val hasValidTarget = game.board.regions.%(_.glyph.onMap).exists(r =>
                    !libRegion.has(r) &&
                    others.exists(f2 => f2.at(r).%(u => u.uclass.utype != MapUnit).any &&
                        game.tomeOverdue.exists { case (tome, overdue) => overdue && game.tomeHolders.get(tome).flatten.has(f2) }) &&
                    !self.gates.has(r) && self.at(r).%(_.uclass.utype == GOO).none)
                if (urgent && hasValidTarget) $((action, 9500, "URGENT: librarian blocking own monster-protected cultist"))
                else if (urgent) $((action, 5000, "librarian blocking cultist but no valid target"))
                else $

            // ── CUSTODIAN ACTIVATION ──
            case SpendOnCustodianAction(_) =>
                val bestRegion = game.board.regions.%(_.glyph.onMap).%(r =>
                    r.name != "Oubliette" && others./~(_.at(r)).%(u => u.uclass.utype != MapUnit).num >= 3)
                if (bestRegion.any)
                    $((action, 2000, "custodian: enemy has 3+ units in non-Oubliette area"))
                else
                    $((action, 500, "custodian: default"))

            // ── CUSTODIAN REGION SELECTION (enemy factions only) ──
            case CustodianMoveAction(_, r) =>
                val isTome = tomeRegions.%(_ == r).any
                val enemyGateOwner = others.find(_.gates.has(r))
                val isOubliette = r.name == "Oubliette"
                if (isOubliette) $((action, -5000, "custodian: don't move to Oubliette"))
                else {
                    val score = enemyGateOwner match {
                        case Some(f) if isTome =>
                            val rank = others.sortBy(-_.gates.num)
                            if (rank.headOption.has(f)) 9000  // tome + most gates
                            else if (others.sortBy(-_.power).headOption.has(f)) 7000  // tome + most power
                            else if (others.sortBy(-_.doom).headOption.has(f)) 5000  // tome + most doom
                            else 4000  // tome + enemy gate
                        case Some(f) =>
                            val rank = others.sortBy(-_.gates.num)
                            if (rank.headOption.has(f)) 8000  // non-tome + most gates
                            else if (others.sortBy(-_.power).headOption.has(f)) 6000  // non-tome + most power
                            else 3000  // non-tome + enemy gate
                        case None if isTome => 2000  // tome but no enemy gate
                        case None =>
                            val enemyUnits = others./~(_.at(r)).%(u => u.uclass.utype != MapUnit).num
                            if (enemyUnits >= 3) 1500
                            else if (enemyUnits >= 1) 500
                            else -500
                    }
                    $((action, score, "custodian: region priority " + score))
                }

            // ── LIBRARIAN ACTIVATION ──
            case SpendOnLibrarianAction(_) =>
                val overdueHolders = others.%(f =>
                    game.tomeOverdue.exists { case (tome, overdue) => overdue && game.tomeHolders.get(tome).flatten.has(f) })
                val hasLargeGroup = overdueHolders.exists(f =>
                    game.board.regions.exists(r => f.at(r).%(u => u.uclass.utype != MapUnit).num >= 3))
                if (hasLargeGroup)
                    $((action, 3000, "librarian: overdue holder has 3+ units in one area"))
                else
                    $((action, 800, "librarian: default"))

            // ── LIBRARIAN REGION SELECTION (enemy factions only) ──
            case LibrarianMoveAction(_, r) =>
                // Must not move to region with own gate or own GOO
                val hasOwnGate = self.gates.has(r)
                val hasOwnGOO = self.at(r).%(_.uclass.utype == GOO).any
                if (hasOwnGate || hasOwnGOO) $((action, -10000, "librarian: can't target own gate/GOO region"))
                else {
                    val hasOwnMonsters = self.at(r).%(u => u.uclass.utype == Monster || u.uclass.utype == Terror || u.uclass.utype == GOO).any
                    val enemyFactions = others.%(_.at(r).%(u => u.uclass.utype != MapUnit).any)
                    val topDoomFaction = others.sortBy(-_.doom).headOption
                    val topPowerFaction = others.sortBy(-_.power).headOption
                    val goos = others./~(_.at(r)).%(_.uclass.utype == GOO)
                    val maxGooCost = if (goos.any) goos./(_.uclass.cost).max else 0

                    val score = if (!hasOwnMonsters && enemyFactions.any) 8000  // no own monsters
                        else if (enemyFactions.exists(f => topDoomFaction.has(f))) 6000  // most doom faction
                        else if (enemyFactions.exists(f => topPowerFaction.has(f))) 4000  // most power faction
                        else if (goos.any) 2000 + maxGooCost * 100  // highest cost GOO
                        else if (enemyFactions.any) 1000
                        else -500

                    $((action, score, "librarian: region priority " + score))
                }

            // ── AGONY ASSIGNMENT (enemy factions only, never self) ──
            case CustodianAssignToFactionAction(f, r, remaining, _, target) =>
                if (target == f) $((action, -10000, "agony: never target self"))
                else {
                    val hasGOO = target.allInPlay.%(_.uclass.utype == GOO).any
                    val unitCount = target.at(r).%(u => u.uclass.utype != MapUnit).num
                    val gooVulnerable = hasGOO && unitCount <= remaining
                    val mostDoom = others.but(self).sortBy(-_.doom).headOption.has(target)
                    val mostPower = others.but(self).sortBy(-_.power).headOption.has(target)
                    val score = if (gooVulnerable) 5000
                        else if (mostDoom) 3000
                        else if (mostPower) 2000
                        else 1000
                    $((action, score, "agony: target " + target.short + " (score " + score + ")"))
                }

            case LibrarianAssignToFactionAction(f, r, _, remaining, _, _, target) =>
                if (target == f) $((action, -10000, "agony: never target self (activator last resort)"))
                else {
                    val hasGOO = target.allInPlay.%(_.uclass.utype == GOO).any
                    val unitCount = target.at(r).%(u => u.uclass.utype != MapUnit).num
                    val gooVulnerable = hasGOO && unitCount <= remaining
                    val mostDoom = others.but(self).sortBy(-_.doom).headOption.has(target)
                    val mostPower = others.but(self).sortBy(-_.power).headOption.has(target)
                    val score = if (gooVulnerable) 5000
                        else if (mostDoom) 3000
                        else if (mostPower) 2000
                        else 1000
                    $((action, score, "agony: target " + target.short + " (score " + score + ")"))
                }

            // Bot heuristic for amount-pick step: assign as much as possible to
            // the highest-priority enemy at once (mirrors the per-faction logic
            // above; we only score "max amount = remaining" plays high).
            case LibrarianAssignAmountAction(f, r, _, remaining, _, _, target, amount) =>
                if (target == f) $((action, -10000, "agony amount: never target self"))
                else if (amount == remaining) $((action, 100, "agony amount: assign all"))
                else $((action, 50 - (remaining - amount), "agony amount: partial"))

            case LibrarianAssignCancelAction(_, _, _, _, _, _) =>
                $((action, -1000, "agony amount: avoid cancel"))

            case LibrarianResetAgonyAction(_, _, _) =>
                $((action, -2000, "agony amount: avoid reset"))

            // ── AGONY RESOLUTION: prefer returning tome > cultist > monster > doom > GOO ──
            case LibrarianReturnTomeMainAction(_, _, _, _, _) =>
                $((action, -100, "agony resolve: return tome (least bad)"))

            case LibrarianEliminateUnitAction(_, uRef, _, _, _, _, _, _) =>
                val u = game.unit(uRef)
                val score = u.uclass.utype match {
                    case Cultist => -300
                    case Monster => -500 - u.uclass.cost * 100
                    case Terror => -1500
                    case GOO => -10000
                    case _ => -200
                }
                $((action, score, "agony resolve: eliminate " + u.uclass.name))

            case LibrarianLoseDoomAction(_, _, _, _, _) =>
                $((action, -2000, "agony resolve: lose doom"))

            // ── BARRIER OF NAACH-TITH: avoid battling barrier holder without token ──
            case a : FactionAction if a.self == self && barrierActive && !hasSilenceToken =>
                // Check if this is an attack action against the barrier holder
                a match {
                    case AttackAction(_, _, defender, _) if barrierHolder.has(defender) =>
                        $((action, -5000, "barrier: no token to pay, avoid battle"))
                    case _ => $
                }

            // ── TOME USAGE: Yr and Larvae ──
            case UseTomeYrMainAction(_) =>
                if (others.exists(_.doom > self.doom))
                    $((action, 2500, "yr tome: enemy has more doom"))
                else
                    $((action, 200, "yr tome: no doom advantage"))

            case UseTomeLarvaeAction(_) =>
                if (others.exists(_.power > self.power))
                    $((action, 2500, "larvae tome: enemy has more power"))
                else
                    $((action, 200, "larvae tome: no power advantage"))

            // ── TOME FLIP-UP: Yr and Larvae high, Guardian medium ──
            case FlipTomeReleaseCultistMainAction(_, tome) =>
                tome match {
                    case TomeYr | TomeLarvae => $((action, 1500, "flip high-value tome via cultist"))
                    case TomeGuardian => $((action, 800, "flip guardian tome via cultist"))
                    case _ => $
                }

            case FlipTomeDiscardTokenAction(_, tome) =>
                tome match {
                    case TomeYr | TomeLarvae => $((action, 1200, "flip high-value tome via token"))
                    case TomeGuardian => $((action, 600, "flip guardian tome via token"))
                    case _ => $
                }

            case FlipTomeDiscardESAction(_, _) =>
                $((action, -800, "flip tome via ES: expensive"))

            // ── BARRIER PAYMENT ──
            case BarrierDiscardTokenAction(_, _) =>
                $((action, 1000, "pay barrier with token"))

            case BarrierReleaseCultistFactionAction(_, captiveFaction, _) =>
                if (captiveFaction == self) $((action, -500, "pay barrier: release own cultist"))
                else $((action, 800, "pay barrier: release enemy cultist"))

            case BarrierDiscardESAction(_, _) =>
                $((action, -300, "pay barrier with ES"))

            case BarrierBlockedAction(_) =>
                $((action, -2000, "barrier blocked: bad"))

            // ── FB: don't Dematerialize away from tome region ──
            case a : FactionAction if a.self == self && self == FB =>
                a match {
                    case _ if a.getClass.getSimpleName.contains("Dematerialize") =>
                        val heldTomeRegions = $(TomeBarrier, TomeGuardian, TomeLarvae, TomeYr)
                            .%(t => game.tomeHolders.getOrElse(t, None).has(self))./(_.region)
                        if (heldTomeRegions.any)
                            $((action, -2000, "FB: don't DM away from tome region"))
                        else $
                    case _ => $
                }

            // ── FB WRITHE: small tiebreaker toward tome regions for first unit ──
            case FBWritheMoveOneToRegionAction(f, _, r, _) if f == self && self == FB =>
                val isTomeRegion = tomeRegions.%(_ == r).any
                if (isTomeRegion) $((action, 300, "FB writhe: tome region tiebreaker"))
                else $

            // ── OW: don't Beyond One away from tome region ──
            case a : FactionAction if a.self == self && self == OW =>
                a match {
                    case _ if a.getClass.getSimpleName.contains("BeyondOne") =>
                        val heldTomeRegions = $(TomeBarrier, TomeGuardian, TomeLarvae, TomeYr)
                            .%(t => game.tomeHolders.getOrElse(t, None).has(self))./(_.region)
                        if (heldTomeRegions.any)
                            $((action, -3000, "OW: don't Beyond One away from tome region"))
                        else $
                    case _ => $
                }

            // ── DS: don't move gate out of controlled tome region ──
            case a : FactionAction if a.self == self && self == DS =>
                a match {
                    case _ if a.getClass.getSimpleName.contains("ChaosGate") || a.getClass.getSimpleName.contains("AnimateMatter") =>
                        val heldTomeRegions = $(TomeBarrier, TomeGuardian, TomeLarvae, TomeYr)
                            .%(t => game.tomeHolders.getOrElse(t, None).has(self))./(_.region)
                        if (heldTomeRegions.any)
                            $((action, -2500, "DS: don't move gate from tome region"))
                        else $
                    case _ => $
                }

            case _ => $
        }}
    }
}
