package cws

import hrf.colmat._


case class Evaluation(weight : Int, desc : String)
case class ActionEval(action : Action, evaluations : $[Evaluation])

class BotX[F <: Faction](ge : Game => GameEvaluation[F]) {
    def sortByAbs(a : $[Int]) : $[Int] =
        a.sortBy(v => -v.abs)

    def compareEL(aaa : $[Int], bbb : $[Int]) : Int =
        (aaa, bbb) match {
            case (a :: aa, b :: bb) => (a == b).?(compareEL(aa, bb)).|((a > b).?(1).|(-1))
            case (0 :: _, Nil) => 0
            case (Nil, 0 :: _) => 0
            case (a :: _, Nil) => (a > 0).?(1).|(-1)
            case (Nil, b :: _) => (0 > b).?(1).|(-1)
            case (Nil, Nil) => 0
        }

    def compare(a : ActionEval, b : ActionEval) = compareEL(sortByAbs(a.evaluations./(_.weight)), sortByAbs(b.evaluations./(_.weight))) > 0

    def ask(actions : $[Action], error : Double)(game : Game) : Action =
        askE(Explode.explode(game, actions), error)(game)

    def askE(actions : $[Action], error : Double)(game : Game) : Action = {
        if (actions.num == 1)
            return actions.head

        val ev = ge(game)
        // Merge faction-specific scores with Library map scores (BotMaps)
        val mapScores = BotMaps.eval(actions, ev.self)(game).groupBy(_._1).view.mapValues(_./(t => Evaluation(t._2, t._3))).toMap
        val eas = actions./(a => ActionEval(a, ev.eval(a) ++ mapScores.getOrElse(a, $)))
        Bot3.lastEval = eas

        val o = eas.sortWith(compare)

        if (ev.self == CC && o.num > 1) {
            val top = o.head.action
            val descs = o./~(_.evaluations./(_.desc)).distinct

            descs.foreach { d =>
                val t = o./(ae => ActionEval(ae.action, ae.evaluations.%(_.desc != d))).sortWith(compare).head.action
                Stats.triggerI(ev.self, d, t != top)
            }
        }

        var v = o
        val e = error * (1 - 2.0 /:/ actions.num)

        if (e > 0)
            while (random() < e) {
                v = v.drop(1)

                if (v.none)
                    v = o
            }

        val chosen = v.head
        chosen.action
    }

    def eval(game : Game, actions : $[Action]) : $[ActionEval] = {
        val ev = ge(game)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}


abstract class GameEvaluation[F <: Faction](val self : F)(implicit game : Game) {
    val others = game.factions.%(_ != self)

    implicit class SelfFactionClassify(val f : F) {
        def realDoom = self.doom + self.es./(_.value).sum
    }

    implicit class FactionClassify(val f : Faction) {
        def exists = game.players.contains(f)
        def aprxDoom = {
            val base = f.doom + (f.es.num * 1.67).round.toInt
            val tomePen = if (game.factions.has(TS) && f != TS) game.cursedTomesOwned.get(f).|(Nil).count { case (_, fd) => fd } else 0
            base - tomePen
        }
        def maxDoom = f.doom + min(6, f.es.num) * 3 + max(0, f.es.num - 6) * 2
        def count(uc : UnitClass) = f.all(uc).num
        def allSB = f.hasAllSB
        def numSB = f.spellbooks.num
        def blind(current : Faction) = willActBeforeFaction(current, f)
    }

    implicit class FactionListClassify(val l : $[Faction]) {
        def active = l.%(_.active)
    }

    val power = self.power
    def realDoom = self.doom + self.es./(_.value).sum
    def need(rq : Requirement) = self.needs(rq)
    def have(sb : Spellbook) = self.has(sb)
    def can(sb : Spellbook) = self.can(sb)
    def have(uc : UnitClass) = self.has(uc)
    def units(uc : UnitClass) = self.units(uc)
    def allSB = self.allSB
    def numSB = self.numSB
    def oncePerRound = self.oncePerRound

    // ─────────────────────────────────────────────────────────────────────
    // Round 9: Firstborn-awareness helpers. When FB is in the game, all other
    // bots should avoid (or heavily de-score) moving units into:
    //   1. Regions with a crater (FB Devil's Mark crater = gate-blocker and
    //      bad juju for anything that lingers there).
    //   2. Regions containing FB Revenants/Ghatanothoa AND FB has Cyclopean
    //      Gaze active — the unit will be immediately pained out.
    //
    // For multi-unit movement (Undulate, Arctic Wind, Screaming Dead, etc.)
    // the movement is only worth it if at least 3 units SURVIVE the CG pains
    // (one pain per FB Rev/Ghato source in the destination region).
    // See OTHER_BOTS_FB_STRATEGY.md for the full rules.
    // ─────────────────────────────────────────────────────────────────────
    val fbInGame = game.factions.has(FB)
    val fbHasCG = fbInGame && FB.has(CyclopeanGaze) && !FB.oncePerGame.has(CyclopeanGaze)
    def isFBGazeRegion(r : Region) : Boolean =
        fbInGame && (FB.at(r, Ghatanothoa).any || FB.at(r, RevenantOfKnaa).any)
    def fbGazeSourceCount(r : Region) : Int =
        if (!fbInGame) 0
        else FB.at(r, Ghatanothoa).num + FB.at(r, RevenantOfKnaa).num
    def hasFBCrater(r : Region) : Boolean =
        fbInGame && game.fbCraters.has(r)
    // Single-unit move: block if destination has ANY CG defender.
    def fbMoveAvoidance(r : Region) : |[(Int, String)] = {
        if (hasFBCrater(r)) |((-7000, "avoid FB crater region"))
        else if (fbHasCG && isFBGazeRegion(r)) |((-7000, "avoid FB CG region (1 unit <= defenders)"))
        else None
    }
    // Multi-unit move: block only if total movers <= CG defender count.
    // If movers > defenders, enough units survive — allow the move.
    def fbMultiMoveAvoidance(r : Region, movers : Int) : |[(Int, String)] = {
        if (hasFBCrater(r)) |((-7000, "avoid multi-move into FB crater region"))
        else if (fbHasCG && isFBGazeRegion(r)) {
            val defenders = fbGazeSourceCount(r)
            if (movers <= defenders) |((-7000, "multi-move blocked: movers(" + movers + ") <= CG defenders(" + defenders + ")"))
            else None
        }
        else None
    }

    implicit class RegionClassify(val r : Region) {
        def empty = allies.none && foes.none
        def allies = self.at(r)
        def foes = others./~(_.at(r))
        def of(f : Faction) = f.at(r)
        def str(f : Faction) = f.strength(of(f), self)
        def ownStr = str(self)
        def gate = game.gates.contains(r)
        def noGate = !gate
        def ownGate = self.gates.contains(r)
        def enemyGate = others.%(_.gates.contains(r)).any
        def chaosGate = game.chaosGateRegions.has(r)
        def freeGate = gate && !ownGate && !enemyGate && !chaosGate
        def controllers = (ownGate || enemyGate).??(owner.at(r).%(_.canControlGate))
        def gateOf(f : Faction) = f.gates.contains(r)
        def owner = game.factions.%(_.gates.contains(r)).single.get
        def capturers = allies.goos.none.??(others.%(f => f.at(r).goos.any || (allies.monsterly.none && f.at(r).monsterly.%(_.canCapture).any)))
        def desecrated = game.desecrated.contains(r)
        def near = r.connected
        def near2 = r.connected./~(_.connected).but(r).%!(near.has)
        def near012 = r.connected./~(_.connected).distinct
        def ocean = r.glyph == Ocean
        /* Check if unaccompanied cultist for given faction at risk of capture in this region */
        def riskyForCultists(f : Faction) = (allies ++ foes).%!(_.cultist).%!(_.faction == f).any
        /* Distance to specified faction unit type */
        def distanceTo(f : Faction, u : UnitType) = f.all(u)./(uf => game.board.distance(r, uf.region)).minOr(999)
        /* Distance from this region to Pole region opposite WW start location */
        def distanceToWWOppPole : Int = WW.exists.?(game.board.distance(r, game.board.starting(WW).but(game.starting(WW)).only)).|(999)
    }

    implicit class UnitListClassify(val us : $[UnitFigure]) {
        def active = us.%(_.active)
    }

    implicit class UnitClassify(val u : UnitFigure) {
        def active = u.faction.active
        def is(uc : UnitClass) = u.uclass == uc
        def ally = u.faction == self
        def foe = u.faction != self
        def friends = u.faction.at(u.region).%(_ != u)
        def enemies = game.factions.%(_ != u.faction)./~(_.at(u.region))
        def ownGate = u.region.ownGate
        def enemyGate = u.region.enemyGate
        def gateController = u.region.gate && u.region.controllers.contains(u)
        def gateKeeper = gateController && friends.%(_.canControlGate).none
        def defender = ownGate && (u.monster || u.terror || u.goo) && friends.monsterly.none
        def protector = (u.monster || u.terror || u.goo) && friends.cultists.any && friends.monsterly.none
        def preventsCaptureM = u.monsterly && friends.cultists.any && friends.monsterly.none && friends.goos.none && enemies.monsterly.any
        def preventsCaptureG = u.goo && friends.cultists.any && friends.goos.none && enemies.goos.any
        def prevents = preventsCaptureM || preventsCaptureG
        def preventsActiveCaptureM = u.monsterly && friends.cultists.any && friends.monsterly.none && friends.goos.none && enemies.monsterly.active.any
        def pretender = u.cultist && !capturable && enemyGate
        def shield = friends.goos.any
        def capturable = u.cultist && capturers.active.any
        def capturers = u.region.capturers
        def vulnerableM = u.cultist && friends.goos.none && friends.monsterly.none
        def vulnerableG = u.cultist && friends.goos.none
    }

    implicit def unitRefToUnitClassify(r : UnitRef) : UnitClassify = UnitClassify(r)

    def maxEnemyPower = others./(_.power).max

    def adjustedOwnStrengthForCosmicUnity(ownStr : Int, allies : $[UnitFigure], foes : $[UnitFigure], opponent : Faction) : Int = {
        val hasDaoloth = foes.exists(_.uclass == Daoloth)
        if (!hasDaoloth) return ownStr

        val allyGOOs = allies.filter(_.uclass.utype == GOO)
        if (allyGOOs.none) return ownStr

        val nyogthas = allies.filter(_.uclass == Nyogtha)
        val nyogthaReduction : Int = if (nyogthas.any) nyogthas.head.faction.strength(nyogthas, opponent) else 0

        val perGOOStrengths : $[Int] = allyGOOs.map(u => u.faction.strength($(u), opponent))
        val strongestGOOStr = perGOOStrengths.foldLeft(0)(math.max)

        val reduction = math.max(nyogthaReduction, strongestGOOStr)
        math.max(0, ownStr - reduction)
    }

    def active = others.%(_.active)

    def canSummon(u : UnitClass) = self.gates.%(r => power >= self.summonCost(u, r)).any && self.pool(u).any
    def canRitual = self.acted.not && power >= game.ritualCost

    def otherOceanGates = others./(_.gates.%(_.glyph == Ocean).any).any

    def instantDeathNow = game.ritualTrack(game.ritualMarker) == 999 || game.factions.%(_.doom >= 30).any
    def instantDeathNext = game.ritualTrack(game.ritualMarker) != 999 && game.ritualTrack(game.ritualMarker + 1) == 999

    def ritualsTillInstantDeath : Int = {
        val track = game.ritualTrack
        var i = game.ritualMarker
        var count = 0
        while (i < track.length && track(i) != 999) { i += 1; count += 1 }
        count
    }

    def instantDeathImminent : Boolean = {
        val remaining = ritualsTillInstantDeath
        if (remaining == 0) return true
        remaining <= game.factions.num
    }

    def doomPhaseTurnsRemaining : Int = {
        if (!game.doomPhase) return 0
        val facs = game.factions
        val playerCount = facs.num
        val firstIdx = facs.indexOf(game.first)
        val selfIdx = facs.indexOf(self)
        if (selfIdx < 0 || firstIdx < 0) return 0
        var count = 0
        var i = (selfIdx + 1) % playerCount
        while (i != firstIdx && count < playerCount) { count += 1; i = (i + 1) % playerCount }
        count
    }

    def instantDeathImminentForDM : Boolean = {
        val remaining = ritualsTillInstantDeath
        if (remaining == 0) return true
        val playerCount = game.factions.num
        val doomLeft = if (game.doomPhase) doomPhaseTurnsRemaining else playerCount
        (doomLeft + playerCount) >= remaining
    }

    def validGatesForRitual : $[Region] = {
        self.gates.filter { r =>
            val filthHere = game.factions.exists { other =>
                other != self &&
                other.has(TheBrood) &&
                other.at(r).exists(_.uclass == Filth)
            }
            !filthHere
        }
    }

    def maxDoomGain = validGatesForRitual.num + self.goos.num * 3
    def aprxDoomGain = validGatesForRitual.num + self.goos.num * 1.666

    def willActBeforeFaction(current : Faction, f : Faction) : Boolean = {
        if (power == 0)
            return false

        if (current == self)
            return power > 1 && f.power == 0

        if (current == f)
            return !f.allSB

        return factions.indexOf(f) > factions.indexOf(self)
    }

    /* Check if  WW could position lone cultist with no protector at opposite pole within specified turns */
    def wwLoneCultistPolarGate(turns : Int) : Boolean = {
        if (WW.exists && WW.needs(OppositeGate) && WW.active) {
            val oppPole = game.board.starting(WW).but(game.starting(WW)).head
            val cd = oppPole.distanceTo(WW, Cultist)
            val pd = min(oppPole.distanceTo(WW, Monster), oppPole.distanceTo(WW, GOO)) // Should consider Terror here as well?
            !oppPole.riskyForCultists(WW) && cd <= turns && WW.power >= cd && pd > cd
       }
        else
            false
    }

    def ofinale(f : Faction) = (3 * f.doom + 6 * f.gates.num + 5 * (f.es.num + (f match {
        case GC =>
            var p = f.power

            if (f.has(Cthulhu))
                p += 4

            p / 4

        case BG =>
            var p = f.power

            if (f.has(ShubNiggurath))
                p += 8

            if (p < 8)
                0
            else
                2

        case CC =>
            var p = f.power

            if (f.has(Nyarlathotep))
                p += 10

            if (p < 10)
                0
            else
                1 + min(p - 10, game.factions.%(_ != f)./(_.goos.num).sum * 2)

        case YS =>
            var p = f.power

            if (f.has(KingInYellow))
                p += 4

            if (f.has(Hastur))
                p += 10

            if (p < 4)
                0
            else
            if (p < 14)
                1
            else
                2 + (p - 14) / 2

        case SL =>
            var p = f.power

            if (f.has(Tsathoggua))
                p += 8

            if (p < 8)
                0
            else
            if (p < 12)
                1
            else
            if (p < 16)
                2
            else
                3

        case WW =>
            var p = f.power

            if (f.has(RhanTegoth))
                p += 6

            if (f.has(Ithaqua))
                p += 6

            val e = f.needs(AnytimeGainElderSigns).?(3).|(0)

            if (p < 6)
                0 + e
            else
            if (p < 12)
                1 + e
            else
                2 + e

        case OW =>
            var p = f.power

            if (f.has(YogSothoth))
                p += 6

            if (p < 6)
                0
            else
                1

        case AN =>
            var p = f.power

            if (p < 6)
                0
            else
                1

        case TS =>
            var p = f.power

            if (f.has(Glaaki))
                p += 8

            // TombHerds provide free power via Shepherd; Hecatomb multiplies that into doom
            if (f.has(Hecatomb))
                p += math.min(4, f.onMap(TombHerd).num) * 2

            p / 4

        // Round 8 (FB): same pattern as GC — Ghatanothoa is FB's GOO and acts
        // as a power-generating engine via Writhe / Eye Opens / etc. Without
        // this case, FB falls into `case _ => 0` and the bots under-estimate
        // FB's endgame doom potential, allowing FB to ritual unopposed.
        case FB =>
            var p = f.power

            if (f.has(Ghatanothoa))
                p += 4

            p / 4

        // Daemon Sultan (DS)
        case DS =>
            var p = f.power

            if (f.has(AvatarSynthesis))
                p += 8

            if (p < 6)
                0
            else
                1

        case _ =>
            0

    }))) >= 30 * 3

    def eval(a : Action) : $[Evaluation]

    // ────────────────────────────────────────────────────────────────────────
    // Round 8 (FB): Default scoring for the three FB-prompted choices that
    // get asked of *non-FB* factions:
    //
    //   1. FBCyclopeanGazePainUnitAction      — pick which of OUR units gets
    //                                           painted (relocated by CG).
    //   2. FBCyclopeanGazeKillChoiceAction    — pick which of OUR units to
    //                                           lose when CG has no legal pain
    //                                           destination.
    //   3. FBTheEyeOpensChooseCultistAction   — pick which cultist to lose to
    //                                           Eye Opens.
    //
    // Each non-FB bot calls `fbPromptedEvals(a)` from its eval and merges the
    // returned scores into its result. BotFB has its own custom logic for the
    // same actions and does NOT call this helper.
    //
    // Strategy: protect valuable units (GOOs, HighPriest, gate keepers,
    // monsters), prefer to sacrifice cheap/expendable off-gate units. Mirrors
    // BotFB's `FBCyclopeanGazeKillChoiceAction` "don't sacrifice valuable"
    // logic but applied from the painted faction's point of view.
    // ────────────────────────────────────────────────────────────────────────
    def fbPromptedEvals(a : Action) : $[Evaluation] = {
        var r : $[Evaluation] = $
        def add(w : Int, d : String) { r +:= Evaluation(w, d) }

        a.unwrap match {
            case FBCyclopeanGazePainUnitAction(_, _, uRef, _, _, _, _) =>
                val u = game.unit(uRef)
                if (u.goo)                              add(-5000, "don't pain own GOO")
                if (u.uclass == HighPriest)             add(-3000, "don't pain own HP")
                if (u.gateKeeper)                       add(-2000, "don't pain own gate keeper")
                if (u.monster)                          add(-1000, "avoid painting own monster")
                if (u.is(Acolyte) && !u.region.ownGate) add( 1000, "pain off-gate acolyte")
                if (u.cultist && !u.region.ownGate)     add(  500, "pain off-gate cultist")
                add(0, "default CG pain target")

            case FBCyclopeanGazeKillChoiceAction(_, _, killRef, _, _, _, _, _) =>
                val k = game.unit(killRef)
                if (k.goo)                              add(-5000, "don't sacrifice own GOO to CG")
                if (k.uclass == HighPriest)             add(-3000, "don't sacrifice own HP to CG")
                if (k.gateKeeper)                       add(-2000, "don't sacrifice own gate keeper to CG")
                if (k.monster)                          add(-1500, "avoid sacrificing own monster to CG")
                if (k.is(Acolyte) && !k.region.ownGate) add( 1000, "sacrifice off-gate acolyte to CG")
                if (k.cultist && !k.region.ownGate)     add(  500, "sacrifice off-gate cultist to CG")
                add(0, "default CG kill choice")

            case FBTheEyeOpensChooseCultistAction(_, _, _, uRef, _) =>
                val u = game.unit(uRef)
                if (u.uclass == HighPriest)             add(-2000, "don't sacrifice HP to eye opens")
                if (u.region.ownGate)                   add(-1500, "keep gate keeper from eye opens")
                if (u.is(Acolyte) && !u.region.ownGate) add( 1000, "lose off-gate acolyte to eye opens")
                add(0, "default eye opens cultist choice")

            case _ =>
        }

        r
    }
}
