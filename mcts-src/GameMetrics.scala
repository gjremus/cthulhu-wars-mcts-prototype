package cws

import hrf.colmat._

// Intermediate-metrics report (user directive 2026-07-27): win/loss + a single doom
// number are too blunt to see WHICH sub-skill the brain is learning. For each faction
// in a finished game we capture the ten metrics the user named as "by far the most
// impactful": specific SBRs achieved, specific SBs chosen, SB count, GOOs awakened,
// total APs run, doom/AP, starting-power/AP, ritual total, avg doom/ritual, avg ES/ritual.
//
// Most are direct end-of-game reads off the Player; the two that the engine keeps NO
// history for — APs run and per-AP starting power — come from the Trajectory recorder
// that already snapshots them during play (Shaping.scala). Faction-agnostic: every value
// is a universal quantity read the same way for any seat.

/** All ten intermediate metrics for one faction in one finished game. */
final case class FactionMetrics(
    faction        : Faction,
    doom           : Int,
    spellbookCount : Int,
    spellbooks     : Seq[String],   // #2 which SBs earned (identities)
    sbrsAchieved   : Seq[String],   // #1 which requirements met
    sbrsRemaining  : Seq[String],
    goosAwakened   : Seq[String],   // #4 own GOOs on the map
    aps            : Int,           // #5 action phases run
    startPowSeries : Seq[Int],      // #7 power at start of each AP
    avgStartPow    : Double,
    rituals        : Int,           // #8 rituals performed
    elderSigns     : Int,           // hidden + revealed count
    avgEndGates    : Double,        // #11 avg CONTROLLED gates at AP end (doom base)
    avgEndAband    : Double         // #12 avg ABANDONED gates on board at AP end
) {
    def doomPerAP    : Double = if (aps > 0) doom.toDouble / aps else 0.0            // #6
    def doomPerRit   : Double = if (rituals > 0) doom.toDouble / rituals else 0.0    // #9
    def esPerRit     : Double = if (rituals > 0) elderSigns.toDouble / rituals else 0.0 // #10

    /** One compact line for the arena/iteration log. */
    def line : String = {
        val sbs  = if (spellbooks.isEmpty) "-" else spellbooks.mkString(",")
        val sbrs = if (sbrsAchieved.isEmpty) "-" else sbrsAchieved.mkString(",")
        val goo  = if (goosAwakened.isEmpty) "-" else goosAwakened.mkString(",")
        f"${faction.name}: doom=$doom%d SB=$spellbookCount%d/6[$sbs] SBR[$sbrs] GOO[$goo] " +
        f"APs=$aps%d doom/AP=${doomPerAP}%.1f startPow/AP=${avgStartPow}%.1f " +
        f"rit=$rituals%d doom/rit=${doomPerRit}%.1f ES/rit=${esPerRit}%.1f ES=$elderSigns%d " +
        f"gates/AP=${avgEndGates}%.1f aband/AP=${avgEndAband}%.1f"
    }
}

object GameMetrics {

    /** Compute the ten metrics for `f` from a finished game `g` and its play Trajectory. */
    def of(g : Game, f : Faction, traj : Trajectory) : FactionMetrics = {
        val p    = g.players(f)
        val reqs = f.requirements(g.options)
        val achieved  = reqs.filterNot(p.needs).map(_.text).toList
        val remaining = reqs.filter(p.needs).map(_.text).toList
        val sbs  = f.library.filter(sb => p.spellbooks.has(sb)).map(_.name).toList
        val goos = p.goos.factionGOOs.map(_.uclass.name).distinct.toList
        val rituals = g.ritualHistory.count(_ == f)
        val es   = p.es.num + p.revealed.num

        FactionMetrics(
            faction        = f,
            doom           = p.doom,
            spellbookCount = p.spellbooks.num,
            spellbooks     = sbs,
            sbrsAchieved   = achieved,
            sbrsRemaining  = remaining,
            goosAwakened   = goos,
            aps            = traj.apCount(f),
            startPowSeries = traj.startPowSeries(f),
            avgStartPow    = traj.avgStartPow(f),
            rituals        = rituals,
            elderSigns     = es,
            avgEndGates    = traj.avgEndGates(f),
            avgEndAband    = traj.avgEndAband(f)
        )
    }

    /** All seated factions' metrics for a finished game. */
    def all(g : Game, traj : Trajectory) : Seq[FactionMetrics] =
        g.setup.toList.map(f => of(g, f, traj))
}
