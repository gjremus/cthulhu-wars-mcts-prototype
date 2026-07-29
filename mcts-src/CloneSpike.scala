package cws

import hrf.colmat._
import com.rits.cloning.{Cloner, IFastCloner, IDeepCloner}
import java.util.{Map => JMap}

// Cheap-clone spike v2 — isolated prototype, touches no build.
//
// JVM serialization failed (Game is not Serializable). This tries a
// REFLECTION-based deep cloner (com.rits.cloning) that copies any object
// graph field-by-field via reflection — no Serializable required, no
// per-field code. If it works and is fast + faithful + isolated, it is a
// usable clone TODAY with zero engine changes.
//
// CAVEAT tested explicitly: the DS faction singleton holds mutable state
// OUTSIDE the Game object (azathothTrack etc.). A graph clone will NOT copy
// that (it's a global object, not reachable from Game). We flag whether that
// leaks — if so, the clone must also snapshot/restore the DS singleton vars.

object CloneSpike {

    def fixedSeating : $[Faction] = $(FB, GC, TS, DS)
    def newGame() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, false, $(UseGhast))

    // Scala immutable collections have mutable INTERNAL pointers (List's cons
    // cells, etc.) that a naive reflective walk corrupts. They are immutable by
    // contract, so sharing the reference is correct AND faster — tell the Cloner
    // "don't deep-clone these, copy the reference".
    // Custom fast-cloner for Scala immutable List: rebuild fresh cons cells,
    // deep-cloning each element (elements may be mutable Player/UnitFigure).
    // This avoids BOTH the corruption (naive reflective walk of cons cells) and
    // the leak (sharing the list shares its mutable elements).
    class ScalaListFastCloner extends IFastCloner {
        def clone(t : Object, dc : IDeepCloner, cache : JMap[Object, Object]) : Object = {
            val l = t.asInstanceOf[List[Any]]
            l.map(e => dc.deepClone(e.asInstanceOf[Object], cache))
        }
    }
    class ScalaMapFastCloner extends IFastCloner {
        def clone(t : Object, dc : IDeepCloner, cache : JMap[Object, Object]) : Object = {
            val m = t.asInstanceOf[Map[Any, Any]]
            m.map { case (k, v) =>
                dc.deepClone(k.asInstanceOf[Object], cache) ->
                dc.deepClone(v.asInstanceOf[Object], cache)
            }
        }
    }

    // Cloner subclass that treats captured lambdas as IMMUTABLE (shared by reference).
    // considerImmutable is consulted for EVERY object the cloner encounters — field
    // value OR collection element — so this catches lambdas wherever they hide,
    // unlike a field-scoped cloning strategy. Proven safe by LambdaCensus: every
    // reachable lambda captures only immutable values.
    class LambdaAwareCloner extends Cloner {
        // Scala's None is a SINGLETON, and Option.isEmpty is defined as `this eq None`.
        // If the reflection cloner copies None, the copy is not `eq None`, so isEmpty
        // wrongly returns false and every downstream `.get`/`.toList` throws None.get.
        // None is immutable — share it by reference (like Nil, the flyweights, lambdas).
        private val noneClass = None.getClass
        override def considerImmutable(clz : Class[_]) : Boolean =
            (clz eq noneClass) ||
            clz.getName.contains("$$Lambda") ||
            super.considerImmutable(clz)
    }

    val cloner : Cloner = {
        val c = new LambdaAwareCloner()
        // (1) Register custom Scala-collection fast-cloners (the jar only ships
        //     Java-collection cloners; Scala Lists/Maps need these).
        c.registerFastCloner(classOf[scala.collection.immutable.$colon$colon[_]], new ScalaListFastCloner)
        c.registerFastCloner(scala.collection.immutable.Nil.getClass, new ScalaListFastCloner)
        c.registerFastCloner(classOf[scala.collection.immutable.Map[_, _]], new ScalaMapFastCloner)
        // (2) Immutable DOMAIN FLYWEIGHTS: Faction/Region/Spellbook/UnitClass/
        //     GameOption/Board are shared case-object/singletons that the engine
        //     dispatches on by IDENTITY (case DS =>, players(DS)). Copying them
        //     breaks pattern-matching + map lookups. Share them.
        //     NOTE: do NOT share cws.Expansion here — `class Game extends
        //     Expansion`, so that rule would treat the whole Game as a flyweight
        //     and never copy it (the clone would BE the original).
        c.dontCloneInstanceOf(
            classOf[cws.Faction],
            classOf[cws.Region],
            classOf[cws.Spellbook],
            classOf[cws.UnitClass],
            classOf[cws.GameOption],
            classOf[cws.Board])
        // (3) CAPTURED LAMBDAS are handled by LambdaAwareCloner.considerImmutable above
        //     (action label thunks: Game => String; JDK-25 hidden classes with final
        //     captured fields the reflection walk cannot write). Proven safe to share
        //     by LambdaCensus (every reachable lambda captures only immutable values).
        c
    }

    def deepCopy(g : Game) : Game = cloner.deepClone(g)

    // Clone the game AND its pending Continue together, so the cloned Continue's
    // actions/closures reference the CLONED game's objects (not the original's).
    // This is the correct unit to fork for a search: (state, whose-turn-what-options).
    def deepCopyPair(g : Game, c : Continue) : (Game, Continue) = {
        val pair = cloner.deepClone((g, c))
        (pair._1, pair._2)
    }

    def playTo(depth : Int) : (Game, Continue) = {
        val game = newGame()
        val (_, cc0) = game.perform(StartAction)
        var c = cc0
        var n = 0
        while (n < depth && !c.isInstanceOf[GameOver]) {
            n += 1
            val a = Host.askFaction(game, c)
            val (_, cc2) = game.perform(a.unwrap)
            c = cc2
        }
        (game, c)
    }

    def stepForward(game : Game, from : Continue, steps : Int) : Int = {
        var c = from; var i = 0
        while (i < steps && !c.isInstanceOf[GameOver]) {
            val a = Host.askFaction(game, c)
            val (_, cc) = game.perform(a.unwrap)
            c = cc; i += 1
        }
        i
    }

    // A cheap, human-readable state fingerprint (avoids needing Serializable).
    // Captures the fields most likely to change: per-faction power/doom/gates/
    // spellbooks/units, plus global turn/round/ritualMarker/gates.
    def fingerprint(g : Game) : String = {
        val sb = new StringBuilder
        sb.append("t=").append(g.turn).append(",r=").append(g.round)
          .append(",rm=").append(g.ritualMarker)
          .append(",gates=").append(g.gates.num)
          .append(",dp=").append(g.doomPhase)
        g.setup.foreach { f =>
            val p = g.players(f)
            sb.append("|").append(f.short)
              .append(":pw=").append(p.power)
              .append(",dm=").append(p.doom)
              .append(",g=").append(p.gates.num)
              .append(",sb=").append(p.spellbooks.num)
              .append(",u=").append(p.units.num)
              .append(",es=").append(p.es.num)
              .append(",act=").append(p.acted)
        }
        // DS state — now stored ON the Game (was the singleton landmine, refactored
        // to game.chaosGateRegions/azathothTrack alongside deathsHead/fbCraters).
        // Because it lives on Game, the reflection clone copies it automatically.
        sb.append("||DS.chaosGates=").append(g.chaosGateRegions.num)
          .append(",DS.azaTrack=").append(g.azathothTrack)
        sb.toString
    }

    def main(args : Array[String]) : Unit = {
        println("=== CW cheap-clone spike v2 (reflection deep-clone) ===")

        val (probe, _) = playTo(150)

        // CRITICAL PRE-CHECK: fingerprint the original BEFORE any cloning.
        // Scala immutable Lists have a mutable internal 'next' pointer; a naive
        // reflection cloner can CORRUPT the shared original by touching it.
        print("Fingerprint ORIGINAL before any clone ... ")
        val fpBeforeClone =
            try { val s = fingerprint(probe); println("OK"); s }
            catch { case e : Throwable => println("CRASH: " + e); return }

        print("Deep-clone the original via reflection ... ")
        val cloned =
            try { val c = deepCopy(probe); println("returned"); c }
            catch { case e : Throwable =>
                println("FAILED: " + e.getClass.getName + ": " + Option(e.getMessage).getOrElse(""))
                e.getStackTrace.take(10).foreach(s => println("    " + s)); return
            }

        // Did cloning CORRUPT the original? (the real hazard)
        print("Fingerprint ORIGINAL again AFTER cloning ... ")
        val fpAfterClone =
            try { val s = fingerprint(probe); println("OK"); s }
            catch { case e : Throwable =>
                println("CRASH — reflection clone CORRUPTED the original's immutable lists.")
                println("  " + e)
                println("  FIX: register Scala immutable collections as immutable in the Cloner.")
                return
            }
        println(f"Original intact through clone (uncorrupted): ${fpBeforeClone == fpAfterClone}")

        // Faithfulness A: fingerprints match right after clone
        val fpOrig = fpAfterClone
        val fpClone =
            try fingerprint(cloned)
            catch { case e : Throwable => println("Clone fingerprint CRASH: " + e); return }
        println(f"Clone fingerprint matches original: ${fpOrig == fpClone}")
        if (fpOrig != fpClone) { println("  orig : " + fpOrig); println("  clone: " + fpClone) }

        // Faithfulness B: isolation. Clone the (game, continue) PAIR together so
        // the cloned Continue references the CLONED game. Step CLONE 30 decisions,
        // original must be unchanged.
        val (orig, origC) = playTo(150)
        val (clone2, clone2C) = deepCopyPair(orig, origC)

        // --- REFERENCE DIAGNOSTIC: find any clone->original leak ---
        println("\n--- reference-identity diagnostic ---")
        println(f"clone Game  !eq orig Game:                 ${!(clone2 eq orig)}")
        val fTS = TS.asInstanceOf[Faction]
        println(f"clone Player(TS) !eq orig Player(TS):       ${!(clone2.players(fTS) eq orig.players(fTS))}")
        // Does the cloned Player's captured implicit-game point at the CLONE?
        val pGameField = classOf[Player].getDeclaredFields.find(_.getName.contains("game"))
        pGameField.foreach { fld =>
            fld.setAccessible(true)
            val clonePlayerGame = fld.get(clone2.players(fTS))
            println(f"clone Player(TS).game eq CLONE Game:        ${clonePlayerGame eq clone2}  (should be true)")
            println(f"clone Player(TS).game eq ORIGINAL Game:     ${clonePlayerGame eq orig}   (LEAK if true)")
        }
        // Direct mutation test: bump clone.round, is orig.round affected?
        val origRoundBefore = orig.round
        clone2.round += 100
        println(f"After clone.round+=100: orig.round moved:   ${orig.round != origRoundBefore}  (LEAK if true)")
        clone2.round -= 100

        // --- CONTROL: does a NON-cloned game crash when stepped the same way? ---
        // If the control crashes too, the crash is a latent engine/bot issue,
        // NOT caused by cloning.
        print("\nCONTROL: step a fresh (non-cloned) game 30 decisions ... ")
        val (control, controlC) = playTo(150)
        val controlOk =
            try { stepForward(control, controlC, 30); true }
            catch { case e : Throwable =>
                println("CRASHED too: " + e.getClass.getSimpleName + " @ " + e.getStackTrace.head)
                false
            }
        if (controlOk) println("OK (no crash)")
        println(f"=> If control is OK but clone crashes, the crash is CLONE-INDUCED.")
        println(f"=> If control also crashes, it is a pre-existing engine/bot bug.\n")

        // --- SAME-POSITION CONTROL: step the ORIGINAL from the IDENTICAL position. ---
        // This is the only valid control: same game, same pending Continue. Because the
        // clone is isolated (proven above), stepping orig cannot affect clone2. If BOTH
        // crash at the same spot, the crash is a pre-existing engine bug at this position,
        // NOT clone-induced. We deep-copy orig first so THIS stepping doesn't disturb the
        // isolation fingerprint check below.
        val (origCtl, origCtlC) = deepCopyPair(orig, origC)
        print("SAME-POSITION CONTROL: step the ORIGINAL's own position 30 decisions ... ")
        val origCtlOk =
            try { stepForward(origCtl, origCtlC, 30); println("OK (no crash)"); true }
            catch { case e : Throwable =>
                println("CRASHED: " + e.getClass.getSimpleName)
                println("  full stack (top 12):")
                e.getStackTrace.take(12).foreach(s => println("    " + s))
                false
            }

        val origPre = fingerprint(orig)
        val stepped =
            try stepForward(clone2, clone2C, 30)
            catch { case e : Throwable =>
                println("CLONE stepping crashed: " + e.getClass.getSimpleName + " @ " + e.getStackTrace.head)
                e.getStackTrace.take(12).foreach(s => println("    " + s))
                -1
            }
        println(f"\n=> same-position ORIGINAL ${if (origCtlOk) "SURVIVED" else "ALSO CRASHED"}; " +
                f"if both crash at the same spot it is a pre-existing engine bug, not clone-induced.")
        val origPost = fingerprint(orig)
        val clonePost = fingerprint(clone2)
        println(f"\nStepped clone $stepped decisions.")
        println(f"Original UNCHANGED after clone stepped (isolation): ${origPre == origPost}")
        if (origPre != origPost) { println("  pre : " + origPre); println("  post: " + origPost) }
        println(f"Clone DIVERGED from original after stepping:        ${clonePost != origPre}")

        // Faithfulness C (the landmine): does DS singleton leak across clones?
        // Step clone; if the ORIGINAL's DS-derived fingerprint moved, the global
        // singleton is shared and the clone is NOT truly isolated.
        val dsLeak = origPre.contains("DS.") && origPost.contains("DS.") && {
            val a = origPre.substring(origPre.indexOf("||DS"))
            val b = origPost.substring(origPost.indexOf("||DS"))
            a != b
        }
        println(f"DS singleton leaked across clone (BAD if true):     $dsLeak")

        // --- DEEP ROLLOUT: clone a mid-game position and play it to GAME OVER. ---
        // The real MCTS use: fork a live position, roll out to the end, original intact.
        // Repeated at several fork depths so a single lucky seed can't hide a crash.
        println("\n--- deep rollouts: clone at various depths, play each CLONE to game over ---")
        var allRolloutsOk = true
        List(50, 100, 150, 200, 300).foreach { forkDepth =>
            val (deepOrig, deepOrigC) = playTo(forkDepth)
            val deepOrigFp = fingerprint(deepOrig)
            val (rollClone, rollCloneC) = deepCopyPair(deepOrig, deepOrigC)
            var rc = rollCloneC
            var rolled = 0
            val rollOk =
                try {
                    while (!rc.isInstanceOf[GameOver] && rolled < 7000) {
                        val a = Host.askFaction(rollClone, rc)
                        val (_, cc) = rollClone.perform(a.unwrap)
                        rc = cc; rolled += 1
                    }
                    true
                } catch { case e : Throwable =>
                    println(f"  fork@$forkDepth%-3d ROLLOUT CRASHED after $rolled%d steps: ${e.getClass.getSimpleName}%s @ ${e.getStackTrace.head}%s")
                    false
                }
            val deepOrigFpAfter = fingerprint(deepOrig)
            val isolated = deepOrigFp == deepOrigFpAfter
            allRolloutsOk &&= rollOk && isolated
            println(f"  fork@$forkDepth%-3d -> rolled $rolled%4d steps to end=${rc.isInstanceOf[GameOver]}%-5s  origUntouched=$isolated%s")
            if (!isolated) { println("     pre : " + deepOrigFp); println("     post: " + deepOrigFpAfter) }
        }
        println(f"  ALL deep rollouts faithful + isolated: $allRolloutsOk%s")

        // Timing vs depth
        println(f"\n--- clone time vs depth (want ~constant, faster than replay) ---")
        println("%-8s %-12s %-14s".format("depth", "clone(ms)", "clones/sec"))
        List(20, 50, 100, 200, 400, 800).foreach { d =>
            val (g, gc) = playTo(d)
            (1 to 3).foreach(_ => deepCopyPair(g, gc))
            val reps = 50
            val t0 = System.nanoTime()
            var i = 0
            while (i < reps) { deepCopyPair(g, gc); i += 1 }
            val ms = (System.nanoTime() - t0) / 1e6 / reps
            println(f"$d%-8d ${ms}%-14.3f ${1000.0/ms}%-14.0f")
        }

        println("\n=== interpretation ===")
        println("Replay-from-scratch (prior spike): depth-200 = 131 forks/sec, collapsing with depth.")
        println("Reflection clone should be ~constant. Compare clones/sec here to that 131.")
        println("If faithful + isolated + fast, this is a usable clone TODAY (zero engine changes).")
        println("If DS leaks, add a DS-singleton snapshot/restore around each rollout.")
    }
}
