package cws

import java.io.{File, PrintWriter}
import scala.io.Source

/** Cross-run persistence for the trained nets.
 *
 *  WHY THIS EXISTS: before this, every self-play run bootstrapped FRESH from 60 bot
 *  games and kept its best net only IN MEMORY — the JVM exited and the learned brain
 *  was discarded. So each run relearned from zero and (because self-play games run in
 *  parallel across cores, giving nondeterministic FP-accumulation order) redrew a
 *  different trajectory each time. This module saves the best policy+value nets to
 *  disk at every new best-checkpoint, and lets the next run WARM-START from them
 *  instead of re-bootstrapping — so learning accumulates across runs.
 *
 *  Format: plain text, full-precision Double.toString (round-trip exact). First line
 *  is a magic/version tag; second line is the dims; the rest are the weights in a fixed
 *  order. Text (not Java serialization) so a dim mismatch after a config change is a
 *  clean parse-time reject, never a silently-wrong load.
 */
object Checkpoint {

    // Default checkpoint dir; override with CW_CKPT_DIR so a challenger run (e.g. a
    // wider-hidden brain, whose dims are INCOMPATIBLE with the saved champion) can
    // persist to an isolated dir WITHOUT clobbering the current best on disk.
    val Dir = {
        val e = System.getenv("CW_CKPT_DIR")
        if (e != null && e.trim.nonEmpty) e.trim
        else "/Users/gremus/cthulhu-wars-mcts-prototype/checkpoints"
    }

    private def policyFile = new File(Dir, "best.policy")
    private def valueFile  = new File(Dir, "best.value")
    private def metaFile   = new File(Dir, "best.meta")

    /** True if a full checkpoint (policy + value) is present on disk. */
    def exists : Boolean = policyFile.exists && valueFile.exists

    // ---- write ---------------------------------------------------------------

    private def writeDoubles(pw : PrintWriter, a : Array[Double]) : Unit = {
        var i = 0
        while (i < a.length) { pw.print(a(i).toString); pw.print(if ((i & 7) == 7) '\n' else ' '); i += 1 }
        pw.print('\n')
    }

    // The encoding CAPACITIES (max regions / factions / unit-slots) that produced this
    // checkpoint. Recorded so a later run under LARGER caps can migrate (warm-start) these
    // weights into its bigger layout instead of cold-starting (Phase 1 extensibility). A
    // legacy file (written before caps existed, 3-field dims line) is base-4 by definition,
    // so the reader defaults to (17,4,6) when the caps are absent.
    private def curCaps : (Int, Int, Int) = (Features.maxRegions, Features.maxFactions, Features.maxUnitSlots)
    private val LegacyCaps = (17, 4, 6)

    def savePolicy(m : PolicyModel) : Unit = {
        new File(Dir).mkdirs()
        val pw = new PrintWriter(policyFile)
        val (r, f, u) = curCaps
        try {
            pw.println("POLICY 2")
            pw.println(s"${m.dinS} ${m.dinA} ${m.hidden} $r $f $u")   // dims + caps (v2)
            pw.println(m.b2.toString)
            writeDoubles(pw, m.w1s); writeDoubles(pw, m.w1a); writeDoubles(pw, m.b1); writeDoubles(pw, m.w2)
        } finally pw.close()
    }

    def saveValue(m : MLPModel) : Unit = {
        new File(Dir).mkdirs()
        val pw = new PrintWriter(valueFile)
        val (r, f, u) = curCaps
        try {
            pw.println("VALUE 2")
            pw.println(s"${m.din} ${m.hidden} $r $f $u")              // dims + caps (v2)
            pw.println(m.b2.toString)
            writeDoubles(pw, m.w1); writeDoubles(pw, m.b1); writeDoubles(pw, m.w2)
        } finally pw.close()
    }

    /** Save both nets + a human-readable meta line (which run/iter/score produced them). */
    def save(policy : PolicyModel, value : MLPModel, iter : Int, score : Double, tag : String) : Unit = {
        savePolicy(policy); saveValue(value)
        val pw = new PrintWriter(metaFile)
        try pw.println(s"tag=$tag iter=$iter score=$score dinS=${policy.dinS} dinA=${policy.dinA} hidden=${policy.hidden}")
        finally pw.close()
    }

    // ---- SAVE EVERY ITERATION (2026-08-08) ------------------------------------
    // Save current weights to `current.*` files every iteration so learning is never
    // lost if the process dies. These are separate from `best.*` (which only updates
    // when score improves). On warm-start, we load from `best.*` as before.

    private def currentPolicyFile = new File(Dir, "current.policy")
    private def currentValueFile  = new File(Dir, "current.value")
    private def currentMetaFile   = new File(Dir, "current.meta")

    /** Save current (possibly not best) weights. Called every iteration. */
    def saveCurrent(policy : PolicyModel, value : MLPModel, iter : Int, score : Double, tag : String) : Unit = {
        new File(Dir).mkdirs()
        // Save policy
        val pwP = new PrintWriter(currentPolicyFile)
        val (r, f, u) = curCaps
        try {
            pwP.println("POLICY 2")
            pwP.println(s"${policy.dinS} ${policy.dinA} ${policy.hidden} $r $f $u")
            pwP.println(policy.b2.toString)
            writeDoubles(pwP, policy.w1s); writeDoubles(pwP, policy.w1a); writeDoubles(pwP, policy.b1); writeDoubles(pwP, policy.w2)
        } finally pwP.close()
        // Save value
        val pwV = new PrintWriter(currentValueFile)
        try {
            pwV.println("VALUE 2")
            pwV.println(s"${value.din} ${value.hidden} $r $f $u")
            pwV.println(value.b2.toString)
            writeDoubles(pwV, value.w1); writeDoubles(pwV, value.b1); writeDoubles(pwV, value.w2)
        } finally pwV.close()
        // Save meta
        val pwM = new PrintWriter(currentMetaFile)
        try pwM.println(s"tag=$tag iter=$iter score=$score dinS=${policy.dinS} dinA=${policy.dinA} hidden=${policy.hidden} time=${System.currentTimeMillis()}")
        finally pwM.close()
    }

    // ---- CLEANUP OLD CHECKPOINTS (2026-08-08) ---------------------------------
    // Move checkpoint files older than N days to Trash (never rm).

    def cleanupOldCheckpoints(daysOld : Int) : Unit = {
        val cutoffMs = System.currentTimeMillis() - daysOld * 24L * 60 * 60 * 1000
        val trashDir = new File(System.getProperty("user.home"), ".Trash")
        val dir = new File(Dir)
        if (!dir.exists) return
        dir.listFiles().filter(f => f.getName.endsWith(".bak") || f.getName.startsWith("checkpoint_")).foreach { f =>
            if (f.lastModified() < cutoffMs) {
                val dest = new File(trashDir, f.getName + "_" + System.currentTimeMillis())
                if (f.renameTo(dest)) println(s"   (trashed old checkpoint: ${f.getName})")
            }
        }
    }

    def metaLine : String = if (metaFile.exists) Source.fromFile(metaFile).getLines().mkString(" ").trim else "(no meta)"

    /** The score recorded in the on-disk checkpoint's meta, or None. Used to seed the
     *  in-memory best-bar on warm-start so a WEAKER run cannot overwrite a stronger
     *  saved net (which would re-introduce the "throw away the good brain" bug). */
    def savedScore : Option[Double] =
        if (!metaFile.exists) None
        else metaLine.split("\\s+").find(_.startsWith("score=")).map(_.stripPrefix("score=").toDouble)

    // ---- read ----------------------------------------------------------------

    /** Tokenize everything AFTER the first `skipLines` lines into doubles. */
    private def bodyDoubles(f : File, skipLines : Int) : Array[Double] =
        Source.fromFile(f).getLines().drop(skipLines).mkString(" ").split("\\s+").filter(_.nonEmpty).map(_.toDouble)

    // A capacity change is a legal WARM-START only when the current caps are a SUPERSET of
    // the saved ones (every reserved slot the saved net knew still exists, at the same
    // relative position, plus new all-zero room). Shrinking a cap would drop learned slots,
    // so that is rejected (cold start) — you never quietly lose brain the champion earned.
    private def isSuperset(oldC : (Int,Int,Int), newC : (Int,Int,Int)) : Boolean =
        newC._1 >= oldC._1 && newC._2 >= oldC._2 && newC._3 >= oldC._3

    /** Load the policy net for the CURRENT (dinS,dinA,hidden), migrating base-4 weights into
     *  a larger layout when the on-disk caps are a subset of the current caps (warm-start
     *  across an environment expansion). Returns None if absent, wrong hidden size, or the
     *  saved caps are not a subset of the current ones. */
    def loadPolicy(dinS : Int, dinA : Int, hidden : Int) : Option[PolicyModel] = {
        if (!policyFile.exists) return None
        val lines = Source.fromFile(policyFile).getLines().toArray
        if (lines.length < 3 || !lines(0).startsWith("POLICY")) return None
        val hdr = lines(1).trim.split("\\s+").map(_.toInt)
        // v1: "dinS dinA hidden"  |  v2: "dinS dinA hidden R F U"
        val (oldDinS, oldDinA, oldHidden) = (hdr(0), hdr(1), hdr(2))
        val savedCaps = if (hdr.length >= 6) (hdr(3), hdr(4), hdr(5)) else LegacyCaps
        if (oldHidden != hidden) return None                 // hidden width is not migratable here
        val b2 = lines(2).trim.toDouble
        val ws = bodyDoubles(policyFile, 3)
        if (ws.length != hidden*oldDinS + hidden*oldDinA + hidden + hidden) return None
        var o = 0
        val w1s = ws.slice(o, o + hidden*oldDinS); o += hidden*oldDinS
        val w1a = ws.slice(o, o + hidden*oldDinA); o += hidden*oldDinA
        val b1  = ws.slice(o, o + hidden); o += hidden
        val w2  = ws.slice(o, o + hidden)
        val curC = curCaps
        if (savedCaps == curC && oldDinS == dinS && oldDinA == dinA)
            Some(new PolicyModel(dinS, dinA, hidden, w1s, w1a, b1, w2, b2))   // exact — raw load
        else if (isSuperset(savedCaps, curC)) {                              // migrate (warm-start)
            val sMap = EncodingLayout.stateMap(savedCaps._1, savedCaps._2, savedCaps._3, curC._1, curC._2, curC._3)
            val aMap = EncodingLayout.actionMap(savedCaps._1, savedCaps._2, savedCaps._3, curC._1, curC._2, curC._3)
            if (sMap.length != dinS || aMap.length != dinA) return None
            val nw1s = EncodingLayout.remapInputMatrix(w1s, hidden, oldDinS, sMap)
            val nw1a = EncodingLayout.remapInputMatrix(w1a, hidden, oldDinA, aMap)
            println(s"warm-start MIGRATION: policy caps $savedCaps -> $curC (dinS $oldDinS->$dinS, dinA $oldDinA->$dinA); transferred learned slots, new slots zero-init")
            Some(new PolicyModel(dinS, dinA, hidden, nw1s, nw1a, b1, w2, b2))
        } else None                                                          // shrink / mismatch -> cold
    }

    /** Load the value net for the CURRENT (din,hidden), migrating across a cap expansion the
     *  same way loadPolicy does. */
    def loadValue(din : Int, hidden : Int) : Option[MLPModel] = {
        if (!valueFile.exists) return None
        val lines = Source.fromFile(valueFile).getLines().toArray
        if (lines.length < 3 || !lines(0).startsWith("VALUE")) return None
        val hdr = lines(1).trim.split("\\s+").map(_.toInt)
        val (oldDin, oldHidden) = (hdr(0), hdr(1))
        val savedCaps = if (hdr.length >= 5) (hdr(2), hdr(3), hdr(4)) else LegacyCaps
        if (oldHidden != hidden) return None
        val b2 = lines(2).trim.toDouble
        val ws = bodyDoubles(valueFile, 3)
        if (ws.length != hidden*oldDin + hidden + hidden) return None
        var o = 0
        val w1 = ws.slice(o, o + hidden*oldDin); o += hidden*oldDin
        val b1 = ws.slice(o, o + hidden); o += hidden
        val w2 = ws.slice(o, o + hidden)
        val curC = curCaps
        if (savedCaps == curC && oldDin == din)
            Some(new MLPModel(din, hidden, w1, b1, w2, b2))                   // exact — raw load
        else if (isSuperset(savedCaps, curC)) {                              // migrate (warm-start)
            val sMap = EncodingLayout.stateMap(savedCaps._1, savedCaps._2, savedCaps._3, curC._1, curC._2, curC._3)
            if (sMap.length != din) return None
            val nw1 = EncodingLayout.remapInputMatrix(w1, hidden, oldDin, sMap)
            println(s"warm-start MIGRATION: value caps $savedCaps -> $curC (din $oldDin->$din); transferred learned slots, new slots zero-init")
            Some(new MLPModel(din, hidden, nw1, b1, w2, b2))
        } else None
    }
}
