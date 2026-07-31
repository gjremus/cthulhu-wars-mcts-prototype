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

    def savePolicy(m : PolicyModel) : Unit = {
        new File(Dir).mkdirs()
        val pw = new PrintWriter(policyFile)
        try {
            pw.println("POLICY 1")
            pw.println(s"${m.dinS} ${m.dinA} ${m.hidden}")
            pw.println(m.b2.toString)
            writeDoubles(pw, m.w1s); writeDoubles(pw, m.w1a); writeDoubles(pw, m.b1); writeDoubles(pw, m.w2)
        } finally pw.close()
    }

    def saveValue(m : MLPModel) : Unit = {
        new File(Dir).mkdirs()
        val pw = new PrintWriter(valueFile)
        try {
            pw.println("VALUE 1")
            pw.println(s"${m.din} ${m.hidden}")
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

    /** Load the policy net, or None if absent/incompatible with (dinS,dinA,hidden). */
    def loadPolicy(dinS : Int, dinA : Int, hidden : Int) : Option[PolicyModel] = {
        if (!policyFile.exists) return None
        val lines = Source.fromFile(policyFile).getLines().toArray
        if (lines.length < 3 || !lines(0).startsWith("POLICY")) return None
        val dims = lines(1).trim.split("\\s+").map(_.toInt)
        if (dims.length != 3 || dims(0) != dinS || dims(1) != dinA || dims(2) != hidden) return None
        val b2 = lines(2).trim.toDouble
        val ws = bodyDoubles(policyFile, 3)
        val nW1s = hidden * dinS; val nW1a = hidden * dinA
        if (ws.length != nW1s + nW1a + hidden + hidden) return None
        var o = 0
        val w1s = ws.slice(o, o + nW1s); o += nW1s
        val w1a = ws.slice(o, o + nW1a); o += nW1a
        val b1  = ws.slice(o, o + hidden); o += hidden
        val w2  = ws.slice(o, o + hidden)
        Some(new PolicyModel(dinS, dinA, hidden, w1s, w1a, b1, w2, b2))
    }

    /** Load the value net, or None if absent/incompatible with (din,hidden). */
    def loadValue(din : Int, hidden : Int) : Option[MLPModel] = {
        if (!valueFile.exists) return None
        val lines = Source.fromFile(valueFile).getLines().toArray
        if (lines.length < 3 || !lines(0).startsWith("VALUE")) return None
        val dims = lines(1).trim.split("\\s+").map(_.toInt)
        if (dims.length != 2 || dims(0) != din || dims(1) != hidden) return None
        val b2 = lines(2).trim.toDouble
        val ws = bodyDoubles(valueFile, 3)
        val nW1 = hidden * din
        if (ws.length != nW1 + hidden + hidden) return None
        var o = 0
        val w1 = ws.slice(o, o + nW1); o += nW1
        val b1 = ws.slice(o, o + hidden); o += hidden
        val w2 = ws.slice(o, o + hidden)
        Some(new MLPModel(din, hidden, w1, b1, w2, b2))
    }
}
