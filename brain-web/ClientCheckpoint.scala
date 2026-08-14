package cws

// ─────────────────────────────────────────────────────────────────────────────
// BROWSER-SAFE WEIGHT LOADER for the client-side (Scala.js) brain port — the last
// piece of blocker #3 (Scala.js portability).
//
// The JVM Checkpoint.loadPolicy / loadValue read the trained weights with
// `Source.fromFile(...)` (java.io) — file IO does not exist in a browser. But the
// PARSING is pure: header parse → tokenize doubles → construct PolicyModel/MLPModel.
// This shim reuses that exact parse logic against weights handed in AS STRINGS
// (the two checkpoint files bundled as a static asset / fetched over HTTP and read
// as text), so it produces byte-identical models to the JVM loader.
//
// SIMPLIFICATION valid ONLY for the base-4 /brain/ build: on disk the champion's
// caps ARE the base-4 constants (NumFactions=4, UnitSlots=6, AbilitySlots=4,
// MaxRegions=17 → dim 777/128, hidden 64), and the browser build's caps are those
// SAME constants (ClientCaps). So savedCaps always == curCaps and the loader only
// ever hits the EXACT raw-load branch — no EncodingLayout migration, no getenv.
// (If a future build ever expands caps in-browser, port Checkpoint.isSuperset +
// EncodingLayout.remapInputMatrix too; base-4 never needs them.)
//
// Staged in isolation (brain-clientport/) so the running champion and live JVM
// builds stay byte-identical. The porter uses these two functions in place of
// Checkpoint.loadPolicy/loadValue when assembling the Scala.js /brain/ build,
// feeding them the bundled checkpoint text.
// ─────────────────────────────────────────────────────────────────────────────

object ClientCheckpoint {

    /** Tokenize everything after the first `skipLines` lines into doubles — the
     *  string-fed twin of Checkpoint.bodyDoubles (which reads from a File). */
    private def bodyDoubles(lines : Array[String], skipLines : Int) : Array[Double] =
        lines.drop(skipLines).mkString(" ").split("\\s+").filter(_.nonEmpty).map(_.toDouble)

    /** Parse a PolicyModel from the raw text of the policy checkpoint file.
     *  `text` = the whole `policy.txt` contents (bundled asset). Mirrors
     *  Checkpoint.loadPolicy's EXACT raw-load branch. Returns None on any shape
     *  mismatch (same guards as the JVM loader), so a corrupt/wrong asset fails
     *  cleanly instead of loading garbage. */
    def loadPolicy(text : String, dinS : Int, dinA : Int, hidden : Int) : Option[PolicyModel] = {
        val lines = text.split("\n")
        if (lines.length < 3 || !lines(0).startsWith("POLICY")) return None
        val hdr = lines(1).trim.split("\\s+").map(_.toInt)
        val (oldDinS, oldDinA, oldHidden) = (hdr(0), hdr(1), hdr(2))
        if (oldHidden != hidden) return None
        // base-4 browser build: caps match, so require exact dims (no migration).
        if (oldDinS != dinS || oldDinA != dinA) return None
        val b2 = lines(2).trim.toDouble
        val ws = bodyDoubles(lines, 3)
        if (ws.length != hidden*dinS + hidden*dinA + hidden + hidden) return None
        var o = 0
        val w1s = ws.slice(o, o + hidden*dinS); o += hidden*dinS
        val w1a = ws.slice(o, o + hidden*dinA); o += hidden*dinA
        val b1  = ws.slice(o, o + hidden); o += hidden
        val w2  = ws.slice(o, o + hidden)
        Some(new PolicyModel(dinS, dinA, hidden, w1s, w1a, b1, w2, b2))
    }

    /** Parse an MLPModel (value net) from the raw text of the value checkpoint file.
     *  Mirrors Checkpoint.loadValue's EXACT raw-load branch. */
    def loadValue(text : String, din : Int, hidden : Int) : Option[MLPModel] = {
        val lines = text.split("\n")
        if (lines.length < 3 || !lines(0).startsWith("VALUE")) return None
        val hdr = lines(1).trim.split("\\s+").map(_.toInt)
        val (oldDin, oldHidden) = (hdr(0), hdr(1))
        if (oldHidden != hidden) return None
        if (oldDin != din) return None
        val b2 = lines(2).trim.toDouble
        val ws = bodyDoubles(lines, 3)
        if (ws.length != hidden*din + hidden + hidden) return None
        var o = 0
        val w1 = ws.slice(o, o + hidden*din); o += hidden*din
        val b1 = ws.slice(o, o + hidden); o += hidden
        val w2 = ws.slice(o, o + hidden)
        Some(new MLPModel(din, hidden, w1, b1, w2, b2))
    }
}
