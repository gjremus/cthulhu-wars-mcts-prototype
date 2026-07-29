package cws

import hrf.colmat._
import java.lang.reflect.{Field, Modifier}

// Lambda census — isolated prototype, touches no build.
//
// The reflection cloner dies on captured lambdas (GameImplicits$$Lambda) whose
// captured fields are `final` (JDK hidden classes). Before deciding HOW to handle
// them (share-by-reference vs snapshot-restore), we must know WHAT they capture:
//   - If every lambda captures ONLY immutable flyweights (Region/Faction/UnitClass/
//     Spellbook/Int/String/etc.), sharing the lambda by reference is SAFE and free.
//   - If ANY lambda captures a mutable Game/Player/Battle/UnitFigure, sharing it
//     would leak the clone back into the original — those must be re-bound.
//
// This walks the object graph reachable from (game, continue), finds every object
// whose class name contains "$$Lambda", and prints its captured field types with
// counts. Empirical answer, no guessing.

object LambdaCensus {

    def fixedSeating : $[Faction] = $(FB, GC, TS, DS)
    def newGame() : Game =
        new Game(EarthMap4v35, RitualTrack.for4, fixedSeating, false, $(UseGhast))

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

    // Types we consider SAFE to share by reference (immutable flyweights + primitives).
    def isSafeShared(o : Object) : Boolean = o match {
        case null                          => true
        case _ : Faction | _ : Region      => true
        case _ : Spellbook | _ : UnitClass => true
        case _ : GameOption | _ : Board    => true
        case _ : String | _ : java.lang.Integer | _ : java.lang.Boolean => true
        case _ : java.lang.Long | _ : java.lang.Double | _ : java.lang.Character => true
        case _                             => false
    }

    def typeName(o : Object) : String =
        if (o == null) "null" else o.getClass.getName

    def main(args : Array[String]) : Unit = {
        println("=== CW lambda census (what do captured closures hold?) ===")

        val depths = List(20, 50, 100, 150, 200, 300)
        depths.foreach { d =>
            val (game, c) = playTo(d)
            val visited = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Object, java.lang.Boolean]())
            val lambdaCaptureTypes = new java.util.TreeMap[String, Int]()
            var lambdaCount = 0
            var unsafeCaptures = 0
            val unsafeExamples = new java.util.TreeMap[String, String]()

            val stack = new java.util.ArrayDeque[Object]()
            stack.push(game); stack.push(c.asInstanceOf[Object])

            while (!stack.isEmpty) {
                val o = stack.pop()
                if (o != null && !visited.contains(o) && !isSafeShared(o)) {
                    visited.add(o)
                    val cls = o.getClass
                    val cn = cls.getName
                    // Don't descend into flyweights or java boxed; already filtered by isSafeShared.
                    if (cn.contains("$$Lambda")) {
                        lambdaCount += 1
                        // Report captured fields (arg$1, arg$2, ...)
                        cls.getDeclaredFields.foreach { f =>
                            try {
                                f.setAccessible(true)
                                val v = f.get(o)
                                val vt = typeName(v)
                                lambdaCaptureTypes.merge(vt, 1, (a, b) => a + b)
                                if (!isSafeShared(v)) {
                                    unsafeCaptures += 1
                                    unsafeExamples.putIfAbsent(vt, cn)
                                }
                            } catch { case _ : Throwable => }
                        }
                        // do NOT descend further into lambda internals
                    } else if (!cn.startsWith("java.") && !cn.startsWith("scala.runtime")) {
                        // Descend: array elements + all declared fields
                        if (cls.isArray) {
                            if (!cls.getComponentType.isPrimitive) {
                                val arr = o.asInstanceOf[Array[Object]]
                                var i = 0
                                while (i < arr.length) { if (arr(i) != null) stack.push(arr(i)); i += 1 }
                            }
                        } else {
                            var k : Class[_] = cls
                            while (k != null && k != classOf[Object]) {
                                k.getDeclaredFields.foreach { f =>
                                    if (!Modifier.isStatic(f.getModifiers) && !f.getType.isPrimitive) {
                                        try {
                                            f.setAccessible(true)
                                            val v = f.get(o)
                                            if (v != null) stack.push(v)
                                        } catch { case _ : Throwable => }
                                    }
                                }
                                k = k.getSuperclass
                            }
                        }
                    }
                }
            }

            println(f"\n--- depth $d%d : ${lambdaCount}%d lambdas reachable, ${unsafeCaptures}%d unsafe captures ---")
            val it = lambdaCaptureTypes.entrySet().iterator()
            while (it.hasNext) {
                val e = it.next()
                val safe = isSafeShared(sampleOf(e.getKey))
                val flag = if (unsafeExamples.containsKey(e.getKey)) "  <== MUTABLE (leak risk)" else ""
                println(f"    ${e.getValue}%5d x ${e.getKey}%s$flag")
            }
            if (unsafeCaptures == 0)
                println("    => ALL captures are immutable flyweights/primitives. Share-by-reference is SAFE.")
            else {
                println("    => SOME captures are mutable. Those lambdas must be re-bound, not shared:")
                val ue = unsafeExamples.entrySet().iterator()
                while (ue.hasNext) { val e = ue.next(); println(f"       ${e.getKey}%s  (e.g. in ${e.getValue}%s)") }
            }
        }
        println("\n=== interpretation ===")
        println("If every depth reports 0 unsafe captures, register a fast-cloner that SHARES")
        println("lambdas by reference (return the same instance) — correct AND free. If any are")
        println("mutable, those specific closures need the captured mutable re-bound to the clone.")
    }

    // crude: we can't reconstruct a value from a type string; just return null (treated safe)
    // — the unsafeExamples map already carries the authoritative mutable flag.
    def sampleOf(typeStr : String) : Object = null
}
