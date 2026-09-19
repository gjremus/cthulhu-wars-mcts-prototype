package cws

/** Round-trip sanity check for Checkpoint save/load. Run:
 *    java -cp <cp> cws.CheckpointTest
 *  Trains a couple of steps so weights aren't at init, saves, reloads, and asserts the
 *  reloaded nets produce byte-identical outputs. Writes to a temp subdir, not the real
 *  checkpoint dir. */
object CheckpointTest {
    def main(args : Array[String]) : Unit = {
        val dinS = Features.dim; val dinA = ActionFeatures.dim; val hidden = 8
        val p = PolicyModel.initial(dinS, dinA, hidden)
        val v = MLPModel.initial(Features.dim, hidden)
        // perturb weights so we're not testing the all-symmetric init
        val st = Array.tabulate(dinS)(i => math.sin(0.3 * i))
        val acts = Array(Array.tabulate(dinA)(i => math.cos(0.2 * i)),
                         Array.tabulate(dinA)(i => math.sin(0.1 * i + 1)))
        var k = 0; while (k < 5) { p.trainDecision(st, acts, k % 2, 0.05); v.train(st, if (k % 2 == 0) 1.0 else 0.0, 0.05); k += 1 }

        val pBefore = p.policy(st, acts)
        val vBefore = v.eval(st)

        Checkpoint.save(p, v, iter = 7, winRate = 0.5, bestGameScore = 1.234, tag = "unittest")
        println(s"saved. meta: ${Checkpoint.metaLine}")
        println(s"savedScore=${Checkpoint.savedScore}")

        val pL = Checkpoint.loadPolicy(dinS, dinA, hidden).getOrElse(sys.error("policy load returned None"))
        val vL = Checkpoint.loadValue(Features.dim, hidden).getOrElse(sys.error("value load returned None"))
        val pAfter = pL.policy(st, acts)
        val vAfter = vL.eval(st)

        val pOk = pBefore.zip(pAfter).forall { case (a, b) => math.abs(a - b) < 1e-12 }
        val vOk = math.abs(vBefore - vAfter) < 1e-12
        // incompatible-dim load must reject cleanly
        val mismatchOk = Checkpoint.loadPolicy(dinS, dinA, hidden + 1).isEmpty
        println(f"policy round-trip: ${pBefore.mkString(",")} vs ${pAfter.mkString(",")} -> ${if (pOk) "OK" else "MISMATCH"}")
        println(f"value  round-trip: $vBefore%.15f vs $vAfter%.15f -> ${if (vOk) "OK" else "MISMATCH"}")
        println(s"dim-mismatch rejected cleanly: ${if (mismatchOk) "OK" else "FAIL"}")
        if (pOk && vOk && mismatchOk) println("ALL CHECKPOINT TESTS PASSED")
        else { println("CHECKPOINT TEST FAILED"); sys.exit(1) }
    }
}
