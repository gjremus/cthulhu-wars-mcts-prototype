package cws

// Phase 1 extensibility (2026-08-01) — WARM-START MIGRATION support.
//
// Features.of / ActionFeatures.of build their vectors as a sequence of BLOCKS whose
// widths depend on three capacities: R = reserved on-map regions, F = reserved faction
// seats, U = per-faction unit slots. When any capacity GROWS (a bigger map, a 5th seat,
// a faction with more unit classes), every block AFTER the one that grew shifts to a new
// offset, and `dim` itself changes. A trained net's INPUT-layer weights are indexed by
// feature position, so they can only be reused if each old feature is copied to its NEW
// position — a raw array copy would silently misalign every block past the first change.
//
// This object is the single source of truth for those block offsets, expressed as PURE
// functions of (R, F, U) so the SAME math describes the old (saved) layout and the new
// (current) layout. `remapState` / `remapAction` build an index map old→new; Checkpoint
// uses it to transfer champion base-4 weights into any larger environment's layout. The
// fixed-width blocks (object layers, per-faction scalar block, ritual/turn/phase/bias,
// action KIND/utype/spellbook/ints/flags) are constants shared with the two encoders — if
// either encoder's layout changes, these MUST change in lock-step (asserted by the dim
// round-trip check in CheckpointTest).
object EncodingLayout {

    // Fixed (cap-independent) block widths — mirror the two encoders exactly.
    val ObjectLayers = 4     // Features: desecrated/cathedral/chaosGate/crater
    val PerFaction   = 38    // Features: per-faction scalar block
    val RitualW      = 3
    val TurnCtxW     = 4
    val PhaseW       = 5
    val BiasW        = 1
    // Action-encoder fixed blocks
    val KindW        = 64
    val UTypeW       = 8
    val SBW          = 6
    val IntsW        = 3
    val FlagsW       = 2

    /** State-vector length for the given capacities (matches Features.dim). */
    def stateDim(R : Int, F : Int, U : Int) : Int =
        R*F*U + R*F + R*F + R*ObjectLayers + F*PerFaction + RitualW + TurnCtxW + PhaseW + BiasW

    /** Action-vector length for the given capacities (matches ActionFeatures.dim). */
    def actionDim(R : Int, F : Int, U : Int) : Int =
        KindW + R + R + U + UTypeW + F + SBW + IntsW + FlagsW + BiasW

    /** Build a map newIndex -> oldIndex for the STATE encoder, transferring every feature
     *  that exists in BOTH layouts (r<min(R), f<min(F), s<min(U)); features that only
     *  exist in the new (larger) layout have no source and are left to their init value.
     *  Returns an Array of length stateDim(newR,newF,newU); entry = old flat index or -1. */
    def stateMap(oldR : Int, oldF : Int, oldU : Int, newR : Int, newF : Int, newU : Int) : Array[Int] = {
        val out = Array.fill(stateDim(newR, newF, newU))(-1)
        val R = math.min(oldR, newR); val F = math.min(oldF, newF); val U = math.min(oldU, newU)

        // running cursors for the OLD and NEW layouts, block by block
        var oc = 0; var nc = 0
        def block(oldW : Int, newW : Int)(map : (Int, Int) => Unit) : Unit = { map(oc, nc); oc += oldW; nc += newW }

        // 1. board tensor R×F×U  (idx = ((r*F)+f)*U + s)
        block(oldR*oldF*oldU, newR*newF*newU) { (ob, nb) =>
            var r = 0; while (r < R) { var f = 0; while (f < F) { var s = 0; while (s < U) {
                out(nb + ((r*newF)+f)*newU + s) = ob + ((r*oldF)+f)*oldU + s; s += 1 }; f += 1 }; r += 1 }
        }
        // 2. combat R×F   3. gate R×F  (idx = r*F + f)
        for (_blk <- 0 until 2) block(oldR*oldF, newR*newF) { (ob, nb) =>
            var r = 0; while (r < R) { var f = 0; while (f < F) { out(nb + r*newF + f) = ob + r*oldF + f; f += 1 }; r += 1 }
        }
        // 4. objects R×OL (idx = r*OL + layer)
        block(oldR*ObjectLayers, newR*ObjectLayers) { (ob, nb) =>
            var r = 0; while (r < R) { var l = 0; while (l < ObjectLayers) { out(nb + r*ObjectLayers + l) = ob + r*ObjectLayers + l; l += 1 }; r += 1 }
        }
        // 5. per-faction F×PF (idx = f*PF + k)
        block(oldF*PerFaction, newF*PerFaction) { (ob, nb) =>
            var f = 0; while (f < F) { var k = 0; while (k < PerFaction) { out(nb + f*PerFaction + k) = ob + f*PerFaction + k; k += 1 }; f += 1 }
        }
        // 6-8. ritual / turn / phase / bias — fixed widths, copy straight across
        for (w <- Seq(RitualW, TurnCtxW, PhaseW, BiasW)) block(w, w) { (ob, nb) =>
            var i = 0; while (i < w) { out(nb + i) = ob + i; i += 1 } }
        out
    }

    /** Build a map newIndex -> oldIndex for the ACTION encoder. Same contract as stateMap. */
    def actionMap(oldR : Int, oldF : Int, oldU : Int, newR : Int, newF : Int, newU : Int) : Array[Int] = {
        val out = Array.fill(actionDim(newR, newF, newU))(-1)
        val R = math.min(oldR, newR); val F = math.min(oldF, newF); val U = math.min(oldU, newU)
        var oc = 0; var nc = 0
        def block(oldW : Int, newW : Int)(map : (Int, Int) => Unit) : Unit = { map(oc, nc); oc += oldW; nc += newW }
        def straight(w : Int) : Unit = block(w, w) { (ob, nb) => var i = 0; while (i < w) { out(nb + i) = ob + i; i += 1 } }
        def regionBlock(oldW : Int, newW : Int) : Unit = block(oldW, newW) { (ob, nb) => var r = 0; while (r < R) { out(nb + r) = ob + r; r += 1 } }
        straight(KindW)                      // 1. kind (fixed 64)
        regionBlock(oldR, newR)              // 2. primary region
        regionBlock(oldR, newR)              // 3. secondary region
        block(oldU, newU) { (ob, nb) => var s = 0; while (s < U) { out(nb + s) = ob + s; s += 1 } }  // 4. unit slot
        straight(UTypeW)                     // 5. unit type (fixed 8)
        block(oldF, newF) { (ob, nb) => var f = 0; while (f < F) { out(nb + f) = ob + f; f += 1 } }   // 6. faction (relative seat)
        straight(SBW)                        // 7. spellbook (fixed 6)
        straight(IntsW)                      // 8. ints (fixed 3)
        straight(FlagsW)                     // 9. flags (fixed 2)
        straight(BiasW)                      // 10. bias
        out
    }

    /** Remap a hidden×oldDin input-weight matrix (row-major, one row per hidden unit) into
     *  a hidden×newDin matrix via `idxMap` (newIndex -> oldIndex, -1 = no source -> `fill`).
     *  Hidden count and output weights are cap-independent and are NOT touched here. */
    def remapInputMatrix(w : Array[Double], hidden : Int, oldDin : Int, idxMap : Array[Int], fill : Double = 0.0) : Array[Double] = {
        val newDin = idxMap.length
        val out = new Array[Double](hidden * newDin)
        var j = 0
        while (j < hidden) {
            val ob = j * oldDin; val nb = j * newDin
            var i = 0
            while (i < newDin) { val src = idxMap(i); out(nb + i) = if (src >= 0) w(ob + src) else fill; i += 1 }
            j += 1
        }
        out
    }
}
