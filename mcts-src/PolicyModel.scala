package cws

// Option A, Phase 7 — the POLICY head (the move-aware brain the value-only net lacked).
//
// Architecture: an action-SCORING net. For a decision with K legal candidates, it maps
// each (state, candidate_action) pair to a scalar logit, then softmaxes across the K
// candidates. This is the standard way to handle a VARIABLE-SIZE, HETEROGENEOUS action
// set (Cthulhu Wars has no fixed action vocabulary to one-hot), and it keeps the head
// faction-agnostic: the same scorer ranks a Yellow-Sign Desecrate and a Great-Cthulhu
// Summon, because both arrive as (772-d state, action-feature) pairs in one space.
//
//   input  : state(dinS=Features.dim) ++ action(dinA=ActionFeatures.dim)
//   trunk  : hidden(H, tanh)     z1 = b1 + W1s·state + W1a·action
//   output : logit = b2 + w2·tanh(z1)     (one scalar per candidate)
//   policy : softmax over the K candidates' logits
//
// Trained by behavior cloning: cross-entropy between the softmax and the ONE-HOT of the
// action the bot actually chose. dLogit_k = softmax_k - 1{k == chosen}. This is the
// move-level signal — "which action did the competent player pick here" — that a value
// net trained on terminal outcomes never sees.
//
// EFFICIENCY: W1s·state is identical for every candidate in a decision, so it is
// computed ONCE per decision (sPart) and reused; only W1a·action varies per candidate.
// Gradients are ACCUMULATED across the K candidates and applied once, so the shared
// weights aren't mutated mid-decision (a correct full-decision SGD step).

final class PolicyModel(
    val dinS : Int,
    val dinA : Int,
    val hidden : Int,
    val w1s : Array[Double],   // hidden x dinS, row-major  (state -> hidden)
    val w1a : Array[Double],   // hidden x dinA, row-major  (action -> hidden)
    val b1  : Array[Double],   // hidden
    val w2  : Array[Double],   // hidden (hidden -> logit)
    var b2  : Double
) {
    require(w1s.length == hidden * dinS && w1a.length == hidden * dinA && b1.length == hidden && w2.length == hidden,
        "PolicyModel weight shapes inconsistent")

    /** W1s·state + b1 into `sPart` — the per-decision constant, computed once. */
    private def statePart(state : Array[Double]) : Array[Double] = {
        val sPart = new Array[Double](hidden)
        var j = 0
        while (j < hidden) {
            var s = b1(j); val base = j * dinS; var i = 0
            while (i < dinS) { s += w1s(base + i) * state(i); i += 1 }
            sPart(j) = s
            j += 1
        }
        sPart
    }

    // Forward for one candidate given the precomputed sPart. Returns (z1, a1, logit).
    private def forwardCand(sPart : Array[Double], action : Array[Double]) : (Array[Double], Array[Double], Double) = {
        val z1 = new Array[Double](hidden)
        val a1 = new Array[Double](hidden)
        var j = 0
        while (j < hidden) {
            var s = sPart(j); val base = j * dinA; var i = 0
            while (i < dinA) { s += w1a(base + i) * action(i); i += 1 }
            z1(j) = s; a1(j) = math.tanh(s)
            j += 1
        }
        var out = b2; j = 0
        while (j < hidden) { out += w2(j) * a1(j); j += 1 }
        (z1, a1, out)
    }

    /** Logits for all K candidates in a decision (one forward per candidate, shared sPart). */
    def logits(state : Array[Double], actions : Array[Array[Double]]) : Array[Double] = {
        val sPart = statePart(state)
        val out = new Array[Double](actions.length)
        var k = 0
        while (k < actions.length) { out(k) = forwardCand(sPart, actions(k))._3; k += 1 }
        out
    }

    /** Softmax over the candidate logits (numerically stable). */
    def policy(state : Array[Double], actions : Array[Array[Double]]) : Array[Double] =
        softmax(logits(state, actions))

    private def softmax(z : Array[Double]) : Array[Double] = {
        var mx = Double.NegativeInfinity; var i = 0
        while (i < z.length) { if (z(i) > mx) mx = z(i); i += 1 }
        val e = new Array[Double](z.length); var s = 0.0; i = 0
        while (i < z.length) { val v = math.exp(z(i) - mx); e(i) = v; s += v; i += 1 }
        i = 0; while (i < e.length) { e(i) /= s; i += 1 }
        e
    }

    /** Index of the highest-scoring candidate (the policy's top pick). */
    def argmax(state : Array[Double], actions : Array[Array[Double]]) : Int = {
        val z = logits(state, actions)
        var best = 0; var bv = Double.NegativeInfinity; var i = 0
        while (i < z.length) { if (z(i) > bv) { bv = z(i); best = i }; i += 1 }
        best
    }

    /**
     * One full-decision SGD step: cross-entropy of the softmax against the one-hot of
     * `chosen`. Gradients accumulate across all K candidates, then apply once. Returns
     * the per-decision cross-entropy loss BEFORE the update (for reporting).
     */
    def trainDecision(state : Array[Double], actions : Array[Array[Double]], chosen : Int, lr : Double) : Double = {
        val K = actions.length
        if (K <= 1) return 0.0                       // forced move: nothing to learn
        val sPart = statePart(state)

        // Forward all candidates, keep a1 for backprop.
        val z    = new Array[Double](K)
        val a1s  = new Array[Array[Double]](K)
        var k = 0
        while (k < K) { val (_, a1, lg) = forwardCand(sPart, actions(k)); a1s(k) = a1; z(k) = lg; k += 1 }
        val p = softmax(z)
        val loss = -math.log(math.max(1e-12, p(chosen)))

        // Gradient accumulators.
        val gW1s = new Array[Double](hidden * dinS)
        val gW1a = new Array[Double](hidden * dinA)
        val gB1  = new Array[Double](hidden)
        val gW2  = new Array[Double](hidden)
        var gB2  = 0.0

        k = 0
        while (k < K) {
            val dLogit = p(k) - (if (k == chosen) 1.0 else 0.0)   // softmax-CE gradient
            val a1 = a1s(k); val act = actions(k)
            var j = 0
            while (j < hidden) {
                gW2(j) += dLogit * a1(j)
                val dZ = dLogit * w2(j) * (1.0 - a1(j) * a1(j))    // through tanh
                gB1(j) += dZ
                val bs = j * dinS; var i = 0
                while (i < dinS) { gW1s(bs + i) += dZ * state(i); i += 1 }
                val ba = j * dinA; i = 0
                while (i < dinA) { gW1a(ba + i) += dZ * act(i); i += 1 }
                j += 1
            }
            gB2 += dLogit
            k += 1
        }

        // Apply once.
        var j = 0
        while (j < hidden) {
            w2(j) -= lr * gW2(j)
            b1(j) -= lr * gB1(j)
            val bs = j * dinS; var i = 0
            while (i < dinS) { w1s(bs + i) -= lr * gW1s(bs + i); i += 1 }
            val ba = j * dinA; i = 0
            while (i < dinA) { w1a(ba + i) -= lr * gW1a(ba + i); i += 1 }
            j += 1
        }
        b2 -= lr * gB2
        loss
    }

    /**
     * SELF-PLAY policy-improvement step. Same softmax cross-entropy as `trainDecision`,
     * but the target is a full DISTRIBUTION over the K candidates (AlphaZero's MCTS
     * visit-count target) instead of the one-hot of a single chosen move. The search,
     * by looking ahead, produces a BETTER move-distribution than the raw net's prior;
     * training the net toward that distribution is what lets it climb PAST the policy it
     * started from — the mechanism a pure imitator (trainDecision) cannot have.
     * Gradient is the general soft-target CE: dLogit_k = softmax_k - target_k. Returns
     * the cross-entropy loss BEFORE the update. `target` must sum to 1 over the K cands.
     */
    def trainDecisionSoft(state : Array[Double], actions : Array[Array[Double]], target : Array[Double], lr : Double) : Double = {
        val K = actions.length
        if (K <= 1) return 0.0
        val sPart = statePart(state)

        val z   = new Array[Double](K)
        val a1s = new Array[Array[Double]](K)
        var k = 0
        while (k < K) { val (_, a1, lg) = forwardCand(sPart, actions(k)); a1s(k) = a1; z(k) = lg; k += 1 }
        val p = softmax(z)
        var loss = 0.0; k = 0
        while (k < K) { if (target(k) > 0.0) loss -= target(k) * math.log(math.max(1e-12, p(k))); k += 1 }

        val gW1s = new Array[Double](hidden * dinS)
        val gW1a = new Array[Double](hidden * dinA)
        val gB1  = new Array[Double](hidden)
        val gW2  = new Array[Double](hidden)
        var gB2  = 0.0

        k = 0
        while (k < K) {
            val dLogit = p(k) - target(k)                     // soft-target CE gradient
            val a1 = a1s(k); val act = actions(k)
            var j = 0
            while (j < hidden) {
                gW2(j) += dLogit * a1(j)
                val dZ = dLogit * w2(j) * (1.0 - a1(j) * a1(j))
                gB1(j) += dZ
                val bs = j * dinS; var i = 0
                while (i < dinS) { gW1s(bs + i) += dZ * state(i); i += 1 }
                val ba = j * dinA; i = 0
                while (i < dinA) { gW1a(ba + i) += dZ * act(i); i += 1 }
                j += 1
            }
            gB2 += dLogit
            k += 1
        }

        var j = 0
        while (j < hidden) {
            w2(j) -= lr * gW2(j)
            b1(j) -= lr * gB1(j)
            val bs = j * dinS; var i = 0
            while (i < dinS) { w1s(bs + i) -= lr * gW1s(bs + i); i += 1 }
            val ba = j * dinA; i = 0
            while (i < dinA) { w1a(ba + i) -= lr * gW1a(ba + i); i += 1 }
            j += 1
        }
        b2 -= lr * gB2
        loss
    }

    def describe : String = s"Policy scorer (state=$dinS + action=$dinA -> hidden=$hidden -> logit, softmax over legal set)"

    /** Deep copy — snapshot the trained weights (for best-checkpoint keeping). */
    def copy : PolicyModel = new PolicyModel(dinS, dinA, hidden, w1s.clone(), w1a.clone(), b1.clone(), w2.clone(), b2)
}

object PolicyModel {
    /** Deterministic small-weight init (no Math.random — banned in this harness, and
     *  reproducible). Trig-spread seeds so hidden units start differentiated; zero
     *  output so the initial policy is ~uniform over any legal set. */
    def initial(dinS : Int = Features.dim, dinA : Int = ActionFeatures.dim, hidden : Int = 64) : PolicyModel = {
        val w1s = new Array[Double](hidden * dinS)
        val w1a = new Array[Double](hidden * dinA)
        val b1  = new Array[Double](hidden)
        val w2  = new Array[Double](hidden)
        var j = 0
        while (j < hidden) {
            val bs = j * dinS; var i = 0
            while (i < dinS) { w1s(bs + i) = 0.05 * math.sin(0.7 * (bs + i) + 1.3); i += 1 }
            val ba = j * dinA; i = 0
            while (i < dinA) { w1a(ba + i) = 0.10 * math.sin(0.6 * (ba + i) + 0.9); i += 1 }
            b1(j) = 0.0
            w2(j) = 0.0                                  // start with uniform policy
            j += 1
        }
        new PolicyModel(dinS, dinA, hidden, w1s, w1a, b1, w2, 0.0)
    }
}
