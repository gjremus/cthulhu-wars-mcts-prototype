package cws

import hrf.colmat._

// Option A, Phase 2 — the search.
//
// A faction-agnostic MCTS that plugs into the SAME seam as the hand-tuned bots
// (it IS a DecisionPolicy). Nothing here names a faction or a strategy: every
// tree node just records "whose turn is it here" and steers toward the move that
// maximizes THAT faction's win rate. Point four opponents at one MCTSPolicy and
// it plays all four seats — the substrate for self-play with zero per-faction code.
//
// How one simulation works (classic UCT, one clone per simulation):
//   1. Clone the root position once (the proven Cloning.copy fork primitive).
//   2. SELECT: from the root, repeatedly pick a child by UCB1 and REPLAY that
//      move on the working clone, until we reach a not-yet-expanded child.
//   3. EXPAND: create that child node.
//   4. ROLLOUT: play the working clone to the end with a fast default policy.
//   5. BACKPROP: for every node on the path, credit the move by whether THAT
//      node's to-move faction ended up among the winners.
//
// The rollout's leaf estimate is the ONE place a learned value net will later
// replace the cheap default — that swap is Phase 3. Everything else stays.

object MCTS {

    // Exploration constant for UCB1. sqrt(2) is the textbook default.
    val DefaultC = 1.41421356
    // Simulations per move. Small by design for the prototype — the point is to
    // prove the architecture, not to play strongly yet.
    val DefaultSims = 60
    // Hard cap on rollout length so a random playout can't run away.
    val RolloutDecisionCap = 4000

    /** A node in the search tree. Holds ONLY statistics — never a game state.
     *  The working game clone is advanced by replaying move indices, so a node
     *  needs to know only how many legal moves it has and their running stats. */
    final class Node(val faction : Faction, val nChildren : Int, val prior : Array[Double] = null) {
        val childN = new Array[Int](nChildren)              // times child i was tried
        val childW = new Array[Double](nChildren)            // wins (this node's faction) via child i
        val children = new Array[Node](nChildren)            // expanded child (null = unexpanded)
        // non-null => edge ends the game; stores the DENSE terminal value per faction
        // (Outcome.value) captured when the edge was first resolved, so cached hits
        // reuse the exact terminal standing without keeping the finished game around.
        val terminalValue = new Array[Map[Faction, Double]](nChildren)
        var totalN = 0

        def firstUnexpanded : Int = {
            var i = 0
            while (i < nChildren) { if (childN(i) == 0) return i; i += 1 }
            -1
        }

        /** UCB1 pick among already-tried children (call only when none unexpanded). */
        def bestUCB(c : Double) : Int = {
            val logN = math.log(totalN.toDouble)
            var best = 0; var bestV = Double.NegativeInfinity
            var i = 0
            while (i < nChildren) {
                val mean = childW(i) / childN(i)
                val ucb = mean + c * math.sqrt(logN / childN(i))
                if (ucb > bestV) { bestV = ucb; best = i }
                i += 1
            }
            best
        }

        /** PUCT pick over ALL children (AlphaZero selection). No forced full expansion:
         *  an unvisited child scores Q=0 plus the prior-weighted exploration bonus
         *  c*P(i)*sqrt(totalN)/(1+N(i)), so the policy prior steers which moves get
         *  tried first instead of a blind left-to-right sweep. Falls back to a uniform
         *  prior if none was supplied (behaves like plain exploration then). */
        def bestPUCT(c : Double) : Int = {
            val sqrtN = math.sqrt(math.max(1, totalN).toDouble)
            val uni = 1.0 / nChildren
            var best = 0; var bestV = Double.NegativeInfinity
            var i = 0
            while (i < nChildren) {
                val q = if (childN(i) > 0) childW(i) / childN(i) else 0.0
                val p = if (prior != null) prior(i) else uni
                val u = c * p * sqrtN / (1 + childN(i))
                val score = q + u
                if (score > bestV) { bestV = score; best = i }
                i += 1
            }
            best
        }

        /** Most-visited child — the move actually returned after search. */
        def mostVisited : Int = {
            var best = 0; var bestN = -1
            var i = 0
            while (i < nChildren) { if (childN(i) > bestN) { bestN = childN(i); best = i }; i += 1 }
            best
        }
    }

    /** reward for one faction: 1.0 if it is among the winners, else 0.0. */
    def reward(winners : $[Faction], f : Faction) : Double =
        if (winners.contains(f)) 1.0 else 0.0
}

/** One self-play policy-improvement example: the state, every legal candidate's action
 *  features, and the MCTS visit distribution over them (the improved move-target the
 *  policy net trains toward). `faction` is who moved, kept for optional per-seat stats. */
final class PolicyTarget(
    val state   : Array[Double],
    val actions : Array[Array[Double]],
    val visits  : Array[Double],
    val faction : Faction
)

/** How MCTS estimates the value of a freshly expanded leaf. */
sealed trait LeafEval
/** Play to a real GameOver with `policy`; reward = 1 for actual winners. The
 *  original, slow, unbiased estimate — good ground truth, terrible throughput. */
final case class RolloutEval(policy : DecisionPolicy) extends LeafEval
/** Evaluate the position directly with a learned value net — no playout. This
 *  is the AlphaZero move: it makes the leaf estimate faction-aware AND turns a
 *  whole-game playout into one forward pass (the big speed win). Accepts any
 *  ValueNet — the one-neuron ValueModel or the deeper MLPModel — unchanged. */
final case class ModelEval(model : ValueNet) extends LeafEval

/** AlphaZero-style leaf: a POLICY net supplies the per-move prior that steers
 *  selection (PUCT), and a VALUE net supplies the leaf value estimate. The policy
 *  is the move-aware brain behavior-cloned from the bots; the value net can be any
 *  ValueNet (or, if null, the leaf value defaults to 0.5 — a pure policy-guided
 *  search whose only signal is the prior + real terminal outcomes). */
final case class PolicyValueEval(policy : PolicyModel, value : ValueNet) extends LeafEval

/**
 * MCTS as a DecisionPolicy. The leaf estimate is pluggable via `leaf`:
 *   - RolloutEval(RandomPolicy): the Phase-2 behaviour, plays out to GameOver.
 *   - ModelEval(valueModel):     the Phase-3 behaviour, evaluates the leaf state.
 * Backprop credits each node by ITS faction's estimated win value at the leaf, so
 * one policy still drives all four seats with zero per-faction code.
 */
class MCTSPolicy(
    sims : Int = MCTS.DefaultSims,
    c : Double = MCTS.DefaultC,
    leaf : LeafEval = RolloutEval(RandomPolicy)
) extends DecisionPolicy {
    import MCTS._

    // Convenience: keep the old positional (sims, c, rolloutPolicy) call working.
    def this(sims : Int, c : Double, rolloutPolicy : DecisionPolicy) =
        this(sims, c, RolloutEval(rolloutPolicy))

    // ---- self-play policy-improvement recorder --------------------------------------
    // When non-null, every genuine decision appends its (state, per-candidate action
    // features, MCTS visit distribution) — the AlphaZero training target. The search's
    // visit distribution is a STRONGER move-picker than the raw policy prior (lookahead
    // sharpens it), so training the policy net toward it is what improves the net past
    // its starting point. Per-game searcher => this buffer is single-threaded per game,
    // safe under parallel self-play (each game builds its own MCTSPolicy).
    var recorder : scala.collection.mutable.ArrayBuffer[PolicyTarget] = null

    // True when a policy net supplies move priors — selection then uses PUCT.
    private val usePUCT : Boolean = leaf.isInstanceOf[PolicyValueEval]

    /** Per-move prior from the policy net over `acts` at this decision, or null when
     *  not in policy mode (selection falls back to firstUnexpanded/UCB1). Read-only on
     *  the game, so it is safe on the shared clone during search. */
    private def priorFor(g : Game, f : Faction, acts : $[Action]) : Array[Double] = leaf match {
        case PolicyValueEval(pol, _) =>
            val state = Features.of(g, f)
            val af = acts.toArray.map(a => ActionFeatures.of(g, f, a))
            pol.policy(state, af)
        case _ => null
    }

    // Degenerate-move filter (Option A). ControlGateAction on a gate the faction
    // ALREADY controls is a pure unit-swap no-op: it costs 0 power, leaves the board
    // in an identical state, and returns to the same control menu — an infinite free
    // loop the brain cannot escape (the value net sees the same state value before and
    // after, so nothing discourages it). The stall trace confirmed this is the stall:
    // 576/1600 decisions were exactly this swap, alternating with the menu it returns
    // to. We never hand these to the search. TAKING control of an ABANDONED gate
    // (f.gates.has(r) == false) is real progress and is kept. Faction-agnostic: read
    // only from the acting faction's own gate set. If filtering would empty the set
    // (shouldn't happen — the menu always also offers done/EndTurn), we keep the
    // originals so the game never deadlocks.
    private def isDegenerate(game : Game, a : Action) : Boolean = a.unwrap match {
        case ControlGateAction(f, r, _, _) => game.players(f).gates.has(r)
        case _                             => false
    }

    def decide(game : Game, faction : Faction, actionsIn : $[Action]) : Action = {
        val kept = actionsIn.%!(a => isDegenerate(game, a))
        val actions = if (kept.any) kept else actionsIn
        if (actions.num == 1) return actions.head
        val rootContinue : Continue = Ask(faction, actions)
        // Root prior from the policy net (null in value-only/rollout mode). Read-only.
        val root = new Node(faction, actions.num, priorFor(game, faction, actions))

        var s = 0
        while (s < sims) { simulateOnce(game, rootContinue, root); s += 1 }

        // Self-play recording: capture the improved move-distribution (normalized root
        // visit counts) as a training target, alongside the state and every candidate's
        // action features. This is what the policy net trains TOWARD to improve.
        if (recorder != null) {
            val K = actions.num
            val visits = new Array[Double](K)
            var tot = 0.0; var i = 0
            while (i < K) { val v = root.childN(i).toDouble; visits(i) = v; tot += v; i += 1 }
            if (tot > 0) {
                i = 0; while (i < K) { visits(i) /= tot; i += 1 }
                val acts = actions.toArray
                recorder += new PolicyTarget(
                    Features.of(game, faction),
                    acts.map(a => ActionFeatures.of(game, faction, a)),
                    visits, faction)
            }
        }

        // Most-visited root child. explode is deterministic on identical state, so
        // the caller's `actions` is index-aligned with the root's children.
        actions(root.mostVisited)
    }

    /** One MCTS iteration: select → expand → evaluate leaf → backprop.
     *  The leaf produces a value FUNCTION `Faction => Double` (win prob for any
     *  faction), so each node on the path is credited from ITS OWN faction's view.
     *  Terminal: 1/0 by actual win. RolloutEval: 1/0 by played-out winner.
     *  ModelEval: the learned win-probability of the leaf state. */
    private def simulateOnce(rootGame : Game, rootContinue : Continue, root : Node) : Unit = {
        // Fork the position once. All mutation below happens on this clone.
        val (g, c0) = Cloning.copy(rootGame, rootContinue)

        val path = scala.collection.mutable.ArrayBuffer[(Node, Int)]()
        var node = root
        var leafValue : Faction => Double = null   // set exactly once, then we stop

        // The exploded, performable moves at the CURRENT node. We never re-explode
        // inside the search: Explode has side effects (it performs Soft menu actions)
        // and calls random(), so it is NOT idempotent (re-exploding can drop a move,
        // e.g. 8 -> 7). The root continue is the cloned Ask carrying the already-
        // exploded actions the harness handed us; every deeper Decision carries its
        // own exploded actions. We use those verbatim.
        var choices : $[Action] = c0 match {
            case Ask(_, exploded) => exploded
            case other            => throw new RuntimeException(
                "MCTS: root continue must be an Ask, got " + other.getClass.getName)
        }

        while (leafValue == null) {
            // Cthulhu Wars is STOCHASTIC: advanceToDecision resolves dice
            // (RollD6/RollBattle/DrawES) with random draws, so descending the SAME
            // edge on two simulations can land in DIFFERENT post-chance states with
            // DIFFERENT legal-move counts. A fixed-arity tree node can't hold that
            // (this is why stochastic games need chance-aware MCTS). Prototype
            // handling: if the freshly-derived move count doesn't match what this
            // node was built for, this visit hit a different chance outcome — rebuild
            // the node for THIS outcome. It trades some cross-outcome stat reuse for
            // correctness and never crashes. The root never drifts (no chance precedes
            // it), so its arity is always stable.
            if (choices.num != node.nChildren) {
                val fresh = new Node(node.faction, choices.num,
                    if (usePUCT) priorFor(g, node.faction, choices) else null)
                if (node eq root) {
                    // Should be unreachable — guard loudly if the invariant breaks.
                    throw new RuntimeException(
                        s"MCTS: root move count drift (${choices.num} vs ${node.nChildren}) — explode nondeterministic at root?")
                }
                // Re-point the parent's child slot at the rebuilt node.
                val (parent, pIdx) = path.last
                parent.children(pIdx) = fresh
                node = fresh
            }

            // PUCT (policy mode) selects over ALL children so the prior steers which
            // move is tried first; value/rollout mode keeps the original expand-each-
            // then-UCB1 sweep.
            val childIdx =
                if (usePUCT) node.bestPUCT(c)
                else { val unexp = node.firstUnexpanded; if (unexp >= 0) unexp else node.bestUCB(c) }
            path += ((node, childIdx))

            // Cached terminal edge — dense standing already captured, no work to do.
            if (node.terminalValue(childIdx) != null) {
                val tv = node.terminalValue(childIdx)
                leafValue = f => tv.getOrElse(f, 0.0)
            } else {
                // Apply the chosen move on the clone and advance to next decision.
                val (_, next) = g.perform(choices(childIdx).unwrap)
                val sit = Engine.advanceToDecision(g, next)

                if (node.children(childIdx) == null) {
                    // EXPANSION — first descent down this edge.
                    sit match {
                        case Ended(w) =>
                            val tv = terminalStanding(g, w)
                            node.terminalValue(childIdx) = tv
                            leafValue = f => tv.getOrElse(f, 0.0)
                        case d @ Decision(f2, acts2, c2) =>
                            node.children(childIdx) = new Node(f2, acts2.num,
                                if (usePUCT) priorFor(g, f2, acts2) else null)
                            leafValue = evalLeaf(g, d)
                    }
                } else {
                    // Descend into the existing child (already advanced g to it).
                    sit match {
                        case Decision(_, acts2, _) => node = node.children(childIdx); choices = acts2
                        case Ended(w) =>
                            val tv = terminalStanding(g, w)
                            node.terminalValue(childIdx) = tv
                            leafValue = f => tv.getOrElse(f, 0.0)
                    }
                }
            }
        }

        // BACKPROP: credit every edge by its own node-faction's leaf value.
        var i = 0
        while (i < path.length) {
            val (n, idx) = path(i)
            n.totalN += 1
            n.childN(idx) += 1
            n.childW(idx) += leafValue(n.faction)
            i += 1
        }
    }

    /** Estimate the value of a freshly expanded, non-terminal leaf as a per-faction
     *  win-probability function, per the configured LeafEval. */
    private def evalLeaf(g : Game, leafSit : Situation) : Faction => Double = leaf match {
        case ModelEval(model) =>
            // One dot product per faction on the CURRENT leaf state — no playout.
            val cache = g.setup.map(f => f -> model.eval(g, f)).toMap
            f => cache.getOrElse(f, 0.5)
        case PolicyValueEval(_, value) =>
            // AlphaZero leaf value: the value net if present, else a neutral 0.5 so the
            // search is driven purely by the policy prior + real terminal outcomes.
            if (value == null) (_ => 0.5)
            else { val cache = g.setup.map(f => f -> value.eval(g, f)).toMap; f => cache.getOrElse(f, 0.5) }
        case RolloutEval(policy) =>
            // Play the clone to a real end; g is now at game over. Use the DENSE
            // terminal standing (win = 1.0, else doom-scaled) — same signal as the
            // trainer, so rollout- and model-leaf searches agree on what "good" means.
            val w = Engine.rollout(g, leafSit, policy, RolloutDecisionCap)
            terminalStanding(g, w)
    }

    /** Dense terminal value per faction, captured from the finished game state. */
    private def terminalStanding(g : Game, winners : $[Faction]) : Map[Faction, Double] =
        g.setup.map(f => f -> Outcome.value(g, winners, f)).toMap
}
