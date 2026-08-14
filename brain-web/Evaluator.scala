package cws

import hrf.colmat._

// Option A, Phase 3 — the learned, faction-agnostic evaluation.
//
// This is the piece that makes the whole thing "one brain for every faction" and
// the piece that kills the random-rollout cost sink. Two parts:
//
//   Features(game, faction): turn any game state, seen through ONE faction's eyes,
//     into a fixed-length vector of plain numbers. It is deliberately faction-
//     AGNOSTIC — it never says "if DS then...". It measures the same universal
//     quantities (my doom vs the leader's, my gates, power, units, spellbooks,
//     board presence, whose turn, how close anyone is to 30) for whoever is asked.
//     A Yellow-Sign position and a Windwalker position produce vectors in the same
//     space, so one model reads them both.
//
//   ValueModel: maps that vector to a single number in (0,1) = "probability the
//     asked faction wins from here". Logistic model, trained by gradient descent
//     on self-play outcomes (Phase 4). Starts as a fixed hand-set guess so the
//     search is usable on turn one; self-play then overwrites the weights with
//     what actually correlates with winning. THIS is the "balance interstitial
//     goals" the RL plateau couldn't hand-author — it's learned from win/loss.
//
// Why in Scala, not Python: keeping evaluation in-process avoids a serialize/round-
// trip per leaf (thousands per move). A deeper net can replace ValueModel later
// behind the same eval(features) call without touching MCTS or the harness.

object Features {

    // ---- RICH board-aware, FACTION-AGNOSTIC encoding -----------------------------
    //
    // The old 12-scalar summary (global stats only) was falsified across three runs:
    // it threw away board structure, so the net could not separate winning from
    // losing positions. This encoding gives the net the whole board, seen through
    // ONE faction's eyes, while staying strictly faction-agnostic — it never says
    // "if faction==X". Every quantity is universal (counts, power, doom, gates,
    // spellbook slots, ritual position) and read identically for whoever is asked.
    //
    // Faction ordering is RELATIVE: the asked faction `me` is always slot 0, the
    // other three follow in stable seating order. So a Yellow-Sign eye and a Great-
    // Cthulhu eye produce vectors in the same space — "me / the faction to my left /
    // ..." — and one model reads them all. Spellbook bits index each faction's OWN
    // 6-book library in its own fixed order, so slot j always means the same book
    // for that faction, captured positionally without ever naming the faction.
    //
    // Layout (all blocks packed by a running cursor; `dim` is derived, never hand-
    // typed, so it can't drift):
    //   1. board tensor : region(17) x faction(4) x unit-role(5) on-map counts
    //   2. gate control : region(17) x faction(4)  (1.0 if that faction holds a gate)
    //   3. per-faction  : power, doom, ES-revealed, ES-hidden, gates, units-on-map,
    //                     combat-proxy, captured-away, win-eligible, GOO-awake,
    //                     + 6 spellbook-earned bits                 (16 x 4)
    //   4. ritual       : marker, cost, can-I-afford
    //   5. turn context : turn, round, doomPhase, player-count
    //   6. bias         : 1.0

    private val DoomGoal    = 30.0

    // ─── ENVIRONMENT CAPACITIES (Phase 1 extensibility, 2026-08-01) ────────────────
    // The encoding was always shape-agnostic (relative seats, own-order unit/spellbook
    // slots — it never says "if faction==X"), but four SIZES were hardcoded to base-4 /
    // Earth: seat count, per-faction unit slots, ability slots, and the on-map region
    // count used as the board-tensor STRIDE. That silently bound the whole net to
    // 17-region, 4-player games. These are now CAPACITIES — the MAX the tensor reserves —
    // and are env-overridable. DEFAULTS ARE TODAY'S EXACT VALUES, so with no env set the
    // vector is byte-identical (dim stays 777/128) and the running champion + its on-disk
    // checkpoint keep working unchanged. To expand (more players, a bigger map, factions
    // with more unit classes/abilities) you RAISE a capacity via env; live data then fills
    // a prefix of the larger reserved block and the extra slots stay zero until used. A
    // capacity change resizes `dim`, so the champion's base-4 weights must be TRANSFERRED
    // into the larger layout (see Checkpoint.migrate) rather than loaded raw — that is the
    // warm-start the directive calls for, never a cold relearn. Clamped to a sane floor so
    // a garbage env can't shrink the tensor below what base-4 needs.
    // BROWSER PORT: the JVM build read these capacities from CW_MAX_* env vars so a
    // larger run could resize the tensor. The browser has no environment and the /brain/
    // build is base-4 / Earth-3.5 ONLY, so the caps are the fixed base-4 constants from
    // ClientCaps (ClientShims.scala) — the EXACT values the JVM defaulted to with no env
    // set, so dim stays 777/128 and the champion's on-disk weights load raw (no migration).
    private val NumFactions  = ClientCaps.NumFactions    // seats reserved (base-4)
    private val UnitSlots     = ClientCaps.UnitSlots     // per-faction distinct unit classes (base-4 uses 5; 1 spare)
    private val AbilitySlots  = ClientCaps.AbilitySlots  // innate abilities per faction (base-4 max = 3; 1 spare)
    private val MaxRegions    = ClientCaps.MaxRegions    // on-map region slots reserved (Earth 4v3.5 = 17)
    private val PerFaction    = 38                // scalar block width per faction (see layout below)

    // package-private so the action encoder and checkpoint-migration share the SAME caps.
    private[cws] def maxRegions   : Int = MaxRegions
    private[cws] def maxFactions  : Int = NumFactions
    private[cws] def maxUnitSlots : Int = UnitSlots

    // Board-OBJECT layers (region-list state on Game). Faction-agnostic map features.
    // Base-4 only populates `desecrated` (YS); the rest are wired now so the encoding
    // is forward-compatible with the expansions (cathedrals, chaos gates, craters,
    // and later faction glyphs/monuments) without another dim change per the plan.
    private val ObjectLayers = 4                 // desecrated, cathedral, chaosGate, crater

    // Per-faction unit-class SLOT map. Each faction's own distinct unit classes, in
    // engine order (Cthulhu/Starspawn/Shoggoth/DeepOne/Acolyte for GC, etc.). Slot j
    // therefore names a SPECIFIC unit (identity preserved) — this is the fix for the
    // old coarse "role" bucketing, which collapsed King-in-Yellow (GOO/4) and Hastur
    // (GOO/10) into one cell and erased their identity. Faction-agnostic exactly like
    // the spellbook bits: slot j means "the j-th unit class of the faction in this
    // relative seat", never "if faction==X". Built once per faction (static vals).
    // BROWSER PORT: was scala.collection.concurrent.TrieMap (self-play built this cache
    // across parallel THREADS). The browser is single-threaded, so a plain mutable.Map
    // is correct and identical in semantics — getOrElseUpdate is the only op used.
    private val slotCache = scala.collection.mutable.Map[Faction, Map[UnitClass, Int]]()
    // package-private so the action encoder (ActionFeatures) can share the SAME
    // faction-agnostic unit-slot map — slot j means "the j-th unit class of this
    // faction", identically for state features and action features.
    private[cws] def slotsOf(f : Faction) : Map[UnitClass, Int] =
        slotCache.getOrElseUpdate(f, f.allUnits.distinct.zipWithIndex.toMap)

    // package-private so ActionFeatures can reuse the cached on-map region index
    // instead of rebuilding it per candidate action (thousands per decision).
    private[cws] def regionIndexOf(game : Game) : (Array[Region], Map[Region, Int]) = regionsOf(game)
    private[cws] def unitSlotWidth : Int = UnitSlots
    private[cws] def numFactions : Int = NumFactions

    // Cache the on-map region ordering per board (stable val on the engine); avoids
    // rebuilding the index on every leaf eval (thousands per move).
    @volatile private var cachedBoard   : Board = null
    @volatile private var cachedRegions : Array[Region] = Array()
    @volatile private var cachedIndex   : Map[Region, Int] = Map()
    private def regionsOf(game : Game) : (Array[Region], Map[Region, Int]) = {
        val b = game.board
        if (b ne cachedBoard) {
            val rs = b.regions.filter(_.glyph.onMap).toArray
            cachedRegions = rs
            cachedIndex   = rs.zipWithIndex.toMap
            cachedBoard   = b
        }
        (cachedRegions, cachedIndex)
    }

    // Per-faction scalar block (38 wide), laid out inside `of`:
    //   [0..9]   10 globals: power, doom, ES-revealed, ES-hidden, gates, units,
    //            TOTAL COMBAT (real engine strength), captured-away, win-eligible, GOO-awake
    //   [10..15] 6 SPELLBOOK-EARNED bits (has the book)          — identity by own library order
    //   [16..21] 6 SPELLBOOK-COOLDOWN bits (book used this turn/round/game/action/battle)
    //   [22..27] 6 REQUIREMENT-REMAINING bits (win condition still unmet) — the "what
    //            do I still need to finish all 6 and win" signal, SEPARATE from books
    //   [28]     battled-this-turn region count (recent-combat event)
    //   [29]     units currently Zeroed (took battle damage — recent-battle residue)
    //   [30..33] 4 ABILITY-OWNED bits (innate powers: GC Devour/Immortal, YS Desecrate...)
    //   [34..37] 4 ABILITY-AVAILABLE bits: owned AND its enabling condition holds right
    //            now (e.g. Devour available only with a GOO on the map, per the engine's
    //            own s.forces(Cthulhu) gate). Pairing owned+available lets the net learn
    //            the conjunction "GC can Devour = owns Devour AND has an awakened GOO",
    //            and lets it see that a RIVAL just has a reusable power up (imitation).

    // Derived dimension — stays in lock-step with the layout. Uses MaxRegions (the
    // reserved capacity), NOT the live region count, so the vector length is CONSTANT
    // for a given set of caps regardless of which map is loaded — a smaller map just
    // leaves trailing region slots zero. Default caps reproduce the historical 777.
    val dim : Int = {
        val nr = MaxRegions
        nr * NumFactions * UnitSlots +   // 1. board tensor (per SPECIFIC unit class)
        nr * NumFactions +               // 2. per-region combat strength (real dice)
        nr * NumFactions +               // 3. gate control
        nr * ObjectLayers +              // 4. board objects (desecration/cathedral/chaosgate/crater)
        NumFactions * PerFaction +       // 5. per-faction block
        3 +                              // 6. ritual
        4 +                              // 7. turn context
        5 +                              // 7b. phase one-hot (setup/action/gather/firstPlayer/doom)
        1                                // 8. bias
    }

    /** Build the rich feature vector for `me` in the current `game` state. */
    def of(game : Game, me : Faction) : Array[Double] = {
        implicit val g : Game = game
        val (regions, ridx) = regionsOf(game)
        // `nr` = live on-map region count for ITERATION; `stride` = MaxRegions reserved
        // capacity used for CURSOR MATH so every block lands at a constant offset no
        // matter which map is loaded. A map with more regions than the cap fills the
        // first MaxRegions and drops the overflow (guarded per write) rather than
        // corrupting later blocks; a smaller map leaves the tail zero.
        val stride = MaxRegions
        val nr = math.min(regions.length, stride)

        // Relative faction order: me first, then the rest in stable seating order.
        val order : Array[Faction] = {
            val rest = game.setup.filter(_ != me).toArray
            Array(me) ++ rest
        }.take(NumFactions)

        val a = new Array[Double](dim)
        var cur = 0

        // --- 1. board tensor: region x faction x SPECIFIC-unit-class counts -------
        // Index = ((r * NumFactions) + fi) * UnitSlots + slot. /3 keeps stacks ~[0,2].
        val boardBase = cur
        var fi = 0
        while (fi < order.length) {
            val f = order(fi)
            val slots = slotsOf(f)
            game.players(f).units.foreach { u =>
                if (u.region.glyph.onMap) {
                    val ri = ridx.getOrElse(u.region, -1)
                    val slot = slots.getOrElse(u.uclass, -1)
                    if (ri >= 0 && ri < stride && fi < NumFactions && slot >= 0 && slot < UnitSlots)
                        a(boardBase + ((ri * NumFactions) + fi) * UnitSlots + slot) += 1.0 / 3.0
                }
            }
            fi += 1
        }
        cur += stride * NumFactions * UnitSlots

        // --- 2. per-region COMBAT strength (real engine dice, per faction) --------
        // The engine's own strength() folds in each unit's combat AND its faction's
        // battle spellbooks/abilities (Frenzy, Red Sign, Hastur=ritualCost dice, GC
        // Absorbed, etc.) — so this is true fighting power in that region, not a proxy.
        val combatBase = cur
        fi = 0
        while (fi < order.length) {
            val f = order(fi)
            val opp = order.find(_ != f).getOrElse(f)   // opponent only tweaks neutral terms
            val p = game.players(f)
            var ri = 0
            while (ri < nr) {
                val here = p.at(regions(ri))
                if (here.nonEmpty && fi < NumFactions) a(combatBase + ri * NumFactions + fi) = f.strength(here, opp) / 10.0
                ri += 1
            }
            fi += 1
        }
        cur += stride * NumFactions

        // --- 3. gate control: 1.0 where that faction holds a gate -----------------
        val gateBase = cur
        fi = 0
        while (fi < order.length) {
            game.players(order(fi)).allGates.foreach { r =>
                val ri = ridx.getOrElse(r, -1)
                if (ri >= 0 && ri < stride && fi < NumFactions) a(gateBase + ri * NumFactions + fi) = 1.0
            }
            fi += 1
        }
        cur += stride * NumFactions

        // --- 4. board OBJECTS: region-indexed tokens/structures --------------------
        // Layer 0 desecration (YS, live in base-4), 1 cathedral, 2 chaos gate, 3 crater.
        // Non-base-4 layers stay all-zero until those expansions seat, but the slots
        // exist so the net's shape never changes when they do.
        val objBase = cur
        def markObjects(layer : Int, rs : $[Region]) : Unit =
            rs.foreach { r => val ri = ridx.getOrElse(r, -1); if (ri >= 0 && ri < stride) a(objBase + ri * ObjectLayers + layer) = 1.0 }
        markObjects(0, game.desecrated)
        markObjects(1, game.cathedrals)
        markObjects(2, game.chaosGateRegions)
        markObjects(3, game.fbCraters)
        cur += stride * ObjectLayers

        // --- 5. per-faction block: globals + books + cooldowns + requirements + events
        fi = 0
        while (fi < order.length) {
            val f = order(fi)
            val p = game.players(f)
            val base = cur + fi * PerFaction
            val opp = order.find(_ != f).getOrElse(f)
            a(base + 0)  = (p.power - 8.0) / 12.0
            a(base + 1)  = p.doom / DoomGoal
            a(base + 2)  = p.revealed.map(_.value).sum / 10.0
            a(base + 3)  = p.es.num / 6.0
            a(base + 4)  = p.allGates.num / 8.0
            a(base + 5)  = p.allInPlay.num / 12.0
            a(base + 6)  = f.strength(p.allInPlay, opp) / 20.0          // TOTAL real combat
            a(base + 7)  = p.units.count(_.region.glyph == Prison) / 6.0
            a(base + 8)  = if (p.hasAllSB) 1.0 else 0.0
            a(base + 9)  = if (p.onMap(GOO).any) 1.0 else 0.0
            val lib  = f.library
            val reqs = f.requirements(game.options)
            var j = 0
            while (j < 6) {
                if (j < lib.num) {
                    val sb = lib(j)
                    if (p.spellbooks.has(sb)) a(base + 10 + j) = 1.0               // earned
                    if (usedNow(p, sb))       a(base + 16 + j) = 1.0               // on cooldown (just used)
                }
                if (j < reqs.num && p.needs(reqs(j))) a(base + 22 + j) = 1.0        // requirement still open
                j += 1
            }
            a(base + 28) = p.battled.num / 6.0                          // battles this turn (recent event)
            a(base + 29) = p.units.count(_.state.contains(Zeroed)) / 6.0 // units zeroed (battle damage)
            // 4 innate-ability owned/available bits (faction's own ability order).
            val abils = f.abilities
            var k = 0
            while (k < AbilitySlots) {
                if (k < abils.num) {
                    val ab = abils(k)
                    a(base + 30 + k) = 1.0                                            // owned (innate)
                    if (abilityAvailable(game, f, p, ab)) a(base + 34 + k) = 1.0      // usable right now
                }
                k += 1
            }
            fi += 1
        }
        cur += NumFactions * PerFaction

        // --- 6. ritual track ------------------------------------------------------
        a(cur + 0) = game.ritualMarker / 8.0
        a(cur + 1) = game.ritualCost / 10.0
        a(cur + 2) = if (game.players(me).power >= game.ritualCost) 1.0 else 0.0
        cur += 3

        // --- 7. turn context ------------------------------------------------------
        a(cur + 0) = math.min(game.turn, 12) / 12.0
        a(cur + 1) = math.min(game.round, 20) / 20.0
        a(cur + 2) = if (game.doomPhase) 1.0 else 0.0
        a(cur + 3) = (game.setup.num - 3).toDouble / 3.0
        cur += 4

        // --- 7b. phase one-hot ----------------------------------------------------
        // The game runs in 5 phases and EVERY decision's meaning depends on which one
        // it is (a ritual is only offered in doom; power is gathered in gather-power;
        // etc.). The old encoding had only a single doomPhase bit, collapsing setup /
        // action / gather-power / first-player into one indistinguishable "not doom"
        // state. Engine.phase (0..4) is a persistent marker set at each phase entry.
        val ph = game.phase
        if (ph >= 0 && ph < 5) a(cur + ph) = 1.0
        cur += 5

        // --- 8. bias --------------------------------------------------------------
        a(cur) = 1.0
        cur += 1

        a
    }

    // A spellbook is "on cooldown" (just used) if it sits in any once-per-* set.
    private def usedNow(p : Player, sb : Spellbook) : Boolean =
        p.oncePerGame.contains(sb) || p.oncePerTurn.contains(sb) ||
        p.oncePerRound.contains(sb) || p.oncePerAction.contains(sb) ||
        p.oncePerBattle.contains(sb)

    // Whether an innate ability is USABLE now, given the board. Encoded so the net
    // sees not just ownership but live capability — e.g. Devour needs an awakened GOO
    // (mirrors the engine's s.forces(Cthulhu) gate), and per-turn abilities gate on
    // not-yet-used-this-turn. Kept faction-agnostic: conditions are expressed as
    // universal board facts (has GOO on map / not on cooldown), not "if faction==X".
    private def abilityAvailable(game : Game, f : Faction, p : Player, ab : Spellbook) : Boolean = {
        if (usedNow(p, ab)) false
        else if (BattleTriggeredAbilities.contains(ab.toString)) p.onMap(GOO).any // needs a GOO in a fight
        else true
    }

    // Abilities whose use is gated on having an awakened GOO on the board (Devour is
    // the canonical case: only GC, only with Cthulhu awakened). Matched on the book's
    // string name to avoid importing every faction's ability symbols here.
    private val BattleTriggeredAbilities : Set[String] = Set("Devour")
}

/**
 * The value estimator seam. MCTS (via ModelEval) only ever needs to (a) evaluate a
 * position for a faction and (b) be trained on a (features,label) pair. Keeping this
 * a trait lets the one-neuron logistic model and the deeper MLP both plug into the
 * SAME search and self-play loop with zero downstream changes — the whole point of
 * "just swap in a bigger brain".
 */
trait ValueNet {
    /** Estimated win/quality value in (0,1) for the asked faction from this state. */
    def eval(f : Array[Double]) : Double
    def eval(game : Game, me : Faction) : Double = eval(Features.of(game, me))
    /** One SGD step on a single (features, label in [0,1]) example. */
    def train(f : Array[Double], label : Double, lr : Double) : Unit
    /** Short human-readable description for run logs. */
    def describe : String
}

/**
 * Logistic value model over Features. eval(f) in (0,1). A one-neuron, zero-hidden-
 * layer net. Mutable weights so the self-play trainer can update them in place.
 * Thread-safe reads are fine (doubles); training happens between self-play batches,
 * not concurrently with search.
 */
class ValueModel(val weights : Array[Double]) extends ValueNet {
    require(weights.length == Features.dim, s"weights must be dim=${Features.dim}")

    private def sigmoid(x : Double) : Double = 1.0 / (1.0 + math.exp(-x))

    def dot(f : Array[Double]) : Double = {
        var s = 0.0; var i = 0
        while (i < f.length) { s += weights(i) * f(i); i += 1 }
        s
    }

    /** Estimated win probability for the asked faction from this position. */
    def eval(f : Array[Double]) : Double = sigmoid(dot(f))

    /** One SGD step on a single (features, label) example. label = 1 won, 0 lost.
     *  Logistic-regression gradient: (pred - label) * feature. */
    def train(f : Array[Double], label : Double, lr : Double) : Unit = {
        val pred = eval(f)
        val err = pred - label
        var i = 0
        while (i < f.length) { weights(i) -= lr * err * f(i); i += 1 }
    }

    def copyWeights : Array[Double] = weights.clone()
    def describe : String = s"logistic (1 neuron, dim=${weights.length})"
}

/**
 * Option A — the "bigger brain": a single-hidden-layer perceptron.
 *   input(dim) --W1--> hidden(H, tanh) --W2--> output(1, sigmoid)
 * Same eval(features)->(0,1) contract as ValueModel, so ModelEval/MCTS/self-play are
 * untouched. Trained by ordinary backprop (one SGD step per example). A tanh hidden
 * layer gives it the capacity to represent interactions between features (e.g.
 * "gates matter MORE when my power is high") that a single neuron structurally
 * cannot — which is the capacity the flat one-neuron run was missing.
 *
 * Reads share the weights (self-play threads only read); training runs between
 * batches, single-threaded. Weight arrays are plain doubles for cheap cloning.
 */
class MLPModel(
    val din : Int,
    val hidden : Int,
    val w1 : Array[Double],   // hidden x din, row-major
    val b1 : Array[Double],   // hidden
    val w2 : Array[Double],   // hidden (output weights)
    var b2 : Double
) extends ValueNet {
    require(w1.length == hidden * din && b1.length == hidden && w2.length == hidden,
        "MLP weight shapes inconsistent")

    private def sigmoid(x : Double) : Double = 1.0 / (1.0 + math.exp(-x))

    // Forward pass, returning (hidden preactivations z1, hidden activations a1, output).
    private def forward(f : Array[Double]) : (Array[Double], Array[Double], Double) = {
        val z1 = new Array[Double](hidden)
        val a1 = new Array[Double](hidden)
        var j = 0
        while (j < hidden) {
            var s = b1(j); val base = j * din; var i = 0
            while (i < din) { s += w1(base + i) * f(i); i += 1 }
            z1(j) = s; a1(j) = math.tanh(s)
            j += 1
        }
        var out = b2; j = 0
        while (j < hidden) { out += w2(j) * a1(j); j += 1 }
        (z1, a1, sigmoid(out))
    }

    def eval(f : Array[Double]) : Double = forward(f)._3

    /** Backprop one example. Loss = logistic (cross-entropy); with a sigmoid output
     *  the output-layer gradient is simply (pred - label), same as logistic reg. */
    def train(f : Array[Double], label : Double, lr : Double) : Unit = {
        val (z1, a1, pred) = forward(f)
        val dOut = pred - label                      // dL/d(output preact)
        // Output layer grads + backprop into hidden activations.
        var j = 0
        val dA1 = new Array[Double](hidden)
        while (j < hidden) {
            dA1(j) = dOut * w2(j)
            w2(j) -= lr * dOut * a1(j)
            j += 1
        }
        b2 -= lr * dOut
        // Hidden layer: tanh'(z) = 1 - tanh(z)^2 = 1 - a1^2.
        j = 0
        while (j < hidden) {
            val dZ = dA1(j) * (1.0 - a1(j) * a1(j))
            val base = j * din; var i = 0
            while (i < din) { w1(base + i) -= lr * dZ * f(i); i += 1 }
            b1(j) -= lr * dZ
            j += 1
        }
    }

    def describe : String = s"MLP (1 hidden layer, ${hidden} units, din=${din})"

    /** Deep copy — snapshot the trained weights (for best-checkpoint keeping). */
    def copy : MLPModel = new MLPModel(din, hidden, w1.clone(), b1.clone(), w2.clone(), b2)

    /** Overwrite THIS net's weights in place from `o` (same dims). Used by warm-start to
     *  continue from an on-disk checkpoint without rebinding the `val value` reference. */
    def adopt(o : MLPModel) : Unit = {
        require(o.din == din && o.hidden == hidden, "MLPModel.adopt: dim mismatch")
        System.arraycopy(o.w1, 0, w1, 0, w1.length)
        System.arraycopy(o.b1, 0, b1, 0, b1.length)
        System.arraycopy(o.w2, 0, w2, 0, w2.length)
        b2 = o.b2
    }
}

object MLPModel {
    /** Deterministic small-weight init (no Math.random — reproducible, and random()
     *  is banned in this harness). Spreads seeds across neurons so hidden units start
     *  differentiated. Output bias set so a blank net predicts ~baseline in a 4p game. */
    def initial(din : Int = Features.dim, hidden : Int = 32) : MLPModel = {
        val w1 = new Array[Double](hidden * din)
        val b1 = new Array[Double](hidden)
        val w2 = new Array[Double](hidden)
        // Cheap deterministic pseudo-spread: interleave a couple of trig seeds.
        var j = 0
        while (j < hidden) {
            val base = j * din; var i = 0
            while (i < din) {
                val k = (j * din + i).toDouble
                w1(base + i) = 0.15 * math.sin(0.7 * k + 1.3) // small, varied
                i += 1
            }
            b1(j) = 0.0
            w2(j) = 0.1 * math.cos(0.9 * j + 0.5)
            j += 1
        }
        new MLPModel(din, hidden, w1, b1, w2, -1.0)
    }
}

object ValueModel {
    /** A sensible fixed starting model so search is useful before any training:
     *  reward my doom + my lead, mild credit for gates/power/spellbooks/eligibility,
     *  penalise the leader's proximity to 30 when it isn't me. Hand-set only to
     *  bootstrap — self-play overwrites all of it. */
    def initial : ValueModel = {
        val w = new Array[Double](Features.dim)
        w(0)  =  2.0   // my doom progress
        w(1)  =  3.0   // my lead over rivals (dominant early signal)
        w(2)  = -1.5   // behind the leader is bad
        w(3)  = -0.5   // game ending while not clearly ahead is bad
        w(4)  =  1.0   // gates
        w(5)  =  0.6   // board presence
        w(6)  =  0.4   // power
        w(7)  =  0.8   // spellbook progress
        w(8)  =  1.2   // nearing win-eligibility
        w(9)  =  1.0   // win-eligible
        w(10) =  0.0   // player-count (let training decide)
        w(11) = -1.5   // bias: baseline < 0.5 win prob in a 4p game
        new ValueModel(w)
    }

    def zero : ValueModel = new ValueModel(new Array[Double](Features.dim))
}
