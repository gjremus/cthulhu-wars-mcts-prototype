package cws

import hrf.colmat._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

// Option A, Phase 7 — the ACTION encoder for the policy head.
//
// The value net answers "how good is this board" (Features.of -> a state vector).
// It is move-BLIND: it never sees WHICH action was taken, so MCTS + a value-only
// net cannot reconstruct the bots' move choices, and three feature expansions
// (12 -> 480 -> 772) all capped at 0% vs the bots. The missing piece is a POLICY
// head that scores ACTIONS. This file is its input half: turn any candidate action,
// seen from the deciding faction's eyes, into a fixed-length vector — the exact
// mirror of Features.of for state.
//
// The hard part: Cthulhu Wars hands each decision a VARIABLE-SIZE, HETEROGENEOUS
// set of legal actions (Move / Summon / Ritual / Capture / Battle / spellbook / ...),
// so there is no fixed AlphaZero-style output vector to softmax over. The fix is an
// action-SCORING head: encode each candidate independently, score it against the
// state, softmax across the legal set. This encoder is what makes the policy able to
// "tell every decision apart" — two candidates that differ in KIND, TARGET REGION,
// TARGET UNIT, TARGET FACTION, SPELLBOOK, or COST produce different vectors.
//
// Faction-agnostic exactly like Features: it never says "if faction==X". It reads an
// action's case-class identity (productPrefix -> a stable KIND slot) and its target
// arguments (productIterator -> Region / UnitClass / UnitRef / Faction / Spellbook /
// UnitType / Int / Boolean), mapping factions to RELATIVE seats (me=0) and unit
// classes to each faction's OWN ordered slot map (shared with Features.slotsOf). A
// Yellow-Sign "Desecrate here" and a Great-Cthulhu "Summon Cthulhu here" land in the
// same vector space, so ONE policy net reads them both.

object ActionFeatures {

    // Stable KIND vocabulary: action case-class name (productPrefix) -> slot. Assigned
    // lazily on first sight, shared across threads. Base-game action kinds number in
    // the dozens; overflow past KIND hashes into the last bucket (never crashes, only
    // collides — acceptable for rare kinds). Faction-agnostic: base action class names
    // ("MoveAction", "SummonAction", "RitualAction") don't name a faction.
    private val KIND = 64
    private val kindMap   = new ConcurrentHashMap[String, Integer]()
    private val kindNext  = new AtomicInteger(0)
    private def kindSlot(name : String) : Int = {
        val existing = kindMap.get(name)
        if (existing != null) existing.intValue
        else {
            val n = kindNext.getAndIncrement()
            if (n >= KIND) { // vocab full: deterministic hash into the last slot band
                val h = (math.abs(name.hashCode) % 4) + (KIND - 4)
                kindMap.putIfAbsent(name, Integer.valueOf(h))
                kindMap.get(name).intValue
            } else {
                val prev = kindMap.putIfAbsent(name, Integer.valueOf(n))
                if (prev != null) prev.intValue else n
            }
        }
    }

    // UnitType -> fixed one-hot slot (engine has 7 types; 1 spare).
    private def utypeSlot(t : UnitType) : Int = t match {
        case Cultist  => 0
        case Monster  => 1
        case Terror   => 2
        case GOO      => 3
        case Token    => 4
        case Building => 5
        case MapUnit  => 6
        case _        => 7
    }

    private val UTYPES   = 8
    private val SBSLOTS  = 6            // spellbook library slots per faction
    private val INTS     = 3           // first few Int args (cost / amount), normalised
    private val FLAGS    = 2           // isMore, isCancel (menu-navigation leaves)

    // Derived dimension — stays in lock-step with the blocks written in `of`.
    val dim : Int = {
        val nr = 17                                  // EarthMap4v35 on-map regions
        KIND +                                        // 1. action kind one-hot
        nr +                                          // 2. primary target region
        nr +                                          // 3. secondary target region (e.g. move destination)
        Features.unitSlotWidth +                      // 4. target unit-class slot (faction's own order)
        UTYPES +                                      // 5. target unit type
        Features.numFactions +                        // 6. target faction (RELATIVE: me=0)
        SBSLOTS +                                     // 7. target spellbook slot (faction's own library order)
        INTS +                                        // 8. numeric args (cost/amount)
        FLAGS +                                       // 9. menu flags
        1                                             // 10. bias
    }

    /** Encode one candidate `action`, seen by `me` in `game`, into a dim-length vector.
     *  `me` is the deciding faction (relative seat 0); faction args are placed relative
     *  to it, so the encoding is identical across seats. */
    def of(game : Game, me : Faction, action : Action) : Array[Double] = {
        val a = new Array[Double](dim)
        val unw = action.unwrap
        val (_, ridx) = Features.regionIndexOf(game)
        val nr = ridx.size
        val slots = Features.slotsOf(me)

        // Relative faction order (me first), for placing faction args by seat.
        val order : Array[Faction] = {
            val rest = game.setup.filter(_ != me).toArray
            (Array(me) ++ rest).take(Features.numFactions)
        }
        def relFaction(f : Faction) : Int = { val i = order.indexOf(f); if (i >= 0) i else -1 }

        var cur = 0
        // --- 1. action KIND one-hot -------------------------------------------------
        val kname = unw match { case p : Product => p.productPrefix; case _ => unw.getClass.getSimpleName }
        a(cur + kindSlot(kname)) = 1.0
        cur += KIND

        val regA = cur;             cur += nr
        val regB = cur;             cur += nr
        val uslot = cur;            cur += Features.unitSlotWidth
        val utype = cur;            cur += UTYPES
        val fac  = cur;             cur += Features.numFactions
        val sbase = cur;            cur += SBSLOTS
        val ibase = cur;            cur += INTS
        val fbase = cur;            cur += FLAGS

        // --- walk the action's fields (Product args) --------------------------------
        // Regions fill primary then secondary (from/to for a move); unit classes and
        // types mark their own-order slot; factions map to relative seat; spellbooks
        // to the deciding faction's library slot; the first ints become numeric slots.
        var regionsSeen = 0
        var intsSeen = 0
        val lib = me.library
        def sbSlot(sb : Spellbook) : Int = { val i = lib.indexOf(sb); if (i >= 0 && i < SBSLOTS) i else -1 }

        def markRegion(r : Region) : Unit = {
            val ri = ridx.getOrElse(r, -1)
            if (ri >= 0) {
                if (regionsSeen == 0)      a(regA + ri) = 1.0
                else if (regionsSeen == 1) a(regB + ri) = 1.0
                // 3rd+ region (rare) folds into the secondary block
                else                        a(regB + ri) = 1.0
            }
            regionsSeen += 1
        }
        def markUnitClass(uc : UnitClass) : Unit = {
            val s = slots.getOrElse(uc, -1)
            if (s >= 0 && s < Features.unitSlotWidth) a(uslot + s) = 1.0
            a(utype + utypeSlot(uc.utype)) = 1.0
        }
        def markFaction(f : Faction) : Unit = {
            val r = relFaction(f); if (r >= 0) a(fac + r) = 1.0
        }
        def markInt(n : Int) : Unit = {
            if (intsSeen < INTS) a(ibase + intsSeen) = n / 12.0
            intsSeen += 1
        }
        def markOne(v : Any) : Unit = v match {
            case r : Region    => markRegion(r)
            case uc : UnitClass => markUnitClass(uc)
            case ur : UnitRef  => markUnitClass(ur.uclass); markFaction(ur.faction)
            case f : Faction   => markFaction(f)
            case sb : Spellbook => val s = sbSlot(sb); if (s >= 0) a(sbase + s) = 1.0
            case ut : UnitType => a(utype + utypeSlot(ut)) = 1.0
            case n : Int       => markInt(n)
            case b : Boolean   => if (b && intsSeen < INTS) { a(ibase + intsSeen) = 1.0; intsSeen += 1 }
            case Some(x)       => markOne(x)          // unwrap |[Spellbook] etc. one level
            case _             => // nested actions / other: ignored (kept out of the noise)
        }

        unw match {
            case p : Product => p.productIterator.foreach(markOne)
            case _           =>
        }

        // --- 9. menu-navigation flags ----------------------------------------------
        if (action.isMore)   a(fbase + 0) = 1.0
        if (action.isCancel) a(fbase + 1) = 1.0

        // --- 10. bias ---------------------------------------------------------------
        a(dim - 1) = 1.0
        a
    }
}
