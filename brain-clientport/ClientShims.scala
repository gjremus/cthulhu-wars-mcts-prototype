package cws

// ─────────────────────────────────────────────────────────────────────────────
// BROWSER-SAFE SHIMS for the client-side (Scala.js) brain port — blocker #3.
//
// The move-time inference path (PolicyModel + MLPModel + MCTS + ModelEval +
// Features/ActionFeatures + EncodingLayout) is almost pure math and ports to
// Scala.js as-is. Only TWO spots use JVM-only plumbing, and BOTH are only there
// for JVM PARALLEL self-play — neither is needed in a single-threaded browser:
//
//   1. ActionFeatures.kindSlot: a ConcurrentHashMap[String,Integer] + AtomicInteger
//      assigning a stable slot to each action-kind name, shared across self-play
//      THREADS. In the browser there are no threads, so a plain mutable.Map + Int
//      is exactly equivalent (and faster). SEMANTICS ARE IDENTICAL: same lazy
//      assignment order, same KIND=64 overflow-hash into the last 4-slot band.
//
//   2. Evaluator.capEnv: reads encoding CAPACITIES from System.getenv. The browser
//      has no environment; the /brain/ build is base-4 ONLY, so these are just the
//      base-4 constants. No env, no getenv.
//
// This file is the SINGLE-THREADED replacement for both. It is staged in isolation
// (brain-clientport/, NOT mcts-src/) so the running champion and the live JVM
// builds are byte-identical and untouched. When the client port is assembled, the
// porter swaps these two members in place of the JVM versions; nothing else in the
// inference path changes.
// ─────────────────────────────────────────────────────────────────────────────

/** Browser-safe action-KIND vocabulary — a drop-in for ActionFeatures' concurrent
 *  version. Single-threaded, so a plain map is safe and behaviourally identical. */
object ClientKindVocab {
    val KIND = 64
    private val kindMap = scala.collection.mutable.HashMap[String, Int]()
    private var kindNext = 0

    /** Same contract as ActionFeatures.kindSlot: first sighting of `name` gets the
     *  next free slot; once the 64 slots fill, further names hash deterministically
     *  into the last 4-slot band (collide, never crash). */
    def kindSlot(name : String) : Int = kindMap.get(name) match {
        case Some(slot) => slot
        case None =>
            val n = kindNext
            if (n >= KIND) {
                val h = (math.abs(name.hashCode) % 4) + (KIND - 4)
                kindMap.getOrElseUpdate(name, h)
            } else {
                kindNext += 1
                kindMap.getOrElseUpdate(name, n)
            }
    }
}

/** Browser-safe encoding capacities — a drop-in for Evaluator.capEnv. The /brain/
 *  build is base-4 / Earth-3.5 ONLY, so these are the exact base-4 constants the
 *  JVM build defaults to when no CW_MAX_* env is set (dim stays 777/128, so the
 *  champion's on-disk weights load raw with no migration). */
object ClientCaps {
    val NumFactions  = 4    // seats reserved (base-4)
    val UnitSlots    = 6    // per-faction distinct unit classes (base-4 uses 5; 1 spare)
    val AbilitySlots = 4    // innate abilities per faction (base-4 max = 3; 1 spare)
    val MaxRegions   = 17   // Earth 4v3.5 = 17 regions
}
