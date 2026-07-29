package cws

import hrf.colmat._
import com.rits.cloning.{Cloner, IFastCloner, IDeepCloner}
import java.util.{Map => JMap}

// Option A — the canonical fast game-state cloner, extracted from the proven
// CloneSpike diagnostic. This is the fork primitive MCTS uses thousands of times
// per decision: copy a (Game, Continue) position so a rollout can mutate the copy
// while the real game is untouched.
//
// Every design choice here was proven in CloneSpike (faithful + isolated + flat
// with depth): custom Scala List/Map fast-cloners (rebuild fresh cons cells,
// deep-clone elements), share immutable domain flyweights by reference, and treat
// captured lambdas + Scala's None singleton as immutable (share by reference —
// copying None breaks Option.isEmpty which is defined as `this eq None`).

object Cloning {

    class ScalaListFastCloner extends IFastCloner {
        def clone(t : Object, dc : IDeepCloner, cache : JMap[Object, Object]) : Object = {
            val l = t.asInstanceOf[List[Any]]
            l.map(e => dc.deepClone(e.asInstanceOf[Object], cache))
        }
    }
    class ScalaMapFastCloner extends IFastCloner {
        def clone(t : Object, dc : IDeepCloner, cache : JMap[Object, Object]) : Object = {
            val m = t.asInstanceOf[Map[Any, Any]]
            m.map { case (k, v) =>
                dc.deepClone(k.asInstanceOf[Object], cache) ->
                dc.deepClone(v.asInstanceOf[Object], cache)
            }
        }
    }

    // Treat as immutable (share by reference, never copy):
    //   (a) captured lambdas — JDK hidden classes with final captured fields the
    //       reflective walk cannot write; proven to capture only immutable values;
    //   (b) EVERY Scala singleton `object` / case-object — detected by the static
    //       MODULE$ field the compiler emits for every module. These are
    //       process-global by definition and the engine dispatches on them by
    //       IDENTITY: `case DoomPhaseAction =>`, `x eq None`, `players(DS)`. Copying
    //       one produces an object that is `!eq` the singleton, so every identity
    //       match against it silently fails (MatchError on DoomPhaseAction; None.get
    //       after a cloned None). Sharing them is both correct and faster. This one
    //       rule subsumes the earlier hand-listed None special-case and covers all
    //       the case-object Actions/Factions/Regions/etc. at once.
    //
    // Cached per-class so the reflective MODULE$ probe runs once per class, not per
    // object encountered.
    class GameCloner extends Cloner {
        private val moduleCache = new java.util.concurrent.ConcurrentHashMap[Class[_], java.lang.Boolean]()
        private def isScalaModule(clz : Class[_]) : Boolean = {
            val cached = moduleCache.get(clz)
            if (cached != null) return cached.booleanValue
            val res =
                try { val f = clz.getField("MODULE$"); (f.getModifiers & java.lang.reflect.Modifier.STATIC) != 0 }
                catch { case _ : NoSuchFieldException => false }
            moduleCache.put(clz, java.lang.Boolean.valueOf(res))
            res
        }
        override def considerImmutable(clz : Class[_]) : Boolean =
            clz.getName.contains("$$Lambda") ||
            isScalaModule(clz) ||
            super.considerImmutable(clz)
    }

    val cloner : Cloner = {
        val c = new GameCloner()
        c.registerFastCloner(classOf[scala.collection.immutable.$colon$colon[_]], new ScalaListFastCloner)
        c.registerFastCloner(scala.collection.immutable.Nil.getClass, new ScalaListFastCloner)
        c.registerFastCloner(classOf[scala.collection.immutable.Map[_, _]], new ScalaMapFastCloner)
        // Immutable domain flyweights the engine dispatches on by identity — share.
        // NOT cws.Expansion (Game extends Expansion; sharing it would never copy Game).
        c.dontCloneInstanceOf(
            classOf[cws.Faction], classOf[cws.Region], classOf[cws.Spellbook],
            classOf[cws.UnitClass], classOf[cws.GameOption], classOf[cws.Board])
        c
    }

    /** Deep-copy a position: (Game, Continue) together so the copied Continue's
     *  actions/closures point at the COPIED game, not the original. */
    def copy(g : Game, c : Continue) : (Game, Continue) = {
        val p = cloner.deepClone((g, c))
        (p._1, p._2)
    }
}
