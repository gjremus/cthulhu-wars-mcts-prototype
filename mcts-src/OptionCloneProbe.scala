package cws

import hrf.colmat._
import com.rits.cloning.Cloner

// Does the reflection cloner corrupt Scala's Option (None singleton / Some)?
// The crash stack was None.toList -> None.get, which only happens if a None's
// isEmpty wrongly returns false — i.e. a mangled Option. Test in isolation
// BEFORE blaming the engine.

object OptionCloneProbe {
    class LambdaAwareCloner extends Cloner {
        private val noneClass = None.getClass
        override def considerImmutable(clz : Class[_]) : Boolean =
            (clz eq noneClass) || clz.getName.contains("$$Lambda") || super.considerImmutable(clz)
    }
    val cloner = new LambdaAwareCloner()

    def main(args : Array[String]) : Unit = {
        println("=== Option clone probe ===")

        val n : Option[String] = None
        val cn = cloner.deepClone(n)
        println(f"None cloned: class=${cn.getClass.getName}")
        println(f"  cloned eq None:      ${cn eq None}   (want true)")
        println(f"  cloned.isEmpty:      ${cn.isEmpty}   (want true)")
        println(f"  cloned.toList:       ${scala.util.Try(cn.toList).toString}   (want Success(List()))")

        val s : Option[String] = Some("hi")
        val cs = cloner.deepClone(s)
        println(f"\nSome cloned: class=${cs.getClass.getName}")
        println(f"  cloned == Some(hi): ${cs == Some("hi")}   (want true)")
        println(f"  cloned.isEmpty:     ${cs.isEmpty}   (want false)")
        println(f"  cloned.get:         ${scala.util.Try(cs.get).toString}")

        // Nested: List of Options, Option of List — the real graph shapes.
        val lo : List[Option[Int]] = List(None, Some(1), None, Some(2))
        val clo = cloner.deepClone(lo)
        println(f"\nList[Option] cloned: $clo")
        println(f"  toList-safe on each: ${scala.util.Try(clo.map(_.toList)).toString}")
    }
}
