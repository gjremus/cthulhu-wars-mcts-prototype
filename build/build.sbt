name := "CW MCTS Prototype"
version := "0.1"
scalaVersion := "2.13.16"

scalacOptions := Seq(
    "-language:postfixOps",
    "-language:implicitConversions",
    "-Wconf:msg=will become a keyword:s,msg=procedure syntax:s,msg=match may not be exhaustive:s,msg=unreachable code:s"
)

val soloDir   = file("/Users/gremus/cthulhu-wars-mcts-prototype/engine-copy/solo")
val simSrcDir = file("/Users/gremus/cthulhu-wars-mcts-prototype/engine-copy/sim/src")
val mctsDir   = file("/Users/gremus/cthulhu-wars-mcts-prototype/mcts-src")

Compile / unmanagedSourceDirectories := Seq(soloDir, simSrcDir, mctsDir)

val jsSpecific = Set(
    "ReflectJS.scala","StatsStub.scala","web.scala","loader.scala",
    "utils.canvas.scala","CthulhuWarsSolo.scala","overlay.scala","hrf.scala",
    "quine.scala","resources.scala","GlyphPlacement.scala"
)
Compile / unmanagedSources / excludeFilter := new SimpleFileFilter(f =>
    jsSpecific(f.getName) || f.getAbsolutePath.contains("/target/"))

libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.0.2"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.7.0"
libraryDependencies += "com.lihaoyi" %% "fansi" % "0.4.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
// reflection-based deep cloner (no Serializable needed) — clone-spike only
libraryDependencies += "io.github.kostaskougios" % "cloning" % "1.10.3"

Compile / sourceGenerators += Def.task {
    val f = (Compile / sourceManaged).value / "info.scala"
    IO.write(f, """package hrf { object BuildInfo { val name = "mcts" ; val version = "0.1" ; val time = 0L ; val seed = "prototypeseed0000" } }""")
    Seq(f)
}.taskValue

bspEnabled := false
maxErrors := 30
