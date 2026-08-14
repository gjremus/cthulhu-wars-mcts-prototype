package cws

import hrf.colmat._

import scala.collection.parallel.CollectionConverters._

object Overlays {
    def imageSource(s : String) = s
}

object CthulhuWarsSolo {
    val DottedLine = "............................................................................................................................................................................................................................................"
    val DoubleLine = "======================================================================================================================="
}

object Host {
    def writeLog(s : String) {
    }

    def askFaction(g : Game, c : Continue) : Action = {
        c match {
            case Force(action) =>
                action

            case Then(action) =>
                action

            case DelayedContinue(_, c) =>
                askFaction(g, c)

            case RollD6(question, roll) =>
                roll((1::2::3::4::5::6).shuffle.first)

            case RollAgony(question, roll) =>
                roll(AgonyDie.faces.shuffle.first)

            case RollBattle(_, n, roll) =>
                roll(1.to(n)./(_ => BattleRoll.roll()))

            case DrawES(_, 0, 0, 0, draw) =>
                draw(0, true)

            case DrawES(_, es1, es2, es3, draw) =>
                draw((es1.times(1) ++ es2.times(2) ++ es3.times(3)).maxBy(_ => random()), false)

            case MultiAsk(asks) =>
                askFaction(g, asks.head)

            case Ask(_, List(action)) =>
                action

            case Ask(faction, actions) =>
                faction match {
                    case GC => BotGC.ask(actions, 0.03)(g)
                    case CC => BotCC.ask(actions, 0.03)(g)
                    case BG => Bot3(BG).ask(actions, 0.03)(g)
                    case YS => BotYS.ask(actions, 0.03)(g)
                    case SL => BotSL.ask(actions, 0.03)(g)
                    case WW => BotWW.ask(actions, 0.03)(g)
                    case OW => BotOW.ask(actions, 0.03)(g)
                    case AN => BotAN.ask(actions, 0.03)(g)
                    // Tombstalker (TS): AI bot decision-making for headless simulation
                    case TS => BotTS.ask(actions, 0.03)(g)
                    // Firstborn (FB): AI bot decision-making for headless simulation
                    case FB => BotFB.ask(actions, 0.03)(g)
                    // Daemon Sultan (DS): AI bot decision-making for headless simulation
                    case DS => BotDS.ask(actions, 0.03)(g)
                }
        }
    }

    def main(args : Array[String]) {
        // Tombstalker (TS), Firstborn (FB), and Daemon Sultan (DS): included in the master faction list for headless simulation runs
        val allFactions = $(GC, CC, BG, YS, SL, WW, OW, AN, TS, FB, DS)

        val numberOfPlayers = 4

        var allComb : $[$[Faction]] = null
        var customComb : $[$[Faction]] = null
        var factions : $[Faction] = null

        if (numberOfPlayers == 3) {
            allComb = allFactions.combinations(3).$
            factions = $(YS, OW, WW)
        }
        else if (numberOfPlayers == 4) {
            allComb = allFactions.combinations(4).$
            customComb = allFactions.but(SL).combinations(4).$
            factions = $(YS, OW, WW, SL)
        }
        else {
            allComb = allFactions.combinations(5).$
            factions = $(YS, OW, WW, SL, AN)
        }

        //val repeat = 1.to(20).map(_ => factions)

        def allSeatings(factions : $[Faction]) = factions.permutations.$.%(s => s.contains(GC).?(s(0) == GC).|(s(0) != WW))
        def randomSeating(factions : $[Faction]) = allSeatings(factions).sortBy(s => random()).first

        var results : $[$[Faction]] = $

        // All faction combos containing FB (for FB bot testing)
        val base = allComb.filter(_.contains(FB)).shuffle

        1.to(10).foreach { i =>
            results ++= base.par.map { ff =>
                var log : $[String] = $
                def writeLog(s : String) {
                    log = s :: log
                }

                val game : Game =
                    if (numberOfPlayers == 3) {
                        new Game(EarthMap3, RitualTrack.for3, randomSeating(ff), true, Nil)
                    }
                    else
                    if (numberOfPlayers == 4) {
                        //game = new Game(EarthMap4v35, RitualTrack.for4, randomSeating(ff), true, $(Opener4P10Gates))
                        new Game(EarthMap4v35, RitualTrack.for4, randomSeating(ff), true, $(UseGhast))
                        //game = new Game(EarthMap4v53, RitualTrack.for4, randomSeating(ff), true, $(AltMap))
                    }
                    else {
                        new Game(EarthMap5, RitualTrack.for5, randomSeating(ff), true, $)
                    }

                var aa : $[Action] = $

                try {
                    val (l, cc) = game.perform(StartAction)
                    var c = cc
                    l.foreach(writeLog)
                    var n = 0
                    var k = 1L
                    var f = false
                    while (!c.isInstanceOf[GameOver]) {
                        n += 1
                        val a = askFaction(game, c)

                        aa +:= a

                        val (l, cc) = game.perform(a.unwrap)
                        c = cc
                        l.foreach(writeLog)

                        k *= (c match {
                            case Ask(_, actions) if actions.num > 1 => min(3, actions.num)
                            case _ => 1
                        })

                        if (game.turn > 1 && !f) {
                            f = true
                        }

                        if (n > 7000)
                            throw null
                    }
                    val w = c.asInstanceOf[GameOver].winners
                    println(w.any.?(w./(_.name).mkString(", ")).|("Humanity") + " won (" + n + ")")
                    // Tombstalker (TS): print end-of-game stats (doom, spellbooks, gates, units, Death's Head, win)
                    if (ff.contains(TS)) {
                        implicit val g = game
                        val ts = factionToState(TS)
                        println("  TS: doom=" + ts.doom + " sb=" + ts.spellbooks.num + " gates=" + ts.gates.num + " tombherds=" + ts.onMap(TombHerd).num + " deeptendrils=" + ts.onMap(DeepTendril).num + " glaaki=" + ts.has(Glaaki) + " dh=" + game.deathsHead + " won=" + w.contains(TS))
                    }
                    // Firstborn (FB): print end-of-game stats (doom, spellbooks, gates, units, craters, augury)
                    if (ff.contains(FB)) {
                        implicit val g = game
                        val fb = factionToState(FB)
                        println("  FB: doom=" + fb.doom + " sb=" + fb.spellbooks.num + " gates=" + fb.gates.num + " desiccated=" + fb.onMap(Desiccated).num + " revenants=" + fb.onMap(RevenantOfKnaa).num + " goo=" + fb.has(Ghatanothoa) + " craters=" + game.fbCraters.num + " augury=" + game.fbAuguryKills + " won=" + w.contains(FB))
                    }
                    w
                }
                catch {
                    case e : Throwable if !false =>
                        println(e)

                        import java.nio.file.{Paths, Files}
                        import java.nio.charset.StandardCharsets

                        val path = "."
                        val serializer = new Serialize(game)

                        Files.write(Paths.get(path + "/game-error-" + java.lang.System.currentTimeMillis + ".txt"), (
                            aa.reverse./(_.unwrap)./(serializer.write).mkString("\n") + "\n\n" +
                            (e.getMessage + "\n" + e.getStackTrace.mkString("\n")) + "\n\n" +
                            log.reverse.map("<div class='p'>" + _ + "</div>").mkString("\n")
                        ).getBytes(StandardCharsets.UTF_8))

                    Nil
                }
            }

            //val wins = results.groupBy(w => w).mapValues(_.size)
            val wins = results.groupBy(w => w).view.mapValues(_.size).toMap

            println()

            wins.keys.toList.sortBy(k => wins(k)).reverse.foreach { k =>
                println(k.any.?(k./(_.name).mkString(", ")).|("Humanity") + ": " + wins(k) + " " + "%6.0f".format(wins(k) * 100.0 / wins.values.sum) + "%")
            }

            println()

            allFactions.map { f =>
                // val ww = wins.filterKeys(_.contains(f))
                // val solo = ww.filterKeys(_.size == 1).values.sum
                // val tie = ww.filterKeys(_.size > 1).values.sum
                val ww = wins.view.filterKeys(_.contains(f))
                val solo = ww.view.filterKeys(_.size == 1).values.sum
                val tie = ww.view.filterKeys(_.size > 1).values.sum
                (solo + tie) -> (f.name + ": " + solo + "+" + tie + " " + "%6.0f".format((solo + tie) * 100.0 / wins.values.sum) + "%")
            }.sortBy(_._1).map(_._2).reverse.foreach(println)

            //println("Humanity" + ": " + wins.filterKeys(_.size == 0).values.sum + " " + "%6.0f".format(wins.filterKeys(_.size == 0).values.sum * 100.0 / wins.values.sum) + "%")
            println("Humanity" + ": " + wins.view.filterKeys(_.size == 0).values.sum + " " + "%6.0f".format(wins.view.filterKeys(_.size == 0).values.sum * 100.0 / wins.values.sum) + "%")
            println("Total: " + results.num)
            println()
        }

        import scala.jdk.CollectionConverters._

        println()
        println("RULES")
        val rules = Stats.rules.asScala.toList.sortBy(r => min(r._2._1, r._2._2))
        rules.foreach { case (k, (on, off)) => println(k + " : " + on + " / " + off) }

        println()
        println("IMPORTANT")
        val important = Stats.important.asScala.toList.sortBy(r => min(r._2._1, r._2._2))
        important.foreach { case (k, (on, off)) => println(k + " : " + on + " / " + off) }
    }
}
