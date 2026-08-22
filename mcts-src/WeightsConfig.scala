package cws

import java.nio.file.{Files, Paths}
import scala.util.{Try, Success, Failure}

/** Dynamic weight configuration loaded from JSON file.
 *  Allows dashboard UI edits to take effect at the START of each iteration. */
object WeightsConfig {

    // State potential weights (for Outcome.statePotential)
    var potDoom   = 0.32
    var potSB     = 0.33
    var potGates  = 0.13
    var potGOO    = 0.09
    var potRitual = 0.09
    var potPower  = 0.04

    // Shaping weights (for Shaping.scoreBreakdown) - placeholders for now
    var spellbooks = 8.0
    var doomEarned = 1.0
    var elderSigns = 1.66
    var ritualValue = 1.0
    var ownGOO = 3.0
    var goodRitual = 0.5
    var gateTaken = 0.4
    var gateDefended = 0.33
    var endAP1Gates = 1.0
    var preDoomPower = 1.0
    var unitsOnMap = 0.8
    var captureEnemy = 0.4
    var unitLoss = -0.1
    var lostGate = -0.6
    var abandon = -0.33
    var capture = -0.4
    var gooLost = -3.0
    var cultistUnprot = -0.2
    var killEnemy = 0.8
    var powerBlock = 0.05
    var apPowerOrder = 0.8
    var enemyGooKill = 2.0
    var placementBonus = 0.1

    /** Load weights from JSON file. Called at start of each iteration.
     *  Falls back to hardcoded defaults if file missing or malformed. */
    def loadFromFile(): Unit = {
        val weightsPath = Paths.get("/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/weights.json")

        if (!Files.exists(weightsPath)) {
            println("  weights: file not found, using hardcoded defaults")
            return
        }

        Try {
            val json = new String(Files.readAllBytes(weightsPath), "UTF-8")
            parseWeights(json)
        } match {
            case Success(_) =>
                println(f"  weights: loaded from weights.json")
            case Failure(e) =>
                println(f"  weights: parse error (${e.getMessage}), using defaults")
        }
    }

    private def parseWeights(json: String): Unit = {
        // Simple JSON parser for flat key-value pairs
        // Format: {"spellbooks": 8, "doomEarned": 1, ...}

        val pairs = json
            .replace("{", "")
            .replace("}", "")
            .split(",")
            .map(_.trim)
            .filter(_.nonEmpty)

        for (pair <- pairs) {
            val parts = pair.split(":")
            if (parts.length == 2) {
                val key = parts(0).trim.stripPrefix("\"").stripSuffix("\"")
                val value = parts(1).trim.toDouble

                key match {
                    // State potential weights
                    case "potDoom" => potDoom = value
                    case "potSB" => potSB = value
                    case "potGates" => potGates = value
                    case "potGOO" => potGOO = value
                    case "potRitual" => potRitual = value
                    case "potPower" => potPower = value

                    // Shaping weights
                    case "spellbooks" => spellbooks = value
                    case "doomEarned" => doomEarned = value
                    case "elderSigns" => elderSigns = value
                    case "ritualValue" => ritualValue = value
                    case "ownGOO" => ownGOO = value
                    case "b:goodRitual" => goodRitual = value
                    case "gateTaken" => gateTaken = value
                    case "gateDefended" => gateDefended = value
                    case "endAP1Gates" => endAP1Gates = value
                    case "preDoomPower" => preDoomPower = value
                    case "unitsOnMap" => unitsOnMap = value
                    case "b:captureEnemy" => captureEnemy = value
                    case "p:unitLoss" => unitLoss = value
                    case "p:lostGate" => lostGate = value
                    case "p:abandon" => abandon = value
                    case "p:capture" => capture = value
                    case "p:gooLost" => gooLost = value
                    case "p:cultistUnprot" => cultistUnprot = value
                    case "r:killEnemy" => killEnemy = value
                    case "r:powerBlock" => powerBlock = value
                    case "r:apPowerOrder" => apPowerOrder = value
                    case "r:enemyGooKill" => enemyGooKill = value
                    case "placementBonus" => placementBonus = value
                    case _ => // ignore unknown keys
                }
            }
        }
    }

    /** Print current weight values for iteration log. */
    def summary(): String = {
        f"weights: pot[doom=${potDoom}%.2f SB=${potSB}%.2f gates=${potGates}%.2f] " +
        f"shaping[SB=${spellbooks}%.1f doom=${doomEarned}%.1f ES=${elderSigns}%.2f]"
    }
}
