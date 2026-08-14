package cws

import scala.scalajs._
import scala.scalajs.js.annotation._

import org.scalajs.dom
import org.scalajs.dom.html

import hrf.colmat._

import html._

import util.canvas._

class InfoOverlay(overlay : html.Element) {
    var showing : |[String] = None
    var soonHide : Int = 0
    var soonShow : Int = 0
    var toShow : |[String] = None

    overlay.parentElement.parentElement.style.display = "none"

    overlay.parentElement.parentElement.addEventListener("click", (_ : dom.Event) => hide(), true)

    private var list : $[dom.HTMLTableElement] = $
    private var old : |[dom.HTMLTableElement] = None
    private var children : $[html.Div] = $
    private var prev : $[html.Div] = $

    def show(tableHtml : String) {
        if (showing.has(tableHtml))
            return

        prev = children
        children = $

        showing = |(tableHtml)

        overlay.parentElement.parentElement.style.display = ""

        list = 20.to(100).reverse.%(_ % 2 == 0).$./ { z =>
            val p = dom.document.createElement("div").asInstanceOf[html.Div]
            p.style.position = "absolute"
            p.style.fontSize = "" + z + "%"
            p.style.opacity = "0"
            p.style.height = "100%"
            p.style.overflowY = "hidden"
            p.innerHTML = tableHtml

            overlay.appendChild(p)
            children :+= p

            p.children(0).as[dom.HTMLTableElement].get
        }

        old = None
    }

    def hide() {
        showing = None

        overlay.parentElement.parentElement.style.display = "none"

        while (overlay.hasChildNodes())
            overlay.removeChild(overlay.lastChild)

        list = $
        prev = $
        children = $
    }

    def removePrev() {
        prev.foreach { c => overlay.removeChild(c) }
        prev = $
    }

    private def readjust() {
        if (soonHide == 1)
            hide()

        if (soonHide > 1)
            soonHide -= 1

        if (soonShow == 3 && toShow.any) {
            show(toShow.get)
            toShow = None
        }

        if (soonShow == 1) {
            removePrev()
        }

        if (list.any && soonHide == 0) {
            val target = overlay.clientHeight

            val best = list.%(_.clientHeight <= target).starting.|(list.minBy(_.clientHeight))

            if (old.has(best).not || soonShow == 1) {
                old = |(best)

                list.foreach { p =>
                    if (p == best && soonShow <= 1) {
                        p.parentElement.style.opacity = "1"
                        p.parentElement.style.pointerEvents = ""
                        p.parentElement.style.overflowY = "auto"
                        p.parentElement.style.height = (p.clientHeight <= target).??("100%")
                    }
                    else {
                        p.parentElement.style.opacity = "0"
                        p.parentElement.style.pointerEvents = "none"
                        p.parentElement.style.overflowY = "hidden"
                        p.parentElement.style.height = "100%"
                    }
                }
            }
        }

        if (soonShow > 1)
            soonShow -= 1

        dom.window.requestAnimationFrame { _ =>
            readjust()
        }
    }

    readjust()
}

object RitualTrackOverlay {
    var numPlayers : Int = 4
    var markerIndex : Int = 0
    var trackLength : Int = 9
    var ritualHistory : $[String] = $          // faction style strings, in order
    var ritualHistoryCeremony : $[Boolean] = $ // true when that ritual was a Ceremony of Annihilation
    // Tombstalker (TS): indices on the ritual track where pure Death's Head hecatomb rituals occurred (no marker advance)
    var tsPureDHMarkerIndices : $[Int] = $

    // Circle centers as (xPct, yPct) of image dimensions, for each track index
    // Positions derived from pixel clustering of track images (threshold=180).
    // Last entry = Instant Death position.
    // Positions derived from brightness-peak analysis of track images.
    // Upper-row y shifted ~9% below number label so numbers show above glyphs (same as 3p fix).
    // Lower-row y set to orb-body center from analysis (~65%).
    // Last entry = Instant Death ellipse center (x=center of ID text, y=40% to cover text).
    // 3p: top row x equal-spacing (confirmed correct); bottom row x = original near-values +2% right shift.
    // ID: x=90% so right edge touches track edge; y=43% (lowered from 40%).
    val positions3p : $[(Double, Double)] = $(
        (33.8, 47.0), (41.0, 65.0), (48.7, 47.0),
        (56.0, 65.0), (63.6, 47.0), (71.0, 65.0), (90.0, 51.0)
    )
    // 4p: top row x shifted right +2% from first, spacing expanded ~1.5% per gap; y top=53% (confirmed).
    // Bottom row y raised to 76% (was 68%: user confirmed too high by ~1/5-1/6 glyph height).
    // ID x=89% so right edge near track edge.
    val positions4p : $[(Double, Double)] = $(
        (30.5, 53.0), (37.4, 76.0), (43.6, 53.0), (49.3, 76.0),
        (55.8, 53.0), (62.0, 76.0), (69.9, 53.0), (77.7, 76.0), (89.0, 49.0)
    )
    // 5p: circle 1 x shifted right +3% (was at number-label position); circle 1 y raised to top-row level.
    // Bottom row y=72% (was 68%, still a little too high per user).
    // ID x=90% so right edge touches track edge.
    // 5p: strict alternating pattern. Top row (odd glyphs) y=53%, bottom row (even) y=72%.
    // Circle 1 x confirmed correct. Circles 2-10 shifted right +3.2% to give uniform ~6% gap from circle 1.
    val positions5p : $[(Double, Double)] = $(
        (28.0, 53.0), (34.1, 72.0), (40.6, 53.0), (46.6, 72.0),
        (51.9, 53.0), (57.7, 72.0), (63.5, 53.0), (69.9, 72.0),
        (76.1, 53.0), (82.6, 72.0), (90.0, 40.0)
    )

    def positions = numPlayers match {
        case 3 => positions3p
        case 5 => positions5p
        case _ => positions4p
    }

    def trackImageId = "ritual-track-" + numPlayers + "p"
}

// Tombstalker (TS) Cursed Tomes overlay: tracks tome ownership per faction and tome text descriptions (I-XI)
object TSCursedTomesOverlay {
    // Map from faction style to list of (tomeNum, isFaceDown)
    var factionTomes : scala.collection.immutable.Map[String, $[(Int, Boolean)]] = scala.collection.immutable.Map()
    var tomesOnCard : Int = 0

    val tomeTexts : scala.collection.immutable.Map[Int, String] = scala.collection.immutable.Map(
        1  -> "Gain 1 Power. Tombstalker places a Tomb-Herd at their Controlled Gate if able.",
        2  -> "Gain 1 Power. Tombstalker places a Tomb-Herd at their Controlled Gate if able.",
        3  -> "Gain 1 Power. Tombstalker places a Deep Tendril at their Controlled Gate if able.",
        4  -> "Gain 1 Power. Tombstalker places a Deep Tendril at their Controlled Gate if able.",
        5  -> "Gain 1 Power. Tombstalker gains 1 Doom.",
        6  -> "Gain 1 Power. Tombstalker gains 1 Doom.",
        7  -> "Gain 1 Power. Tombstalker gains Death's Head equal to the number of Tomb-Herd in play.",
        8  -> "Gain 1 Power. Tombstalker gains Death's Head equal to the number of Tomb-Herd in play.",
        9  -> "Gain 1 Power. Tombstalker gains an Elder Sign.",
        10 -> "Gain 1 Power. Tombstalker gains an Elder Sign.",
        11 -> "Gain 1 Power. Tombstalker gains Doom equal to the Ritual Track marker minus 5."
    )
}

object Overlays {
    val overlay = new InfoOverlay(dom.document.getElementById("overlay").asInstanceOf[html.Element])
    var temp = false

    // [2026-04-03] Force-refresh the overlay if it's currently showing
    def refreshIfShowing() {
        if (overlay.showing.any) {
            // Regenerate content by re-clicking with the same args
            // Clear showing so show() doesn't early-return
            val wasShowing = overlay.showing
            overlay.showing = None
            // Re-show with current data
            val text = info($("RoA"))
            if (text.any) {
                overlay.show(text.get)
            }
        }
    }

    @JSExportTopLevel("onExternalClick")
    def onExternalClick(s : Any*) {
        temp = false

        val text = info(s.$)

        overlay.soonHide = 0
        overlay.soonShow = 0

        if (text.none)
            overlay.hide()
        else {
            overlay.toShow = text
            overlay.soonShow = 3
            overlay.soonHide = 0
        }
    }

    @JSExportTopLevel("onExternalOver")
    def onExternalOver(s : Any*) {
        // println("onExternalOver " + s)

        if (overlay.showing.none || overlay.soonHide > 0) {
            overlay.toShow = info(s.$)
            if (overlay.toShow.any) {
                overlay.soonShow = 30
                overlay.soonHide = 0
                temp = true
            }
        }
    }

    @JSExportTopLevel("onExternalOut")
    def onExternalOut(s : Any*) {
        // println("onExternalOut " + s)

        if (overlay.showing.any || overlay.soonShow > 0) {
            if (temp) {
                overlay.soonHide = 20
                overlay.soonShow = 0
                temp = false
            }
        }
    }

    def imageSource(id : String) = hrf.web.getElem(id).as[dom.html.Image]./(_.src).|!("unknown image source " + id)

    implicit class ElementString(val s : String) extends AnyVal {
        def & = "<span style=inline-block>" + s + "</span>"
    }

    def info(s : $[Any]) : |[String] = (s @@ {
        case $("GC") => faction(GC, "info:gc-background", Immortal, "Ongoing", "Once Cthulhu has Awakened, he costs only 4 Power each subsequent time he is Awakened. Whenever you Awaken any Great Old One, gain <span class=es>1 Elder Sign.</span>",
            $(), $(
            (Acolyte,   6, "1", "0", s"""<div class=p>Spellbook: ${reference(GC, Dreams)}</div>"""),
            (DeepOne,   4, "1", "1", s"""<div class=p>Spellbook: ${reference(GC, Devolve)}</div>"""),
            (Shoggoth,  2, "2", "2", s"""<div class=p>Spellbook: ${reference(GC, Absorb)}</div>"""),
            (Starspawn, 2, "3", "3", s"""<div class=p>Spellbook: ${reference(GC, Regenerate)}</div>"""),
            (Cthulhu,   1, "10/4", "6", s"""
                <div class=p>${cost(s"How to Awaken ${Cthulhu.name}:")}</div>
                <div class=p>${cost("1)")} There must be a Gate in Great Cthulhu's starting Area (Can be abandoned or enemy-Controlled).</div>
                <div class=p>${cost("2)")} If this is the first Awakening, pay <span class=cost-color>10 Power</span>. Otherwise pay <span class=cost-color>4 Power</span>.</div>
                <div class=p>${cost("3)")} ${Cthulhu.name} appears in its starting Area (Remember to gain <span class=es>1 Elder Sign</span>).</div>
                <div class=p>${ref(Devour)} ${cost("(Pre-Battle):")} The enemy player chooses and Eliminates one of his Monsters or Cultists in the Battle.</div>
                <div class=p>Spellbooks: ${reference(GC, Submerge)}, ${reference(GC, YhaNthlei)}</div>"""
            ),
        ))

        case $("GC", FirstDoomPhase.text) => requirement("Receive this Spellbook in the first Doom Phase.<br/>Receive <span class=es>1 Elder Sign.</span>")
        case $("GC", KillDevour1.text) => requirement("Kill and/or Devour an enemy Unit in a Battle.<br/><br/>* You may earn both Spellbooks in a single Battle, if you Kill and/or Devour 3 or more Units.")
        case $("GC", KillDevour2.text) => requirement("Kill and/or Devour 2 enemy Units in a Battle.<br/><br/>* You may earn both Spellbooks in a single Battle, if you Kill and/or Devour 3 or more Units.")
        case $("GC", AwakenCthulhu.text) => requirement("Awaken Cthulhu.")
        case $("GC", OceanGates.text) => requirement("Control 3 Gates in Ocean/sea Areas<br/>OR<br/>4 Gates exist in Ocean/sea Areas.")
        case $("GC", FiveSpellbooks.text) => requirement("This must be the last Faction Spellbook you receive.<br/>It must be taken during the Doom Phase.<br/>Receive <span class=es>1 Elder Sign.</span>")

        case $("GC", Dreams.name) => spellbook(Dreams.name, "Action: Cost 2", "Choose an Area containing an enemy’s Acolyte Cultist. Your enemy must Eliminate one of his Acolyte Cultists from that Area and replace it with one from your Pool.")
        case $("GC", Devolve.name) => spellbook(Devolve.name, "Ongoing", "After any player's Action, replace one or more of your Acolyte Cultists anywhere on the Map with Deep Ones from your Pool.")
        case $("GC", Absorb.name) => spellbook(Absorb.name, "Pre-Battle", "If a Shoggoth is present, Eliminate one or more of your Monsters or Cultists in the Battle. For each Unit so removed, add 3 dice to the Shoggoth's Combat for that Battle.")
        case $("GC", Regenerate.name) => spellbook(Regenerate.name, "Post-Battle", "Assign up to 2 Kill or Pain Battle results to the same Starspawn. If 2 Kills are applied, the Starspawn is Killed. On any other combination of Kill or Pain results, the Starspawn is only Pained.")
        case $("GC", Submerge.name) => spellbook(Submerge.name, "Action: Cost 1", "If Cthulhu is in an ocean or sea Area, remove him from the Map and place him on your Faction Card, along with any or all of your Units in the Area. Later, as a 0-cost Action, you may place Cthulhu, plus all accompanying Units, into any Area.")
        case $("GC", YhaNthlei.name) => spellbook(YhaNthlei.name, "Gather Power Phase", "During Gather Power, if Cthulhu is in play, gain 1 Power for each enemy-controlled Gate in an ocean or sea Area.")


        case $("CC") => faction(CC, "info:cc-background", Flight, "Ongoing", "All your units can fly (even Cultists). When moved, they can travel 2 Areas. They can fly over Areas containing enemy Units.",
            $(Madness), $(
            (Acolyte,       6, "1", "0", ""),
            (Nightgaunt,    3, "1", "0", s"""<div class=p>Spellbook: ${reference(CC, Abduct)}</div>"""),
            (FlyingPolyp,   3, "2", "1", s"""<div class=p>Spellbook: ${reference(CC, Invisibility)}</div>"""),
            (HuntingHorror, 2, "3", "2", s"""<div class=p>Spellbook: ${reference(CC, SeekAndDestroy)}</div>"""),
            (Nyarlathotep,  1, "10", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Nyarlathotep.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Controlled Gate.</div>
                <div class=p>${cost("2)")} Pay ${power(10)}. Nyarlathotep appears at the controlled Gate.</div>
                <div class=p>${combat} Equals the total of your own Faction Spellbooks plus the Faction Spellbooks of your opponent in the Battle.</div>
                <div class=p>${ref(Harbinger)} ${cost("(Post-Battle):")} If Nyarlathotep is in a Battle in which one or more enemy Great Old Ones are Pained or Killed, you receive Power equal to half the cost to Awaken those Great Old Ones. Per enemy Great Old One, you may choose to receive 2 Elder Signs instead of Power. Harbringer takes effect even if Nyarlathotep is Killed or Pained in the Battle himself.</div>
                <div class=p>Spellbooks: ${reference(CC, Emissary)}, ${reference(CC, ThousandForms)}</div>"""
            ),
        ))

        case $("CC", Pay4Power.text) => requirement("As your Action, pay 4 Power.*<br/><br/>* You may earn both these Spellbooks in a single Round by paying 10 Power.")
        case $("CC", Pay6Power.text) => requirement("As your Action, pay 6 Power.*<br/><br/>* You may earn both these Spellbooks in a single Round by paying 10 Power.")
        case $("CC", Gates3Power12.text) => requirement("Control three Gates OR have 12 Power.")
        case $("CC", Gates4Power15.text) => requirement("Control four Gates OR have 15 Power.")
        case $("CC", CaptureCultist.text) => requirement("Capture an enemy Cultist.")
        case $("CC", AwakenNyarlathotep.text) => requirement("Awaken Nyarlathotep.")

        case $("CC", Abduct.name) => spellbook(Abduct.name, "Pre-Battle", "Eliminate one or more Nightgaunts from the Battle. For each one Eliminated, your enemy must Eliminate one of his own Monsters or Cultists from the Battle.")
        case $("CC", Invisibility.name) => spellbook(Invisibility.name, "Pre-Battle", "Select one Monster or Cultist (from either Faction) for each Flying Polyp present and \"exempt\" it. The selected Unit does not participate in the rest of the Battle.")
        case $("CC", SeekAndDestroy.name) => spellbook(SeekAndDestroy.name, "Pre-Battle", "Immediately move any or all Hunting Horrors from any Area to the Battle Area.")
        case $("CC", Emissary.name) => spellbook(Emissary.name, "Post-Battle", "Unless an enemy Great Old One is involved in the Battle, a Kill applied to Nyarlathotep becomes a Pain. If Nyarlathotep cannot be Pained due to being surrounded, he is not Eliminated.")
        case $("CC", ThousandForms.name) => spellbook(ThousandForms.name, "Action: Cost 0", s"If Nyarlathotep is in play, roll 1d6. Your foes lose that much Power between them; they have 1 minute to decide how much each loses. If they cannot agree, you get the rolled number as Power added to your total. This spellbook cannot be used again this Action Phase.")
        case $("CC", Madness.name) => spellbook(Madness.name, "Post-Battle", "After all Pain results have been assigned, you, rather than the Units' owners, choose the Area(s) to which all Pained Units will go. You may apply these results in any order (rather than the normal 'attacker first, then defender'), but you must still follow all other rules. Do this even for Battles in which you did not participate.")


        case $("BG") => faction(BG, "info:bg-background", Fertility, "Ongoing", "You may Summon Monsters as an Unlimited Action.",
            $(BloodSacrifice), $(
            (Acolyte,       6, "1", "0/1", s"""<div class=p>${combat} 1 with ${reference(BG, Frenzy)}</div>"""),
            (Ghoul,         2, "1/0", "0", s"""
                <div class=p>${cost("Cost:")} 0 with ${reference(BG, ThousandYoung)}.</div>
                <div class=p>Spellbook: ${reference(BG, Necrophagy)}</div>"""
            ),
            (Fungi,         4, "2/1", "1", s"""
                <div class=p>${cost("Cost:")} 1 with ${reference(BG, ThousandYoung)}.</div>
                <div class=p>Spellbook: ${reference(BG, Ghroth)}</div>"""
            ),
            (DarkYoung,     3, "3/2", "2", s"""
                <div class=p>${cost("Cost:")} 2 with ${reference(BG, ThousandYoung)}.</div>
                <div class=p>Spellbook: ${reference(BG, RedSign)}</div>"""
            ),
            (ShubNiggurath, 1, "8", "?", s"""
                <div class=p>${cost(s"How to Awaken ${ShubNiggurath.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Controlled Gate, and at least 2 Cultists on the Map &mdash; they can be in any Area(s).</div>
                <div class=p>${cost("2)")} Pay ${power(8)}.</div>
                <div class=p>${cost("3)")} Remove your 2 Cultists, then place Shub-Niggurath at your Controlled Gate.</div>
                <div class=p>${combat} Equals to the sum of your Controlled Gates and in-play Cultists. If you have The Red Sign, add another +1 for each Dark Young you have in play.</div>
                <div class=p>${ref(Avatar)} ${cost("(Action: Cost 1):")} Choose an Area and a Faction. Swap the location of Shub-Niggurath with that of a Monster or Cultist in the chosen Area. The owner of the chosen Faction chooses which Unit to relocate.</div>"""
            ),
        ))

        case $("BG", Spread4.text) => requirement("Have units in 4 Areas.")
        case $("BG", Spread6.text) => requirement("Have units in 6 Areas.")
        case $("BG", Spread8.text) => requirement("Have units in 8 Areas.")
        case $("BG", SpreadSocial.text) => requirement("Share Areas with all enemies. Gain 1 Power per enemy player.")
        case $("BG", EliminateTwoCultists.text) => requirement("As your Action, for a Round, Eliminate 2 of your Cultists from any Area(s) on the Map.")
        case $("BG", AwakenShubNiggurath.text) => requirement("Awaken Shub-Niggurath.")

        case $("BG", ThousandYoung.name) => spellbook(ThousandYoung.name, "Ongoing", "If Shub-Niggurath is in play, Ghouls, Fungi, and Dark Young cost 1 fewer Power each to Summon.")
        case $("BG", Frenzy.name) => spellbook(Frenzy.name, "Battle", "Your Cultists now have 1 Combat.")
        case $("BG", Necrophagy.name) => spellbook(Necrophagy.name, "Post-Battle", "Move any or all Ghouls (who did not partecipate in the Battle) from any Area to the Battle Area, even if your Faction was not involved in the Battle. For each Ghoul so moved, both sides in the Battle suffer an additional Pain result.")
        case $("BG", Ghroth.name) => spellbook(Ghroth.name, "Action: Cost 2", "Roll a die. If the roll is less than or equal to the number of Areas containing Fungi, your enemies must collectively Eliminate Cultists equal to the die roll. They have 1 minute to decide how to distribute these Eliminations. If time runs out, you choose for them. If the roll is greater than the number of Areas with Fungi, place 1 Acolyte from any Faction's pool anywhere on the map.")
        case $("BG", RedSign.name) => spellbook(RedSign.name, "Ongoing", "Dark Young can Create and Control Gates. Each Dark Young adds 1 to Shub-Niggurath's Combat and each provides 1 Power during the Gather Power Phase. They do not act as Cultists with respect to any other purpose.")
        case $("BG", BloodSacrifice.name) => spellbook(BloodSacrifice.name, "Doom Phase", "If Shub-Niggurath is in play during the Doom Phase, you can choose to Eliminate one of your Cultists (from anywhere on the map). If you do, gain <span class=es>1 Elder Sign.</span>")


        case $("YS") => faction(YS, "info:ys-background", Feast, "Gather Power Phase", "During Gather Power, you gain 1 Power for each Area containing both a Desecration Token and one or more of your units.",
            $(), $(
            (Acolyte,   6, "1",  "0", s"""<div class=p>Spellbook: ${reference(YS, Passion)}</div>"""),
            (Undead,    6, "1", "1-", s"""
                <div class=p>${combat} Roll 1 die less than the total Undead in the area.</div>
                <div class=p>Spellbook: ${reference(YS, Zingaya)}</div>"""),
            (Byakhee,   4, "2", "1+", s"""
                <div class=p>${combat} Roll 1 die more than the total Byakhee in the area.</div>
                <div class=p>Spellbook: ${reference(YS, Shriek)}</div>"""),
            (KingInYellow, 1, "4", "0", s"""
                <div class=p>${cost(s"How to Awaken the ${KingInYellow.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Unit in an Area lacking a Gate.</div>
                <div class=p>${cost("2)")} Pay ${power(4)}. ${KingInYellow.name} appears in that Area.</div>
                <div class=p>${ref(Desecrate)} ${cost("(Action: Cost 2):")} If the King is in an Area with no Desecration Token, roll 1 die and compare to your total units in the Area (including the King). On a roll equal or less than your unit total, place a Desecration Token in the Area. If you succeed or fail, place a Monster or Cultist with a cost of 2 or less in the Area.</div>
                <div class=p>Spellbook: ${reference(YS, ScreamingDead)}</div>"""
            ),
            (Hastur, 1, "10", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Hastur.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Controlled Gate and the King in Yellow in the same area.</div>
                <div class=p>${cost("2)")} Pay ${power(10)}. ${Hastur.name} appears in the King's Area.</div>
                <div class=p>${combat} Equals the current Cost of a Ritual of Annihilation.</div>
                <div class=p>${ref(Vengeance)} ${cost("(Post-Battle):")} If Hastur is involved in a Battle, choose which Combat results are applied to which enemy Unit.</div>
                <div class=p>Spellbooks: ${reference(YS, ThirdEye)}, ${reference(YS, HWINTBN)}</div>"""
            ),
        ))

        case $("YS", Provide3Doom.text) => requirement("As your Action for a round, select another player.<br/>That player gains three Doom points.")
        case $("YS", AwakenKing.text) => requirement("Awaken the King in Yellow.")
        case $("YS", DesecrateAA.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-aa")} class=inline-glyph />")
        case $("YS", DesecrateOO.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-oo")} class=inline-glyph />")
        case $("YS", DesecrateWW.text) => requirement(s"Place a Desecration Token in an Area marked<br/>with the Glyph: <img src=${imageSource("sign-ww")} class=inline-glyph />")
        case $("YS", AwakenHastur.text) => requirement("Awaken Hastur. Also receive <span class=es>1 Elder Sign</span>.")

        case $("YS", Passion.name) => spellbook(Passion.name, "Ongoing", "When one or more of your Cultists are Eliminated by an enemy (Killed, Captured, etc.), gain 1 Power total.")
        case $("YS", Zingaya.name) => spellbook(Zingaya.name, "Action: Cost 1", "If Undead are in an Area with enemy Acolyte Cultists, your enemy must Eliminate an Acolyte Cultist. Then, place an Undead in the Area.")
        case $("YS", Shriek.name) => spellbook(Shriek.name, "Action: Cost 1", "Move any or all Byakhee from their current Area(s) to any one Area on the Map.")
        case $("YS", ScreamingDead.name) => spellbook(ScreamingDead.name, "Action: Cost 1", "Move the King in Yellow to an adjacent Area. Any Undead in the same Area can move with him for free. You may then take a second, different Action. You may NOT take He Who is Not to be Named as your second Action.")
        case $("YS", ThirdEye.name) => spellbook(ThirdEye.name, "Ongoing", "If Hastur is in play, the cost of Desecration is reduced to 1. If the Desecration succeeds, you also get 1 Elder Sign.")
        case $("YS", HWINTBN.name) => spellbook(HWINTBN.name, "Action: Cost 1", "Move Hastur to any Area containing a Cultist of any Faction. You may then take a second, different Action. You may NOT take The Screaming Dead as your second Action.")


        case $("SL") => faction(SL, "info:sl-background", DeathFromBelow, "Doom Phase", "Place your lowest-cost Monster from your Pool into any Area containing at least 1 of your Units.",
            $(Burrow, CursedSlumber), $(
            (Acolyte,       6, "1", "0", ""),
            (Wizard,        2, "1", "0", s"""<div class=p>Spellbook: ${reference(SL, EnergyNexus)}</div>"""),
            (SerpentMan,    3, "2", "1", s"""<div class=p>Spellbook: ${reference(SL, AncientSorcery)}</div>"""),
            (FormlessSpawn, 4, "3", "?", s"""<div class=p>${combat} Equals count of Formless Spawns on the map, +1 if Tsathoggua is also on the map.</div>"""),
            (Tsathoggua,    1, "8", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Tsathoggua.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Formless Spawn on the map.</div>
                <div class=p>${cost("2)")} Pay ${power(8)}. Place Tsathoggua in the Area with the Formless Spawn.</div>
                <div class=p>${combat} Equals the opponent's current Power or 2, whichever is greater.</div>
                <div class=p>${ref(Lethargy)} ${cost("(Action: Cost 0):")} If Tsathoggua is in play, do nothing. This counts as an Action.</div>
                <div class=p>Spellbooks: ${reference(SL, DemandSacrifice)}, ${reference(SL, CaptureMonster)}</div>"""
            ),
        ))

        case $("SL", Pay3SomeoneGains3.text) => requirement("As your action spend 3 Power. Select another player.<br/>He gains 3 Power.")
        case $("SL", Pay3EverybodyGains1.text) => requirement("As your action spend 3 Power.<br/>Each other player gains 1 Power.")
        case $("SL", Pay3EverybodyLoses1.text) => requirement("As your action spend 3 Power.<br/>Each other player loses 1 Power.")
        case $("SL", Roll6DiceInBattle.text) => requirement("Roll 6 or more combat dice in a single Battle.")
        case $("SL", PerformRitual.text) => requirement("Perform a Ritual of Annihilation.")
        case $("SL", AwakenTsathoggua.text) => requirement("Awaken Tsathoggua.")

        case $("SL", Burrow.name) => spellbook(Burrow.name, "Ongoing", "After a Move Action in which you spend 2 or more Power moving Units, regain 1 Power.")
        case $("SL", EnergyNexus.name) => spellbook(EnergyNexus.name, "Ongoing", "Just before a Battle in an Area containing a Wizard, you may take one Action that originates in the Area for its normal Power cost. The Battle proceeds once that Action is finished, starting with Pre-Battle Spellbooks and abilities.")
        case $("SL", AncientSorcery.name) => spellbook(AncientSorcery.name, "Action: Cost 1", "Remove a Serpent Man from the Map and place him on an enemy's Faction Card. You now have access to that Faction's Unique Ability until the end of the next Doom Phase. At that point, gain 1 Power and replace the Serpent Man anywhere on the Map. If a Faction's Unique Ability mentions a Great Old One, it is also considered to include Tsathoggua.")
        case $("SL", CaptureMonster.name) => spellbook(CaptureMonster.name, "Action: Cost 1", "Tsathoggua can Capture Enemy Monsters in the same manner as Cultists are Captured. They are sacrificed for 1 Power in the next Gather Power Phase.")
        case $("SL", DemandSacrifice.name) => spellbook(DemandSacrifice.name, "Pre-Battle", "If Tsathoggua is in play, your enemy chooses ONE of the following options before a Battle with you:<br/>1) You gain <span class=es>1 Elder Sign</span>.<br/>OR<br/>2) All of their Kill results against your Units in this Battle count as Pains instead.")
        case $("SL", CursedSlumber.name) => spellbook(CursedSlumber.name, "Action: Cost 1", "Remove your Controlled Gate and its Cultist from the map and place it on your Faction Card. This Gate and Cultist still provide Power and Doom points, but are immune to enemy abilities. As a Cost 1 Action, return the Gate and Cultist to any Area lacking a Gate. You may only have one Gate on your Faction Card at a time.")


        case $("WW") => faction(WW, "info:ww-background", Hibernate, "Action: Cost 0", "Add +1 Power to your total for each enemy Great Old One in play (but not more than your current Power). You can take no further Actions during this Action Phase. At the start of the next Gather Power Phase, do NOT lose your Power, but add it to your total.",
            $(IceAge, Herald), $(
            (Acolyte,   6, "1", "0", s"""<div class=p>Spellbook: ${reference(WW, Cannibalism)}</div>"""),
            (Wendigo,   4, "1", "1", s"""<div class=p>Spellbooks: ${reference(WW, Cannibalism)}, ${reference(WW, Howl)}</div>"""),
            (GnophKeh,  4, "?", "3", s"""
                <div class=p>${cost("Cost:")} Equals the number of Gnoph-Kehs in your Unit pool.</div>
                <div class=p>${reference(WW, Berserkergang)}</div>"""
            ),
            (RhanTegoth, 1, "6", "3", s"""
                <div class=p>${cost(s"How to Awaken ${RhanTegoth.name}:")}</div>
                <div class=p>${cost("1)")} Pay ${power(6)}.</div>
                <div class=p>${cost("2)")} ${RhanTegoth.name} appears in an Area containing the Windwalker Glyph.</div>
                <div class=p>${ref(Eternal)} ${cost("(Post-Battle):")} If ${RhanTegoth.name} receives a Pain or a Kill, you may pay 1 Power to cancel its effect on him. He can only receive one combat result per Battle.</div>"""
            ),
            (Ithaqua, 1, "6", "?", s"""
                <div class=p>${cost(s"How to Awaken ${Ithaqua.name}:")}</div>
                <div class=p>${cost("1)")} ${RhanTegoth.name} has been awakened (he need not be in play).</div>
                <div class=p>${cost("2)")} A Gate must exist in an Area marked with your Glyph. You need not control the Gate.</div>
                <div class=p>${cost("3)")} Pay ${power(6)} and replace the Gate with ${Ithaqua.name}.</div>
                <div class=p>${combat} Equal to half of your opponent's Doom, rounded up.</div>
                <div class=p>${ref(Ferox)} ${cost("(Ongoing):")} While ${Ithaqua.name} is in play, your Cultists cannot be captured by enemy Monsters or Terrors. They are still vulnerable to enemy Great Old Ones.</div>
                <div class=p>Spellbook: ${reference(WW, ArcticWind)}</div>"""
            ),
        ))

        case $("WW", FirstPlayer.text) => requirement("You are the First Player.")
        case $("WW", OppositeGate.text) => requirement("A Gate exists in the Area marked with the Windwalker Glyph and in which you did not start.")
        case $("WW", AnytimeGainElderSigns.text) => requirement("Take this spellbook at any time. Gain <span class=es>1 Elder Sign</span> for each enemy player with 6 Spellbooks on their Faction Card, to a maximum of <span class=es>3 Elder Signs</span>.")
        case $("WW", AnotherFactionAllSpellbooks.text) => requirement("Another Faction has 6 Spellbooks on their Faction Card.")
        case $("WW", AwakenIthaqua.text) => requirement("Awaken Ithaqua.")
        case $("WW", AwakenRhanTegoth.text) => requirement("Awaken Rhan Tegoth.")

        case $("WW", Cannibalism.name) => spellbook(Cannibalism.name, "Post-Battle", "After all Battle results have been applied, if one or more enemy Units were killed, you may place a Wendigo or Acolyte from your Pool into the Battle Area. You may do this even if you were not involved in the Battle.")
        case $("WW", Howl.name) => spellbook(Howl.name, "Pre-Battle", "Before Battle, if any Wendigos are present, you may force the enemy to Retreat one Unit (of their choice) out of the Area to an adjacent Area. This is not a Pain - the Unit may be moved to an Area containing your Units.")
        case $("WW", Berserkergang.name) => spellbook(Berserkergang.name, "Post-Battle", "For each Gnoph-Keh assigned a Kill in Battle, the enemy must Eliminate 1 Monster or Cultist.")
        case $("WW", ArcticWind.name) => spellbook(ArcticWind.name, "Ongoing", "When Ithaqua uses Move Action, any or all your Units in the same Area can Move with him for no additional cost.")
        case $("WW", IceAge.name) => spellbook(IceAge.name, "Action: Cost 1", "Place or move your Ice Age token to any Area. When an enemy Faction takes any Action ending in the Ice Age Area, they must pay +1 Power.")
        case $("WW", Herald.name) => spellbook(Herald.name, "Doom Phase", "Pay 5 Power for Windwalker's Ritual of Annihilation, regardless of the number indicated on the Ritual track.")


        case $("OW") => faction(OW, "info:ow-background", BeyondOne, "Action: Cost 1", "Select one of your your Units with a Cost of 3+ in an Area that contains a Gate and no enemy Great Old Ones. Move that Unit, the Gate, and any Controlling Unit to any Area on the map that does not already have a Gate.",
            $(TheyBreakThrough, ChannelPower, DragonAscending, DragonDescending), $(
            (Acolyte,     6, "1", "0", s"""<div class=p>Spellbook: ${reference(OW, MillionFavoredOnes)}</div>"""),
            (Mutant,      4, "2", "1", s"""<div class=p>Spellbook: ${reference(OW, MillionFavoredOnes)}</div>"""),
            (Abomination, 3, "3", "2", s"""<div class=p>Spellbooks: ${reference(OW, MillionFavoredOnes)}, ${reference(OW, DreadCurse)}</div>"""),
            (SpawnOW,     2, "4", "3", s"""<div class=p>Spellbooks: ${reference(OW, MillionFavoredOnes)}, ${reference(OW, DreadCurse)}</div>"""),
            (YogSothoth,  1, "6", "?", s"""
                <div class=p>${cost(s"How to Awaken ${YogSothoth.name}:")}</div>
                <div class=p>${cost("1)")} You must have a Spawn of Yog-Sothoth on the Map.</div>
                <div class=p>${cost("2)")} Pay ${power(6)}. Replace the Spawn with Yog-Sothoth.</div>
                <div class=p>${combat} Equal to twice the number of enemy-Controlled Faction Great Old Ones in play.</div>
                <div class=p>${ref(KeyAndGate)} ${cost("(Ongoing):")} Yog-Sothoth counts as a Gate for every purpose, except for The Beyond One ability. Yog-Sothoth is not Controlled by any Cultist, and can exist in the same Area as another Gate.</div>"""
            ),
        ))

        case $("OW", EightGates.text) => requirement("There are 8 Gates on the Map.")
        case $("OW", TenGates.text) => requirement("There are 10 Gates on the Map.")
        case $("OW", TwelveGates.text) => requirement("There are 12 Gates on the Map.")
        case $("OW", UnitsAtEnemyGates.text) => requirement("You have Units in at least 2 Areas containing<br/>enemy-Controlled Gates.")
        case $("OW", LoseUnitInBattle.text) => requirement("Lose 1 of your own Units in Battle.")
        case $("OW", GooMeetsGoo.text) => requirement("Your Great Old One is in the same Area with<br/>an enemy Great Old One.")
        case $("OW", AwakenYogSothoth.text) => requirement("Awaken Yog-Sothoth.")

        case $("OW", TheyBreakThrough.name) => spellbook(TheyBreakThrough.name, "Ongoing", "You can Summon Monsters at enemy-Controlled and Abandoned Gates. You do not need to have any Units present in the Area.")
        case $("OW", MillionFavoredOnes.name) => spellbook(MillionFavoredOnes.name, "Post-Battle", "After Pains and Kills are resolved, replace any or all surviving Acolytes to Mutants, Mutants with Abominations, and Abominations with Spawns of Yog-Sothoth. You can replace a Spawn of Yog-Sothoth with as many Mutants as are in your Pool.")
        case $("OW", ChannelPower.name) => spellbook(ChannelPower.name, "Battle", "After rolling Battle dice, you may pay 1 Power to reroll all dice which were not Kills or Pains. You may do this more than once.")
        case $("OW", DreadCurse.name) => spellbook(DreadCurse.name, "Action: Cost 2", "Select an Area and roll 1 Battle die per Abomination and Spawn of Yog-Sothoth in play. Apply the results as Kills and Pains to enemy Factions in the Area. You choose which Faction receives which results, but they choose which of their Units receives each result. No Battle-type abilities apply. You choose to which Area each Unit is Pained, ignoring normal Pain rules.")
        case $("OW", DragonAscending.name) => spellbook(DragonAscending.name, "Once Only", "Once during the game (at any time), set your Power to be equal to the current Power of one chosen enemy Faction.")
        case $("OW", DragonDescending.name) => spellbook(DragonDescending.name, "Once Only", "Once during the game when you perform a Ritual of Annihilation, you receive twice the normal Doom points.")


        case $("AN") => faction(AN, "info:an-background", Dematerialization, "Doom Phase", "Relocate any or all of your own Units from one Area to a single other Area, anywhere on the Map.",
            $, $(
            (Acolyte,    6,   "1",   "0", s""""""),
            (UnMan,      3, "3/0",   "0", s"""<div class=p><span class=cost-color>Cost:</span> 0 with ${reference(AN, Festival)}</div>"""),
            (Reanimated, 3, "4/1",   "2", s"""<div class=p><span class=cost-color>Cost:</span> 1 with ${reference(AN, Brainless)}</div>"""),
            (Yothan,     3, "6/3",   "7", s"""<div class=p><span class=cost-color>Cost:</span> 3 with ${reference(AN, Extinction)}</div>"""),
            (Cathedral,  4, "3/1", "n/a", s"""
                <div class=p>You may use the Create Gate Action to Create Cathedrals instead of Gates.</div>
                <div class=p>${cost("Cost:")} 1 if built not adjacent to another Cathedral</div>
                <div class=p>Spellbooks: ${reference(AN, WorshipServices)}, ${reference(AN, Consecration)}, ${reference(AN, UnholyGround)}</div>
                <div class=p>${cost("Special:")} If all 4 Cathedrals are in play, you may Awaken an Independent Great Old One without your own Great Old One (when Awakening Cthugha this way, just pay 6 Power).</div>"""
            )
        ))

        case $("AN", CathedralAA.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-aa")} class=inline-glyph />")
        case $("AN", CathedralOO.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-oo")} class=inline-glyph />")
        case $("AN", CathedralWW.text) => requirement(s"A Cathedral is in an Area marked with this Glyph: <img src=${imageSource("sign-ww")} class=inline-glyph />")
        case $("AN", CathedralNG.text) => requirement(s"A Cathedral is in an Area without<br/>any of these Glyphs: <img src=${imageSource("sign-aa")} class=inline-glyph /><img src=${imageSource("sign-oo")} class=inline-glyph /><img src=${imageSource("sign-ww")} class=inline-glyph />")
        case $("AN", GiveWorstMonster.text) => requirement("As your Action, each enemy Summons their lowest cost Monster at their Controlled Gate for free.")
        case $("AN", GiveBestMonster.text) => requirement("As your Action, each enemy Summons their highest cost Monster at their Controlled Gate for free.")

        case $("AN", Brainless.name) => spellbook(Brainless.name, "Ongoing", "Reanimated now cost 1 Power to Summon. They may only Move, Capture, or declare Battle if they share an Area with one or more of your non-Reanimated Units.")
        case $("AN", Festival.name) => spellbook(Festival.name, "Ongoing", "Un-Men now cost 0 Power to Summon. When you Summon an Un-Man, also select an enemy to gain 1 Power.")
        case $("AN", Extinction.name) => spellbook(Extinction.name, "Ongoing", "Yothans now cost 3 Power to Summon. When a Yothan is Killed or Eliminated, remove it permanently from the game.")
        case $("AN", UnholyGround.name) => spellbook(UnholyGround.name, "Post Battle", "If there is a Cathedral in the Battle Area, you may choose to remove a Cathedral from anywhere. If you do, an enemy Great Old One in the Battle must be eliminated by its owner.")
        case $("AN", Consecration.name) => spellbook(Consecration.name, "Doom Phase", "When you perform a Ritual of Annihilation, gain <span style=es>1 Elder Sign</span> if at least one Cathedral is in play. If all four Cathedrals are in play, gain <span style=es>2 Elder Signs</span> instead.")
        case $("AN", WorshipServices.name) => spellbook(WorshipServices.name, "Gather Power Phase", "Gain 1 Power for each Cathedral that shares an Area with an enemy Gate. Those enemies each gain 1 Power.")

            case $("DS") => faction(DS, "info:ds-background", Psychosis, "Ongoing", "Psychosis (Action: Cost 0): You must have an Acolyte in your pool. Select an area that has no units from any faction. Place an acolyte from your pool there. During each Doom phase, flip ALL your face-down faction spellbooks face-up again.",
                $(Consummation), $(                
                (Acolyte,       6, "1", "0", ""),
                (LarvaThesis,   3, "1", "0", s"<div class=p>${combat} 2 if Avatar Thesis is in play.</div>"),
                (LarvaAntithesis, 3, "1", "0", s"<div class=p>${combat} 2 if Avatar Antithesis is in play.</div>"),
                (LarvaSynthesis, 3, "1", "0", s"<div class=p>${combat} 2 if Avatar Synthesis is in play.</div>"),
                (ChaosGate, 1, "n/a", "n/a", s"<div class=p>Spellbook: ${reference(DS, ChaosGateSB)}</div>"),
                (AvatarThesis, 1, "?", "?", s"""
                    <div class=p>${cost("How to Awaken Avatar Thesis:")}</div>
                    <div class=p>${cost("1)")} Set the Azathoth marker on the Doom track on any desired spot from 0 to 8.</div>
                    <div class=p>${cost("2)")} Pay Power equal to the Azathoth marker's setting.</div>
                    <div class=p>${cost("3)")} Each other player gains 1 Power OR choose an enemy player to gain 2 Power.</div>
                    <div class=p>${combat} Equals the Azathoth marker's position.</div>
                    <div class=p>Spellbooks: ${reference(DS, UndirectedEnergy)}</div>
                """),
                (AvatarAntithesis, 1, "?", "?", s"""
                    <div class=p>${cost("How to Awaken Avatar Antithesis:")}</div>
                    <div class=p>${cost("1)")} Avatar Thesis must have been Awakened (it need not be in play).</div>
                    <div class=p>${cost("2)")} Pay Power equal to 8 minus the Azathoth marker's setting.</div>
                    <div class=p>${cost("3)")} Each other player gains 1 Doom OR choose an enemy player who gains an Elder Sign.</div>
                    <div class=p>${combat} Equals 8 minus the Azathoth marker's position.</div>
                    <div class=p>Spellbooks: ${reference(DS, FiendishGrowth)}</div>
                """),
                (AvatarSynthesis, 1, "8", "?", s"""
                    <div class=p>${cost(s"How to Awaken ${AvatarSynthesis.name}:")}</div>
                    <div class=p>${cost("1)")} Both ${AvatarThesis.name} and ${AvatarAntithesis.name} must have been Awakened (they need not be in play).</div>      
                    <div class=p>${cost("2)")} Pay ${power(8)}.</div>
                    <div class=p>${cost("3)")} Roll the Azathoth die. All enemies have 2 minutes to decide how to collectively lose that much Power and/or Doom, if they cannot agree then you win the game. In a 2-3 player game halve the die roll result (rounded up).</div>
                    <div class=p>${combat} First roll the Azathoth die, then roll that many combat dice.</div>
                    <div class=p>${ref(CosmicRuler)} ${cost("(Post-Battle):")} When any Avatar is choosen to recieve a Kill or Elimination, instead you can Eliminate another Avatar in its stead, from anywhere on the map.</div>
                """),
            )
        )

            case $("DS", OneLarvaEach.text) => requirement("You have one of each Larva type in play.")
            case $("DS", AbandonedGateGP.text) => requirement("An abandoned Gate is on the Map during Gather Power Phase.")
            case $("DS", PowerDoomOffer.text) => requirement("During any Doom Phase, you declare that each other player chooses whether to receive 1 Power or 1 Doom. You gain everything they gain.")
            case $("DS", AwakenAvatarThesis.text) => requirement("Awaken Avatar Thesis.")
            case $("DS", AwakenAvatarAntithesis.text) => requirement("Awaken Avatar Antithesis.")
            case $("DS", AwakenAvatarSynthesis.text) => requirement("Awaken Avatar Synthesis.")

            case $("DS", Psychosis.name) => spellbook(Psychosis.name, "Action: Cost 0", "You must have an Acolyte in your pool. Select an area that has no units from any faction. Place an acolyte from your pool there. During each Doom phase, flip ALL your face-down faction spellbooks face-up again.")
            case $("DS", CosmicRuler.name) => spellbook(CosmicRuler.name, "Post-Battle", "When any Avatar is choosen to recieve a Kill or Elimination, instead you can Eliminate another Avatar in its stead, from anywhere on the map.")
            case $("DS", AnimateMatter.name) => spellbook(AnimateMatter.name, "Action: Cost 1", "Flip this Spellbook face down. Move a controlled Chaos Gate from its Area to an adjacent Area, taking its Cultist along. You cannot move the Gate to another player’s Start Area. If the new Area has an existing physical Gate, replace that Gate with the Chaos Gate.")
            case $("DS", ChaosGateSB.name) => spellbook(ChaosGateSB.name, "Action: Cost 1", "Flip this Spellbook face down, then place a Chaos Gate in any area without a Gate.")
            case $("DS", Consummation.name) => spellbook(Consummation.name, "Action: Cost 1", "Flip this spellbook face-down. Then flip one of your face-down faction spellbooks face-up again, permitting its re-use this turn.")
            case $("DS", FiendishGrowth.name) => spellbook(FiendishGrowth.name, "Action: Cost 1", "Flip this spellbook face down if Avatar Antithesis is in play. Place 1 free Monster or Acolyte from your pool in Avatar Antithesis' Area per Faction with Units in that Area, including yours.")
            case $("DS", Traitors.name) => spellbook(Traitors.name, "Action: Cost 1", "Flip this Spellbook face down. Replace your controlled Chaos Gate with a Normal Gate, and replace its Controlling Cultist with another player's Cultist from his Pool. Your cultist returns to your Pool. If you have no other Units in the Area, gain an Elder Sign.")
            case $("DS", UndirectedEnergy.name) => spellbook(UndirectedEnergy.name, "Action: Cost 1", "Flip this spellbook face-down if Avatar Thesis is in play. Gain 1 Power per faction with units in Avatar Thesis' area, including yours.")


        case $(_, MaoCeremony.name) => spellbook(MaoCeremony.name, "Ongoing", "At the end of Gather Power, after Power has been added (i.e., before Determine First Player), you may choose to sacrifice 1 or more of your own Cultists, adding 1 Power apiece to your total.")
        case $(_, Recriminations.name) => spellbook(Recriminations.name, "Action: Cost 1", "Remove any spellbook (including this one) from your Faction Card and replace it with another available spellbook.")
        case $(_, Shriveling.name) => spellbook(Shriveling.name, "Pre-Battle", "Select an enemy Monster or Cultist in the Battle. That Unit is Eliminated, and the owner receives Power equal to the Unit's cost.")
        case $(_, StarsAreRight.name) => spellbook(StarsAreRight.name, "Ongoing", "During the Doom Phase, if you turn in Elder Signs for Doom points, you also immediately receive Power equal to their face value.")
        case $(_, UmrAtTawil.name) => spellbook(UmrAtTawil.name, "Ongoing", "Gates now cost you 2 Power to Build.")
        case $(_, Undimensioned.name) => spellbook(Undimensioned.name, "Action: Cost 2", "Rearrange your Units among their Areas as you see fit. You may completely empty an Area, but you may not move to any new Areas.")


        case $("Ghast") => loyaltyCard(GhastCard.name, GhastCard.quantity, GhastCard.cost, GhastCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place all 4 Ghasts at your controlled Gate(s).", "Hordeling", "Ongoing", "When you spend 2 Power to Summon Ghasts, all Ghasts in your pool are immediately placed on the map at any Gate(s) you control.")
        case $("Gug") => loyaltyCard(GugCard.name, GugCard.quantity, GugCard.cost, GugCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Gug at your controlled Gate.", "Clumsy", "Ongoing", "A Gug cannot Capture a Cultist.")
        case $("Shantak") => loyaltyCard(ShantakCard.name, ShantakCard.quantity, ShantakCard.cost, ShantakCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Shantak at your controlled Gate.", "Horror Steed", "Ongoing", "When Moving a Shantak, it can reach any Area on the map. In addition, the Shantak may carry one of your Cultists with it for free.")
        case $("Voonith") => loyaltyCard(VoonithCard.name, VoonithCard.quantity, VoonithCard.cost, VoonithCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Voonith at your controlled Gate.", "Vicious", "Battle", "After rolling dice in Battle, before Kills are assigned, add extra Kills equal to the number of Vooninths in Battle minus the number of Kills rolled (minimum 0).")

        case $("Star Vampire") => loyaltyCard(StarVampireCard.name, StarVampireCard.quantity, StarVampireCard.cost, StarVampireCard.combat, "Pay 2 Doom to obtain this Loyalty Card, plus place 1 Star Vampire at your controlled Gate.", "Vampirism", "Battle", "Roll the Star Vampire's combat dice separately. Each Pain they roll drains 1 Power from the enemy Faction. Each Kill they roll drains 1 Doom point from the enemy Faction. The drained point(s) are transferred to you immediately. If the enemy Faction lacks Power or Doom points, you get nothing. The Pains and Kills rolled still count towards your Combat Results.")

        case $("Dimensional Shambler") => loyaltyCard(DimensionalShamblerCard.name, DimensionalShamblerCard.quantity, DimensionalShamblerCard.cost, DimensionalShamblerCard.combat, "Pay 2 Doom to obtain this Loyalty Card, then place 1 Dimensional Shambler onto your Faction Card.", "Walk Between Worlds", "Ongoing", "When Summoning a Dimensional Shambler, place it onto your Faction Card. After any Action (by any player), you may place one or more Dimensional Shamblers from your Faction Card into any Area. Once placed, Dimensional Shamblers remain on the Map (until Killed or otherwise Eliminated).")
        case $("Gnorri") => loyaltyCard(GnorriCard.name, GnorriCard.quantity, GnorriCard.cost, GnorriCard.combat, "Pay 2 Doom to obtain this Loyalty Card, then place 1 Gnorri at your Controlled Gates.", "Grottos", "Doom Phase", "During the Doom Phase, if you have 2 Gnorri in play, you earn 1 extra Doom point. If you have 3 Gnorri in play, you earn 2 extra Doom points.")
    case $("Dimensional Shambler") => loyaltyCard(DimensionalShamblerCard.name, DimensionalShamblerCard.quantity, DimensionalShamblerCard.cost, DimensionalShamblerCard.combat, "Pay 2 Doom to obtain this Loyalty Card, then place 1 Dimensional Shambler onto your Faction Card.", "Walk Between Worlds", "Ongoing", "When Summoning a Dimensional Shambler, place it onto your Faction Card. After any Action (by any player), you may place one or more Dimensional Shamblers from your Faction Card into any Area. Once placed, Dimensional Shamblers remain on the Map (until Killed or otherwise Eliminated).")
    case $("High Priest") => loyaltyCard(HighPriestCard.name, HighPriestCard.quantity, HighPriestCard.cost, HighPriestCard.combat, "The High Priest is a new type of Cultist, it is Recruited like an Acolyte. Each High Priest generates 1 Power during the Gather Power Phase, can Create and Control a Gate, and can be Captured.", "Unspeakable Oath", "Ongoing", "At the end of any player's Action (even if it is not your turn), Sacrifice your High Priest (return him to your Pool) and gain 2 Power. This may also be done during the Gather Power and Doom Phases.")

        // Round 8 Bug 40: third parameter (facedown : Boolean) added for Infernal Pact strikethrough
        case $("Byatis", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(ByatisCard.name, "" + ByatisCard.cost, "" + ByatisCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Byatis in the Area containing the Gate.", "Toad of Berkeley", "Ongoing", "Byatis may not Move, nor can he be moved with movement-type abilities (such as Arctic Winds or Submerge). He can still be Pained. If there are no enemy Units in Byatis' Area during the Doom Phase, earn 1 Elder Sign.", "Byatis survives a Battle in which at least one enemy Unit is Killed", "God of Forgetfulness", "Action: Cost 1", "Select all enemy Cultists in an Area adjacent to Byatis. Those Cultists are moved into Byatis' Area.", facedown)
        case $("Abhoth", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(AbhothCard.name, "" + AbhothCard.cost, "Equals the number of Filth Tokens in play (0-12)", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Abhoth in the Area containing the Gate.", "Filth", "Action: Cost 1", "Place a Filth Token in any Area.", "Choose One: EITHER your Faction has 4+ different Monster types in play, including Filth Tokens, OR Your faction has 8+ total Monsters in play, including Filth Tokens", "The Brood", "Ongoing", "Gates in Areas containing Filth Tokens do not count during the Doom phase. Does not apply to Abhoth's Faction.", facedown)
        case $("Daoloth", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(DaolothCard.name, "" + DaolothCard.cost, "" + DaolothCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Daoloth in the Area containing the Gate.", "Cosmic Unity", "Pre-Battle", "In a Battle involving Daoloth, choose one enemy Great Old One. It rolls no Combat dice (it still gets its Battle Ability, if any).", "A Great Old One is Killed (anywhere on the map)", "Interdimensional", "Ongoing", "When Daoloth enters an Area without a Gate, immediately place a Gate there.", facedown)
        case $("Nyogtha", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(NyogthaCard.name, "" + NyogthaCard.cost, "4 if Nyogtha's Faction declared the Battle, 1 if not.", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place all Nyogtha Units from your Pool to the Area containing the Gate.", "From Below", "Ongoing", "Nyogtha is two Units. Any Common Action involving one of these Units can be applied simultaneously to the other as part of the same Action and at no extra cost. When one Nyogtha Unit Moves, so may the other. When one Captures a Cultist, so may the other. If Battle is declared in one's Area, you can also declare a Battle in the other's Area, for free. You only lose this Loyalty Card if both Nyogtha Units have been Killed.", "Nyogtha survives a Battle against an enemy Great Old One", "Nightmare Web", "Ongoing", "If one of the Nyogtha Units is in your pool, you can Awaken it for 2 Power, placing it in any Area in which you have at least one Unit.", facedown)
        case $("Tulzscha", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(TulzschaCard.name, "" + TulzschaCard.power, "" + TulzschaCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Tulzscha in the Area containing the Gate.", "Undying Flame", "Gather Power Phase", "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom than you; gain 1 Elder Sign if any Faction has more Elder Signs than you; gain 1 Power if any Faction has more Power than you.", "As an Action, each enemy Faction gains 2 Power.", "Ceremony of Annihilation", "Doom Phase", "When you perform a Ritual of Annihilation, you may choose to pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs.", facedown)
        case $("Y'Golonac", spellbook : Boolean, facedown : Boolean) => loyaltyCardIGOO(YgolonacCard.name, "" + YgolonacCard.power, "" + YgolonacCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 2 Power, place Y'Golonac in the Area containing the Gate.", "Orifices", "Post-Battle", "If Y'Golonac is Killed in a Battle, select a surviving enemy Terror, Monster, or Cultist. Replace it with Y'Golonac, then give Y'Golonac's Loyalty Card to that player. If no enemies survived, Y'Golonac dies normally (placing this Loyalty Card in the general Pool).", "You have just received Y'Golonac as a result of his Orifices ability.", "The Revelations", "Doom Phase", "Every player except you gets 1 Elder Sign. This is not optional.", facedown)
        // Backwards compatibility: old overlays without facedown parameter
        case $("Byatis", spellbook : Boolean) => loyaltyCardIGOO(ByatisCard.name, "" + ByatisCard.cost, "" + ByatisCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Byatis in the Area containing the Gate.", "Toad of Berkeley", "Ongoing", "Byatis may not Move, nor can he be moved with movement-type abilities (such as Arctic Winds or Submerge). He can still be Pained. If there are no enemy Units in Byatis' Area during the Doom Phase, earn 1 Elder Sign.", "Byatis survives a Battle in which at least one enemy Unit is Killed", "God of Forgetfulness", "Action: Cost 1", "Select all enemy Cultists in an Area adjacent to Byatis. Those Cultists are moved into Byatis' Area.")
        case $("Abhoth", spellbook : Boolean) => loyaltyCardIGOO(AbhothCard.name, "" + AbhothCard.cost, "Equals the number of Filth Tokens in play (0-12)", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Abhoth in the Area containing the Gate.", "Filth", "Action: Cost 1", "Place a Filth Token in any Area.", "Choose One: EITHER your Faction has 4+ different Monster types in play, including Filth Tokens, OR Your faction has 8+ total Monsters in play, including Filth Tokens", "The Brood", "Ongoing", "Gates in Areas containing Filth Tokens do not count during the Doom phase. Does not apply to Abhoth's Faction.")
        case $("Daoloth", spellbook : Boolean) => loyaltyCardIGOO(DaolothCard.name, "" + DaolothCard.cost, "" + DaolothCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place Daoloth in the Area containing the Gate.", "Cosmic Unity", "Pre-Battle", "In a Battle involving Daoloth, choose one enemy Great Old One. It rolls no Combat dice (it still gets its Battle Ability, if any).", "A Great Old One is Killed (anywhere on the map)", "Interdimensional", "Ongoing", "When Daoloth enters an Area without a Gate, immediately place a Gate there.")
        case $("Nyogtha", spellbook : Boolean) => loyaltyCardIGOO(NyogthaCard.name, "" + NyogthaCard.cost, "4 if Nyogtha's Faction declared the Battle, 1 if not.", spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 6 Power, and place all Nyogtha Units from your Pool to the Area containing the Gate.", "From Below", "Ongoing", "Nyogtha is two Units. Any Common Action involving one of these Units can be applied simultaneously to the other as part of the same Action and at no extra cost. When one Nyogtha Unit Moves, so may the other. When one Captures a Cultist, so may the other. If Battle is declared in one's Area, you can also declare a Battle in the other's Area, for free. You only lose this Loyalty Card if both Nyogtha Units have been Killed.", "Nyogtha survives a Battle against an enemy Great Old One", "Nightmare Web", "Ongoing", "If one of the Nyogtha Units is in your pool, you can Awaken it for 2 Power, placing it in any Area in which you have at least one Unit.")
        case $("Tulzscha", spellbook : Boolean) => loyaltyCardIGOO(TulzschaCard.name, "" + TulzschaCard.power, "" + TulzschaCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 4 Power, and place Tulzscha in the Area containing the Gate.", "Undying Flame", "Gather Power Phase", "At the end of the Gather Power Phase: gain 1 Doom if any Faction has more Doom than you; gain 1 Elder Sign if any Faction has more Elder Signs than you; gain 1 Power if any Faction has more Power than you.", "As an Action, each enemy Faction gains 2 Power.", "Ceremony of Annihilation", "Doom Phase", "When you perform a Ritual of Annihilation, you may choose to pay nothing and instead EARN Power equal to the current Ritual marker position, then advance the marker 1 step. You earn no extra Doom or Elder Signs.")
        case $("Y'Golonac", spellbook : Boolean) => loyaltyCardIGOO(YgolonacCard.name, "" + YgolonacCard.power, "" + YgolonacCard.combat, spellbook, "1. Your Controlled Gate is in an Area with your Great Old One.<br>2. Pay 2 Power, place Y'Golonac in the Area containing the Gate.", "Orifices", "Post-Battle", "If Y'Golonac is Killed in a Battle, select a surviving enemy Terror, Monster, or Cultist. Replace it with Y'Golonac, then give Y'Golonac's Loyalty Card to that player. If no enemies survived, Y'Golonac dies normally (placing this Loyalty Card in the general Pool).", "You have just received Y'Golonac as a result of his Orifices ability.", "The Revelations", "Doom Phase", "Every player except you gets 1 Elder Sign. This is not optional.")


        // ── Library at Celaeno: Silence Token overlay ──
        case $("SilenceToken", _ : Boolean) => tomeOverlay("Silence Token", "",
            "Spend to activate the Custodian or the Librarian.")

        // ── Library at Celaeno: Tome Info overlay ──
        case $("LibraryTomeInfo", _ : Boolean) => tomeOverlay("Library Tomes", "",
            "Library Tomes act as neutral Spellbooks. When you Control a Gate in an Area with a Tome, take possession of that Area's Library Tome. It may be used as a standard Spellbook.<br><br>" +
            "If you lose Control of the Tome's Gate, the Tome is considered Overdue but remains in your possession. Library Tomes may only be returned to the Map by Satisfying the Librarian's Agony.<br><br>" +
            "They cannot be otherwise voluntarily relinquished, and the player cannot be forced to return them if they choose to lose Doom or Units instead.<br><br>" +
            "When a Tome is relinquished and returned to its slot, it is immediately acquired by the Faction currently Controlling its respective Gate, if any.")

        // ── Library at Celaeno: Hint Card overlay ──
        case $("LibraryHintCard", _ : Boolean) => libraryHintCard()

        // ── Library at Celaeno: Custodian and Librarian overlays ──
        case $("Custodian", _ : Boolean) => mapUnitOverlay("Custodian",
            "<div class='p nt black-border'>Spend a silence token. Place the Custodian in any Area. Roll the Agony Die. " +
            "If the Custodian stays in the same Area, add +1 to the Agony die result, for this roll only. " +
            "The Activating player assigns the Agony result between the Factions present in the Custodian's Area. " +
            "Those Factions can satisfy one Agony for each of their Units in the Area that they move to the Oubliette, " +
            "and must satisfy as much of the Custodian's Agony as possible.</div>" +
            "<div class='p nt black-border'>No player may perform the Control Gate Action in the Custodian's Area. " +
            "However, if a Gate is already Controlled when the Custodian is Moved into its Area, that Gate may remain Controlled.</div>",
            "Agony Die faces: 1, 2, 2, 3, 3, 4")

        case $("Librarian", _ : Boolean) => mapUnitOverlay("Librarian",
            "<div class='p nt black-border'>The Librarian may only be Activated if at least one enemy Faction has an Overdue Library Tome. " +
            "Spend a silence token. Place the Librarian in an Area with units from one of those Factions (with an Overdue Library Tome). " +
            "Roll the Agony Die. If the Librarian stays in the same Area, add +1 to the Agony die result, for this roll only. " +
            "The Activating player assigns the Agony result between the Factions present in the Librarian's Area. " +
            "Those Factions can satisfy one Agony for each of their Units in the Area of their choice that they Eliminate, " +
            "for each point of Doom they choose to lose, or for each Overdue Tome they return to its slot, " +
            "and must satisfy as much of the Librarian's Agony as possible.</div>" +
            "<div class='p nt black-border'>No player may perform the Control Gate Action in the Librarian's Area. " +
            "However, if a Gate is already Controlled when the Librarian is Moved into its Area, that Gate may remain Controlled.</div>",
            "Agony Die faces: 1, 2, 2, 3, 3, 4")

        // Library Tomes
        case $("Barrier of Naach-Tith", faceUp : Boolean) => tomeOverlay("Barrier of Naach-Tith", "Always Active",
            "When an enemy declares Battle against the holder, the attacker must pay one of:<br>" +
            "• Release a Captured Cultist (any faction's) back to its owner's Pool<br>" +
            "• Discard an Elder Sign<br>" +
            "• Discard a Silence Token<br><br>" +
            "If none can be paid, the Battle is blocked.", faceUp)

        case $("Guardian under the Lake", faceUp : Boolean) => tomeOverlay("Guardian under the Lake", "Action: Cost 1 (flippable)",
            "Choose an Archway region containing enemy units. Move all units of one enemy faction " +
            "from that Archway to another Archway region. Flips face-down after use. " +
            "You may flip it face-up again (and reuse it) by releasing a Captured Cultist back to its owner's Pool, discarding an Elder Sign, or discarding a Silence Token. It also flips in the next Doom Phase.", faceUp)

        case $("Larvae of the Outer Gods", faceUp : Boolean) => tomeOverlay("Larvae of the Outer Gods", "Action: Cost 1 (flippable)",
            "If any opponent has more Power than you, gain 1 Elder Sign. " +
            "Flips face-down after use. " +
            "You may flip it face-up again (and reuse it) by releasing a Captured Cultist back to its owner's Pool, discarding an Elder Sign, or discarding a Silence Token. It also flips in the next Doom Phase.", faceUp)

        case $("Yr and the Nhhngr", faceUp : Boolean) => tomeOverlay("Yr and the Nhhngr", "Action: Cost 1 (flippable)",
            "If any opponent has more Doom than you, choose one:<br>" +
            "• Place 1 Monster from your Pool at a Controlled Gate (no additional cost)<br>" +
            "• Gain 2 Power (net +1 after the 1 Power cost)<br><br>" +
            "Flips face-down after use. You may flip it face-up again (and reuse it) by releasing a Captured Cultist back to its owner's Pool, discarding an Elder Sign, or discarding a Silence Token. It also flips in the next Doom Phase.", faceUp)

        // Tombstalker (TS): faction info card showing Death March ability, units (TombHerd, DeepTendril, Gla'aki)
        case $("TS") => faction(TS, "info:ts-background", DeathMarch, "Ongoing",
            "Increment the Death's Head each time an enemy Unit dies in any Battle. In the Doom Phase, spend 1 Death's Head to place a Tomb-Herd in any Area; repeat as much as possible. Then reset the Death's Head to 0.",
            $(), $(
            (Acolyte,    6, "1",   "0", s""""""),
            (TombHerd,   6, "2",   "3", s"""<div class=p>The first Tomb-Herd in an Area has 3 Combat. Any others have 0 Combat.</div>"""),
            (DeepTendril, 3, "3", "1-3", s"""<div class=p>Combat: 1, +1 if Gla'aki is in the same Area, +1 if in an Ocean/Sea Area.</div>"""),
            (Glaaki,     1, "7",   "?", s"""<div class=p>Combat: equals double the number of Deep Tendrils in play.</div><div class=p>Awaken: control an Ocean/Sea gate. May spend Death's Head as Power.</div><div class=p><span class=ability-color>Shepherd of the Crypt</span> (Gather Power Phase): choose an Area and gain 1 Power per Tomb-Herd there.</div>""")
        ))

        // Tombstalker (TS): spellbook requirement info card overlays
        case $("TS", TSAwakenGlaaki.text) => requirement("Awaken Tombstalker Gla'aki.")
        case $("TS", TSTombHerdKilled.text) => requirement("A Tomb-Herd is Killed in Battle.")
        case $("TS", TSRollKill.text) => requirement("Roll a Kill in a Battle.")
        case $("TS", TSRoll3Pains.text) => requirement("Roll 3 or more Pains in a single Battle.")
        case $("TS", TSGlaakiBattlesGOO.text) => requirement("Tombstalker Gla'aki is in a Battle with an enemy Great Old One.")
        case $("TS", TSRitualOrEnemyGate.text) => requirement("Perform a Ritual of Annihilation OR Control a Gate in an enemy faction's starting Area.")

        // Tombstalker (TS): spellbook info card overlays
        case $("TS", ElevenRevelations.name) => spellbook(ElevenRevelations.name, "Action: Cost 1", "Give an enemy your topmost Cursed Tome (I-XI) which they place on their Faction Sheet.")
        case $("TS", Oleaginous.name) => spellbook(Oleaginous.name, "Post-Battle", "Any Pains applied to Tombstalker Gla'aki and Deep Tendrils instead become Retreats (you can move them to any adjacent Areas, regardless of the presence of enemy Units).")
        case $("TS", GraspingDead.name) => spellbook(GraspingDead.name, "Action: Cost 1 Power or 2 Death's Head", "Resolve a Battle in each Area containing a Tomb-Herd and enemy Units, with you as the attacker, for free. Only your Tomb-Herd participate in the Battle.")
        case $("TS", Hecatomb.name) => spellbook(Hecatomb.name, "Doom Phase", "After placing all Tomb-Herd from Death March, but before resetting the counter to 0, you may spend residual Death's Head toward the cost of a Ritual of Annihilation as Power. If you only spent Death's Head toward the cost of a Ritual, do not advance the Ritual Marker.")
        case $("TS", GreenDecay.name) => spellbook(GreenDecay.name, "Gather Power Phase", "If Tombstalker Gla'aki is in play, captured Cultists gain you 1 Elder Sign (instead of 1 Power) during the Gather Power Phase.")
        case $("TS", Undulate.name) => spellbook(Undulate.name, "Ongoing", "Your Units may carry one of your Units of lesser cost when moving for free. This effect stacks.")


        // Firstborn (FB): faction info card showing unique ability (Writhe), units, and Crater building
        case $("FB") => faction(FB, "info:fb-background", Writhe, "Action: Cost 2",
            "Roll dice equal to your Power. For each Kill: Eliminate a Unit you control, any of your Acolytes Eliminated are instead replaced with Desiccated. For each Pain, relocate your Unit to any Area. Before applying these results, you may reroll ALL these dice once.<br/><br/><span class=ability-color>Crater</span> <span class=cost-color>(Building):</span> Any Gate (other than Yog-Sothoth) that coexists in an Area with a Crater is immediately destroyed.",
            $(), $(
            (Acolyte,        6, "1", "0", s""""""),
            (Desiccated,     6, "2", "0+", s"""<div class=p>Combat is 1 if in a land Area, 0 if in a sea Area.</div>"""),
            (RevenantOfKnaa, 2, "3", "?", s"""<div class=p>Combat equals the number of Desiccated in play.</div>"""),
            (Ghatanothoa,    1, "?", "?", s"""<div class=p>Cost: 11 minus Ritual cost. Combat equals your Power.</div><div class=p><span class=ability-color>Infernal Pact</span> (Ongoing): You may discount the cost of any Action you perform by flipping any number of your faceup spellbooks, reducing that cost by 1 per spellbook flipped.</div>""")
        ))

        // Firstborn (FB): spellbook requirement info card overlays
        case $("FB", FBNoAcolytesInStart.text) => requirement("None of your Acolytes are in your Start Area.")
        case $("FB", FBAwakenGhatanothoa.text) => requirement("Awaken Ghatanothoa.")
        case $("FB", FBTwoFacedownSpellbooks.text) => requirement("Have two facedown spellbooks.")
        case $("FB", FBSecondAwakening.text) => requirement("Awaken Ghatanothoa a second time.")
        case $("FB", FBMostDoomOrMoreGates.text) => requirement("Have more Doom than any other player OR Control more Gates than the First Player.")
        case $("FB", FBThirdAwakening.text) => requirement("Awaken Ghatanothoa a third time.")

        // Firstborn (FB): spellbook info card overlays
        case $("FB", Augury.name) => spellbook("Augury", "Ongoing", "Put a Kill on this spellbook for each Kill that was cancelled or unapplied in a Battle you were in. Whenever you roll a blank (for Battle or Writhe) you may replace it with a Kill from this Spellbook. If this spellbook is flipped, discard all dice on it.")
        case $("FB", Carnage.name) => spellbook("Carnage", "Post-Battle", "If you AND your opponent Killed or Eliminated Units in the Battle, you may pay 1 Power or flip any of your spellbooks facedown (including this one) to gain an Elder Sign.")
        case $("FB", TheEyeOpens.name) => spellbook("The Eye Opens", "Action: Cost 1", "For each Area containing a Desiccated and enemy Cultist(s): choose an Enemy Faction to Eliminate one of their Cultists in that Area then Eliminate your Desiccated in that Area to gain a Power.")
        case $("FB", CyclopeanGaze.name) => spellbook("Cyclopean Gaze", "Ongoing", "Whenever an opponent ends an Action in any Area(s) containing Revenant(s) and/or Ghatanothoa, they must Pain one of their Units from those Areas for each Revenant and/or Ghatanothoa present, with Firstborn choosing where the Units are Pained, following normal Pain rules.")
        case $("FB", DevilsMark.name) => spellbook("Devil's Mark", "Doom Phase", "Place a Crater in a LAND AREA with your Controlled Gate. Destroy all Gates you Control there &amp; gain an Elder Sign for each Gate destroyed. If the Crater was placed in an Area with a Faction Glyph (even your own) gain 1 Power for each Crater in play (including that one).")
        case $("FB", CallOfTheFaithful.name) => spellbook("Call of the Faithful", "Unlimited Action: Cost 0", "Place an Acolyte from your Pool in an Area with Ghatanothoa and/or a Revenant. You may not use this ability in an Area containing one of your Acolytes.")


        // Tombstalker (TS): Cursed Tomes overlay for TS's own card — shows all 11 tomes (white=remaining, grey=given away)
        case $("cursed-tomes", fStyle) if fStyle.toString == "ts" =>
            val givenAway = TSCursedTomesOverlay.tomesOnCard
            val allTomes = TSCursedTomesOverlay.factionTomes
            // Show all 11 tomes in ascending order — white if still on card, grey if given away
            val onCardRows = (1 to 11)./ { n =>
                val text = "Vol. " + tomeNumToRoman(n) + " \u2014 " + TSCursedTomesOverlay.tomeTexts.getOrElse(n, "")
                val color = if (n <= givenAway) "grey" else "white"
                s"""<tr><td style="color:$color;padding:3px 12px;font-size:90%">$text</td></tr>"""
            }.mkString("")
            // Show tomes held by other factions
            val factionRows = allTomes.toList.flatMap { case (fs, tomes) =>
                tomes.sortBy(_._1)./ { case (n, faceDown) =>
                    val text = "Vol. " + tomeNumToRoman(n) + " \u2014 " + TSCursedTomesOverlay.tomeTexts.getOrElse(n, "")
                    val color = if (faceDown) "grey" else "white"
                    s"""<tr><td style="color:$color;padding:3px 12px;font-size:90%"><span class="$fs">[$fs]</span> $text</td></tr>"""
                }
            }.mkString("")
            s"""<table class="requirement-table"><tbody><tr><td style="color:white;padding:4px 12px;font-weight:bold;border-bottom:1px solid grey">Cursed Tomes</td></tr>$onCardRows$factionRows</tbody></table>"""

        // Tombstalker (TS): Cursed Tomes overlay for other factions — shows tomes held (white=face-up, grey=face-down)
        case $("cursed-tomes", fStyle) =>
            val tomes = TSCursedTomesOverlay.factionTomes.getOrElse(fStyle.toString, Nil)
            if (tomes.isEmpty) ""
            else {
                val rows = tomes.sortBy(_._1)./ { case (n, faceDown) =>
                    val text = "Vol. " + tomeNumToRoman(n) + " \u2014 " + TSCursedTomesOverlay.tomeTexts.getOrElse(n, "")
                    val color = if (faceDown) "grey" else "white"
                    s"""<tr><td style="color:$color;padding:3px 12px;font-size:90%">$text</td></tr>"""
                }.mkString("")
                s"""<table class="requirement-table"><tbody><tr><td style="color:white;padding:4px 12px;font-weight:bold;border-bottom:1px solid grey">Cursed Tomes</td></tr>$rows</tbody></table>"""
            }

        case $("RoA") =>
            val pos = RitualTrackOverlay.positions
            val marker = RitualTrackOverlay.markerIndex
            val history = RitualTrackOverlay.ritualHistory
            val trackLen = RitualTrackOverlay.trackLength
            val imgSrc = imageSource(RitualTrackOverlay.trackImageId)

            val markerDiv = if (marker < pos.length) {
                val (mx, my) = pos(marker)
                val isID = marker == pos.length - 1
                if (isID) {
                    // Ellipse centered on the stored ID position (which points to center of "Instant Death" text).
                    // width=20% of track width; aspect-ratio=3.0 gives correct height across all track aspect ratios.
                    s"""<div style="
                        position: absolute;
                        left: ${mx}%;
                        top: ${my}%;
                        width: 20%;
                        aspect-ratio: 3.0;
                        transform: translate(-50%, -50%);
                        border-radius: 50%;
                        border: 0.18em solid #ff2020;
                        box-shadow: 0 0 0.5em 0.2em rgba(255,32,32,0.8), inset 0 0 0.3em rgba(255,32,32,0.3);
                        box-sizing: border-box;
                        pointer-events: none;">
                    </div>"""
                } else {
                    s"""<div style="
                        position: absolute;
                        left: ${mx}%;
                        top: ${my}%;
                        width: 8%;
                        aspect-ratio: 1;
                        transform: translate(-50%, -50%);
                        border-radius: 50%;
                        border: 0.18em solid #ff2020;
                        box-shadow: 0 0 0.5em 0.2em rgba(255,32,32,0.8), inset 0 0 0.3em rgba(255,32,32,0.3);
                        box-sizing: border-box;
                        pointer-events: none;">
                    </div>"""
                }
            } else ""

            val ceremony = RitualTrackOverlay.ritualHistoryCeremony
            val glyphs = history.indexed./ { (style, i) =>
                val isID = i >= trackLen - 1
                // ID glyphs anchor to circle 10 (second-to-last position) and extend right
                val (gx, gy) =
                    if (isID) pos(math.min(trackLen - 2, pos.length - 1))
                    else pos(math.min(i, pos.length - 1))
                val extraOffset = if (isID) (i - (trackLen - 1)) * 9.0 + 7.0 else 0.0
                val finalX = gx + extraOffset
                val isCeremony = i < ceremony.length && ceremony(i)
                // Firstborn (FB) Bug fix Round 3: FB map glyph was shrunk to 60x60 (60% of normal)
                // so the faction icon on the map wouldn't dominate the region, but the RoA track
                // uses a separate render context — the map size change left FB's RoA track marker
                // looking tiny next to every other faction's glyph. Bump FB's RoA track glyph width
                // by 30% (6% -> 7.8%) ONLY on the RoA track; the map render still uses 60x60.
                val glyphWidthPct = if (style == "fb") "7.8%" else "6%"
                if (isCeremony) {
                    // CSS filter applied directly to img — only affects non-transparent pixels,
                    // no square background artifact. sepia(1) gives warm base, hue-rotate shifts to faction hue.
                    val tintFilter = style match {
                        case "gc" => "sepia(1) hue-rotate(67deg) saturate(3) brightness(1.0)"
                        case "cc" => "sepia(1) hue-rotate(181deg) saturate(3) brightness(0.9)"
                        case "bg" => "sepia(1) hue-rotate(323deg) saturate(5) brightness(1.0)"
                        case "ys" => "sepia(1) hue-rotate(12deg) saturate(8) brightness(1.2)"
                        case "ww" => "sepia(1) hue-rotate(168deg) saturate(2) brightness(1.2)"
                        case "sl" => "sepia(1) hue-rotate(344deg) saturate(5) brightness(1.0)"
                        case "ow" => "sepia(1) hue-rotate(241deg) saturate(3) brightness(0.8)"
                        case "an" => "sepia(1) hue-rotate(156deg) saturate(3) brightness(1.0)"
                        case "ts" => "sepia(1) hue-rotate(74deg) saturate(2) brightness(1.3)"
                        // Firstborn (FB): magenta/pink tint for RoA track ceremony glyphs
                        case "fb" => "sepia(1) hue-rotate(295deg) saturate(4) brightness(1.0)"
                        case _    => "sepia(1)"
                    }
                    s"""<img src="${imageSource("n-tulzscha")}"
                        style="
                            position: absolute;
                            left: ${finalX}%;
                            top: ${gy}%;
                            width: ${glyphWidthPct};
                            height: auto;
                            transform: translate(-50%, -50%);
                            opacity: 0.9;
                            pointer-events: none;
                            filter: drop-shadow(0 0 0.15em rgba(0,0,0,0.9)) ${tintFilter};" />"""
                } else {
                    s"""<img src="${imageSource(style + "-glyph")}"
                        style="
                            position: absolute;
                            left: ${finalX}%;
                            top: ${gy}%;
                            width: ${glyphWidthPct};
                            height: auto;
                            transform: translate(-50%, -50%);
                            opacity: 0.9;
                            pointer-events: none;
                            filter: drop-shadow(0 0 0.15em rgba(0,0,0,0.9));" />"""
                }
            }.mkString("")

            // Tombstalker (TS) Pure DH Hecatomb: render TS glyph markers on RoA track for rituals paid entirely with Death's Head
            val pureDHFixedY = RitualTrackOverlay.numPlayers match {
                case 3 => 28.0   // 3p: ID Y=51, one glyph height above
                case 5 => 17.0   // 5p: ID Y=40, one glyph height above
                case _ => 26.0   // 4p: ID Y=49, one glyph height above
            }
            val glyphWidth = 6.0
            var lastPureDHX : Double = -999.0
            var lastPureDHY : Double = -999.0
            val pureDHGlyphHtml = RitualTrackOverlay.tsPureDHMarkerIndices.map { idx =>
                val (baseX, _) = if (idx < pos.length) pos(idx) else pos(pos.length - 1)
                val dhY = pureDHFixedY
                // Check if last pure-DH glyph is in same spot — offset right if so
                val (finalX, finalY) =
                    if ((lastPureDHX - baseX).abs < 1.0 && (lastPureDHY - dhY).abs < 1.0)
                        (lastPureDHX + glyphWidth, dhY)
                    else
                        (baseX, dhY)
                lastPureDHX = finalX
                lastPureDHY = finalY
                s"""<img src="${imageSource("ts-glyph")}"
                    style="
                        position: absolute;
                        left: ${finalX}%;
                        top: ${finalY}%;
                        width: 6%;
                        height: auto;
                        transform: translate(-50%, -50%);
                        opacity: 0.85;
                        pointer-events: none;
                        filter: drop-shadow(0 0 0.15em rgba(0,0,0,0.9)) drop-shadow(0 0 0.3em rgba(100,255,100,0.6));" />"""
            }.mkString("")

            s"""<table style="background: rgba(0,0,0,0.85); width: 100%;">
              <tbody>
                <tr>
                  <td style="text-align: center; vertical-align: middle; padding: 1.5em 0.5em;">
                    <div style="position: relative; display: inline-block; max-width: 100%;">
                      <img src="${imgSrc}"
                           style="display: block; width: 100%; height: auto;" />
                      ${markerDiv}
                      ${glyphs}
                      ${pureDHGlyphHtml}
                    </div>
                  </td>
                </tr>
              </tbody>
            </table>"""

        case _ =>
            // println("onExternalClick " + s.mkString(" | "))
            ""
    }).but("")

    def combat = s"<span class=combat-color>Combat:</span>"

    def cost(s : String) = s"<span class=cost-color>${s}</span>"

    def power(n : Int) = cost(s"${n} Power")

    def loyaltyCard(name : String, quantity : Int, cost : Int, combat : Int, obtainText : String, ability : String, phase : String, abilityText : String) = s"""
        <table class="loyalty-card-table">
            <thead>
                <tr>
                    <th style=width:10%>
                    </th>
                    <th style=width:80%>
                    </th>
                    <th style=width:10%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>
                    </td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="h2 abaddon nt">${name}<sup><span class="deh3 nt">(${quantity.toString})</span></sup></span></div>
                        <img class="img" src="${imageSource("info:" + "n-" + name.toLowerCase.replace("'", "").replace(" ", "-"))}">
                        <div>&nbsp;</div>
                        <div>
                            <span class="cost-color black-border">Cost: ${cost.toString}</span>
                        </div>
                        <div>
                            <span class="combat-color black-border">Combat: ${combat.toString}</span>
                        </div>
                        <div>&nbsp;</div>
                        <div class="black-border">
                            <span class="nt">${obtainText}</span>
                        </div>
                        <div>&nbsp;</div>
                        <div class="black-border">
                            <span class="ability-color">${ability} </span><span class="cost-color">(${phase})</span><span class="nt">: ${abilityText}</span>
                        </div>
                    </td>
                    <td>
                    </td>
                </tr>
                <tr>
                    <td>
                    </td>
                    <td>
                    </td>
                    <td>
                    </td>
                </tr>
            </tbody>
        </table>"""

    // Round 8 Bug 40: added facedown parameter. When true, spellbook title and text
    // are shown with strikethrough (text-decoration: line-through) to indicate the
    // spellbook has been flipped facedown via Infernal Pact and its power is disabled.
    def loyaltyCardIGOO(name : String, cost : String, combat : String, hasSpellbook : Boolean, obtainText : String, ability : String, abilityPhase : String, abilityText : String, spellbookRequirement : String, spellbook : String, spellbookPhase : String, spellbookText : String, facedown : Boolean = false) = {
        val dimmedClass = if (hasSpellbook) "" else " dimmed"
        val strikeStyle = if (facedown) " style=\"text-decoration: line-through\"" else ""
        s"""<table class="loyalty-card-table">
            <thead>
                <tr>
                    <th style="width:10%"></th>
                    <th style="width:38%"></th>
                    <th style="width:4%"></th>
                    <th style="width:38%"></th>
                    <th style="width:10%"></th>
                </tr>
            </thead>
            <tbody>
                <tr><td colspan="4">&nbsp;</td></tr>
                <tr><td colspan="4">&nbsp;</td></tr>
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="h2 abaddon nt">${name}</span></div>
                        <img class="img" src="${imageSource("info:" + "n-" + name.toLowerCase.replace("'", "").replace(" ", "-"))}">
                        <div>&nbsp;</div>
                        <div>
                            <span class="cost-color black-border">Cost: ${cost}</span>
                        </div>
                        <div>
                            <span class="combat-color black-border">Combat: ${combat}</span>
                        </div>
                    </td>
                    <td></td>
                    <td>
                        <div class="black-border">
                            <span class="nt">${obtainText}</span>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td colspan="4">&nbsp;</td></tr>
                <tr>
                    <td></td>
                    <td>
                        <div class="black-border">
                            <span class="ability-color">${ability}</span>
                            <span class="cost-color"> (${abilityPhase})</span>
                            <span class="nt">: ${abilityText}</span>
                        </div>
                    </td>
                    <td></td>
                    <td>
                        <div class="black-border">
                            <span class="nt">${spellbookRequirement}:</span>
                        </div>
                        <div>&nbsp;</div>
                        <div class="black-border"${strikeStyle}>
                            <span class="ability-color$dimmedClass">${spellbook}</span>
                            <span class="cost-color$dimmedClass"> (${spellbookPhase})</span>
                            <span class="nt$dimmedClass">: ${spellbookText}</span>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td colspan="4">&nbsp;</td></tr>
                <tr><td colspan="4">&nbsp;</td></tr>
            </tbody>
        </table>"""
    }

    def libraryHintCard() = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:10%></th>
                    <th style=width:80%></th>
                    <th style=width:10%></th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border"><span class="lb inline-block">Library at Celaeno</span></div>
                        <div class="white-border">
                            <div class="p black-border"><span class="ability-color">First Doom Phase Only</span> <span class="nt">&mdash; Gates appear at Library Tome Areas, if not already present</span></div>
                            <div class="p black-border"><span class="ability-color">All Doom Phases</span> <span class="nt">&mdash; Each player receives a silence token</span></div>
                            <div class="p black-border"><span class="ability-color">All Gather Power Phases</span> <span class="nt">&mdash; Each player loses any remaining silence token</span></div>
                        </div>
                        <div>&nbsp;</div>
                        <div class="h1 black-border"><span class="lb inline-block">Stairwells</span></div>
                        <div class="white-border">
                            <div class="p nt black-border">Certain Areas contain Stairwells; these are lettered A&ndash;F on each floor. These Areas are adjacent to their matching Areas on the other level. For instance, in the 3-Player side of the Lower Floor, the Chamber of Apkallu contains Stairwell D. It is therefore adjacent to the Area of Barrier of Naach-Tith, which is also marked with a D but is on the Upper Floor (for both 3- and 5-player sides).</div>
                            <div class="p nt black-border">A Pained Unit in a Stairwell Area can go through the Stairwell to the other Floor, because these Areas are adjacent for all purposes.</div>
                        </div>
                        <div>&nbsp;</div>
                        <div class="h1 black-border"><span class="lb inline-block">Archways</span></div>
                        <div class="white-border">
                            <div class="p nt black-border">Several Areas contain Archways. These are similar to Stairwells, except that they are not lettered. For purposes of the Move Action and movement-type abilities, each Archway Area is adjacent to EVERY OTHER Archway Area on both Floors of the Library.</div>
                            <div class="p nt black-border">However, unlike the Stairwells, the Archways are NOT adjacent when being Pained or Retreated. Players using other movement-type abilities to travel may do so freely between the two boards. For instance, Shub-Niggurath can Avatar to either side, Crawling Chaos Hunting Horrors can use Seek and Destroy to access either board, Cthulhu can use Submerge to hit both boards, etc.</div>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td></td><td></td><td></td></tr>
            </tbody>
        </table>"""

    def mapUnitOverlay(name : String, rulesText : String, dieText : String) = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:15%></th>
                    <th style=width:70%></th>
                    <th style=width:15%></th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border"><span class="lb inline-block">${name}</span></div>
                        <div class="black-border"><span class="cost-color">Action Cost: 0</span></div>
                        <div>
                            ${rulesText}
                            <div class="p black-border"><span class="cost-color">${dieText}</span></div>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td></td><td></td><td></td></tr>
            </tbody>
        </table>"""

    def tomeOverlay(name : String, phase : String, text : String, faceUp : Boolean = true) = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:15%></th>
                    <th style=width:70%></th>
                    <th style=width:15%></th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td></td>
                    <td>
                        <div class="h1 black-border"><span class="lb inline-block">${name}</span> <span class="cost-color inline-block">${if (phase.nonEmpty) "(" + phase + ")" else ""}</span></div>
                        <div class="white-border">
                            <div class="nt black-border" ${if (!faceUp) "style='opacity:0.4'" else ""}>${text}</div>
                        </div>
                    </td>
                    <td></td>
                </tr>
                <tr><td></td><td></td><td></td></tr>
            </tbody>
        </table>"""

    def spellbook(name : String, phase : String, text : String) = s"""
        <table class="spellbook-table" style="">
            <thead>
                <tr>
                    <th style=width:20%>
                    </th>
                    <th style=width:60%>
                    </th>
                    <th style=width:20%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>
                    </td>
                    <td>
                        <div class="h1 black-border" style="margin-right: -3ex; margin-left: -3ex; "><span class="ability-color inline-block">${name}</span> <span class="cost-color inline-block">(${phase})</span></div>
                        <div class="white-border">
                            ${text}
                        </div>
                    </td>
                    <td>
                    </td>
                </tr>
                <tr>
                    <td>
                    </td>
                    <td>
                    </td>
                    <td>
                    </td>
                </tr>
            </tbody>
        </table>"""

    def requirement(text : String) = s"""
        <table class="requirement-table" style="">
            <thead>
                <tr>
                    <th style=width:20%>
                    </th>
                    <th style=width:60%>
                    </th>
                    <th style=width:20%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td>
                    </td>
                    <td>
                        <div class="white-border">
                            ${text}
                        </div>
                    </td>
                    <td>
                    </td>
                </tr>
                <tr>
                    <td>
                    </td>
                    <td>
                    </td>
                    <td>
                    </td>
                </tr>
            </tbody>
        </table>"""

    def ref(spellbook : Spellbook) = s"""<span class=ability-color>${spellbook.name}</span>"""

    def reference(f : Faction, spellbook : Spellbook) = s"""<span class="ability-color pointer" onclick="onExternalClick('${f.short}', '${spellbook.name}')">${spellbook.name}</span>"""

    def faction(f : Faction, background : String, unique : Spellbook, uniquePhase : String, uniqueText : String, miscSpellbooks : $[Spellbook], units : $[(UnitClass, Int, String, String, String)]) = s"""
        <table class="faction-table" style="background-image:url(${imageSource(background)})">
            <thead>
                <tr>
                    <th style=width:9%>
                    </th>
                    <th style=width:13%>
                    </th>
                    <th style=width:4%>
                    </th>
                    <th style=width:7%>
                    </th>
                    <th style=width:7%>
                    </th>
                    <th style=width:60%>
                    </th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td colspan=6>
                        <div style="padding-left: 3ex; padding-right: 3ex; padding-bottom: 1ex;">
                            <div class="h1 abaddon">${f.name}</div>
                            <div class="border-outer">
                                <div class="border-inner">
                                    <span class=ability-color>${unique.name}</span>
                                    <span class=cost-color>(${uniquePhase}):</span>
                                    <span>${uniqueText}</span>
                                </div>
                            </div>
                        </div>
                    </td>
                </tr>
                ${
                    if (miscSpellbooks.any) { s"""
                        <tr>
                            <td colspan=6>
                                <div style="padding-left: 3ex; padding-right: 3ex; padding-bottom: 1ex;">
                                    Spellbooks:
                                    ${
                                        miscSpellbooks./{ sb =>
                                            s"""${reference(f, sb)}"""
                                        }.join(", ")
                                    }
                                </div>
                            </td>
                        </tr>"""
                    }
                    else
                        ""
                }
                <tr>
                    <td colspan=2>
                        <div class=h3>Unit<sup><span class="deh3">(Total)</span></sup></div>
                    </td>
                    <td>
                    </td>
                    <td>
                        <div class=h3><span class=cost-color>Cost</span></div>
                    </td>
                    <td>
                        <div class=h3><span class=combat-color>Combat</span></div>
                    </td>
                    <td>
                        <div class=h3>Notes</div>
                    </td>
                </tr>
                <tr>
                    <td colspan=6>
                        <div class="separator">
                        </div>
                    </td>
                </tr>
                ${
                    units./{ case (uc, n, c, b, t) => s"""
                        <tr>
                            <td>
                                <img class="img" src=${imageSource("info:" + f.short.toLowerCase + "-" + uc.name.toLowerCase.replace(" ", "-"))}>
                            </td>
                            <td>
                                <div class="unit-desc">
                                    <div class="p"><span class=unit-name>${uc.name}${(n > 1).??(s"""<sup>(${n})</sup>""")}</div>
                                    <div class="p"><span class=unit-type>${uc.utype.name.replace("GOO", "Great Old One")}</div>
                                </div>
                            </td>
                            <td>
                            </td>
                            <td>
                                <span class=cost-color>${c}</span>
                            </td>
                            <td>
                                <span class=combat-color>${b}</span>
                            </td>
                            <td>
                                <div class="notes">
                                    ${t}
                                </div>
                            </td>
                        </tr>"""
                    }.join("""
                        <tr>
                            <td colspan=6>
                                <div class="separator">
                                </div>
                            </td>
                        </tr>""")
                }
            </tbody>
        </table>"""

}
