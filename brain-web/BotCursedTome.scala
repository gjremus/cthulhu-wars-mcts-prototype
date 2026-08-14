package cws

import hrf.colmat._

/**
 * BotCursedTome — shared decision logic for Cursed Tome (Tombstalker Eleven Revelations).
 *
 * Usage:
 *   Bot3 / faction bots call BotCursedTome.scoreUseTome(self, tomeNum) for TSUseTomeAction.
 *   BotTS calls BotCursedTome.scoreGiveTomeTo(f) for TSElevenRevelationsAction.
 *   BotTS calls BotCursedTome.willLikelyUseTome(f, tomeNum) for ElevenRevelations spellbook
 *     acquisition scoring and TSElevenRevelationsMainAction scoring.
 *
 * Tome mechanics:
 *   - Using a tome: the holder gains +1 power (free action, PowerNeutral).
 *     The tome flips face-down. TS gets a benefit based on tome number.
 *   - Face-down tomes cost -1 doom each at game end (non-TS factions only).
 *   - After any ritual, the ritualing faction may remove ONE face-down tome for free.
 *   - TS is exempt from all face-down tome penalties.
 *
 * Tome benefits to TS (by tome number):
 *   I-II:     TS places a Tomb Herd at a controlled gate
 *   III-IV:   TS places a Deep Tendril at a controlled gate
 *   V-VI:     TS gains 1 doom
 *   VII-VIII: TS gains Death's Head equal to # Tomb Herds on map
 *   IX-X:     TS gains 1 Elder Sign (~1.67 expected doom)
 *   XI:       TS gains max(0, ritualCost - 5) doom (0-3 depending on track position)
 *
 * All weights use |=> N -> "desc" format so tune_ts.py can optimize them.
 */
object BotCursedTome {

    /**
     * Expected score gain for TS when the given tome is used.
     * Factual approximation of TS benefit — not a tunable weight.
     */
    def tsGainForTome(tomeNum : Int)(implicit game : Game) : Int = {
        val tsHasManyTombs = game.factions.has(TS) && TS.onMap(TombHerd).num >= 3
        tomeNum match {
            case n if n >= 9 => 500                                 // IX-X: ~1.67 doom via ES
            case n if n >= 7 => if (tsHasManyTombs) 700 else 200   // VII-VIII: DH × TombHerds
            case n if n >= 5 => 300                                 // V-VI: 1 doom
            case n if n >= 3 => 200                                 // III-IV: Deep Tendril
            case _           => 250                                 // I-II: Tomb Herd
        }
    }

    /**
     * Score for a non-TS faction evaluating whether to use a Cursed Tome.
     * Positive total = use the tome; negative total = skip this turn.
     *
     * Trade-off: using gives +1 power now, but leaves a face-down tome that
     * costs -1 doom at game end unless removed via a post-ritual free discard.
     */
    def scoreUseTome(self : Faction, tomeNum : Int)(implicit game : Game) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=>(e : (Int, String)) { if (bool) result :+= Evaluation(e._1, e._2) }
        }

        val power         = self.power
        val ritualCost    = game.ritualCost
        val faceDownCount = game.cursedTomesOwned.get(self).|(Nil).count { case (_, fd) => fd }
        val others        = game.factions.but(self)
        val avgOtherPower = if (others.any) others./(_.power).sum.toDouble / others.num else 0.0
        val tsCritical    = game.factions.has(TS) && (TS.doom >= 26 || (TS.gates.num >= 3 && TS.spellbooks.num >= 5))
        val instantDeathNow  = game.ritualTrack(game.ritualMarker) == 999 || game.factions.%(_.doom >= 30).any
        val instantDeathNext = !instantDeathNow && game.ritualTrack(game.ritualMarker + 1) == 999

        // ── Base: ALWAYS use tomes — they're FREE power with no action cost ──
        // +1 power NOW is almost always worth -1 doom LATER (removed after ritual)
        true |=> 2000 -> "base: tome is free power, ALWAYS use"

        // ── Risk from existing face-down tomes ─────────────────────────────────
        // Each face-down tome costs -1 doom at game end; stack of them is crippling
        (faceDownCount >= 3) |=> -800 -> "3+ face-down tomes: severe end-game doom risk"
        (faceDownCount == 2) |=> -500 -> "2 face-down tomes: high end-game doom risk"
        (faceDownCount == 1) |=> -217 -> "1 face-down tome: moderate doom risk"

        // ── Enables ritual this turn: discard face-down for free → net cost 0 ──
        val enablesRitual = self.acted.not && power + 1 >= ritualCost && power < ritualCost
        enablesRitual |=> 1200 -> "tome enables ritual: face-down discarded immediately, net gain"

        // Can already ritual → extra power is low-risk if no face-down tomes yet
        val canAlreadyRitual = self.acted.not && power >= ritualCost
        (canAlreadyRitual && faceDownCount == 0) |=> 300 -> "can ritual anyway: safely discardable"

        // ── Stall rescue: at low power, tome is a free stall + action ────────────
        (power == 0) |=> 1500 -> "stall rescue: 0 power, tome enables action"
        (power == 1) |=> 1200 -> "low power: extra action has high value"
        (power == 2) |=> 800 -> "low power: tome extends turn"
        (power <= 3) |=> 500 -> "moderate power: free stall is valuable"

        // ── Relative power: behind others = tome is more valuable ──────────────
        (power <= 2 && avgOtherPower >= 5) |=> 500 -> "very low vs others: tome is critical stall"
        (power < avgOtherPower - 2) |=> 343 -> "well below avg power: extra power helps"
        (power < avgOtherPower - 4) |=> 300 -> "far below avg power: urgently need action"

        // ── Gate defense: threatened gate could be better defended ─────────────
        // Holding an extra unit on a threatened gate delays capture / deters attack
        val threatenedGate = self.gates.%(r =>
            game.factions.but(self).exists(f => f.power > 0 && f.at(r).any)
        ).any
        threatenedGate |=> 350 -> "threatened gate: extra power supports defense"

        // ── Gate steal: can capture an enemy gate with self's current position ──
        // If already positioned at a lightly-defended enemy gate, power enables capture
        val canStealGate = game.board.regions.%(r =>
            game.gates.has(r) && !self.gates.has(r) &&
            self.at(r).any &&
            game.factions.but(self).exists(f => f.gates.has(r) && f.at(r).monsterly.none && f.at(r).goos.none)
        ).any
        canStealGate |=> 300 -> "positioned to steal enemy gate: extra power helps"

        // ── Gate build opportunity: 1 power away from affording a gate ─────────
        val wantGate = power + 1 >= 3 && power < 3 && game.board.regions.%(r =>
            self.at(r).cultists.any && !game.gates.has(r) &&
            !game.factions.exists(f => f != self && f.power > 0 && f.at(r).any)
        ).any
        wantGate |=> 400 -> "tome enables gate build"

        // ── Urgency: instant death track ───────────────────────────────────────
        instantDeathNow  |=> 500 -> "instant death now: use everything available"
        instantDeathNext |=> 200 -> "instant death next round: conserve nothing"

        // ── TS danger: tome benefit could clinch their win ──────────────────────
        tsCritical |=> -400 -> "TS near win: tome benefit accelerates their victory"

        // Tome IX-X specifically: TS gains an Elder Sign (~1.67 doom) — extra dangerous
        (tsCritical && tomeNum >= 9 && tomeNum <= 10) |=> -605 -> "tome IX-X: TS gains ES, dangerous when near win"

        result
    }

    /**
     * Returns true if the faction is likely to net-positive on using this tome.
     * Used by BotTS to assess whether a tome-give will actually trigger TS's benefit.
     */
    def willLikelyUseTome(f : Faction, tomeNum : Int)(implicit game : Game) : Boolean = {
        scoreUseTome(f, tomeNum)./(_.weight).sum > 0
    }

    /**
     * Score for TS choosing which faction to give a tome to (TSElevenRevelationsAction).
     *
     * TS wants targets that will ACTUALLY USE the tome (triggering TS's benefit),
     * but who won't win the game because of the extra power.
     */
    def scoreGiveTomeTo(f : Faction)(implicit game : Game) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=>(e : (Int, String)) { if (bool) result :+= Evaluation(e._1, e._2) }
        }

        // [2026-04-04] tsTomesOnCard = # given away. Next tome to give = given + 1.
        val tomeNum    = game.tsTomesOnCard + 1
        val ritualCost = game.ritualCost
        val others     = game.factions.but(f)
        val avgOtherPower = if (others.any) others./(_.power).sum.toDouble / others.num else 0.0

        // Base value of this tome to TS — objective reference, not tunable
        result :+= Evaluation(tsGainForTome(tomeNum), "tome TS benefit (base)")

        // Will they actually use it? No use → no TS benefit triggers
        val likelyToUse = willLikelyUseTome(f, tomeNum)
        likelyToUse    |=> 400  -> "target likely to use tome: TS benefit will activate"
        (!likelyToUse) |=> 257 -> "target unlikely to use tome: TS benefit wasted"

        // Power level: 0 power = won't use; near ritual = will use AND discard
        (f.power == 0) |=> -743 -> "target at 0 power: cannot use tome"
        (f.power == 1) |=> 300  -> "target at 1 power: tome enables meaningful extra action"
        (f.power >= 3) |=> 200  -> "target has plenty of power: will use tome"

        // Near-ritual target: will use AND discard immediately (no doom reluctance)
        val fCanRitualWithTome = f.acted.not && f.power + 1 >= ritualCost
        fCanRitualWithTome |=> 545 -> "target can ritual with tome: will use and discard"

        // Power-starved targets have stronger incentive to use the tome
        (f.power < avgOtherPower - 2) |=> 300 -> "target power-starved: strong incentive to use tome"

        // Threatened gate: faction motivated to use power for defense
        val fHasThreatenedGate = f.gates.%(r =>
            game.factions.but(f).exists(e => e.power > 0 && e.at(r).any)
        ).any
        fHasThreatenedGate |=> 200 -> "target has threatened gate: motivated to use power"

        // Factions with many units are generally more active (more to do with power)
        (f.units.num >= 8) |=> 200 -> "target many units: motivated to use power"

        // NEVER give to doom leaders near 30 — extra power could clinch their win
        val fIsLeader   = f.hasAllSB && f.doom >= 26
        val fIsFinalist = game.factions.filter { g =>
            val p = 3 * g.doom + 6 * g.gates.num + 5 * g.es.num
            p >= 30 * 3
        }.has(f)
        fIsLeader   |=> -600   -> "target doom leader near 30: extra power is risky"
        fIsFinalist |=> -1000  -> "target ofinale: giving power could let them win"

        // Never give to self
        (f == TS) |=> -99999 -> "cant give to self"

        result
    }
}
