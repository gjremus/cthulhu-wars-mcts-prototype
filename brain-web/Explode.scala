package cws

import hrf.colmat._

object Explode {
    def explode(game : Game, actions : $[Action]) : $[Action] = {
        var result : $[Action] = $

        // Round 8 (FB): track Soft actions we've already explored in this
        // explosion so we don't infinitely recurse through Soft sub-menus that
        // cycle back to themselves. Concretely the FB Infernal Pact main button
        // (FBInfernalPactMainAction) is Soft and its Done sub-action forces
        // back to MainAction(FB), which renders the same FBInfernalPactMainAction
        // button again. Without a visited set, Explode.process recurses on the
        // same Soft action forever and crashes with StackOverflowError.
        // Other factions don't hit this because their Soft menu buttons either
        // disappear after being clicked once (oncePerAction state changes) or
        // their Done sub-action forces back to a non-MainAction continuation.
        // FB IP can be re-clicked unconditionally as long as faceUpSpellbooks
        // exist and !f.acted, so the menu recurses.
        val visited = scala.collection.mutable.Set.empty[Action]

        def process(actions : $[Action]) {
            val aaa = actions.unwrap
            var aa = aaa.%(_.isMore).some.|(aaa)

            aa = aa.%!(_.isCancel).%!(_.isInfo)

            aa = aa.distinct

            aa.%(_.isSoft).foreach { a =>
                if (!visited.contains(a)) {
                    visited += a
                    val (_, c) = game.perform(a)
                    c match {
                        case Ask(_, actions) => process(actions)
                        case Then(then) => process($(then))
                        case Force(action) => process($(action))
                        case DrawES(_, 0, 0, 0, draw) => process($(draw(0, true)))
                        case DrawES(_, es1, es2, es3, draw) => process($(draw((es1.times(1) ++ es2.times(2) ++ es3.times(3)).maxBy(_ => random()), false)))
                        case MultiAsk(asks) => asks.foreach { case Ask(_, actions) => process(actions); case _ => }
                        case DelayedContinue(_, inner) => // skip delayed continues in exploration
                        case _ => // ignore other continue types (GameOver, etc.)
                    }
                }
            }

            aa.%!(_.isSoft).foreach { a =>
                result :+= a
            }
        }

        process(actions)

        result
    }

    def isOffense(friend : Faction)(a : Action)(implicit game : Game) = a match {
        case MoveAction(self, _, _, dest, cost) if self != friend && friend.gates.has(dest) => true
        case AttackAction(_, _, f, _) if f == friend => true
        case CaptureAction(_, _, f, _) if f == friend => true
        case AvatarAction(self, _, _, f) if self != friend && f == friend => true
        case ThousandFormsAskAction(f, _, _, _, _, _, power) if f == friend && power > 0 => true
        case DreamsAction(_, _, f) if f == friend => true
        case UnsubmergeAction(self, r) if self != friend && friend.gates.contains(r) => true
        case HWINTBNAction(self, _, r) if self != friend && friend.gates.contains(r) => true
        case ShriekAction(self, r) if self != friend && friend.gates.contains(r) => true

        case BuildGateAction(self, r) if game.turn == 1 => false
        case BuildGateAction(GC, r) if game.starting(GC) == r => false
        case BuildGateAction(WW, r) if game.board.starting(WW).contains(r) => false
        case BuildGateAction(CC, r) if CC.all(Nyarlathotep).none => false
        case BuildGateAction(YS, r) if YS.all(Hastur).none => false
        // Implement something for AN here? It's used for survival mode.
        case BuildGateAction(_, r) => true

        case _ => false
    }

}
