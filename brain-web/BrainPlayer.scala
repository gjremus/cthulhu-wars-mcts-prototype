package cws

import hrf.colmat._
import org.scalajs.dom

// ─────────────────────────────────────────────────────────────────────────────
// CLIENT-SIDE BRAIN FACADE — the /brain/ opponent's entry point.
//
// Owns the self-play-trained neural nets (policy + value), a single shared
// MCTSPolicy, and the one-time async fetch of the champion checkpoint weights.
// Exposes a SYNCHRONOUS decide() that the game loop can call on the brain's turn.
//
// WHY A SINGLETON. The browser plays exactly one game at a time on the main thread,
// and the loaded nets are immutable, so one MCTSPolicy is reused across every brain
// decision and across games. `recorder` stays null (no self-play recording in the
// browser); `rootPos` is set fresh before each decide() so the fork-by-replay clone
// rebuilds the CURRENT position (see ClientClone / MCTS.simulateOnce).
//
// ASYNC LOAD, SYNC PLAY. Weights are ~2 MB of text fetched over XHR — asynchronous —
// but the game loop is synchronous and cannot block. So decide() falls back to the
// caller-supplied bot move until the nets finish loading. In practice the fetch
// completes during setup/first turn, so at most the first brain decision or two use
// the fallback bot; every decision after that is the real brain. A parse failure
// (corrupt/absent asset) latches `failed` and the brain permanently plays the bot
// fallback — the game never crashes.
// ─────────────────────────────────────────────────────────────────────────────

object BrainPlayer {

    // Champion tensor dims (best.meta: dinS=777 dinA=128 hidden=64). ClientCheckpoint
    // requires an exact match or returns None, so a mismatched asset fails cleanly.
    private val DinS   = 777
    private val DinA   = 128
    private val Hidden = 64

    // MCTS simulations per move. The champion TRAINS at CW_SIMS=160, but each browser
    // simulation is a full fork-by-replay of the game so far — latency scales with both
    // sims AND game length. 40 keeps a mid-game move responsive on a typical device
    // while still giving the search real lookahead. Bump if devices prove fast enough.
    val Sims = 40

    private var mcts    : MCTSPolicy = null
    private var loading : Boolean = false
    private var failed  : Boolean = false

    /** True once the nets are parsed and the search is ready to play. */
    def ready : Boolean = mcts != null

    /** Fetch the two checkpoint assets (relative to the page) and build the shared
     *  MCTSPolicy. Idempotent and self-guarding: safe to call on every brain decision
     *  and at game start; only the first call does work. `base` is the URL prefix the
     *  weights are served under (""=same directory as index.html, i.e. /brain/). */
    def ensureLoading(base : String = "") : Unit = {
        if (mcts != null || loading || failed) return
        loading = true
        fetchText(base + "best.policy") { pText =>
            fetchText(base + "best.value") { vText =>
                (ClientCheckpoint.loadPolicy(pText, DinS, DinA, Hidden),
                 ClientCheckpoint.loadValue(vText, DinS, Hidden)) match {
                    case (Some(p), Some(v)) =>
                        mcts = new MCTSPolicy(sims = Sims, leaf = PolicyValueEval(p, v))
                        loading = false
                        println("BrainPlayer: nets loaded (sims=" + Sims + ")")
                    case _ =>
                        failed = true; loading = false
                        println("BrainPlayer: checkpoint parse failed — falling back to bot play")
                }
            }
        }
    }

    /** Decide a move for `faction`. `exploded` MUST already be Explode.explode'd (the
     *  brain, like the self-play searcher, decides over performable leaves, not raw
     *  menu actions). `pos` is the fork-by-replay Position for the CURRENT game. Until
     *  the nets load (or if loading failed), returns `fallback` — a by-name bot move
     *  evaluated only when needed. */
    def decide(game : Game, faction : Faction, exploded : $[Action],
               pos : ClientClone.Position, fallback : => Action) : Action = {
        if (mcts == null) fallback
        else {
            mcts.rootPos = pos
            mcts.decide(game, faction, exploded)
        }
    }

    /** Begin a RESUMABLE (chunked) search for the browser so the UI can paint a live
     *  "Brain X% thinking…" overlay while the search runs. Returns None until the nets
     *  finish loading (the caller then plays the bot fallback for that one decision, as
     *  `decide` does). `pos` is the fork-by-replay Position for the CURRENT game state. */
    def beginSearch(game : Game, faction : Faction, exploded : $[Action],
                    pos : ClientClone.Position) : Option[MCTSPolicy#Search] = {
        if (mcts == null) None
        else {
            mcts.rootPos = pos
            Some(mcts.beginSearch(game, faction, exploded))
        }
    }

    // Minimal XHR text GET. Self-contained (no coupling to the driver's helpers) so the
    // facade stays a standalone unit. onerror latches nothing here — ensureLoading's
    // outer guard prevents re-entry; a network failure simply never completes the load
    // and decide() keeps using the fallback.
    private def fetchText(url : String)(then : String => Unit) : Unit = {
        val xhr = new dom.XMLHttpRequest()
        xhr.onload  = (_ : dom.Event)         => then(xhr.response.asInstanceOf[String])
        xhr.onerror = (_ : dom.ProgressEvent) => { failed = true; loading = false }
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }
}
