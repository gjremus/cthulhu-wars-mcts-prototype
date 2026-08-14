package cws

import org.scalajs.dom
import org.scalajs.dom.html

import hrf.colmat._

import util.canvas._

// ── GLYPH PLACEMENT ENGINE ──
// Round 8 Bug 53: shared engine for placing dynamic-start faction glyphs on the map.
// Used by FB, TS, and any future dynamic-start factions. Centralized so the algorithm
// stays consistent across factions.
//
// Algorithm:
//   1. Compute the Chebyshev distance from every pixel to the nearest non-region pixel
//      (i.e., the radius of the largest axis-aligned box centered at that pixel that
//      fits entirely within the region). Standard 2-pass distance transform — O(N) per
//      pass, exact answer.
//   2. Filter candidates where dist >= halfGlyph (glyph box fits entirely in region).
//   3. Compute gate overlap area for each candidate (gate is 76x76 = halfGate 38).
//      overlap on each axis = max(0, halfGate + halfGlyph - |delta|), area = ox * oy.
//   4. Pick the candidate with the smallest gate overlap (PRIMARY), tiebreaking by
//      the largest distance to boundary (== most central in the largest empty space).
//
// Guarantees:
//   - Zero region-border overlap (mandatory)
//   - Minimized gate overlap (preference)
//   - Centered in the largest contiguous empty space within the region
//
// Caller pattern (from CthulhuWarsSolo.scala):
//   val placer = new GlyphPlacement(board.id)
//   if (game.setup.has(NEW_FACTION) && game.starting.get(NEW_FACTION).contains(r)) {
//       val (glyphX, glyphY) = placer.findStaticGlyphPos(px, py, halfGlyph = 33)
//       fixed +:= DrawItem(r, NEW_FACTION, FactionGlyph, Alive, $, glyphX, glyphY)
//   }
class GlyphPlacement(boardId : String) {
    private val mplace : html.Image = dom.document.getElementById(boardId + "-place").asInstanceOf[html.Image]
    private val placeb = new Bitmap(mplace.width, mplace.height)
    placeb.context.drawImage(mplace, 0, 0)
    private val placed = placeb.context.getImageData(0, 0, placeb.width, placeb.height).data
    val place : Array[Array[Int]] = Array.tabulate(placeb.width, placeb.height)((x, y) =>
        placed((y * placeb.width + x) * 4) * 0x010000 +
        placed((y * placeb.width + x) * 4 + 1) * 0x0100 +
        placed((y * placeb.width + x) * 4 + 2)
    )
    val width : Int = placeb.width
    val height : Int = placeb.height

    // Random valid placement within the same region as (x, y) — used by the unit layout engine.
    // Constrains search to the same vertical half to avoid cross-floor color collisions
    // on Library maps where joined bitmaps may share colors between floors.
    def findAnother(x : Int, y : Int) : (Int, Int) = {
        if (x < 0 || x >= placeb.width || y < 0 || y >= placeb.height)
            return (x, y)
        val p = place(x)(y)
        val halfH = placeb.height / 2
        val yMin = if (y < halfH) 0 else halfH
        val yMax = if (y < halfH) halfH else placeb.height
        var xx = 0
        var yy = 0
        var tries = 0
        do {
            xx = (placeb.width * scala.math.random()).toInt
            yy = yMin + ((yMax - yMin) * scala.math.random()).toInt
            tries += 1
            if (tries > 5000) return (x, y) // safety valve
        }
        while (place(xx)(yy) != p)
        (xx, yy)
    }

    // Deterministic position for a faction glyph.
    // gx, gy: gate center coordinates (the glyph should avoid overlapping the gate).
    // halfGlyph: half-size of the glyph's bounding box (e.g., 33 for a 66x66 glyph).
    // halfGate: half-size of the gate sprite (default 38 for the standard 76x76 gate).
    //
    // Round 8 Bug 55: the place map has BLACK (0,0,0) "gap" pixels between regions that
    // are NOT visible on the rendered map (the visual map blends regions together). Some
    // of these gaps visually belong to one region, others to a neighboring region.
    //
    // Algorithm:
    //   1. Build the "effective region" = the place map's core region pixels PLUS all
    //      adjacent gap pixels within MaxExpansion (12) pixels of the core. We use BFS
    //      from the core into gap pixels only. This expands the region into nearby
    //      visual gutters but stops before crossing into a neighboring region's visual
    //      area (because gaps near other regions are reached from THEIR core first if
    //      they're closer).
    //   2. Compute Chebyshev distance from each effective-region pixel to the nearest
    //      non-effective-region pixel (boundary of the expanded region).
    //   3. Filter candidates with dist >= halfGlyph (60x60 box fits in effective region).
    //   4. Pick the candidate with the smallest gate overlap area; tiebreak by largest
    //      distance to boundary (most central in the largest empty space).
    //
    // Constraint: the chosen position itself must be in the place map's core region
    // (not a gap pixel) so the glyph CENTER is in the playable area.
    def findStaticGlyphPos(gx : Int, gy : Int, halfGlyph : Int = 33, halfGate : Int = 38, maxRadius : Int = 200) : (Int, Int) = {
        // Bounds check: if gate coords exceed bitmap, return gate coords as fallback
        if (gx < 0 || gx >= placeb.width || gy < 0 || gy >= placeb.height)
            return (gx, gy)
        val regionColor = place(gx)(gy)
        val GAP = 0
        val MaxExpansion = 12
        val MaxRadiusFromGate = maxRadius
        val w = placeb.width
        val h = placeb.height
        val INF = w + h

        // BFS from core region pixels into adjacent gap pixels (max MaxExpansion steps).
        // Result: `effective` is true for pixels that visually belong to this region.
        val effective = Array.fill(w, h)(false)
        // Use parallel arrays for the BFS queue (avoids tuple boxing in scalajs)
        val qx = scala.collection.mutable.Queue[Int]()
        val qy = scala.collection.mutable.Queue[Int]()
        val qd = scala.collection.mutable.Queue[Int]()
        var iy = 0
        while (iy < h) {
            var ix = 0
            while (ix < w) {
                if (place(ix)(iy) == regionColor) {
                    effective(ix)(iy) = true
                    qx.enqueue(ix); qy.enqueue(iy); qd.enqueue(0)
                }
                ix += 1
            }
            iy += 1
        }
        while (qx.nonEmpty) {
            val x = qx.dequeue(); val y = qy.dequeue(); val d = qd.dequeue()
            if (d < MaxExpansion) {
                var k = 0
                val deltas = Array((-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1))
                while (k < 8) {
                    val (dx, dy) = deltas(k)
                    val nx = x + dx
                    val ny = y + dy
                    if (nx >= 0 && nx < w && ny >= 0 && ny < h && !effective(nx)(ny) && place(nx)(ny) == GAP) {
                        effective(nx)(ny) = true
                        qx.enqueue(nx); qy.enqueue(ny); qd.enqueue(d + 1)
                    }
                    k += 1
                }
            }
        }

        // dist(x)(y) = Chebyshev distance from (x, y) to nearest non-effective-region pixel
        val dist = Array.tabulate(w, h)((x, y) => if (effective(x)(y)) INF else 0)

        // Forward pass: top-left to bottom-right, considering 4 backward neighbors
        var y = 1
        while (y < h) {
            var x = 1
            while (x < w - 1) {
                if (dist(x)(y) > 0) {
                    var mn = dist(x - 1)(y - 1)
                    val b = dist(x)(y - 1); if (b < mn) mn = b
                    val c = dist(x + 1)(y - 1); if (c < mn) mn = c
                    val d = dist(x - 1)(y); if (d < mn) mn = d
                    val nv = mn + 1
                    if (nv < dist(x)(y)) dist(x)(y) = nv
                }
                x += 1
            }
            y += 1
        }

        // Backward pass: bottom-right to top-left, considering 4 forward neighbors
        y = h - 2
        while (y >= 0) {
            var x = w - 2
            while (x >= 1) {
                if (dist(x)(y) > 0) {
                    var mn = dist(x + 1)(y)
                    val b = dist(x - 1)(y + 1); if (b < mn) mn = b
                    val c = dist(x)(y + 1); if (c < mn) mn = c
                    val d = dist(x + 1)(y + 1); if (d < mn) mn = d
                    val nv = mn + 1
                    if (nv < dist(x)(y)) dist(x)(y) = nv
                }
                x -= 1
            }
            y -= 1
        }

        // Find the best candidate. Constrain candidates to actual region pixels (not gaps)
        // so the glyph CENTER stays in the playable area, even though the glyph BOX may
        // extend into surrounding gap pixels (which is fine — gaps blend visually).
        var bestX = gx
        var bestY = gy
        var bestOverlap = Int.MaxValue
        var bestDist = -1
        y = halfGlyph
        while (y < h - halfGlyph) {
            var x = halfGlyph
            while (x < w - halfGlyph) {
                val dxy = dist(x)(y)
                if (dxy >= halfGlyph && place(x)(y) == regionColor) {
                    val dx = scala.math.abs(x - gx)
                    val dy = scala.math.abs(y - gy)
                    // Round 9: hard radius cap — reject candidates beyond MaxRadiusFromGate
                    // from the gate. Defensive against mobile-viewer bitmap loading drift.
                    val radialDist = scala.math.max(dx, dy)
                    if (radialDist <= MaxRadiusFromGate) {
                        val ox = scala.math.max(0, halfGate + halfGlyph - dx)
                        val oy = scala.math.max(0, halfGate + halfGlyph - dy)
                        val overlap = ox * oy
                        if (overlap < bestOverlap || (overlap == bestOverlap && dxy > bestDist)) {
                            bestOverlap = overlap
                            bestDist = dxy
                            bestX = x
                            bestY = y
                        }
                    }
                }
                x += 1
            }
            y += 1
        }
        (bestX, bestY)
    }
}
