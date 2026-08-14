## 2026-08-14 - New Gate Rewards

### Overview
Two new gate-focused rewards were added to enhance the brain's gate-control strategy.

### New Rewards

**r:buildGate (0.2 weight, BEHAVIOR)**
- Rewards building and controlling a new gate
- Capped at 3 controlled gates to prevent over-focus
- Encourages early gate expansion without runaway accumulation

**r:EndAPGates (0.8 weight, RESULT)**
- Rewards holding gates during the doom phase (end-of-action-point observations)
- No upper cap; rewards consistent gate maintenance
- Higher weight (0.8) emphasizes gate control as a key endgame strategy

### Implementation
Both rewards were implemented in `Shaping.scala` (located in both `mcts-src/` and `brain-web/` directories).

### Dashboard Changes
- **Breakdown display format**: Changed to show `name: count = 0.XXX` format for clearer reward contribution visualization
- **Doom ranking extraction**: Added extraction of doom rankings from trace files for comparative analysis
- **Placement calculation**: Added calculation of final placement from trace data
- **Winner column**: Fixed to distinguish Learning vs Champion outcomes
- **Sort order**: Corrected to DESCENDING by run then iteration for chronological review
