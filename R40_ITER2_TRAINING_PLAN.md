# R40 Iter 2 Training Plan
**Created:** 2026-09-28 23:00
**Status:** Planning

## User Directive

### Training Approach
1. **Score R40 iter 1 games** (1626 games) with weighted -1 to +1 based on performance
2. **Mix in bot corpus** - reuse existing bot-vs-bot game corpus
3. **Train on combined weighted corpus**
4. **Start R40 iter 2** with updated checkpoint

### Scoring Formula for R40 Games

**Median point:** 8 doom (higher than current 5.7 avg)

**Formula:**
```
base_score = (doom - 8) / 8
sb_bonus = (spellbooks - 1.5) * 0.2
final_score = clamp(base_score + sb_bonus, -1.0, 1.0)
```

**Example scores:**
- 0 doom, 0 SBs: -1.0 (strong negative)
- 3 doom, 1 SB: -0.72
- 5 doom, 1 SB: -0.47
- 8 doom, 2 SB: +0.10
- 10 doom, 2 SB: +0.35
- 15 doom, 3 SB: +1.0 (strong positive)

### Bot Corpus Inclusion

**Purpose:** Reuse existing bot-vs-bot game corpus as stable "gold standard" anchor

**Filtering criteria:**
- Choose highest-scoring bot games
- Verify winner in game log (6 SBs, 30-60 doom)
- All decisions by winning faction = +1.0

**Mix ratio:** 
- 70% R40 games (1626 games, weighted -1 to +1)
- 30% bot games (~700 games, all +1.0)
- Total corpus: ~2326 games

**Risk assessment:** NO MAJOR RISK
- Bot games = stable baseline across iters
- 70% self-play evolves each iter
- Prevents brain from drifting into degenerate strategies
- Minor risk: slight overfitting to bot patterns, mitigated by large self-play majority

### Training Configuration

**Epochs:** 12 (same as R39/R40 iter 1)
- Checkpoint saved after each epoch
- Early stopping if validation loss plateaus

**Expected outcome:**
- Brain learns to avoid low-doom patterns (strong negative signal)
- Brain learns high-doom strategies from both self-play and bot corpus

## Bot Corpus Location

**FOUND and DOCUMENTED:**

**Primary corpus:** `/Users/gremus/Library/CloudStorage/GoogleDrive-gremus@salesforce.com/My Drive/Personal/Games/Cthulhu Wars/admin/sim/bot-replays/`

- **Count:** 139 HTML replay files (bot-vs-bot games)
- **Format:** HTML replays with embedded game logs
- **Factions:** All 4 factions (GC, CC, BG, YS)
- **Quality:** Bot3/BotX level games (FB Earth 3/4/5p)

**Reusable across iters:** YES - stable "gold standard" baseline

## Implementation Steps

1. [ ] Locate and document bot corpus location
2. [ ] Filter bot corpus: top ~700 games by doom score
3. [ ] Calculate weights for R40 games using formula above
4. [ ] Build combined training corpus file
5. [ ] Run training epochs (12 epochs)
6. [ ] Monitor training completion
7. [ ] Start R40 iter 2 with new checkpoint
