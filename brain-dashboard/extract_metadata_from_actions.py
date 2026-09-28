#!/usr/bin/env python3
"""Extract metadata from R40 action strings and recreate metadata sections."""

import re
from pathlib import Path

TRACES_DIR = Path("/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces")

def extract_metadata_from_actions(trace_file):
    """Parse action strings to extract game metadata."""
    content = trace_file.read_text()

    # Get action strings (first section)
    parts = content.split('\n\n')
    if not parts:
        return None

    action_lines = parts[0].strip().split('\n')

    # Parse filename for brain seat
    # arena-R40-iter1-gc-game1-d6.txt -> GC
    match = re.match(r'arena-R\d+-iter\d+-(\w+)-game\d+-d\d+', trace_file.stem)
    if not match:
        return None

    brain_faction = match.group(1).upper()

    # Count Action Phases (PlayDirectionAction marks start of AP)
    action_phases = len([a for a in action_lines if 'PlayDirectionAction' in a])

    # Extract spellbooks for brain faction
    spellbooks = 0
    for action in action_lines:
        if f'SpellbookAction({brain_faction}' in action:
            spellbooks += 1

    # Extract doom from RitualActions for brain faction
    doom = 0
    for action in action_lines:
        if f'RitualAction({brain_faction}' in action:
            # RitualAction(GC, 9, 1) -> power=9, doom_gain=1
            match = re.match(rf'RitualAction\({brain_faction}, (\d+), (\d+)\)', action)
            if match:
                doom_gain = int(match.group(2))
                doom += doom_gain

    # Determine result - check if brain faction won
    # Parse ALL_DOOM line if exists in action strings or calculate from all factions
    all_doom = {}
    for faction in ['GC', 'BG', 'YS', 'CC', 'OW', 'SL']:
        faction_doom = 0
        for action in action_lines:
            if f'RitualAction({faction}' in action:
                match = re.match(rf'RitualAction\({faction}, (\d+), (\d+)\)', action)
                if match:
                    faction_doom += int(match.group(2))
        if faction_doom > 0:
            all_doom[faction] = faction_doom

    # Determine placement by doom
    if all_doom:
        sorted_factions = sorted(all_doom.items(), key=lambda x: x[1], reverse=True)
        place = next((i+1 for i, (f, d) in enumerate(sorted_factions) if f == brain_faction), "?")
        result = "win" if place == 1 else "loss"
    else:
        place = "?"
        result = "loss"

    # Placeholder score (would need more complex calculation)
    score = 1.0 if result == "win" else 0.0

    return {
        "brain_seat": brain_faction,
        "result": result,
        "doom": doom,
        "spellbooks": spellbooks,
        "action_phases": action_phases,
        "score": score,
        "place": place,
        "all_doom": all_doom
    }


def restore_metadata_section(trace_file):
    """Add metadata section back to trace file."""
    content = trace_file.read_text()

    # If already has metadata, skip
    if "Brain seat:" in content:
        return False

    metadata = extract_metadata_from_actions(trace_file)
    if not metadata:
        return False

    # Append metadata section
    metadata_lines = [
        "",
        f"Brain seat: {metadata['brain_seat']}",
        f"Result: {metadata['result']}",
        f"Doom: {metadata['doom']}",
        f"Spellbooks: {metadata['spellbooks']}",
        f"Action Phases: {metadata['action_phases']}",
        f"Score (0-1): {metadata['score']:.3f}",
        f"Place: {metadata['place']}"
    ]

    # Add ALL_DOOM line
    if metadata['all_doom']:
        doom_pairs = [f"{f}={d}" for f, d in sorted(metadata['all_doom'].items(), key=lambda x: x[1], reverse=True)]
        metadata_lines.append(f"ALL_DOOM={' '.join(doom_pairs)}")

    new_content = content.rstrip() + '\n' + '\n'.join(metadata_lines) + '\n'

    trace_file.write_text(new_content)
    return True


def main():
    traces = list(TRACES_DIR.glob("arena-R40-iter1-*.txt"))
    print(f"Found {len(traces)} R40 traces")

    restored = 0
    skipped = 0

    for trace in traces:
        if restore_metadata_section(trace):
            restored += 1
            if restored % 100 == 0:
                print(f"  Restored {restored}...")
        else:
            skipped += 1

    print(f"\n✓ Restored metadata to {restored} traces")
    print(f"  Skipped {skipped} (already had metadata)")


if __name__ == '__main__':
    main()
