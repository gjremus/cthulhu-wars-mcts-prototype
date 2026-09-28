#!/usr/bin/env python3
"""Convert R40 action strings to proper HTML log format expected by build-replay.py"""

import re
from pathlib import Path

TRACES_DIR = Path("/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces")

# Map faction abbreviations to full names
FACTION_FULL = {
    "GC": "Great Cthulhu",
    "CC": "Crawling Chaos",
    "BG": "Black Goat",
    "YS": "Yellow Sign",
    "OW": "Opener of the Way",
    "SL": "Sleeper",
    "WW": "Windwalker",
    "AN": "The Ancients",
    "TS": "Tombstalker",
    "FB": "Firstborn",
    "DS": "Daemon Sultan",
}

def action_to_proper_html(action_str):
    """Convert action string to HTML in format expected by event_handlers.py"""

    # Extract faction (first parameter)
    faction_match = re.match(r'(\w+Action)\((\w+)', action_str)
    if not faction_match:
        return f'<div class="log-entry">{action_str}</div>'

    action_type = faction_match.group(1)
    faction_short = faction_match.group(2)
    faction_full = FACTION_FULL.get(faction_short, faction_short)

    # Convert specific actions to proper format
    if 'PlayDirectionAction' in action_str:
        # Just a marker, no need to log details
        return f'<div class="log-entry">{faction_full} set play direction</div>'

    elif 'MoveAction' in action_str:
        # MoveAction(GC, GC/Acolyte/2, SouthPacific, NorthPacific, 1)
        match = re.match(r'MoveAction\((\w+), (\w+/\w+/\d+), (\w+), (\w+), (\d+)\)', action_str)
        if match:
            _, unit_path, from_reg, to_reg, _ = match.groups()
            unit = unit_path.split('/')[1]  # Extract unit type from path
            return f'<div class="log-entry">{faction_full} moved {unit} from {from_reg} to {to_reg}</div>'

    elif 'MoveDoneAction' in action_str:
        # This is an internal marker, not a game log event
        return f'<div class="log-entry">{faction_full} finished moving</div>'

    elif 'SummonAction' in action_str:
        # SummonAction(BG, DarkYoung, WestAfrica)
        match = re.match(r'SummonAction\((\w+), (\w+), (\w+)\)', action_str)
        if match:
            _, unit, region = match.groups()
            return f'<div class="log-entry">{faction_full} placed {unit} in {region}</div>'

    elif 'BuildGateAction' in action_str:
        # BuildGateAction(GC, SouthAtlantic)
        match = re.match(r'BuildGateAction\((\w+), (\w+)\)', action_str)
        if match:
            _, region = match.groups()
            return f'<div class="log-entry">{faction_full} built gate in {region}</div>'

    elif 'SpellbookAction' in action_str:
        # SpellbookAction(YS, Passion, PreMainAction(YS))
        match = re.match(r'SpellbookAction\((\w+), (\w+),', action_str)
        if match:
            _, spellbook = match.groups()
            return f'<div class="log-entry">{faction_full} earned {spellbook}</div>'

    elif 'RitualAction' in action_str:
        # RitualAction(CC, 9, 1)
        match = re.match(r'RitualAction\((\w+), (\d+), (\d+)\)', action_str)
        if match:
            _, power, doom = match.groups()
            return f'<div class="log-entry">{faction_full} spent {power} Power on Ritual of Annihilation and gained {doom} Doom</div>'

    elif 'AwakenAction' in action_str:
        # AwakenAction(YS, KingInYellow, NorthAsia, 4)
        match = re.match(r'AwakenAction\((\w+), (\w+), (\w+), (\d+)\)', action_str)
        if match:
            _, goo, region, cost = match.groups()
            return f'<div class="log-entry">{faction_full} spent {cost} Power to awaken {goo} in {region}</div>'

    elif 'CaptureAction' in action_str:
        # CaptureAction(YS, NorthPacific, CC, None)
        match = re.match(r'CaptureAction\((\w+), (\w+), (\w+),', action_str)
        if match:
            _, region, victim = match.groups()
            return f'<div class="log-entry">{faction_full} captured cultist from {victim} in {region}</div>'

    elif 'EndTurnAction' in action_str:
        match = re.match(r'EndTurnAction\((\w+)\)', action_str)
        if match:
            return f'<div class="log-entry">{faction_full} passed and forfeited 0 Power</div>'

    # Default: generic action description
    return f'<div class="log-entry">{faction_full} {action_type.replace("Action", "")}</div>'


def convert_trace_in_place(trace_path):
    """Re-convert trace file with proper HTML format"""
    content = trace_path.read_text()

    # Split into sections
    parts = content.split('\n\n')
    if len(parts) < 2:
        return False  # Not converted yet

    action_lines = [l.strip() for l in parts[0].strip().split('\n') if l.strip()]

    # Convert actions to proper HTML
    html_lines = [action_to_proper_html(action) for action in action_lines]

    # Rebuild file: actions + blank line + HTML + blank line + metadata
    new_content = '\n'.join(action_lines)
    new_content += '\n\n'
    new_content += '\n'.join(html_lines)
    if len(parts) > 2:
        new_content += '\n\n'
        new_content += parts[2]

    # Write back
    trace_path.write_text(new_content)
    return True


def main():
    traces = list(TRACES_DIR.glob("arena-R40-iter1-*.txt"))
    print(f"Found {len(traces)} R40 traces")

    converted = 0

    for trace in traces:
        if convert_trace_in_place(trace):
            converted += 1
            if converted % 100 == 0:
                print(f"  Converted {converted}...")

    print(f"\n✓ Re-converted {converted} traces with proper HTML format")


if __name__ == '__main__':
    main()
