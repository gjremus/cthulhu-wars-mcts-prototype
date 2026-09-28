#!/usr/bin/env python3
"""Convert all R40 traces to include HTML log format."""

import re
import sys
from pathlib import Path

TRACES_DIR = Path("/Users/gremus/cthulhu-wars-mcts-prototype/arena-traces")

def action_to_html(action_str):
    """Convert a single action string to HTML log entry."""
    faction_match = re.match(r'(\w+Action)\((\w+)', action_str)
    if not faction_match:
        return f'<div class="log-entry">{action_str}</div>'

    action_type = faction_match.group(1)
    faction = faction_match.group(2)

    if 'MoveAction' in action_str:
        match = re.match(r'MoveAction\((\w+), (\w+/\w+/\d+), (\w+), (\w+), (\d+)\)', action_str)
        if match:
            faction, unit, from_reg, to_reg, cost = match.groups()
            return f'<div class="log-entry">[{faction}] Move {unit.split("/")[1]} from {from_reg} to {to_reg}</div>'
    elif 'SummonAction' in action_str:
        match = re.match(r'SummonAction\((\w+), (\w+), (\w+)\)', action_str)
        if match:
            faction, unit, region = match.groups()
            return f'<div class="log-entry">[{faction}] Summon {unit} in {region}</div>'
    elif 'BuildGateAction' in action_str:
        match = re.match(r'BuildGateAction\((\w+), (\w+)\)', action_str)
        if match:
            faction, region = match.groups()
            return f'<div class="log-entry">[{faction}] Build Gate in {region}</div>'
    elif 'SpellbookAction' in action_str:
        match = re.match(r'SpellbookAction\((\w+), (\w+),', action_str)
        if match:
            faction, spellbook = match.groups()
            return f'<div class="log-entry">[{faction}] Earn Spellbook: {spellbook}</div>'
    elif 'RitualAction' in action_str:
        match = re.match(r'RitualAction\((\w+), (\d+), (\d+)\)', action_str)
        if match:
            faction, power, doom = match.groups()
            return f'<div class="log-entry">[{faction}] Ritual: spend {power} power, gain {doom} doom</div>'
    elif 'AwakenAction' in action_str:
        match = re.match(r'AwakenAction\((\w+), (\w+), (\w+), (\d+)\)', action_str)
        if match:
            faction, goo, region, cost = match.groups()
            return f'<div class="log-entry">[{faction}] Awaken {goo} in {region}</div>'
    elif 'CaptureAction' in action_str:
        match = re.match(r'CaptureAction\((\w+), (\w+), (\w+),', action_str)
        if match:
            faction, region, victim = match.groups()
            return f'<div class="log-entry">[{faction}] Capture cultist from {victim} in {region}</div>'
    elif 'EndTurnAction' in action_str:
        match = re.match(r'EndTurnAction\((\w+)\)', action_str)
        if match:
            faction = match.group(1)
            return f'<div class="log-entry">[{faction}] End Turn</div>'

    return f'<div class="log-entry">[{faction}] {action_type.replace("Action", "")}</div>'


def convert_trace_in_place(trace_path):
    """Convert trace file in place by adding HTML log section."""
    content = trace_path.read_text()

    # Check if already has HTML log (has blank line separator)
    if '\n\n' in content:
        parts = content.split('\n\n', 1)
        # Check if second part looks like HTML
        if len(parts) > 1 and '<div class="log-entry">' in parts[1]:
            return False  # Already converted

    # Split at first blank line or metadata
    lines = content.split('\n')
    action_lines = []
    metadata_start = None

    for i, line in enumerate(lines):
        if line.strip() == '' or line.startswith('Brain seat:') or line.startswith('Result:'):
            metadata_start = i
            break
        action_lines.append(line.strip())

    # Convert actions to HTML
    html_lines = [action_to_html(action) for action in action_lines if action]

    # Rebuild file: actions + blank line + HTML + blank line + metadata
    new_content = '\n'.join(action_lines)
    new_content += '\n\n'
    new_content += '\n'.join(html_lines)
    if metadata_start is not None:
        new_content += '\n\n'
        new_content += '\n'.join(lines[metadata_start:])

    # Write back
    trace_path.write_text(new_content)
    return True


def main():
    traces = list(TRACES_DIR.glob("arena-R40-iter1-*.txt"))
    print(f"Found {len(traces)} R40 traces")

    converted = 0
    skipped = 0

    for trace in traces:
        if convert_trace_in_place(trace):
            converted += 1
            if converted % 100 == 0:
                print(f"  Converted {converted}...")
        else:
            skipped += 1

    print(f"\n✓ Converted {converted} traces")
    print(f"  Skipped {skipped} (already had HTML)")


if __name__ == '__main__':
    main()
