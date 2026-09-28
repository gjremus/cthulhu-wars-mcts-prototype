#!/usr/bin/env python3
"""Functional audit - verify dashboard data correctness, not just syntax."""

import json
import subprocess
import sys
from pathlib import Path

BASE_URL = "http://localhost:8765"

def curl_get(url):
    """Use curl to get URL content."""
    result = subprocess.run(['curl', '-s', url], capture_output=True, text=True)
    if result.returncode != 0:
        return None, result.returncode
    return result.stdout, 200

def audit_performance_history():
    """Verify Performance History shows correct R40 data."""
    print("\n=== Performance History Audit ===")

    # Get API data
    content, status = curl_get(f"{BASE_URL}/api/performance-history")
    if status != 200 or not content:
        print(f"✗ API failed: {status}")
        return False

    data = json.loads(content)
    r40_entries = [x for x in data if x.get('run') == 'R40']

    if not r40_entries:
        print("✗ No R40 entries in API")
        return False

    entry = r40_entries[0]

    # Check required fields exist
    required_fields = ['avg_doom', 'avg_spellbooks', 'avg_length', 'total_games']
    missing = [f for f in required_fields if f not in entry]
    if missing:
        print(f"✗ Missing fields: {missing}")
        return False

    # Verify reasonable values
    doom = entry['avg_doom']
    sbs = entry['avg_spellbooks']
    aps = entry['avg_length']
    total = entry['total_games']

    issues = []
    if doom < 0 or doom > 60:
        issues.append(f"doom={doom} out of range")
    if sbs < 0 or sbs > 6:
        issues.append(f"spellbooks={sbs} out of range")
    if aps < 1 or aps > 20:
        issues.append(f"action_phases={aps} out of range")
    if total < 1:
        issues.append(f"total_games={total} invalid")

    if issues:
        print(f"✗ Data validation failed:")
        for issue in issues:
            print(f"  - {issue}")
        return False

    print(f"✓ R40 data correct:")
    print(f"  doom={doom:.1f}, sbs={sbs:.1f}, aps={aps:.1f}, total={total}")
    return True


def audit_html_table():
    """Verify HTML has correct table structure and JavaScript rendering."""
    print("\n=== HTML Table Audit ===")

    html, status = curl_get(BASE_URL)
    if status != 200 or not html:
        print(f"✗ Dashboard failed: {status}")
        return False

    # Check table has all required columns
    required_cols = ['Run', 'Iter', 'Avg Doom', 'Avg SBs', 'Avg APs', 'Avg Score', 'Type', 'Wins', 'Total']
    missing_cols = [col for col in required_cols if col not in html]

    if missing_cols:
        print(f"✗ Missing columns: {missing_cols}")
        return False

    # Check colspan matches column count
    if 'colspan="9"' not in html:
        print("✗ Colspan mismatch (should be 9 for 9 columns)")
        return False

    # CRITICAL: Check JavaScript uses row.total_games NOT row.total
    if 'row.total_games' not in html:
        print("✗ JavaScript still using old 'row.total' instead of 'row.total_games'")
        return False

    if 'row.total > 0' in html:
        print("✗ JavaScript has bug: 'row.total' should be 'row.total_games'")
        return False

    # Check JavaScript has undefined checks
    if 'row.avg_spellbooks !== undefined' not in html:
        print("✗ Missing undefined check for avg_spellbooks")
        return False

    if 'row.avg_length !== undefined' not in html:
        print("✗ Missing undefined check for avg_length")
        return False

    if 'row.avg_doom !== undefined' not in html:
        print("✗ Missing undefined check for avg_doom")
        return False

    # Check faction breakdown rendering exists
    if 'row.factions' not in html:
        print("✗ Missing faction breakdown code in JavaScript")
        return False

    if 'perf-breakdown-' not in html:
        print("✗ Missing faction breakdown row ID generation")
        return False

    print(f"✓ HTML table has all {len(required_cols)} columns")
    print(f"✓ Colspan correct (9)")
    print(f"✓ JavaScript uses row.total_games correctly")
    print(f"✓ JavaScript has undefined checks for all fields")
    print(f"✓ Faction breakdown rendering present")
    return True


def audit_game_browser():
    """Verify Game Browser API returns valid data."""
    print("\n=== Game Browser Audit ===")

    content, status = curl_get(f"{BASE_URL}/api/games?run=R40&iter=1&limit=10")
    if status != 200 or not content:
        print(f"✗ API failed: {status}")
        return False

    games = json.loads(content)

    # Handle dict response with games array
    if isinstance(games, dict):
        games = games.get('games', [])

    if not games:
        print("✗ No games returned")
        return False

    # Check first game has required fields
    game = games[0]
    # API uses 'game_length' and 'placement', not 'length' and 'place'
    required = ['faction', 'doom', 'spellbooks', 'game_length', 'placement', 'score']
    missing = [f for f in required if f not in game]

    if missing:
        print(f"✗ Missing fields: {missing}")
        print(f"  Available fields: {list(game.keys())}")
        return False

    # Verify field values are reasonable (placement can be int or string like "1st")
    placement = game['placement']
    if isinstance(placement, str):
        if placement not in ['1st', '2nd', '3rd', '4th', 'N/A', '?']:
            print(f"✗ Invalid placement string: {placement}")
            return False
    elif isinstance(placement, int):
        if placement < 1 or placement > 4:
            print(f"✗ Invalid placement int: {placement}")
            return False
    else:
        print(f"✗ Invalid placement type: {type(placement)}")
        return False

    # game_length should be a string like "7APs"
    if not isinstance(game['game_length'], str) or 'AP' not in game['game_length']:
        print(f"✗ Invalid game_length value: {game['game_length']}")
        return False

    print(f"✓ Game Browser returns {len(games)} games")
    print(f"✓ Games have all required fields")
    print(f"✓ Field values are valid (placement={game['placement']}, length={game['game_length']})")
    return True


def audit_canonical_store():
    """Verify canonical store has correct R40 data."""
    print("\n=== Canonical Store Audit ===")

    store_path = Path("/Users/gremus/cthulhu-wars-mcts-prototype/brain-dashboard/canonical_games.json")
    if not store_path.exists():
        print("✗ Canonical store not found")
        return False

    with open(store_path, 'r') as f:
        data = json.load(f)

    # Handle both 'iter' and 'iteration' field names
    r40_games = [g for g in data['games'] if g.get('run') == 'R40' and (g.get('iter') == 1 or g.get('iteration') == 1)]

    if not r40_games:
        print("✗ No R40 games in store")
        return False

    # Calculate metrics
    completed = [g for g in r40_games if g.get('result') != 'KILLED']
    if not completed:
        print("✗ No completed games")
        return False

    avg_doom = sum(g.get('doom', 0) for g in completed) / len(completed)
    avg_sbs = sum(g.get('spellbooks', 0) for g in completed) / len(completed)
    avg_aps = sum(g.get('length', 0) for g in completed) / len(completed)

    print(f"✓ Canonical store has {len(r40_games)} R40 games")
    print(f"✓ Metrics: doom={avg_doom:.1f}, sbs={avg_sbs:.1f}, aps={avg_aps:.1f}")

    # Check performance history exists
    perf_hist = data.get('performanceHistory', [])
    r40_perf = [p for p in perf_hist if p.get('run') == 'R40']

    if not r40_perf:
        print("✗ No R40 in performance history")
        return False

    print(f"✓ Performance history has R40 entry")
    return True


def main():
    print("=" * 60)
    print("FUNCTIONAL DASHBOARD AUDIT")
    print("=" * 60)

    results = {
        'Canonical Store': audit_canonical_store(),
        'Performance History API': audit_performance_history(),
        'HTML Table Structure': audit_html_table(),
        'Game Browser API': audit_game_browser(),
    }

    print("\n" + "=" * 60)
    print("AUDIT SUMMARY")
    print("=" * 60)

    for name, passed in results.items():
        status = "✓ PASS" if passed else "✗ FAIL"
        print(f"{status}: {name}")

    all_passed = all(results.values())
    print("\n" + ("="*60))
    if all_passed:
        print("✓✓✓ ALL AUDITS PASSED ✓✓✓")
    else:
        print("✗✗✗ SOME AUDITS FAILED ✗✗✗")
    print("=" * 60)

    return 0 if all_passed else 1


if __name__ == '__main__':
    sys.exit(main())
