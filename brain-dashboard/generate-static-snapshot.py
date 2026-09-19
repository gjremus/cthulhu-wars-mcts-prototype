#!/usr/bin/env python3
"""
Generate static dashboard snapshot with data baked in.
"""
import json
import urllib.request
from pathlib import Path

# Fetch all data from local APIs
print("Fetching data from local dashboard...")
with urllib.request.urlopen('http://localhost:8765/api/status') as f:
    status = json.loads(f.read().decode())
with urllib.request.urlopen('http://localhost:8765/api/games') as f:
    games = json.loads(f.read().decode())

# Read template
template_path = Path(__file__).parent / "dashboard-static.html"
html = template_path.read_text()

# Replace title
html = html.replace('<title>Brain Training Dashboard</title>',
                    '<title>Brain Training Dashboard (Static Export)</title>')

# REMOVE DANGEROUS CONTROL SECTION
# Remove the "Run Control" tab from navigation
html = html.replace('<button class="tab" onclick="showTab(\'control\')">Run Control</button>', '')

# Remove the entire control section div
import re
control_pattern = r'<div id="control"[^>]*>.*?(?=<div id="(?:games|corpus|weights|progress|status)")'
html = re.sub(control_pattern, '', html, flags=re.DOTALL)

# Stub out dangerous control functions to prevent accidental calls
dangerous_functions = [
    ('async function stopRun()', 'async function stopRun() { console.log("Control disabled in static export"); }'),
    ('async function saveCheckpoint()', 'async function saveCheckpoint() { console.log("Control disabled in static export"); }'),
    ('function startRun()', 'function startRun() { console.log("Control disabled in static export"); }'),
    ('function showSaveModal()', 'function showSaveModal() { console.log("Control disabled in static export"); }'),
]
for old, new in dangerous_functions:
    if old in html:
        # Find the function and replace its entire body
        func_start = html.find(old)
        if func_start != -1:
            func_end = html.find('}', func_start)
            # Find the matching closing brace (handle nested braces)
            brace_count = 0
            i = html.find('{', func_start)
            while i < len(html):
                if html[i] == '{':
                    brace_count += 1
                elif html[i] == '}':
                    brace_count -= 1
                    if brace_count == 0:
                        func_end = i + 1
                        break
                i += 1
            html = html[:func_start] + new + html[func_end:]

# Inject status data as JavaScript
status_js = f"""
<script>
// BAKED DATA - DO NOT CALL APIS
const STATIC_STATUS = {json.dumps(status, indent=2)};
const STATIC_GAMES = {json.dumps(games if isinstance(games, list) else games.get('games', []), indent=2)};

// Override fetch to return baked data
const originalFetch = window.fetch;
window.fetch = function(url) {{
    if (url.includes('/api/status')) {{
        return Promise.resolve({{
            ok: true,
            json: () => Promise.resolve(STATIC_STATUS)
        }});
    }}
    if (url.includes('/api/games')) {{
        return Promise.resolve({{
            ok: true,
            json: () => Promise.resolve(STATIC_GAMES)
        }});
    }}
    // For trace files, use original fetch
    return originalFetch.apply(this, arguments);
}};
</script>
"""

# Inject before </head>
html = html.replace('</head>', status_js + '</head>')

# Write output
output_path = Path(__file__).parent / "export" / "index.html"
output_path.write_text(html)

print(f"Generated static snapshot:")
print(f"  Status: iter {status.get('current_iter')}, game {status.get('current_game')}/{status.get('total_games')}")
print(f"  Games: {len(games if isinstance(games, list) else games.get('games', []))} games")
print(f"  Output: {output_path}")
