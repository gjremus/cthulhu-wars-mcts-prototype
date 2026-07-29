#!/usr/bin/env python3
"""Aggregate SimRunner METRICS| lines into per-faction and overall averages.

Usage: agg_metrics.py <logfile>

Parses lines of the form:
  METRICS|won=<bool>|<Faction Name>: doom=.. SB=x/6[..] SBR[..] GOO[..] APs=.. \
    doom/AP=.. startPow/AP=.. rit=.. doom/rit=.. ES/rit=.. ES=..
and reports mean of the 10 intermediate metrics per faction and overall, plus a
winners-only vs losers-only split (what a WIN looks like on these metrics).
"""
import sys, re
from collections import defaultdict

def parse(line):
    # strip "METRICS|won=X|"
    m = re.match(r"METRICS\|won=(true|false)\|(.+?): (.*)", line.strip())
    if not m:
        return None
    won = m.group(1) == "true"
    fac = m.group(2)
    rest = m.group(3)
    d = {"won": won, "faction": fac}
    d["doom"]      = int(re.search(r"doom=(\d+)", rest).group(1))
    d["sb"]        = int(re.search(r"SB=(\d+)/6", rest).group(1))
    d["sbr"]       = len([x for x in re.search(r"SBR\[(.*?)\]", rest).group(1).split(",") if x and x != "-"])
    d["goo"]       = len([x for x in re.search(r"GOO\[(.*?)\]", rest).group(1).split(",") if x and x != "-"])
    d["aps"]       = int(re.search(r"APs=(\d+)", rest).group(1))
    d["doomAP"]    = float(re.search(r"doom/AP=([\d.]+)", rest).group(1))
    d["startPow"]  = float(re.search(r"startPow/AP=([\d.]+)", rest).group(1))
    d["rit"]       = int(re.search(r"rit=(\d+)", rest).group(1))
    d["doomRit"]   = float(re.search(r"doom/rit=([\d.]+)", rest).group(1))
    d["esRit"]     = float(re.search(r"ES/rit=([\d.]+)", rest).group(1))
    d["es"]        = int(re.search(r"ES=(\d+)", rest).group(1))
    d["gatesAP"]   = float(re.search(r"gates/AP=([\d.]+)", rest).group(1))
    d["abandAP"]   = float(re.search(r"aband/AP=([\d.]+)", rest).group(1))
    return d

FIELDS = [("doom","doom"),("sb","SB/6"),("sbr","SBR"),("goo","GOO"),("aps","APs"),
          ("doomAP","doom/AP"),("startPow","startPow/AP"),("rit","rit"),
          ("doomRit","doom/rit"),("esRit","ES/rit"),("es","ES"),
          ("gatesAP","gates/AP"),("abandAP","aband/AP")]

def avg(rows, key):
    return sum(r[key] for r in rows)/len(rows) if rows else 0.0

def line(rows, label):
    if not rows:
        return f"{label:16s} (n=0)"
    parts = [f"{lab}={avg(rows,k):.2f}" for k,lab in FIELDS]
    return f"{label:16s} (n={len(rows):4d}) " + " ".join(parts)

def main():
    rows = []
    with open(sys.argv[1]) as f:
        for ln in f:
            if ln.startswith("METRICS|"):
                r = parse(ln)
                if r: rows.append(r)
    if not rows:
        print("no METRICS lines found"); return
    ngames = len(rows)//4
    print(f"=== aggregate over {ngames} games ({len(rows)} faction-rows) ===\n")
    print(line(rows, "ALL SEATS"))
    print(line([r for r in rows if r["won"]],  "  WINNERS"))
    print(line([r for r in rows if not r["won"]], "  LOSERS"))
    print()
    byfac = defaultdict(list)
    for r in rows: byfac[r["faction"]].append(r)
    for fac in sorted(byfac):
        fr = byfac[fac]
        wr = sum(1 for r in fr if r["won"])
        print(line(fr, fac[:15]) + f"  winrate={100*wr/len(fr):.0f}%")

if __name__ == "__main__":
    main()
