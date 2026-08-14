#!/usr/bin/env python3
"""
Track Claude Code session usage and display costs.

Usage:
  ./track-usage.py <input_tokens> <output_tokens> [cache_write] [cache_read]

Example: ./track-usage.py 110000 0 0 0

Pricing (Sonnet 4):
- Input: $3.00 per 1M tokens
- Output: $15.00 per 1M tokens
- Cache Write: $3.75 per 1M tokens
- Cache Read: $0.30 per 1M tokens
"""

import sys
from pathlib import Path

# Pricing per 1M tokens (Sonnet 4)
PRICING = {
    "input": 3.00,
    "output": 15.00,
    "cache_write": 3.75,
    "cache_read": 0.30,
}

def calculate_cost(tokens, price_per_million):
    """Calculate cost for given tokens at price per million."""
    return (tokens / 1_000_000) * price_per_million

def format_cost(amount):
    """Format cost as currency."""
    return f"${amount:.4f}"

def main():
    """Calculate and display costs."""
    if len(sys.argv) < 3:
        print("Usage: ./track-usage.py <input_tokens> <output_tokens> [cache_write] [cache_read]")
        print("\nExample based on this session (~110K tokens used):")
        print("  ./track-usage.py 110000 0 0 0")
        sys.exit(1)

    usage = {
        "input_tokens": int(sys.argv[1]),
        "output_tokens": int(sys.argv[2]),
        "cache_write_tokens": int(sys.argv[3]) if len(sys.argv) > 3 else 0,
        "cache_read_tokens": int(sys.argv[4]) if len(sys.argv) > 4 else 0,
    }

    input_cost = calculate_cost(usage["input_tokens"], PRICING["input"])
    output_cost = calculate_cost(usage["output_tokens"], PRICING["output"])
    cache_write_cost = calculate_cost(usage["cache_write_tokens"], PRICING["cache_write"])
    cache_read_cost = calculate_cost(usage["cache_read_tokens"], PRICING["cache_read"])
    total_cost = input_cost + output_cost + cache_write_cost + cache_read_cost

    print("\n" + "="*80)
    print("💰 CLAUDE CODE SESSION COSTS (Sonnet 4)")
    print("="*80)
    print(f"Input tokens:       {usage['input_tokens']:>12,} × ${PRICING['input']}/1M  = {format_cost(input_cost):>10}")
    print(f"Output tokens:      {usage['output_tokens']:>12,} × ${PRICING['output']}/1M = {format_cost(output_cost):>10}")
    print(f"Cache Write tokens: {usage['cache_write_tokens']:>12,} × ${PRICING['cache_write']}/1M  = {format_cost(cache_write_cost):>10}")
    print(f"Cache Read tokens:  {usage['cache_read_tokens']:>12,} × ${PRICING['cache_read']}/1M  = {format_cost(cache_read_cost):>10}")
    print("-"*80)
    print(f"TOTAL COST:                                              {format_cost(total_cost):>10}")
    print("="*80 + "\n")

if __name__ == "__main__":
    main()
