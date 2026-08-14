#!/usr/bin/env python3
"""
Track Claude Code session usage and display costs in terminal.
Monitors the conversation and calculates costs based on Sonnet 4 pricing.

Pricing (as of 2024):
- Input: $3.00 per 1M tokens
- Output: $15.00 per 1M tokens
- Cache Write: $3.75 per 1M tokens
- Cache Read: $0.30 per 1M tokens
"""

import json
import sys
import time
from pathlib import Path

# Pricing per 1M tokens
PRICING = {
    "input": 3.00,
    "output": 15.00,
    "cache_write": 3.75,
    "cache_read": 0.30,
}

def parse_session_usage():
    """
    Parse Claude Code session data to extract usage.
    This reads from Claude Code's session directory if available.
    """
    # Try to find the most recent session file
    claude_dir = Path.home() / ".claude" / "projects"

    # For now, return mock data - in production this would parse actual session files
    # The user can wire this to the actual Claude Code session API
    return {
        "input_tokens": 0,
        "output_tokens": 0,
        "cache_write_tokens": 0,
        "cache_read_tokens": 0,
    }

def calculate_cost(tokens, price_per_million):
    """Calculate cost for given tokens at price per million."""
    return (tokens / 1_000_000) * price_per_million

def format_cost(amount):
    """Format cost as currency."""
    return f"${amount:.4f}"

def main():
    """Main cost tracking loop."""
    print("\033[?25l", end="", flush=True)  # Hide cursor

    try:
        while True:
            usage = parse_session_usage()

            # Calculate costs
            input_cost = calculate_cost(usage["input_tokens"], PRICING["input"])
            output_cost = calculate_cost(usage["output_tokens"], PRICING["output"])
            cache_write_cost = calculate_cost(usage["cache_write_tokens"], PRICING["cache_write"])
            cache_read_cost = calculate_cost(usage["cache_read_tokens"], PRICING["cache_read"])
            total_cost = input_cost + output_cost + cache_write_cost + cache_read_cost

            # Print to bottom of terminal
            sys.stdout.write("\033[s")  # Save cursor position
            sys.stdout.write("\033[999;0H")  # Move to bottom
            sys.stdout.write("\033[K")  # Clear line

            cost_line = (
                f"💰 Claude Usage: "
                f"Input: {format_cost(input_cost)} | "
                f"Output: {format_cost(output_cost)} | "
                f"Cache Write: {format_cost(cache_write_cost)} | "
                f"Cache Read: {format_cost(cache_read_cost)} | "
                f"TOTAL: {format_cost(total_cost)}"
            )

            sys.stdout.write(cost_line)
            sys.stdout.write("\033[u")  # Restore cursor position
            sys.stdout.flush()

            time.sleep(5)  # Update every 5 seconds

    except KeyboardInterrupt:
        sys.stdout.write("\033[?25h")  # Show cursor
        sys.stdout.write("\n")
        sys.exit(0)

if __name__ == "__main__":
    main()
