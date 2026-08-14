#!/bin/bash
# Display Claude usage costs in terminal footer
# Run in background: ./show-costs-terminal.sh &

INPUT_TOKENS=${1:-135000}
OUTPUT_TOKENS=${2:-0}
CACHE_WRITE=${3:-0}
CACHE_READ=${4:-0}

# Pricing per 1M tokens (Sonnet 4)
INPUT_RATE=3.00
OUTPUT_RATE=15.00
CACHE_WRITE_RATE=3.75
CACHE_READ_RATE=0.30

calc_cost() {
    echo "scale=4; $1 / 1000000 * $2" | bc
}

INPUT_COST=$(calc_cost $INPUT_TOKENS $INPUT_RATE)
OUTPUT_COST=$(calc_cost $OUTPUT_TOKENS $OUTPUT_RATE)
CACHE_W_COST=$(calc_cost $CACHE_WRITE $CACHE_WRITE_RATE)
CACHE_R_COST=$(calc_cost $CACHE_READ $CACHE_READ_RATE)
TOTAL=$(echo "$INPUT_COST + $OUTPUT_COST + $CACHE_W_COST + $CACHE_R_COST" | bc)

# Hide cursor
tput civis

# Trap to show cursor on exit
trap 'tput cnorm; tput cup $(tput lines) 0; exit' INT TERM EXIT

while true; do
    # Save cursor position
    tput sc

    # Move to bottom of screen
    tput cup $(tput lines) 0

    # Clear line and print cost footer
    tput el
    printf "\033[48;5;234m\033[38;5;196m💰 Claude: \033[38;5;46mIn:\$$INPUT_COST \033[38;5;46mOut:\$$OUTPUT_COST \033[38;5;46mCacheW:\$$CACHE_W_COST \033[38;5;46mCacheR:\$$CACHE_R_COST \033[38;5;196mTOTAL:\$$TOTAL\033[0m"

    # Restore cursor position
    tput rc

    sleep 5
done
