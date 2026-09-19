#!/usr/bin/env python3
"""Calculate extrapolation predictions for doom and score targets."""

import json
import sys
from pathlib import Path
from datetime import datetime

MCTS_ROOT = Path("/Users/gremus/cthulhu-wars-mcts-prototype")
CANONICAL_STORE = MCTS_ROOT / "brain-dashboard" / "canonical_games.json"
PREDICTIONS_FILE = MCTS_ROOT / "brain-dashboard" / "predictions_history.json"
STATUS_LOG = MCTS_ROOT / "brain-dashboard" / "latest_predictions.json"

def calculate_predictions(run_tag):
    """Calculate linear regression predictions for doom=40 and score=0.7."""

    # Load canonical store
    if not CANONICAL_STORE.exists():
        return None

    store = json.loads(CANONICAL_STORE.read_text())
    games = store.get("games", [])

    # Filter for target run, arena games only
    run_games = [g for g in games if g["run"] == run_tag and g["type"] == "arena"]

    if len(run_games) < 2:
        return None

    # Group by iteration, calculate averages
    from collections import defaultdict
    iter_data = defaultdict(lambda: {"doom": [], "score": []})

    for g in run_games:
        iter_data[g["iter"]]["doom"].append(g["doom"])
        iter_data[g["iter"]]["score"].append(g["score"])

    # Calculate averages per iteration
    iters = sorted(iter_data.keys())
    iter_points = []
    for i in iters:
        avg_doom = sum(iter_data[i]["doom"]) / len(iter_data[i]["doom"])
        avg_score = sum(iter_data[i]["score"]) / len(iter_data[i]["score"])
        iter_points.append({
            "iter": i,
            "doom": avg_doom,
            "score": avg_score
        })

    if len(iter_points) < 2:
        return None

    # Linear regression on doom
    n = len(iter_points)
    x = [p["iter"] for p in iter_points]
    y_doom = [p["doom"] for p in iter_points]
    y_score = [p["score"] for p in iter_points]

    sx = sum(x)
    sx2 = sum(xi**2 for xi in x)

    # Doom regression
    sy_doom = sum(y_doom)
    sxy_doom = sum(x[i] * y_doom[i] for i in range(n))
    m_doom = (n * sxy_doom - sx * sy_doom) / (n * sx2 - sx * sx)
    b_doom = (sy_doom - m_doom * sx) / n

    # Score regression
    sy_score = sum(y_score)
    sxy_score = sum(x[i] * y_score[i] for i in range(n))
    m_score = (n * sxy_score - sx * sy_score) / (n * sx2 - sx * sx)
    b_score = (sy_score - m_score * sx) / n

    # Extrapolate to targets
    current_max_iter = max(iters)

    # Doom → 40
    target_doom = 40
    iters_to_doom_40 = None
    if m_doom > 0.01:  # Positive trend
        predicted_iter = (target_doom - b_doom) / m_doom
        if predicted_iter > current_max_iter:
            iters_to_doom_40 = int(predicted_iter - current_max_iter)

    # Score → 0.7
    target_score = 0.7
    iters_to_score_07 = None
    if m_score > 0.001:  # Positive trend
        predicted_iter = (target_score - b_score) / m_score
        if predicted_iter > current_max_iter:
            iters_to_score_07 = int(predicted_iter - current_max_iter)

    return {
        "run": run_tag,
        "current_iter": current_max_iter,
        "doom_slope": m_doom,
        "doom_intercept": b_doom,
        "score_slope": m_score,
        "score_intercept": b_score,
        "iters_to_doom_40": iters_to_doom_40,
        "iters_to_score_07": iters_to_score_07,
        "timestamp": datetime.now().isoformat()
    }

def save_prediction(prediction):
    """Save prediction to history and latest status."""
    if not prediction:
        return

    # Save to latest (for dashboard API)
    STATUS_LOG.write_text(json.dumps(prediction, indent=2))

    # Append to history
    history = []
    if PREDICTIONS_FILE.exists():
        try:
            history = json.loads(PREDICTIONS_FILE.read_text())
        except:
            history = []

    history.append(prediction)

    # Keep last 1000 entries
    history = history[-1000:]

    PREDICTIONS_FILE.write_text(json.dumps(history, indent=2))

    print(f"Predictions saved: doom→40 in {prediction.get('iters_to_doom_40', 'N/A')} iters, score→0.7 in {prediction.get('iters_to_score_07', 'N/A')} iters")

if __name__ == "__main__":
    run_tag = sys.argv[1] if len(sys.argv) > 1 else "R28"
    prediction = calculate_predictions(run_tag)
    if prediction:
        save_prediction(prediction)
    else:
        print("Insufficient data for predictions")
