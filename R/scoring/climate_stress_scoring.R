# ============================================================
# GENERIC CLIMATE-STRESS SCORING HELPERS
# ============================================================
# Policy:
# 1. Score baseline and future on the SAME scale.
# 2. Climate-stress change = Future score - Baseline score.
# 3. Combine dimensions, not duplicated indicators.
# 4. Optional dimensions do not alter the core application score unless
#    the user explicitly requests a selected-dimensions composite.
# ============================================================

clamp01 <- function(x) pmax(0, pmin(1, x))

score_threshold <- function(x, onset, severe, direction = c("high", "low")) {
  direction <- match.arg(direction)
  stopifnot(is.finite(onset), is.finite(severe), onset != severe)

  if (direction == "high") {
    if (severe <= onset) stop("For high-risk direction severe must be > onset.")
    z <- (x - onset) / (severe - onset)
  } else {
    if (severe >= onset) stop("For low-risk direction severe must be < onset.")
    z <- (onset - x) / (onset - severe)
  }

  100 * clamp01(z)
}

score_fixed_relative <- function(x, p05, p95, direction = c("high", "low")) {
  direction <- match.arg(direction)
  if (!is.finite(p05) || !is.finite(p95) || p95 <= p05) {
    stop("Valid fixed p05/p95 references are required.")
  }

  if (direction == "high") {
    z <- (x - p05) / (p95 - p05)
  } else {
    z <- (p95 - x) / (p95 - p05)
  }

  100 * clamp01(z)
}

stress_change <- function(baseline_score, future_score) {
  future_score - baseline_score
}

combine_indicator_scores <- function(scores, weights = NULL) {
  scores <- as.numeric(scores)
  ok <- is.finite(scores)
  if (!any(ok)) return(NA_real_)
  scores <- scores[ok]

  if (is.null(weights)) return(mean(scores))
  weights <- as.numeric(weights)[ok]
  if (sum(weights, na.rm = TRUE) <= 0) return(NA_real_)
  weighted.mean(scores, weights, na.rm = TRUE)
}

combine_dimension_scores <- function(dimension_scores, weights = NULL) {
  # Use this for an application score. Each input should already represent
  # a distinct dimension, so correlated raw indicators are not counted twice.
  combine_indicator_scores(dimension_scores, weights)
}

score_baseline_future <- function(
    baseline_value,
    future_value,
    method = c("threshold_direct", "fixed_relative"),
    direction = c("high", "low"),
    onset = NA_real_,
    severe = NA_real_,
    p05 = NA_real_,
    p95 = NA_real_) {

  method <- match.arg(method)
  direction <- match.arg(direction)

  scorer <- if (method == "threshold_direct") {
    function(x) score_threshold(x, onset, severe, direction)
  } else {
    function(x) score_fixed_relative(x, p05, p95, direction)
  }

  baseline_score <- scorer(baseline_value)
  future_score <- scorer(future_value)

  list(
    baseline_score = baseline_score,
    future_score = future_score,
    climate_stress_change = stress_change(baseline_score, future_score)
  )
}
