# geom_ribbon_step(): a drop-in replacement for ggplot2::geom_ribbon() that
# draws the ribbon as a step function, for confidence bands around step curves
# (Kaplan-Meier, cumulative incidence, ROC).

# Expand x/ymin/ymax data to step-function coordinates, mirroring ggplot2's
# internal stairstep() (geom_step) but carrying ymin and ymax instead of y.
# Sorts by x first; all other columns are carried along by row-indexing.
stairstep_ribbon <- function(data, direction = c("hv", "vh", "mid")) {
  direction <- match.arg(direction)
  data <- data[order(data$x), , drop = FALSE]
  n <- nrow(data)
  if (n <= 1) return(data)

  if (direction == "hv") {
    # y-levels held over each x-interval, jump at the next x (right-continuous)
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
  } else if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  } else { # mid: jump midway between adjacent x values
    xs <- rep(1:(n - 1), each = 2)
    ys <- rep(1:n, each = 2)
  }

  other_cols <- setdiff(names(data), c("x", "ymin", "ymax"))
  if (direction == "mid") {
    mid_x <- (data$x[-1] + data$x[-n]) / 2
    out <- data[c(1, xs, n), other_cols, drop = FALSE]
    out$x <- c(data$x[1], rep(mid_x, each = 2), data$x[n])
  } else {
    out <- data[xs, other_cols, drop = FALSE]
    out$x <- data$x[xs]
  }
  out$ymin <- data$ymin[ys]
  out$ymax <- data$ymax[ys]
  rownames(out) <- NULL
  out
}
