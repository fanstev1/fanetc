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

# Stat powering geom_ribbon_step(). Only "x" is declared required so the
# default Stat$compute_layer() NA-removal leaves ymin/ymax untouched: NA
# confidence limits must reach GeomRibbon un-dropped and warning-free, where
# they become gaps in the ribbon -- exactly what geom_ribbon() does with
# pre-stepped data (see the old prepare_survfit() pipeline). setup_data()
# supplies the ymin/ymax presence check instead.
StatStepRibbon <- ggproto("StatStepRibbon", Stat,
  required_aes = "x",
  setup_data = function(data, params) {
    if (!all(c("ymin", "ymax") %in% names(data))) {
      stop("geom_ribbon_step() requires the ymin and ymax aesthetics.",
           call. = FALSE)
    }
    data
  },
  compute_group = function(data, scales, direction = "hv") {
    stairstep_ribbon(data, direction = direction)
  }
)

#' @title geom_ribbon_step
#'
#' @details
#' A drop-in replacement for \code{ggplot2::geom_ribbon()} that draws the
#' ribbon as a step function instead of interpolating linearly between points,
#' for confidence bands around step curves such as Kaplan-Meier, cumulative
#' incidence or ROC curves. Within each group the data are sorted by x before
#' the steps are built, so the input row order does not matter. Rows with NA
#' ymin/ymax are kept and yield gaps in the ribbon, as in geom_ribbon().
#'
#' @param mapping,data,na.rm,show.legend,inherit.aes as in \code{ggplot2::geom_ribbon()}
#' @param ... other arguments passed on to \code{ggplot2::layer()}, e.g. the
#'   fill, alpha, colour, linetype or outline.type of the ribbon, as in
#'   \code{ggplot2::geom_ribbon()}
#' @param direction the step direction, as in \code{ggplot2::geom_step()}:
#'   "hv" (default) holds each value over the following x-interval and jumps at
#'   the next x (right-continuous, as survival and ROC curves need); "vh" jumps
#'   at the current x; "mid" jumps midway between adjacent x values
#' @return a ggplot2 layer
#' @examples
#' library(ggplot2)
#' roc <- data.frame(fpr      = c(0, .1, .25, .5, 1),
#'                   tpr      = c(.1, .3, .55, .8, 1),
#'                   tpr_low  = c(0, .2, .45, .7, .95),
#'                   tpr_high = c(.15, .4, .65, .85, 1))
#' ggplot(roc, aes(fpr, ymin = tpr_low, ymax = tpr_high)) +
#'   geom_ribbon_step(alpha = 0.3) +
#'   geom_step(aes(y = tpr))
#' @export
geom_ribbon_step<- function(mapping = NULL, data = NULL, ...,
                            direction = "hv",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  direction <- match.arg(direction, c("hv", "vh", "mid"))
  layer(
    data = data,
    mapping = mapping,
    stat = StatStepRibbon,
    geom = GeomRibbon,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(direction = direction, na.rm = na.rm, ...)
  )
}
