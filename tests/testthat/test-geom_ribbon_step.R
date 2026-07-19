# Unit tests for geom_ribbon_step() and its stairstep_ribbon() helper
# (spec: docs/superpowers/specs/2026-07-18-geom-ribbon-step-design.md).

# The original manual "hv" index logic (step_ribbon() / the old
# prepare_survfit() pre-stepping), kept as the independent reference.
ref_step_hv <- function(df) {
  df <- df[order(df$x), ]
  nn <- nrow(df)
  ys <- rep(1:nn, each = 2)[-2 * nn]
  xs <- c(1, rep(2:nn, each = 2))
  data.frame(x = df$x[xs], ymin = df$ymin[ys], ymax = df$ymax[ys])
}

roc <- data.frame(
  x    = c(0, .1, .25, .5, 1),
  ymin = c(0, .2, .45, .7, .95),
  ymax = c(.05, .4, .65, .85, 1)
)

test_that("stairstep_ribbon() hv matches the original step_ribbon() index logic", {
  out <- fanetc:::stairstep_ribbon(roc)
  ref <- ref_step_hv(roc)
  expect_equal(nrow(out), 2 * nrow(roc) - 1)
  expect_equal(out$x, ref$x)
  expect_equal(out$ymin, ref$ymin)
  expect_equal(out$ymax, ref$ymax)
})

test_that("stairstep_ribbon() sorts by x first", {
  set.seed(1)
  shuffled <- roc[sample(nrow(roc)), ]
  expect_equal(fanetc:::stairstep_ribbon(shuffled), fanetc:::stairstep_ribbon(roc))
})

test_that("stairstep_ribbon() vh jumps at the current x", {
  out <- fanetc:::stairstep_ribbon(roc, direction = "vh")
  nn <- nrow(roc)
  xs <- rep(1:nn, each = 2)[-2 * nn]
  ys <- c(1, rep(2:nn, each = 2))
  expect_equal(out$x, roc$x[xs])
  expect_equal(out$ymin, roc$ymin[ys])
  expect_equal(out$ymax, roc$ymax[ys])
})

test_that("stairstep_ribbon() mid jumps midway between adjacent x values", {
  out <- fanetc:::stairstep_ribbon(roc, direction = "mid")
  nn <- nrow(roc)
  mids <- (roc$x[-1] + roc$x[-nn]) / 2
  expect_equal(out$x, c(roc$x[1], rep(mids, each = 2), roc$x[nn]))
  expect_equal(out$ymin, rep(roc$ymin, each = 2))
  expect_equal(out$ymax, rep(roc$ymax, each = 2))
})

test_that("stairstep_ribbon() carries other columns along by row", {
  withcols <- transform(roc, fill = "grey20", PANEL = 1L)
  out <- fanetc:::stairstep_ribbon(withcols)
  expect_equal(out$fill, rep("grey20", 2 * nrow(roc) - 1))
  expect_equal(out$PANEL, rep(1L, 2 * nrow(roc) - 1))
})

test_that("stairstep_ribbon() returns 0- and 1-row input unchanged", {
  expect_equal(fanetc:::stairstep_ribbon(roc[1, ]), roc[1, ], ignore_attr = TRUE)
  expect_equal(nrow(fanetc:::stairstep_ribbon(roc[0, ])), 0)
})

test_that("stairstep_ribbon() rejects an invalid direction", {
  expect_error(fanetc:::stairstep_ribbon(roc, direction = "diagonal"))
})
