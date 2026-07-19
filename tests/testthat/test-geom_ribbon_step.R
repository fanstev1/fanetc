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

# ---- geom_ribbon_step() layer behavior ----

test_that("geom_ribbon_step() is a drop-in for geom_ribbon() with stepped output", {
  p <- ggplot(roc, aes(x, ymin = ymin, ymax = ymax)) +
    geom_ribbon_step(alpha = .3, fill = "steelblue")
  ld <- layer_data(p)
  ref <- ref_step_hv(roc)
  expect_equal(ld$x, ref$x)
  expect_equal(ld$ymin, ref$ymin)
  expect_equal(ld$ymax, ref$ymax)
  expect_equal(unique(ld$fill), "steelblue")
})

test_that("geom_ribbon_step() output does not depend on input row order", {
  set.seed(1)
  shuffled <- roc[sample(nrow(roc)), ]
  p_sorted <- ggplot(roc, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step()
  p_shuffled <- ggplot(shuffled, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step()
  expect_equal(layer_data(p_shuffled), layer_data(p_sorted))
})

test_that("geom_ribbon_step() steps each group independently", {
  two <- rbind(transform(roc, g = "a"),
               transform(roc, ymin = ymin / 2, ymax = ymax / 2, g = "b"))
  p <- ggplot(two, aes(x, ymin = ymin, ymax = ymax, fill = g)) + geom_ribbon_step()
  ld <- layer_data(p)
  for (i in 1:2) {
    ref <- ref_step_hv(two[two$g == c("a", "b")[i], ])
    grp <- ld[ld$group == i, ]
    expect_equal(grp$x, ref$x)
    expect_equal(grp$ymin, ref$ymin)
    expect_equal(grp$ymax, ref$ymax)
  }
})

test_that("geom_ribbon_step() keeps NA limits as gaps, without warnings", {
  withna <- roc
  withna$ymin[3] <- NA
  p <- ggplot(withna, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step()
  expect_silent(ld <- layer_data(p))
  # hv holds row 3 over two step segments, so its NA appears twice
  expect_equal(nrow(ld), 2 * nrow(withna) - 1)
  expect_equal(sum(is.na(ld$ymin)), 2)
})

test_that("geom_ribbon_step() supports direction = 'vh' and 'mid' at layer level", {
  p_vh <- ggplot(roc, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step(direction = "vh")
  expect_equal(layer_data(p_vh)$ymax,
               fanetc:::stairstep_ribbon(roc, direction = "vh")$ymax)
  p_mid <- ggplot(roc, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step(direction = "mid")
  expect_equal(layer_data(p_mid)$x,
               fanetc:::stairstep_ribbon(roc, direction = "mid")$x)
})

test_that("geom_ribbon_step() rejects an invalid direction at construction", {
  expect_error(geom_ribbon_step(direction = "diagonal"))
})

test_that("geom_ribbon_step() errors informatively when ymin/ymax are not mapped", {
  p <- ggplot(roc, aes(x)) + geom_ribbon_step()
  expect_error(ggplot_build(p), "ymin")
})
