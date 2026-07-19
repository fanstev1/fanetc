# Integration tests: the CI ribbons drawn by show_surv()/show_cif() via
# geom_ribbon_step() must be identical to the old manual pre-stepping that
# lived in prepare_survfit()
# (spec: docs/superpowers/specs/2026-07-18-geom-ribbon-step-design.md).

# The old manual "hv" stepping formerly applied per stratum in prepare_survfit().
old_step_ci <- function(df) {
  nn <- nrow(df)
  ys <- rep(1:nn, each = 2)[-2 * nn]
  xs <- c(1, rep(2:nn, each = 2))
  dplyr::tibble(
    time      = df$time[xs],
    conf_low  = df$conf_low[ys],
    conf_high = df$conf_high[ys]
  )
}

ribbon_layer_data <- function(p) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)))
  expect_length(i, 1)
  layer_data(p, i)
}

fit <- estimate_km(lung, evt_time = time, evt = status, group = sex)

set.seed(42)
lung_cr <- lung
lung_cr$cr_evt <- factor(ifelse(lung$status == 2, sample(1:2, nrow(lung), TRUE, prob = c(.7, .3)), 0))
fit_cr <- estimate_cif(lung_cr, evt_time = time, evt = cr_evt, group = sex)

# raw (unstepped) per-stratum CI limits, as post-refactor prepare_survfit()
# stores them in plot_ci_d
km_ci_raw <- prepare_survfit(fit) %>%
  dplyr::select(strata, plot_ci_d) %>%
  tidyr::unnest(cols = c(plot_ci_d))

test_that("show_surv() survival-function ribbon equals the old manual stepping", {
  p <- suppressMessages(show_surv(fit, add_ci = TRUE, add_atrisk = FALSE,
                                  add_pvalue = FALSE, print_fig = FALSE))
  ld <- ribbon_layer_data(p)
  strata_levels <- levels(km_ci_raw$strata)
  expect_equal(length(unique(ld$group)), length(strata_levels))
  for (i in seq_along(strata_levels)) {
    ref <- old_step_ci(km_ci_raw[km_ci_raw$strata == strata_levels[i], ])
    grp <- ld[ld$group == i, ]
    expect_equal(grp$x, ref$time)
    expect_equal(grp$ymin, ref$conf_low)
    expect_equal(grp$ymax, ref$conf_high)
  }
})

test_that("show_surv() failure-function (plot_cdf = TRUE) ribbon equals the old pipeline", {
  p <- suppressMessages(show_surv(fit, plot_cdf = TRUE, add_ci = TRUE, add_atrisk = FALSE,
                                  add_pvalue = FALSE, print_fig = FALSE))
  ld <- ribbon_layer_data(p)
  strata_levels <- levels(km_ci_raw$strata)
  for (i in seq_along(strata_levels)) {
    stepped <- old_step_ci(km_ci_raw[km_ci_raw$strata == strata_levels[i], ])
    grp <- ld[ld$group == i, ]
    # the old pipeline stepped first, then took 1 - x and swapped low/high
    expect_equal(grp$x, stepped$time)
    expect_equal(grp$ymin, 1 - stepped$conf_high)
    expect_equal(grp$ymax, 1 - stepped$conf_low)
  }
  expect_true(all(ld$ymin <= ld$ymax, na.rm = TRUE))
})

test_that("show_cif() ribbon equals the old manual stepping", {
  p <- suppressMessages(show_cif(fit_cr, add_ci = TRUE, add_atrisk = FALSE,
                                 add_pvalue = FALSE, print_fig = FALSE))
  ld <- ribbon_layer_data(p)
  cif_ci_raw <- prepare_survfit(fit_cr) %>%
    dplyr::filter(state %in% 1) %>%  # show_cif() default evt_type = 1
    dplyr::select(strata, plot_ci_d) %>%
    tidyr::unnest(cols = c(plot_ci_d))
  strata_levels <- levels(droplevels(cif_ci_raw$strata))
  expect_equal(length(unique(ld$group)), length(strata_levels))
  for (i in seq_along(strata_levels)) {
    ref <- old_step_ci(cif_ci_raw[cif_ci_raw$strata == strata_levels[i], ])
    grp <- ld[ld$group == i, ]
    expect_equal(grp$x, ref$time)
    expect_equal(grp$ymin, ref$conf_low)
    expect_equal(grp$ymax, ref$conf_high)
  }
})
