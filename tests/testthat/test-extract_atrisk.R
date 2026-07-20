# Converted from dev-tests/test_extract_atrisk.R.
# Checks extract_atrisk()'s at-risk table shape/values against survival::summary()
# directly, and that add_atrisk() renders the right counts onto a ggplot object
# without disturbing the caller's panel ranges/coord limits.

fit <- survfit(Surv(time, status) ~ sex, data = lung)
breaks <- c(0, 250, 500, 750)
# reference counts from survival itself (number at risk at each requested time)
ref <- summary(fit, times = breaks, extend = TRUE)
ref_wide <- tapply(ref$n.risk, list(ref$time, ref$strata), identity)

test_that("stratified: extract_atrisk() shape and values", {
  r <- extract_atrisk(fit, time.list = breaks)
  expect_identical(class(r), "data.frame")
  expect_identical(names(r), c("time", "1", "2"))
  expect_equal(unname(as.matrix(r[, -1])), unname(ref_wide), ignore_attr = TRUE)
  expect_true(all(vapply(r[-1], is.integer, TRUE)))
})

test_that("no strata: extract_atrisk() shape and values", {
  fit1 <- survfit(Surv(time, status) ~ 1, data = lung)
  ref1 <- summary(fit1, times = breaks, extend = TRUE)
  r1 <- extract_atrisk(fit1, time.list = breaks)
  expect_identical(names(r1), c("time", "Overall"))
  expect_equal(r1$Overall, as.integer(ref1$n.risk))
})

test_that("default time.list still works", {
  r_def <- extract_atrisk(fit)
  expect_identical(names(r_def), c("time", "1", "2"))
})

## ---- add_atrisk renders the right numbers ----

# base plot mimics show_surv/show_cif: no scale limits (they use coord_cartesian or none)
p <- ggplot(data.frame(time = c(0, 1000), prob = c(1, 0)), aes(time, prob)) +
  geom_step()

# text labels regardless of implementation (annotation_custom grob or text layer data)
get_labels <- function(p) {
  trimws(unlist(lapply(p$layers, function(l) {
    g <- l$geom_params$grob
    if (inherits(g, "text")) return(as.character(g$label))
    if (is.data.frame(l$data) && !is.null(l$data$label)) return(as.character(l$data$label))
    NULL
  })))
}
panel_ranges <- function(p) {
  pp <- ggplot_build(p)$layout$panel_params[[1]]
  list(x = pp$x.range, y = pp$y.range)
}

test_that("add_atrisk: header + 2 strata rows + 8 cells rendered correctly", {
  out <- add_atrisk(p, fit, x_break = breaks)
  labs <- get_labels(out)
  expected_counts <- as.character(c(ref_wide))          # 138 62 20 7 90 53 21 3
  expect_true(sum(labs == "At-risk N:") == 1 && all(c("1:", "2:") %in% labs))
  expect_true(all(expected_counts %in% labs) &&
                sum(labs %in% expected_counts) == length(expected_counts))
  expect_equal(panel_ranges(p), panel_ranges(out))
})

test_that("add_atrisk no strata: overall counts rendered, panel ranges unchanged", {
  fit1 <- survfit(Surv(time, status) ~ 1, data = lung)
  ref1 <- summary(fit1, times = breaks, extend = TRUE)
  out1 <- add_atrisk(p, fit1, x_break = breaks)
  labs1 <- get_labels(out1)
  expect_true(all(as.character(ref1$n.risk) %in% labs1))
  expect_equal(panel_ranges(p), panel_ranges(out1))
})

test_that("add_atrisk: existing coord ylim preserved", {
  # caller-supplied coord limits (as in show_surv) must survive add_atrisk
  p_lim <- p + coord_cartesian(ylim = c(0, 1), clip = "on")
  out_lim <- add_atrisk(p_lim, fit, x_break = breaks)
  expect_equal(panel_ranges(p_lim)$y, panel_ranges(out_lim)$y)
})

test_that("extract_atrisk() matches the pre-refactor baseline exactly", {
  atrisk_baseline<- readRDS(testthat::test_path("fixtures", "atrisk_baseline.rds"))
  fit_grp<- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
  fit_all<- estimate_km(survival::lung, evt_time = time, evt = status)
  expect_equal(extract_atrisk(fit_grp), atrisk_baseline$strata_default, tolerance = 0)
  expect_equal(extract_atrisk(fit_grp, time.list = c(100, 300, 500)),
               atrisk_baseline$strata_times, tolerance = 0)
  expect_equal(extract_atrisk(fit_all), atrisk_baseline$overall_default, tolerance = 0)
  expect_equal(extract_atrisk(fit_all, time.list = 0:2, time.scale = 365.25),
               atrisk_baseline$overall_scaled, tolerance = 0)
})

test_that("add_atrisk() falls back to font defaults when plot_theme has no text element", {
  fit<- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
  p<- suppressMessages(show_surv(fit, add_ci = FALSE, add_atrisk = FALSE,
                                 add_pvalue = FALSE, print_fig = FALSE))
  expect_s3_class(add_atrisk(p, fit, plot_theme = theme()), "ggplot")
})
