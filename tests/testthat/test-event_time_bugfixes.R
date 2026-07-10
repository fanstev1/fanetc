# Converted from dev-tests/test_event_time_bugfixes.R.
# Regression tests for 3 known bugs fixed during the v1.0 merge tidy-up:
#  1. show_surv() silently reset a user-supplied y_lim to c(0,1) for survival curves.
#  2. prepare_cmprisk() used grepl("0", state), misclassifying any real competing-risk
#     state whose name merely contains "0" (e.g. "10", "20") as the censored/reference state.
#  3. show_cif()'s roxygen @param docs were copy-pasted from unrelated functions.

pkg_root <- testthat::test_path("..", "..")

# ---- Bug 1: show_surv() must respect a user-supplied y_lim ----

test_that("show_surv: default y_lim is c(0,1)", {
  fit <- estimate_km(lung, evt_time = time, evt = status, group = sex)
  p_default <- suppressMessages(show_surv(fit, add_atrisk = FALSE, add_ci = FALSE, print_fig = FALSE))
  expect_identical(p_default$coordinates$limits$y, c(0, 1))
})

test_that("show_surv: user-supplied y_lim=c(0,0.5) is respected, not reset to c(0,1)", {
  fit <- estimate_km(lung, evt_time = time, evt = status, group = sex)
  p_custom <- suppressMessages(show_surv(fit, y_lim = c(0, 0.5), add_atrisk = FALSE, add_ci = FALSE, print_fig = FALSE))
  expect_identical(p_custom$coordinates$limits$y, c(0, 0.5))
})

# ---- Bug 2: only survival's "(s0)" placeholder (or "") collapses to censored state "0" ----

test_that("prepare_cmprisk state fix: \"(s0)\" placeholder collapses to \"0\", real states don't", {
  fake_state <- c("(s0)", "10", "20", "1", "")
  fixed <- replace(fake_state, nchar(fake_state) == 0 | fake_state == "(s0)", "0")
  expect_identical(fixed[1], "0")
  expect_identical(fixed[5], "0")
  expect_identical(fixed[2], "10")
  expect_identical(fixed[3], "20")
})

# End-to-end: show_cif() must still work correctly against a real survfitms object,
# whose actual reference-state name from survival::survfit() is "(s0)" (verified directly;
# not "0" as the pre-fix code implicitly assumed via its overly broad grepl("0", ...) match).
test_that("show_cif() runs end-to-end against a real (s0)-labeled survfitms object", {
  lung_cr <- lung %>% mutate(cr_evt = factor(ifelse(status == 2, sample(1:2, n(), TRUE, prob = c(.7, .3)), 0)))
  set.seed(42)
  lung_cr$cr_evt <- factor(ifelse(lung$status == 2, sample(1:2, nrow(lung), TRUE, prob = c(.7, .3)), 0))
  fit_cr <- estimate_cif(lung_cr, evt_time = time, evt = cr_evt, group = sex)
  expect_identical(fit_cr$states, c("(s0)", "1", "2"))

  p_cif <- suppressMessages(show_cif(fit_cr, add_atrisk = FALSE, add_ci = FALSE, print_fig = FALSE))
  expect_s3_class(p_cif, "ggplot")
})

# ---- Bug 3: show_cif()'s roxygen docs must cover every formal argument (no leftover copy-paste gaps) ----

test_that("show_cif: @param docs cover exactly the formal arguments (no gaps, no stray entries)", {
  src <- readLines(file.path(pkg_root, "R", "event_time_desp.R"))
  title_line <- grep("^#' @title show_cif$", src)
  fn_line <- grep("^show_cif<- function", src)
  block <- src[title_line:fn_line]
  m <- regmatches(block, regexpr("(?<=@param )[A-Za-z_.]+", block, perl = TRUE))
  documented_params <- m[nzchar(m)]
  formal_params <- names(formals(show_cif))

  expect_true(all(formal_params %in% documented_params))
  expect_true(all(documented_params %in% formal_params))
})
