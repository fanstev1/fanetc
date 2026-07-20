# Pins the stored-call contract that run_logrank_test()/run_gray_test() rely
# on: unqualified survfit/Surv symbols, the formula as written, the data frame
# embedded literally, and extra arguments carried through.

test_that("estimate_km() stores the historical call shape", {
  fit <- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
  expect_identical(deparse(fit$call[[1]]), "survfit")
  expect_identical(deparse(fit$call[[2]]), "Surv(time, status) ~ sex")
  expect_true(is.data.frame(fit$call$data))
  expect_identical(fit$call$conf.type, "log-log")

  fit0 <- estimate_km(survival::lung, evt_time = time, evt = status)
  expect_identical(deparse(fit0$call[[2]]), "Surv(time, status) ~ 1")

  fit_ci <- estimate_km(survival::lung, evt_time = time, evt = status, ci_transformation = "plain")
  expect_identical(fit_ci$call$conf.type, "plain")
})

test_that("estimate_cif() stores the historical call shape", {
  set.seed(42)
  lung_cr <- survival::lung
  lung_cr$status2 <- factor(replace(lung_cr$status - 1, sample(nrow(lung_cr), 60), 2))
  fit <- estimate_cif(lung_cr, time, status2, group = sex)
  expect_identical(deparse(fit$call[[1]]), "survfit")
  expect_identical(deparse(fit$call[[2]]), "Surv(time, status2) ~ sex")
  expect_true(is.data.frame(fit$call$data))
})

test_that("the stored call re-evaluates to the same fit (run_*_test contract)", {
  fit <- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
  refit <- eval(fit$call)
  expect_equal(refit$surv, fit$surv)
  expect_silent(pv <- run_logrank_test(fit))
  expect_true(is.numeric(pv) && length(pv) == 1)
})
