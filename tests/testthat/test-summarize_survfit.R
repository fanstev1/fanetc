# Pins summarize_km()/summarize_cif() output -- values, column names, column
# order, row order AND class -- against a baseline captured before the
# reshape2 -> tidyr refactor.

summarize_baseline<- readRDS(testthat::test_path("fixtures", "summarize_baseline.rds"))

fit_grp<- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
fit_all<- estimate_km(survival::lung, evt_time = time, evt = status)

set.seed(42)
lung_cr<- survival::lung
lung_cr$status2<- factor(replace(lung_cr$status - 1, sample(nrow(lung_cr), 60), 2))
cif_grp<- estimate_cif(lung_cr, time, status2, group = sex)
cif_all<- estimate_cif(lung_cr, time, status2)

test_that("summarize_km() matches the pre-refactor baseline exactly", {
  expect_equal(summarize_km(fit_grp), summarize_baseline$km_strata, tolerance = 0)
  expect_equal(summarize_km(fit_grp, times = c(0, 250, 500, 750)),
               summarize_baseline$km_strata_times, tolerance = 0)
  expect_equal(summarize_km(fit_all), summarize_baseline$km_overall, tolerance = 0)
  expect_equal(summarize_km(fit_grp, failure_fun = TRUE),
               summarize_baseline$km_failure, tolerance = 0)
})

test_that("summarize_cif() matches the pre-refactor baseline exactly (incl. class and column order)", {
  expect_equal(summarize_cif(cif_grp), summarize_baseline$cif_strata, tolerance = 0)
  expect_identical(class(summarize_cif(cif_grp)), class(summarize_baseline$cif_strata))
  expect_identical(names(summarize_cif(cif_grp)), names(summarize_baseline$cif_strata))
  expect_equal(summarize_cif(cif_all), summarize_baseline$cif_overall, tolerance = 0)
  expect_equal(summarize_cif(cif_grp, times = c(0, 250, 500, 750)),
               summarize_baseline$cif_times, tolerance = 0)
})
