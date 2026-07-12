# Tests for the multiple-imputation helpers in R/fan_util_fun.R:
# calculate_type3_mi(), summarize_mi_glm(), summarize_mi_coxph(),
# generate_mi_glm_termplot_df().
#
# These functions historically relied on require(mitools) side-effects (and on
# the user having mice attached from creating the mira object), so they broke
# at runtime once fanetc v1.0 stopped auto-attaching packages. Every test here
# runs with mice/mitools/sandwich explicitly DETACHED (namespaces loaded is
# fine -- that is what ::-qualified calls need) to pin down that the functions
# work from a clean search path.

detach_if_attached <- function(pkg) {
  nm <- paste0("package:", pkg)
  if (nm %in% search()) detach(nm, character.only = TRUE)
}

detach_mi_packages <- function() {
  for (pkg in c("mitools", "mice", "sandwich")) detach_if_attached(pkg)
}

# Fixtures are built lazily and cached: mice::mice() is the slowest step and
# several tests share the same mira objects. Only ::-qualified calls are used
# so building the fixtures never attaches anything.
glm_mira_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      imp <- mice::mice(mice::nhanes2, m = 3, seed = 101, printFlag = FALSE)
      cache <<- with(imp, glm(chl ~ age + bmi))
    }
    cache
  }
})

cox_mira_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      d <- survival::lung[, c("time", "status", "age", "sex", "wt.loss")]
      imp <- mice::mice(d, m = 3, seed = 202, printFlag = FALSE)
      cache <<- with(imp,
                     survival::coxph(survival::Surv(time, status) ~ age + sex + wt.loss))
    }
    cache
  }
})

test_that("calculate_type3_mi returns Wald tests without mice/mitools attached", {
  skip_if_not_installed("mice")
  skip_if_not_installed("mitools")
  detach_mi_packages()

  out <- calculate_type3_mi(glm_mira_fixture())

  expect_type(out, "list")
  res <- do.call(rbind, out)
  expect_true(all(c("var", "rid", "df", "stat", "chisq_p") %in% names(res)))
  # nhanes2$age is a 3-level factor -> its type-3 test has 2 df
  expect_equal(res$df[res$var == "age"], 2)
  expect_equal(res$df[res$var == "bmi"], 1)
  expect_true(all(res$chisq_p > 0 & res$chisq_p <= 1))
  expect_true(all(res$stat >= 0))
})

test_that("summarize_mi_glm pools by Rubin's rules without mice/mitools attached", {
  skip_if_not_installed("mice")
  skip_if_not_installed("mitools")
  detach_mi_packages()

  mira <- glm_mira_fixture()
  res <- summarize_mi_glm(mira)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("var", "stat", "pval", "est") %in% names(res)))

  # Rubin's-rules point estimate = mean of the per-imputation coefficients
  expected <- rowMeans(sapply(mira$analyses, coef))
  idx <- match(names(expected), res$var)
  expect_false(anyNA(idx))
  expect_equal(res$est[idx], unname(expected), tolerance = 1e-10)

  # exponentiate = TRUE exponentiates the estimates
  res_exp <- summarize_mi_glm(mira, exponentiate = TRUE)
  expect_equal(res_exp$est[match(names(expected), res_exp$var)],
               unname(exp(expected)), tolerance = 1e-10)
})

test_that("summarize_mi_coxph pools by Rubin's rules without mice/mitools attached", {
  skip_if_not_installed("mice")
  skip_if_not_installed("mitools")
  detach_mi_packages()

  mira <- cox_mira_fixture()
  res <- summarize_mi_coxph(mira)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("var", "stat", "pval", "est") %in% names(res)))

  # default exponentiate = TRUE: est = exp(mean per-imputation coefficient)
  expected <- exp(rowMeans(sapply(mira$analyses, coef)))
  idx <- match(names(expected), res$var)
  expect_false(anyNA(idx))
  expect_equal(res$est[idx], unname(expected), tolerance = 1e-10)
})

test_that("generate_mi_glm_termplot_df builds term plot data without mice/mitools attached", {
  skip_if_not_installed("mice")
  skip_if_not_installed("mitools")
  detach_mi_packages()

  res <- generate_mi_glm_termplot_df(glm_mira_fixture())

  expect_type(res, "list")
  expect_named(res, c("age", "bmi"))
  for (d in res) {
    expect_true(all(c("x", "y", "se", "conf_low", "conf_high") %in% names(d)))
    expect_true(all(d$se > 0))
    expect_true(all(d$conf_low < d$conf_high))
  }
})
