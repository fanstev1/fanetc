# Converted from dev-tests/test_construct_equiv.R.
# Checks that the vectorized construct_surv_var()/construct_cmprisk_var() (in
# R/construct_event_time.R) give results equivalent to the pre-refactor reference
# implementation (tests/testthat/fixtures/ref_construct.R, sourced into its own
# environment so its old, same-named functions don't shadow the package's
# current exports).
#
# The original dev-tests script also measured and printed wall-clock speedup
# (old vs. new) at the end. That's dropped here entirely: it isn't a pass/fail
# assertion and would just make automated test runs slower and non-deterministic
# to no benefit (timing comparisons are inherently flaky in a shared test-runner
# environment).

old_env <- new.env(parent = globalenv())
sys.source(testthat::test_path("fixtures", "ref_construct.R"), envir = old_env)
old_surv <- old_env$construct_surv_var
old_cmp <- old_env$construct_cmprisk_var

quiet <- function(expr) {
  # silence the flagged-row printout and the zero/negative warnings, keep result
  out <- withCallingHandlers(
    capture.output(res <- expr),
    warning = function(w) invokeRestart("muffleWarning")
  )
  res
}

same <- function(a, b) {
  isTRUE(all.equal(as.data.frame(a), as.data.frame(b), check.attributes = FALSE)) &&
    identical(unname(sapply(a, function(x) class(x)[1])),
              unname(sapply(b, function(x) class(x)[1]))) &&
    identical(names(a), names(b)) &&
    identical(lapply(a, levels), lapply(b, levels))
}

make_data <- function(n, seed = 1) {
  set.seed(seed)
  d0 <- as.Date("2020-01-01")
  df <- data.frame(
    patid = seq_len(n),
    idx_dt = d0 + sample(0:100, n, TRUE),
    evt_dt = d0 + sample(0:400, n, TRUE),
    cmp1_dt = d0 + sample(0:400, n, TRUE),
    cmp2_dt = d0 + sample(0:400, n, TRUE),
    end_dt = d0 + sample(100:500, n, TRUE)
  )
  df$evt_dt[sample(n, n * 0.4)] <- NA
  df$cmp1_dt[sample(n, n * 0.5)] <- NA
  df$cmp2_dt[sample(n, n * 0.6)] <- NA
  # edge cases: tie evt==end, tie cmp1==end, tie evt==cmp1, zero time, negative time
  df$evt_dt[1] <- df$end_dt[1]
  df$cmp1_dt[2] <- df$end_dt[2]
  df$evt_dt[3] <- df$cmp1_dt[3]
  df$evt_dt[4] <- df$idx_dt[4]                    # time zero
  df$evt_dt[5] <- df$idx_dt[5] - 10               # negative time
  df
}

df <- make_data(3000)

## ---- construct_surv_var ----

test_that("surv: default names match old implementation", {
  expect_true(same(quiet(old_surv(df, patid, idx_dt, evt_dt, end_dt)),
                   quiet(construct_surv_var(df, patid, idx_dt, evt_dt, end_dt))))
})

test_that("surv: custom surv_varname matches old implementation", {
  expect_true(same(
    quiet(old_surv(df, patid, idx_dt, evt_dt, end_dt, surv_varname = c("day_dth", "dth"))),
    quiet(construct_surv_var(df, patid, idx_dt, evt_dt, end_dt, surv_varname = c("day_dth", "dth")))
  ))
})

test_that("surv: append = TRUE matches old implementation", {
  expect_true(same(
    quiet(old_surv(df, patid, idx_dt, evt_dt, end_dt, append = TRUE)),
    quiet(construct_surv_var(df, patid, idx_dt, evt_dt, end_dt, append = TRUE))
  ))
})

## ---- construct_cmprisk_var ----

test_that("cmprisk: 1 competing event matches old implementation", {
  expect_true(same(
    quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt)),
    quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt))
  ))
})

test_that("cmprisk: 2 competing events matches old implementation", {
  expect_true(same(
    quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt, cmp2 = cmp2_dt)),
    quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt, cmp2 = cmp2_dt))
  ))
})

test_that("cmprisk: cmprisk_varname (old arg name) matches old implementation", {
  expect_true(same(
    quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = c("ftime", "fstatus"), cmp1 = cmp1_dt)),
    quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = c("ftime", "fstatus"), cmp1 = cmp1_dt))
  ))
})

test_that("cmprisk: varname (new arg name) matches old cmprisk_varname", {
  expect_true(same(
    quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = c("ftime", "fstatus"), cmp1 = cmp1_dt)),
    quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, varname = c("ftime", "fstatus"), cmp1 = cmp1_dt))
  ))
})

test_that("cmprisk: append = TRUE matches old implementation", {
  expect_true(same(
    quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, append = TRUE, cmp1 = cmp1_dt)),
    quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, append = TRUE, cmp1 = cmp1_dt))
  ))
})

test_that("cmprisk: OLD POSITIONAL call (cmprisk_varname & append as args 6-7)", {
  expect_true(same(
    quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, c("ftime", "fstatus"), TRUE, cmp1 = cmp1_dt)),
    quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, c("ftime", "fstatus"), TRUE, cmp1 = cmp1_dt))
  ))
})

test_that("cmprisk: 0 competing events -- new fixes old censored-NA bug", {
  # Old fn has a bug with 0 competing events (paste0 zero-length recycling makes a
  # phantom "cmp_evt_" level; censored subjects become evt = NA). Assert the NEW
  # behavior is correct instead: matches old everywhere except censored get evt = 0.
  o <- quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt))
  n2 <- quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt))
  cens <- is.na(o$evt) & !is.na(o$evt_time)
  expect_true(isTRUE(all.equal(o$evt_time, n2$evt_time)))
  expect_true(all(as.character(n2$evt[cens]) == "0"))
  expect_identical(as.character(o$evt[!cens]), as.character(n2$evt[!cens]))
})

## ---- new-only: all-dates-missing subject, no zero/negative rows present ----

test_that("cmprisk: all-NA subject -- old errors, new returns an NA row", {
  df_na <- make_data(50, seed = 2)[-(1:5), ]        # drop the forced edge rows
  df_na$evt_dt[1] <- df_na$cmp1_dt[1] <- df_na$cmp2_dt[1] <- df_na$end_dt[1] <- NA
  old_res <- tryCatch(quiet(old_cmp(df_na, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt)),
                      error = function(e) "OLD ERRORS")
  new_res <- quiet(construct_cmprisk_var(df_na, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt))
  expect_identical(old_res, "OLD ERRORS")
  expect_true(is.na(new_res$evt_time[1]))
  expect_true(is.na(new_res$evt[1]))
})
