suppressPackageStartupMessages({
  library(lazyeval); library(dplyr); library(rlang)  # rlang last so its as_name() wins
})
setwd("/Users/sfan/Documents/projects/fanetc")
S <- "dev-tests"

old_env <- new.env(parent = globalenv())
sys.source(file.path(S, "ref_construct.R"), envir = old_env)
old_surv <- old_env$construct_surv_var
old_cmp <- old_env$construct_cmprisk_var
source("R/construct_event_time.R")

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

ok <- TRUE
check <- function(label, a, b) {
  if (same(a, b)) cat("PASS:", label, "\n")
  else { cat("FAIL:", label, "\n"); ok <<- FALSE }
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
check("surv: default names",
      quiet(old_surv(df, patid, idx_dt, evt_dt, end_dt)),
      quiet(construct_surv_var(df, patid, idx_dt, evt_dt, end_dt)))
check("surv: custom surv_varname",
      quiet(old_surv(df, patid, idx_dt, evt_dt, end_dt, surv_varname = c("day_dth", "dth"))),
      quiet(construct_surv_var(df, patid, idx_dt, evt_dt, end_dt, surv_varname = c("day_dth", "dth"))))
check("surv: append = TRUE",
      quiet(old_surv(df, patid, idx_dt, evt_dt, end_dt, append = TRUE)),
      quiet(construct_surv_var(df, patid, idx_dt, evt_dt, end_dt, append = TRUE)))

## ---- construct_cmprisk_var ----
check("cmprisk: 1 competing event",
      quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt)),
      quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt)))
check("cmprisk: 2 competing events",
      quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt, cmp2 = cmp2_dt)),
      quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt, cmp2 = cmp2_dt)))
check("cmprisk: cmprisk_varname (old arg name)",
      quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = c("ftime", "fstatus"), cmp1 = cmp1_dt)),
      quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = c("ftime", "fstatus"), cmp1 = cmp1_dt)))
check("cmprisk: varname (new arg name) matches old cmprisk_varname",
      quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = c("ftime", "fstatus"), cmp1 = cmp1_dt)),
      quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, varname = c("ftime", "fstatus"), cmp1 = cmp1_dt)))
check("cmprisk: append = TRUE",
      quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, append = TRUE, cmp1 = cmp1_dt)),
      quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, append = TRUE, cmp1 = cmp1_dt)))
check("cmprisk: OLD POSITIONAL call (cmprisk_varname & append as args 6-7)",
      quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt, c("ftime", "fstatus"), TRUE, cmp1 = cmp1_dt)),
      quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, c("ftime", "fstatus"), TRUE, cmp1 = cmp1_dt)))
# Old fn has a bug with 0 competing events (paste0 zero-length recycling makes a
# phantom "cmp_evt_" level; censored subjects become evt = NA). Assert the NEW
# behavior is correct instead: matches old everywhere except censored get evt = 0.
o <- quiet(old_cmp(df, patid, idx_dt, evt_dt, end_dt))
n2 <- quiet(construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt))
cens <- is.na(o$evt) & !is.na(o$evt_time)
cat(if (isTRUE(all.equal(o$evt_time, n2$evt_time)) &&
        all(as.character(n2$evt[cens]) == "0") &&
        identical(as.character(o$evt[!cens]), as.character(n2$evt[!cens])))
      "PASS: cmprisk 0 competing events: new fixes old censored-NA bug\n"
    else {ok <- FALSE; "FAIL: cmprisk 0 competing events\n"})

## ---- new-only: all-dates-missing subject, no zero/negative rows present ----
df_na <- make_data(50, seed = 2)[-(1:5), ]        # drop the forced edge rows
df_na$evt_dt[1] <- df_na$cmp1_dt[1] <- df_na$cmp2_dt[1] <- df_na$end_dt[1] <- NA
old_res <- tryCatch(quiet(old_cmp(df_na, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt)),
                    error = function(e) "OLD ERRORS")
new_res <- quiet(construct_cmprisk_var(df_na, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt))
cat(if (identical(old_res, "OLD ERRORS") && is.na(new_res$evt_time[1]) && is.na(new_res$evt[1]))
      "PASS: all-NA subject: old errors, new returns NA row\n"
    else {ok <- FALSE; "FAIL: all-NA subject handling\n"})

## ---- benchmarks ----
bench <- function(f, reps = 3) median(vapply(seq_len(reps), function(i) system.time(f())[["elapsed"]], 0))

df_b <- make_data(20000, seed = 3)[-(1:5), ]      # clean of zero/neg so both run silently
cat(sprintf("\nconstruct_cmprisk_var, n = %d (2 competing events):\n", nrow(df_b)))
t_old <- bench(function() quiet(old_cmp(df_b, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt, cmp2 = cmp2_dt)))
t_new <- bench(function() quiet(construct_cmprisk_var(df_b, patid, idx_dt, evt_dt, end_dt, cmp1 = cmp1_dt, cmp2 = cmp2_dt)))
cat(sprintf("  old: %.3fs   new: %.3fs   speedup: %.0fx\n", t_old, t_new, t_old / t_new))

df_s <- make_data(200000, seed = 4)[-(1:5), ]
cat(sprintf("construct_surv_var, n = %d:\n", nrow(df_s)))
t_old <- bench(function() quiet(old_surv(df_s, patid, idx_dt, evt_dt, end_dt)))
t_new <- bench(function() quiet(construct_surv_var(df_s, patid, idx_dt, evt_dt, end_dt)))
cat(sprintf("  old: %.3fs   new: %.3fs   speedup: %.0fx\n", t_old, t_new, t_old / t_new))

cat(if (ok) "\nALL EQUIVALENCE CHECKS PASS\n" else "\nEQUIVALENCE FAILURES PRESENT\n")
