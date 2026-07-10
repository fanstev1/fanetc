# Regression tests for 3 known bugs fixed during the v1.0 merge tidy-up:
#  1. show_surv() silently reset a user-supplied y_lim to c(0,1) for survival curves.
#  2. prepare_cmprisk() used grepl("0", state), misclassifying any real competing-risk
#     state whose name merely contains "0" (e.g. "10", "20") as the censored/reference state.
#  3. show_cif()'s roxygen @param docs were copy-pasted from unrelated functions.
suppressPackageStartupMessages({
  library(survival); library(dplyr); library(tidyr); library(purrr); library(magrittr)
  library(rlang); library(ggplot2); library(grid); library(scales); library(forcats)
  library(viridis); library(cmprsk)
})
if (basename(getwd()) == "dev-tests") setwd("..")
source("R/desp_table_gtsummary.R")
source("R/event_time_helpers.R")
source("R/event_time_desp.R")

ok <- TRUE
check <- function(label, cond) {
  if (isTRUE(cond)) cat("PASS:", label, "\n") else { cat("FAIL:", label, "\n"); ok <<- FALSE }
}

# ---- Bug 1: show_surv() must respect a user-supplied y_lim ----
fit <- estimate_km(lung, evt_time = time, evt = status, group = sex)
p_default <- suppressMessages(show_surv(fit, add_atrisk = FALSE, add_ci = FALSE, print_fig = FALSE))
p_custom  <- suppressMessages(show_surv(fit, y_lim = c(0, 0.5), add_atrisk = FALSE, add_ci = FALSE, print_fig = FALSE))
check("show_surv: default y_lim is c(0,1)", identical(p_default$coordinates$limits$y, c(0, 1)))
check("show_surv: user-supplied y_lim=c(0,0.5) is respected, not reset to c(0,1)",
      identical(p_custom$coordinates$limits$y, c(0, 0.5)))

# ---- Bug 2: only survival's "(s0)" placeholder (or "") collapses to censored state "0" ----
fake_state <- c("(s0)", "10", "20", "1", "")
fixed <- replace(fake_state, nchar(fake_state) == 0 | fake_state == "(s0)", "0")
check("prepare_cmprisk state fix: \"(s0)\" placeholder collapses to \"0\"", fixed[1] == "0")
check("prepare_cmprisk state fix: empty string collapses to \"0\"", fixed[5] == "0")
check("prepare_cmprisk state fix: real state \"10\" is NOT misclassified as \"0\"", fixed[2] == "10")
check("prepare_cmprisk state fix: real state \"20\" is NOT misclassified as \"0\"", fixed[3] == "20")

# End-to-end: show_cif() must still work correctly against a real survfitms object,
# whose actual reference-state name from survival::survfit() is "(s0)" (verified directly;
# not "0" as the pre-fix code implicitly assumed via its overly broad grepl("0", ...) match).
lung_cr <- lung %>% mutate(cr_evt = factor(ifelse(status == 2, sample(1:2, n(), TRUE, prob = c(.7, .3)), 0)))
set.seed(42)
lung_cr$cr_evt <- factor(ifelse(lung$status == 2, sample(1:2, nrow(lung), TRUE, prob = c(.7, .3)), 0))
fit_cr <- estimate_cif(lung_cr, evt_time = time, evt = cr_evt, group = sex)
check("real survfitms competing-risk state placeholder is literally \"(s0)\"",
      identical(fit_cr$states, c("(s0)", "1", "2")))
p_cif <- tryCatch(
  suppressMessages(show_cif(fit_cr, add_atrisk = FALSE, add_ci = FALSE, print_fig = FALSE)),
  error = function(e) e
)
check("show_cif() runs end-to-end without error against a real (s0)-labeled survfitms object",
      inherits(p_cif, "ggplot"))

# ---- Bug 3: show_cif()'s roxygen docs must cover every formal argument (no leftover copy-paste gaps) ----
documented_params <- {
  src <- readLines("R/event_time_desp.R")
  title_line <- grep("^#' @title show_cif$", src)
  fn_line <- grep("^show_cif<- function", src)
  block <- src[title_line:fn_line]
  m <- regmatches(block, regexpr("(?<=@param )[A-Za-z_.]+", block, perl = TRUE))
  m[nzchar(m)]
}
formal_params <- names(formals(show_cif))
check("show_cif: every formal argument has a @param doc entry",
      all(formal_params %in% documented_params))
check("show_cif: no stray @param doc entry for a nonexistent argument",
      all(documented_params %in% formal_params))

cat(if (ok) "\nALL PASS\n" else "\nFAILURES PRESENT\n")
if (!ok) quit(status = 1)
