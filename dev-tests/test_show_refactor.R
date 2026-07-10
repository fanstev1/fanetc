# Baseline/regression test for the show_surv/show_cif dedup refactor.
# First run (pre-refactor) writes dev-tests/show_baseline.rds; later runs compare
# the refactored functions against that baseline.
suppressPackageStartupMessages({
  library(survival); library(dplyr); library(tidyr); library(purrr); library(magrittr)
  library(rlang); library(ggplot2); library(grid); library(scales); library(forcats)
  library(viridis); library(cmprsk)
})
if (basename(getwd()) == "dev-tests") setwd("..")
source("R/desp_table_gtsummary.R")  # format_pvalue
source("R/event_time_helpers.R")
source("R/event_time_desp.R")

baseline_file <- "dev-tests/show_baseline.rds"
positions <- c("topleft", "topright", "bottomleft", "bottomright", "left", "right", "top", "bottom")
schemes <- c("brewer", "grey", "viridis", "manual")

fit <- estimate_km(lung, evt_time = time, evt = status, group = sex)

# competing-risk fit: factor status with censored level "0"
lung_cr <- lung %>%
  mutate(cr_evt = factor(ifelse(status == 2, sample(1:2, n(), TRUE, prob = c(.7, .3)), 0)))
set.seed(42)
lung_cr$cr_evt <- factor(ifelse(lung$status == 2, sample(1:2, nrow(lung), TRUE, prob = c(.7, .3)), 0))
fit_cr <- estimate_cif(lung_cr, evt_time = time, evt = cr_evt, group = sex)

pvalue_grob_params <- function(p) {
  for (l in p$layers) {
    g <- l$geom_params$grob
    if (inherits(g, "text") && grepl("Log-rank|Gray", as.character(g$label))) {
      return(c(x = as.numeric(g$x), y = as.numeric(g$y), hjust = g$hjust, vjust = g$vjust))
    }
  }
  NULL
}

scale_palette <- function(p, aes_name, n = 4) {
  s <- Filter(function(s) aes_name %in% s$aesthetics, p$scales$scales)
  if (!length(s)) return(NULL)
  tryCatch(s[[1]]$palette(n), error = function(e) paste("palette error:", conditionMessage(e)))
}

capture <- function() {
  out <- list()
  for (pos in positions) {
    p <- suppressMessages(show_surv(fit, pvalue_pos = pos, add_atrisk = FALSE,
                                    add_ci = FALSE, print_fig = FALSE))
    out[[paste0("surv_pvalue_", pos)]] <- pvalue_grob_params(p)
  }
  for (sch in schemes) {
    cl <- if (sch == "manual") list(values = c("red", "blue")) else NULL
    p <- suppressMessages(show_surv(fit, color_scheme = sch, color_list = cl,
                                    add_pvalue = FALSE, add_atrisk = FALSE, print_fig = FALSE))
    out[[paste0("surv_col_", sch)]] <- scale_palette(p, "colour", 2)
    out[[paste0("surv_fill_", sch)]] <- scale_palette(p, "fill", 2)
    pc <- suppressMessages(show_cif(fit_cr, color_scheme = sch, color_list = cl,
                                    add_pvalue = FALSE, add_atrisk = FALSE, print_fig = FALSE))
    out[[paste0("cif_col_", sch)]] <- scale_palette(pc, "colour", 2)
    out[[paste0("cif_fill_", sch)]] <- scale_palette(pc, "fill", 2)
  }
  for (pos in c("topleft", "bottomright")) {
    pc <- suppressMessages(show_cif(fit_cr, pvalue_pos = pos, add_atrisk = FALSE,
                                    add_ci = FALSE, print_fig = FALSE))
    out[[paste0("cif_pvalue_", pos)]] <- pvalue_grob_params(pc)
  }
  out
}

current <- capture()

if (!file.exists(baseline_file)) {
  saveRDS(current, baseline_file)
  cat("Baseline written:", baseline_file, "(", length(current), "entries )\n")
} else {
  baseline <- readRDS(baseline_file)
  ok <- TRUE
  for (nm in names(baseline)) {
    same <- isTRUE(all.equal(baseline[[nm]], current[[nm]]))
    if (!same) { ok <- FALSE; cat("FAIL:", nm, "\n")
      cat("  baseline:", paste(baseline[[nm]], collapse = " "), "\n")
      cat("  current :", paste(current[[nm]], collapse = " "), "\n") }
  }
  cat(if (ok) sprintf("ALL %d BASELINE CHECKS PASS\n", length(baseline)) else "BASELINE MISMATCHES\n")
}
