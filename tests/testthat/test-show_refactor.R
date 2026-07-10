# Converted from dev-tests/test_show_refactor.R.
# Baseline/regression test for the show_surv()/show_cif() dedup refactor: compares
# current pvalue-grob placement and color/fill palettes against a baseline snapshot
# captured before the refactor (tests/testthat/fixtures/show_baseline.rds).
#
# Chose option (a) from the conversion instructions -- plain expect_equal(current,
# baseline) inside test_that() blocks against the existing saved baseline object --
# over expect_snapshot_value(), because the baseline already existed as a
# pre-refactor reference point with clear semantics ("does the refactor change
# observable output"), and a fresh testthat snapshot would just be re-recording
# the *current* (already-refactored) output, silently losing that reference.

baseline <- readRDS(testthat::test_path("fixtures", "show_baseline.rds"))

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

test_that("show_surv() p-value placement matches baseline for every position", {
  for (pos in positions) {
    p <- suppressMessages(show_surv(fit, pvalue_pos = pos, add_atrisk = FALSE,
                                    add_ci = FALSE, print_fig = FALSE))
    expect_equal(pvalue_grob_params(p), baseline[[paste0("surv_pvalue_", pos)]],
                label = paste0("surv_pvalue_", pos))
  }
})

test_that("show_cif() p-value placement matches baseline", {
  for (pos in c("topleft", "bottomright")) {
    pc <- suppressMessages(show_cif(fit_cr, pvalue_pos = pos, add_atrisk = FALSE,
                                    add_ci = FALSE, print_fig = FALSE))
    expect_equal(pvalue_grob_params(pc), baseline[[paste0("cif_pvalue_", pos)]],
                label = paste0("cif_pvalue_", pos))
  }
})

test_that("show_surv()/show_cif() color and fill palettes match baseline for every scheme", {
  for (sch in schemes) {
    cl <- if (sch == "manual") list(values = c("red", "blue")) else NULL
    p <- suppressMessages(show_surv(fit, color_scheme = sch, color_list = cl,
                                    add_pvalue = FALSE, add_atrisk = FALSE, print_fig = FALSE))
    expect_equal(scale_palette(p, "colour", 2), baseline[[paste0("surv_col_", sch)]],
                label = paste0("surv_col_", sch))
    expect_equal(scale_palette(p, "fill", 2), baseline[[paste0("surv_fill_", sch)]],
                label = paste0("surv_fill_", sch))

    pc <- suppressMessages(show_cif(fit_cr, color_scheme = sch, color_list = cl,
                                    add_pvalue = FALSE, add_atrisk = FALSE, print_fig = FALSE))
    expect_equal(scale_palette(pc, "colour", 2), baseline[[paste0("cif_col_", sch)]],
                label = paste0("cif_col_", sch))
    expect_equal(scale_palette(pc, "fill", 2), baseline[[paste0("cif_fill_", sch)]],
                label = paste0("cif_fill_", sch))
  }
})
