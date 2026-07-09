suppressPackageStartupMessages({
  library(dplyr); library(forcats); library(rlang); library(tidyselect)
  library(gtsummary); library(tidyr)
})
setwd("/Users/sfan/Documents/projects/fanetc")
src <- readLines("R/fan_util_fun.R")
starts <- grep("^[a-zA-Z_.0-9]+ *<- *function", src)
dp <- grep("^decimalplaces *<- *function", src)
eval(parse(text = src[dp:(min(starts[starts > dp]) - 1)]))
source("R/desp_table_gtsummary.R")
source("R/desp_table_paired.R")

ok <- TRUE
check <- function(label, cond) {
  if (isTRUE(cond)) cat("PASS:", label, "\n")
  else { cat("FAIL:", label, "\n"); ok <<- FALSE }
}
check_err <- function(label, expr, pattern) {
  res <- tryCatch({ force(expr); NULL }, error = function(e) e)
  if (!is.null(res) && grepl(pattern, conditionMessage(res))) cat("PASS:", label, "\n")
  else { cat("FAIL:", label, "-> ", if (is.null(res)) "no error thrown" else conditionMessage(res), "\n"); ok <<- FALSE }
}

## ---- .paired_resolve_ref_level() ----

check("ref: factor default is first level",
      .paired_resolve_ref_level(factor(c("B", "A", "B"), levels = c("B", "A")), NULL, c("B", "A")) == "B")
check("ref: character default is most frequent",
      .paired_resolve_ref_level(c("x", "y", "y", "y"), NULL, c("x", "y")) == "y")
check("ref: character tie broken by first-observed",
      .paired_resolve_ref_level(c("y", "x", "y", "x"), NULL, c("x", "y")) == "y")
check("ref: logical default is sorted-first (FALSE)",
      .paired_resolve_ref_level(c(TRUE, FALSE, TRUE), NULL, c("FALSE", "TRUE")) == "FALSE")
check("ref: numeric default is sorted-first (smaller)",
      .paired_resolve_ref_level(c(2, 1, 2), NULL, c("1", "2")) == "1")
check("ref: explicit ref_group honored",
      .paired_resolve_ref_level(c("x", "y"), "y", c("x", "y")) == "y")
check_err("ref: ref_group not observed errors",
      .paired_resolve_ref_level(c("x", "y"), "z", c("x", "y")),
      "not one of the observed")

## ---- .paired_prepare_data() ----

df1 <- data.frame(pid = c(1, 1, 2, 2, 3), grp = c("A", "B", "A", "B", "A"), val = 1:5)
p1 <- .paired_prepare_data(df1, "pid", "grp", NULL)
check("prep: unpaired singleton kept in data", nrow(p1$data) == 5)
check("prep: ref/other levels resolved", p1$ref_level == "A" && p1$other_level == "B")
check("prep: group column is a 2-level factor with ref first",
      is.factor(p1$data$grp) && levels(p1$data$grp)[1] == "A")

df_missgrp <- data.frame(pid = c(1, 1, 2), grp = c("A", NA, "B"), val = 1:3)
p2 <- suppressMessages(.paired_prepare_data(df_missgrp, "pid", "grp", NULL))
check("prep: missing-group row dropped", nrow(p2$data) == 2)

df_misspid <- data.frame(pid = c(1, NA, 2, 2), grp = c("A", "A", "A", "B"), val = 1:4)
p3 <- suppressMessages(.paired_prepare_data(df_misspid, "pid", "grp", NULL))
check("prep: missing-pair_id row dropped", nrow(p3$data) == 3)

df_emptypid <- data.frame(pid = c("1", "", "2", "2"), grp = c("A", "A", "A", "B"), val = 1:4)
p4 <- suppressMessages(.paired_prepare_data(df_emptypid, "pid", "grp", NULL))
check("prep: empty-string pair_id row dropped", nrow(p4$data) == 3)

df_emptypid_f <- data.frame(pid = factor(c("1", "", "2", "2")), grp = c("A", "A", "A", "B"), val = 1:4)
p4f <- suppressMessages(.paired_prepare_data(df_emptypid_f, "pid", "grp", NULL))
check("prep: empty-level FACTOR pair_id row dropped too", nrow(p4f$data) == 3)

check_err("prep: 3 group levels errors",
      .paired_prepare_data(data.frame(pid = 1:3, grp = c("A", "B", "C"), val = 1:3), "pid", "grp", NULL),
      "exactly 2 observed levels")
check_err("prep: 1 group level errors",
      .paired_prepare_data(data.frame(pid = 1:3, grp = c("A", "A", "A"), val = 1:3), "pid", "grp", NULL),
      "exactly 2 observed levels")
check_err("prep: all-missing group errors (0 levels)",
      .paired_prepare_data(data.frame(pid = 1:3, grp = c(NA, NA, NA), val = 1:3), "pid", "grp", NULL),
      "exactly 2 observed levels")
check_err("prep: duplicate pair member errors",
      .paired_prepare_data(data.frame(pid = c(1, 1, 1), grp = c("A", "A", "B"), val = 1:3), "pid", "grp", NULL),
      "Duplicate pair ID")
check_err("prep: bad ref_group errors",
      .paired_prepare_data(df1, "pid", "grp", "Z"),
      "not one of the observed")

check("prep: pair_id as character works",
      { d <- df1; d$pid <- as.character(d$pid); nrow(.paired_prepare_data(d, "pid", "grp", NULL)$data) == 5 })
check("prep: pair_id as factor works",
      { d <- df1; d$pid <- factor(d$pid); nrow(.paired_prepare_data(d, "pid", "grp", NULL)$data) == 5 })

## ---- .paired_wide() ----

dfw <- data.frame(
  pid = c(1, 1, 2, 2, 3, 3, 4),
  grp = c("A", "B", "A", "B", "A", "B", "A"),
  x   = c(10, 12, NA, 5, 7, 8, 99)
)
dfw$grp <- factor(dfw$grp, levels = c("A", "B"))
w <- .paired_wide(dfw, "pid", "grp", "A", "B", "x")
check("wide: only complete pairs kept (pair 2 has NA, pair 4 has no B row)", nrow(w) == 2)
check("wide: pair 1 values correct", w$.ref[w$pid == 1] == 10 && w$.other[w$pid == 1] == 12)
check("wide: pair 3 values correct", w$.ref[w$pid == 3] == 7 && w$.other[w$pid == 3] == 8)

## ---- paired test closures ----

set.seed(11)
n <- 30
dpaired <- data.frame(pid = rep(1:n, each = 2), grp = rep(c("A", "B"), n))
dpaired$grp <- factor(dpaired$grp, levels = c("A", "B"))
base_val <- rnorm(n, 50, 10)
dpaired$cont <- NA_real_
dpaired$cont[dpaired$grp == "A"] <- base_val
dpaired$cont[dpaired$grp == "B"] <- base_val + rnorm(n, 2, 3)

cont_fn_meansd <- .paired_make_cont_test_fn(dpaired, "pid", "grp", "A", "B", "meansd")
p_meansd <- cont_fn_meansd(data = NULL, variable = "cont", by = NULL)$p.value
wide_c <- .paired_wide(dpaired, "pid", "grp", "A", "B", "cont")
expected_t <- stats::t.test(wide_c$.other, wide_c$.ref, paired = TRUE)$p.value
check("cont test: meansd matches direct paired t.test", isTRUE(all.equal(p_meansd, expected_t)))

cont_fn_mediqr <- .paired_make_cont_test_fn(dpaired, "pid", "grp", "A", "B", "mediqr")
p_mediqr <- suppressWarnings(cont_fn_mediqr(data = NULL, variable = "cont", by = NULL)$p.value)
expected_w <- suppressWarnings(stats::wilcox.test(wide_c$.other, wide_c$.ref, paired = TRUE)$p.value)
check("cont test: mediqr matches direct paired wilcox.test", isTRUE(all.equal(p_mediqr, expected_w)))

dpaired$cat2 <- factor(sample(c("y", "n"), 2 * n, TRUE))
cat_fn <- .paired_make_cat_test_fn(dpaired, "pid", "grp", "A", "B")
p_cat <- cat_fn(data = NULL, variable = "cat2", by = NULL)$p.value
wide_cat <- .paired_wide(dpaired, "pid", "grp", "A", "B", "cat2")
lv <- union(as.character(unique(wide_cat$.ref)), as.character(unique(wide_cat$.other)))
tab_expected <- table(factor(as.character(wide_cat$.ref), levels = lv),
                       factor(as.character(wide_cat$.other), levels = lv))
expected_mc <- stats::mcnemar.test(tab_expected)$p.value
check("cat test: matches direct mcnemar.test on union-of-levels table", isTRUE(all.equal(p_cat, expected_mc)))

# 3-category variable -> McNemar-Bowker (mcnemar.test on a k x k table)
dpaired$cat3 <- factor(sample(c("x", "y", "z"), 2 * n, TRUE))
cat_fn3 <- .paired_make_cat_test_fn(dpaired, "pid", "grp", "A", "B")
p_cat3 <- cat_fn3(data = NULL, variable = "cat3", by = NULL)$p.value
check("cat test: 3-level factor returns a numeric p-value (Bowker)", is.numeric(p_cat3) && !is.na(p_cat3))

# degenerate: all-concordant pairs -> NA, no error
dconc <- data.frame(pid = rep(1:5, each = 2), grp = factor(rep(c("A", "B"), 5), levels = c("A", "B")),
                     same = rep(c("yes"), 10))
p_conc <- .paired_make_cat_test_fn(dconc, "pid", "grp", "A", "B")(data = NULL, variable = "same", by = NULL)$p.value
# strict identical(), not just is.na(): mcnemar.test() on an all-concordant table
# returns NaN internally (0/0), and the design specifies NA -- this asserts the
# closure's NaN -> NA_real_ normalization actually ran, not merely that is.na()
# happens to accept both.
check("cat test: all-concordant pairs -> exactly NA_real_, not NaN (no error)", identical(p_conc, NA_real_))

# degenerate: single complete pair -> NA, no error (t.test needs >= 2)
dsingle <- data.frame(pid = c(1, 1), grp = factor(c("A", "B"), levels = c("A", "B")), v = c(1, 2))
p_single <- .paired_make_cont_test_fn(dsingle, "pid", "grp", "A", "B", "meansd")(data = NULL, variable = "v", by = NULL)$p.value
check("cont test: single complete pair -> NA (no error)", identical(p_single, NA_real_))

# degenerate: zero complete pairs -> NA, no error
dzero <- data.frame(pid = c(1, 2), grp = factor(c("A", "B"), levels = c("A", "B")), v = c(1, 2))
p_zero <- .paired_make_cont_test_fn(dzero, "pid", "grp", "A", "B", "meansd")(data = NULL, variable = "v", by = NULL)$p.value
check("cont test: zero complete pairs -> NA (no error)", identical(p_zero, NA_real_))

if (ok) cat("\nALL PASS\n") else { cat("\nFAILURES PRESENT\n"); quit(status = 1) }
