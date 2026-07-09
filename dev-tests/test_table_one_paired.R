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

## ---- SMD and N-pairs closures ----

n_fn <- .paired_make_n_pairs_fn(dpaired, "pid", "grp", "A", "B")
check("n_pairs: counts complete pairs for cont", n_fn(data = NULL, variable = "cont", by = NULL)$n_pairs == as.character(n))

# introduce one incomplete pair for a fresh variable to check N pairs != nrow
dpaired$cont_partial <- dpaired$cont
dpaired$cont_partial[dpaired$pid == 1 & dpaired$grp == "B"] <- NA
n_fn2 <- .paired_make_n_pairs_fn(dpaired, "pid", "grp", "A", "B")
check("n_pairs: excludes incomplete pair", n_fn2(data = NULL, variable = "cont_partial", by = NULL)$n_pairs == as.character(n - 1))

# SMD: matching method matches direct smd::smd on complete pairs, SIGN-FLIPPED.
# smd::smd()'s own convention is reference-minus-other (verified empirically: with
# gref=1 and group 1 having the smaller mean, its `estimate` comes out negative) --
# the opposite of this design's "non-reference minus reference" convention -- so
# the closure negates it and this test must too.
smd_fn_match <- .paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "matching")
smd_match <- smd_fn_match(data = NULL, variable = "cont", by = NULL)$smd
wide_c2 <- .paired_wide(dpaired, "pid", "grp", "A", "B", "cont")
x_long <- c(wide_c2$.ref, wide_c2$.other)
g_long <- factor(c(rep("A", nrow(wide_c2)), rep("B", nrow(wide_c2))), levels = c("A", "B"))
expected_smd <- -smd::smd(x = x_long, g = g_long, gref = 1L)$estimate[1]
dec_cont <- max(1L, decimalplaces(dpaired$cont))
check("smd: matching method matches direct smd::smd (sign-corrected)",
      smd_match == formatC(expected_smd, digits = dec_cont, format = "f"))

# SMD: repeated_measure method matches hand-computed Cohen's d_z
smd_fn_rm <- .paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "repeated_measure")
smd_rm <- smd_fn_rm(data = NULL, variable = "cont", by = NULL)$smd
diff <- wide_c2$.other - wide_c2$.ref
expected_dz <- mean(diff) / sd(diff)
check("smd: repeated_measure matches hand-computed Cohen's d_z",
      smd_rm == formatC(expected_dz, digits = dec_cont, format = "f"))

# SMD: categorical uses marginal smd in BOTH methods (no within-pair categorical SMD)
smd_cat_match <- .paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "cat2", by = NULL)$smd
smd_cat_rm    <- .paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "repeated_measure")(data = NULL, variable = "cat2", by = NULL)$smd
check("smd: categorical identical under both pairing methods", smd_cat_match == smd_cat_rm)
check("smd: categorical uses 1 decimal", grepl("^-?[0-9]+\\.[0-9]$", smd_cat_match))

# SMD: integer-valued continuous variable still gets floor-of-1 decimal
dpaired$int_var <- round(dpaired$cont)
check("decimalplaces: integer-valued var has 0 decimals natively", decimalplaces(dpaired$int_var) == 0L)
smd_int <- .paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "int_var", by = NULL)$smd
check("smd: integer-valued continuous var floored to 1 decimal", grepl("^-?[0-9]+\\.[0-9]$", smd_int))

# degenerate: zero-variance within-pair differences (d_z) -> "---"
dconst <- data.frame(pid = rep(1:5, each = 2), grp = factor(rep(c("A", "B"), 5), levels = c("A", "B")), v = 5)
smd_const <- .paired_make_smd_fn(dconst, "pid", "grp", "A", "B", "repeated_measure")(data = NULL, variable = "v", by = NULL)$smd
check("smd: zero-variance differences -> '---'", smd_const == "---")

# degenerate: single complete pair -> d_z NA -> "---"
smd_single <- .paired_make_smd_fn(dsingle, "pid", "grp", "A", "B", "repeated_measure")(data = NULL, variable = "v", by = NULL)$smd
check("smd: single complete pair (d_z) -> '---'", smd_single == "---")

# degenerate: zero complete pairs -> "---"
smd_zero <- .paired_make_smd_fn(dzero, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "v", by = NULL)$smd
check("smd: zero complete pairs -> '---'", smd_zero == "---")

# Sign sanity check, independent of the closure's own internal computation: when
# the "other" (non-reference) level is unambiguously and consistently larger, the
# displayed SMD must be positive under BOTH pairing methods. Realistic (nonzero,
# comparable-magnitude) within-group variance is used deliberately -- a degenerate
# near-zero-variance setup makes the marginal pooled-variance SMD numerically
# unstable (division by a near-zero denominator), which very nearly produced a
# false negative in this exact check while this plan was being written.
set.seed(41)
n_sign <- 30
dsign <- data.frame(pid = rep(1:n_sign, each = 2), grp = rep(c("A", "B"), n_sign), v = NA_real_)
dsign$grp <- factor(dsign$grp, levels = c("A", "B"))
v_a <- rnorm(n_sign, 0, 3)
dsign$v[dsign$grp == "A"] <- v_a
dsign$v[dsign$grp == "B"] <- v_a + rnorm(n_sign, 5, 2)  # other = reference + positive shift + noise
sign_match <- .paired_make_smd_fn(dsign, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "v", by = NULL)$smd
sign_rm    <- .paired_make_smd_fn(dsign, "pid", "grp", "A", "B", "repeated_measure")(data = NULL, variable = "v", by = NULL)$smd
check("smd: other-minus-reference sign is positive when other > reference (matching)", !grepl("^-", sign_match))
check("smd: other-minus-reference sign is positive when other > reference (repeated_measure)", !grepl("^-", sign_rm))

# Same sign check for a CATEGORICAL (logical) variable -- the negation fix applies
# to the marginal smd::smd() path used for all categorical variables under both
# pairing methods, not just continuous ones, so this needs its own independent check.
set.seed(7)
n_cat <- 30
dsign_cat <- data.frame(pid = rep(1:n_cat, each = 2), grp = rep(c("A", "B"), n_cat), flag = NA)
dsign_cat$grp <- factor(dsign_cat$grp, levels = c("A", "B"))
dsign_cat$flag[dsign_cat$grp == "A"] <- sample(c(TRUE, FALSE), n_cat, TRUE, prob = c(0.2, 0.8))
dsign_cat$flag[dsign_cat$grp == "B"] <- sample(c(TRUE, FALSE), n_cat, TRUE, prob = c(0.8, 0.2))
sign_cat_match <- .paired_make_smd_fn(dsign_cat, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "flag", by = NULL)$smd
check("smd: categorical other-minus-reference sign is positive when other prevalence > reference", !grepl("^-", sign_cat_match))

## ---- table_one_paired(): plumbing ----

set.seed(21)
nn <- 25
tdf <- data.frame(
  pid = rep(1:nn, each = 2),
  visit = rep(c("Baseline", "Followup"), nn),
  age = NA_real_,
  sex = factor(sample(c("F", "M"), 2 * nn, TRUE))
)
base_age <- rnorm(nn, 55, 8)
tdf$age[tdf$visit == "Baseline"] <- base_age
tdf$age[tdf$visit == "Followup"] <- base_age + rnorm(nn, 1, 3)

res <- table_one_paired(tdf, pair_id = pid, group = visit)
check("main: returns a tbl_summary/gtsummary object", inherits(res, "tbl_summary") && inherits(res, "gtsummary"))
check("main: default column order is Overall, ref(Baseline), other(Followup), N pairs, SMD, p-value",
      identical(tail(res$table_styling$header$column, 6), c("stat_0", "stat_1", "stat_2", "n_pairs", "smd", "p.value")))
check("main: pair_id never appears as a table_body variable", !("pid" %in% res$table_body$variable))

# include normalization: pair_id explicitly listed is dropped with a message, not an error
res_inc <- suppressMessages(table_one_paired(tdf, pair_id = pid, group = visit, include = c(pid, age)))
check("main: include=c(pid, age) drops pid, keeps age, no error", "age" %in% res_inc$table_body$variable)

# var_name/var_desp forwarding to table_one() via datadic
dic <- data.frame(nm = c("age", "sex"), lbl = c("Age (years)", "Sex"), stringsAsFactors = FALSE)
res_dic <- table_one_paired(tdf, pair_id = pid, group = visit, datadic = dic, var_name = nm, var_desp = lbl)
check("main: var_name/var_desp forwarded to table_one() datadic labels",
      "Age (years)" %in% as_tibble(res_dic)[[1]])

# add_* toggles
res_off <- table_one_paired(tdf, pair_id = pid, group = visit, add_p = FALSE, add_smd = FALSE, add_n_pairs = FALSE, add_overall = FALSE)
check("main: add_p/add_smd/add_n_pairs/add_overall = FALSE removes those columns",
      !any(c("stat_0", "n_pairs", "smd", "p.value") %in% res_off$table_styling$header$column))

if (ok) cat("\nALL PASS\n") else { cat("\nFAILURES PRESENT\n"); quit(status = 1) }
