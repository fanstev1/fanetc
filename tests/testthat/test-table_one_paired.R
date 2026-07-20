# Converted from dev-tests/test_table_one_paired.R.
# table_one_paired()'s internal helper functions (.paired_*) are not exported,
# so they're reached here via fanetc::: (works whether the suite is run against
# an installed package via library(fanetc) or a dev-loaded one via
# devtools::load_all(), since load_all's default export_all=TRUE also keeps :::
# working unambiguously).

## ---- .paired_resolve_ref_level() ----

test_that(".paired_resolve_ref_level: factor default is first level", {
  expect_equal(fanetc:::.paired_resolve_ref_level(factor(c("B", "A", "B"), levels = c("B", "A")), NULL, c("B", "A")), "B")
})

test_that(".paired_resolve_ref_level: character default is most frequent", {
  expect_equal(fanetc:::.paired_resolve_ref_level(c("x", "y", "y", "y"), NULL, c("x", "y")), "y")
})

test_that(".paired_resolve_ref_level: character tie broken by first-observed", {
  expect_equal(fanetc:::.paired_resolve_ref_level(c("y", "x", "y", "x"), NULL, c("x", "y")), "y")
})

test_that(".paired_resolve_ref_level: logical default is sorted-first (FALSE)", {
  expect_equal(fanetc:::.paired_resolve_ref_level(c(TRUE, FALSE, TRUE), NULL, c("FALSE", "TRUE")), "FALSE")
})

test_that(".paired_resolve_ref_level: numeric default is sorted-first (smaller)", {
  expect_equal(fanetc:::.paired_resolve_ref_level(c(2, 1, 2), NULL, c("1", "2")), "1")
})

test_that(".paired_resolve_ref_level: explicit ref_group honored", {
  expect_equal(fanetc:::.paired_resolve_ref_level(c("x", "y"), "y", c("x", "y")), "y")
})

test_that(".paired_resolve_ref_level: ref_group not observed errors", {
  expect_error(fanetc:::.paired_resolve_ref_level(c("x", "y"), "z", c("x", "y")), "not one of the observed")
})

## ---- .paired_prepare_data() ----

df1 <- data.frame(pid = c(1, 1, 2, 2, 3), grp = c("A", "B", "A", "B", "A"), val = 1:5)

test_that("prep: unpaired singleton kept in data; ref/other resolved; grp is a 2-level factor with ref first", {
  p1 <- fanetc:::.paired_prepare_data(df1, "pid", "grp", NULL)
  expect_equal(nrow(p1$data), 5)
  expect_equal(p1$ref_level, "A")
  expect_equal(p1$other_level, "B")
  expect_true(is.factor(p1$data$grp))
  expect_equal(levels(p1$data$grp)[1], "A")
})

test_that("prep: missing-group row dropped", {
  df_missgrp <- data.frame(pid = c(1, 1, 2), grp = c("A", NA, "B"), val = 1:3)
  p2 <- suppressMessages(fanetc:::.paired_prepare_data(df_missgrp, "pid", "grp", NULL))
  expect_equal(nrow(p2$data), 2)
})

test_that("prep: missing-pair_id row dropped", {
  df_misspid <- data.frame(pid = c(1, NA, 2, 2), grp = c("A", "A", "A", "B"), val = 1:4)
  p3 <- suppressMessages(fanetc:::.paired_prepare_data(df_misspid, "pid", "grp", NULL))
  expect_equal(nrow(p3$data), 3)
})

test_that("prep: empty-string pair_id row dropped", {
  df_emptypid <- data.frame(pid = c("1", "", "2", "2"), grp = c("A", "A", "A", "B"), val = 1:4)
  p4 <- suppressMessages(fanetc:::.paired_prepare_data(df_emptypid, "pid", "grp", NULL))
  expect_equal(nrow(p4$data), 3)
})

test_that("prep: empty-level FACTOR pair_id row dropped too", {
  df_emptypid_f <- data.frame(pid = factor(c("1", "", "2", "2")), grp = c("A", "A", "A", "B"), val = 1:4)
  p4f <- suppressMessages(fanetc:::.paired_prepare_data(df_emptypid_f, "pid", "grp", NULL))
  expect_equal(nrow(p4f$data), 3)
})

test_that("prep: 3 group levels errors", {
  expect_error(
    fanetc:::.paired_prepare_data(data.frame(pid = 1:3, grp = c("A", "B", "C"), val = 1:3), "pid", "grp", NULL),
    "exactly 2 observed levels"
  )
})

test_that("prep: 1 group level errors", {
  expect_error(
    fanetc:::.paired_prepare_data(data.frame(pid = 1:3, grp = c("A", "A", "A"), val = 1:3), "pid", "grp", NULL),
    "exactly 2 observed levels"
  )
})

test_that("prep: all-missing group errors (0 levels)", {
  expect_error(
    fanetc:::.paired_prepare_data(data.frame(pid = 1:3, grp = c(NA, NA, NA), val = 1:3), "pid", "grp", NULL),
    "exactly 2 observed levels"
  )
})

test_that("prep: duplicate pair member errors", {
  expect_error(
    fanetc:::.paired_prepare_data(data.frame(pid = c(1, 1, 1), grp = c("A", "A", "B"), val = 1:3), "pid", "grp", NULL),
    "Duplicate pair ID"
  )
})

test_that("prep: bad ref_group errors", {
  expect_error(fanetc:::.paired_prepare_data(df1, "pid", "grp", "Z"), "not one of the observed")
})

test_that("prep: pair_id as character works", {
  d <- df1; d$pid <- as.character(d$pid)
  expect_equal(nrow(fanetc:::.paired_prepare_data(d, "pid", "grp", NULL)$data), 5)
})

test_that("prep: pair_id as factor works", {
  d <- df1; d$pid <- factor(d$pid)
  expect_equal(nrow(fanetc:::.paired_prepare_data(d, "pid", "grp", NULL)$data), 5)
})

## ---- .paired_wide() ----

test_that(".paired_wide: only complete pairs kept, values correct", {
  dfw <- data.frame(
    pid = c(1, 1, 2, 2, 3, 3, 4),
    grp = c("A", "B", "A", "B", "A", "B", "A"),
    x   = c(10, 12, NA, 5, 7, 8, 99)
  )
  dfw$grp <- factor(dfw$grp, levels = c("A", "B"))
  w <- fanetc:::.paired_wide(dfw, "pid", "grp", "A", "B", "x")
  expect_equal(nrow(w), 2)  # pair 2 has NA, pair 4 has no B row
  expect_equal(w$.ref[w$pid == 1], 10)
  expect_equal(w$.other[w$pid == 1], 12)
  expect_equal(w$.ref[w$pid == 3], 7)
  expect_equal(w$.other[w$pid == 3], 8)
})

## ---- paired test closures ----

set.seed(11)
n <- 30
dpaired <- data.frame(pid = rep(1:n, each = 2), grp = rep(c("A", "B"), n))
dpaired$grp <- factor(dpaired$grp, levels = c("A", "B"))
base_val <- rnorm(n, 50, 10)
dpaired$cont <- NA_real_
dpaired$cont[dpaired$grp == "A"] <- base_val
dpaired$cont[dpaired$grp == "B"] <- base_val + rnorm(n, 2, 3)

test_that("cont test: meansd matches direct paired t.test", {
  cont_fn_meansd <- fanetc:::.paired_make_cont_test_fn(dpaired, "pid", "grp", "A", "B", "meansd")
  p_meansd <- cont_fn_meansd(data = NULL, variable = "cont", by = NULL)$p.value
  wide_c <- fanetc:::.paired_wide(dpaired, "pid", "grp", "A", "B", "cont")
  expected_t <- stats::t.test(wide_c$.other, wide_c$.ref, paired = TRUE)$p.value
  expect_equal(p_meansd, expected_t)
})

test_that("cont test: mediqr matches direct paired wilcox.test", {
  cont_fn_mediqr <- fanetc:::.paired_make_cont_test_fn(dpaired, "pid", "grp", "A", "B", "mediqr")
  p_mediqr <- suppressWarnings(cont_fn_mediqr(data = NULL, variable = "cont", by = NULL)$p.value)
  wide_c <- fanetc:::.paired_wide(dpaired, "pid", "grp", "A", "B", "cont")
  expected_w <- suppressWarnings(stats::wilcox.test(wide_c$.other, wide_c$.ref, paired = TRUE)$p.value)
  expect_equal(p_mediqr, expected_w)
})

dpaired$cat2 <- factor(sample(c("y", "n"), 2 * n, TRUE))

test_that("cat test: matches direct mcnemar.test on union-of-levels table", {
  cat_fn <- fanetc:::.paired_make_cat_test_fn(dpaired, "pid", "grp", "A", "B")
  p_cat <- cat_fn(data = NULL, variable = "cat2", by = NULL)$p.value
  wide_cat <- fanetc:::.paired_wide(dpaired, "pid", "grp", "A", "B", "cat2")
  lv <- union(as.character(unique(wide_cat$.ref)), as.character(unique(wide_cat$.other)))
  tab_expected <- table(factor(as.character(wide_cat$.ref), levels = lv),
                        factor(as.character(wide_cat$.other), levels = lv))
  expected_mc <- stats::mcnemar.test(tab_expected)$p.value
  expect_equal(p_cat, expected_mc)
})

# 3-category variable -> McNemar-Bowker (mcnemar.test on a k x k table)
dpaired$cat3 <- factor(sample(c("x", "y", "z"), 2 * n, TRUE))

test_that("cat test: 3-level factor returns a numeric p-value (Bowker)", {
  cat_fn3 <- fanetc:::.paired_make_cat_test_fn(dpaired, "pid", "grp", "A", "B")
  p_cat3 <- cat_fn3(data = NULL, variable = "cat3", by = NULL)$p.value
  expect_true(is.numeric(p_cat3) && !is.na(p_cat3))
})

test_that("cat test: all-concordant pairs -> exactly NA_real_, not NaN (no error)", {
  # strict identical(), not just is.na(): mcnemar.test() on an all-concordant table
  # returns NaN internally (0/0), and the design specifies NA -- this asserts the
  # closure's NaN -> NA_real_ normalization actually ran, not merely that is.na()
  # happens to accept both.
  dconc <- data.frame(pid = rep(1:5, each = 2), grp = factor(rep(c("A", "B"), 5), levels = c("A", "B")),
                      same = rep(c("yes"), 10))
  p_conc <- fanetc:::.paired_make_cat_test_fn(dconc, "pid", "grp", "A", "B")(data = NULL, variable = "same", by = NULL)$p.value
  expect_identical(p_conc, NA_real_)
})

dsingle <- data.frame(pid = c(1, 1), grp = factor(c("A", "B"), levels = c("A", "B")), v = c(1, 2))

test_that("cont test: single complete pair -> NA (no error)", {
  p_single <- fanetc:::.paired_make_cont_test_fn(dsingle, "pid", "grp", "A", "B", "meansd")(data = NULL, variable = "v", by = NULL)$p.value
  expect_identical(p_single, NA_real_)
})

dzero <- data.frame(pid = c(1, 2), grp = factor(c("A", "B"), levels = c("A", "B")), v = c(1, 2))

test_that("cont test: zero complete pairs -> NA (no error)", {
  p_zero <- fanetc:::.paired_make_cont_test_fn(dzero, "pid", "grp", "A", "B", "meansd")(data = NULL, variable = "v", by = NULL)$p.value
  expect_identical(p_zero, NA_real_)
})

## ---- SMD and N-pairs closures ----

test_that("n_pairs: counts complete pairs for cont, excludes incomplete pair", {
  n_fn <- fanetc:::.paired_make_n_pairs_fn(dpaired, "pid", "grp", "A", "B")
  expect_equal(n_fn(data = NULL, variable = "cont", by = NULL)$n_pairs, as.character(n))

  # introduce one incomplete pair for a fresh variable to check N pairs != nrow
  dpaired$cont_partial <- dpaired$cont
  dpaired$cont_partial[dpaired$pid == 1 & dpaired$grp == "B"] <- NA
  n_fn2 <- fanetc:::.paired_make_n_pairs_fn(dpaired, "pid", "grp", "A", "B")
  expect_equal(n_fn2(data = NULL, variable = "cont_partial", by = NULL)$n_pairs, as.character(n - 1))
})

test_that("smd: matching method matches direct smd::smd (sign-corrected)", {
  # SMD: matching method matches direct smd::smd on complete pairs, SIGN-FLIPPED.
  # smd::smd()'s own convention is reference-minus-other (verified empirically: with
  # gref=1 and group 1 having the smaller mean, its `estimate` comes out negative) --
  # the opposite of this design's "non-reference minus reference" convention -- so
  # the closure negates it and this test must too.
  smd_fn_match <- fanetc:::.paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "matching")
  smd_match <- smd_fn_match(data = NULL, variable = "cont", by = NULL)$smd
  wide_c2 <- fanetc:::.paired_wide(dpaired, "pid", "grp", "A", "B", "cont")
  x_long <- c(wide_c2$.ref, wide_c2$.other)
  g_long <- factor(c(rep("A", nrow(wide_c2)), rep("B", nrow(wide_c2))), levels = c("A", "B"))
  expected_smd <- -smd::smd(x = x_long, g = g_long, gref = 1L)$estimate[1]
  dec_cont <- max(1L, decimalplaces(dpaired$cont))
  expect_equal(smd_match, formatC(expected_smd, digits = dec_cont, format = "f"))
})

test_that("smd: repeated_measure matches hand-computed Cohen's d_z", {
  smd_fn_rm <- fanetc:::.paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "repeated_measure")
  smd_rm <- smd_fn_rm(data = NULL, variable = "cont", by = NULL)$smd
  wide_c2 <- fanetc:::.paired_wide(dpaired, "pid", "grp", "A", "B", "cont")
  diff <- wide_c2$.other - wide_c2$.ref
  expected_dz <- mean(diff) / sd(diff)
  dec_cont <- max(1L, decimalplaces(dpaired$cont))
  expect_equal(smd_rm, formatC(expected_dz, digits = dec_cont, format = "f"))
})

test_that("smd: categorical identical under both pairing methods, 1 decimal", {
  smd_cat_match <- fanetc:::.paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "cat2", by = NULL)$smd
  smd_cat_rm    <- fanetc:::.paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "repeated_measure")(data = NULL, variable = "cat2", by = NULL)$smd
  expect_equal(smd_cat_match, smd_cat_rm)
  expect_match(smd_cat_match, "^-?[0-9]+\\.[0-9]$")
})

test_that("smd: integer-valued continuous var floored to 1 decimal", {
  dpaired$int_var <- round(dpaired$cont)
  expect_equal(decimalplaces(dpaired$int_var), 0L)
  smd_int <- fanetc:::.paired_make_smd_fn(dpaired, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "int_var", by = NULL)$smd
  expect_match(smd_int, "^-?[0-9]+\\.[0-9]$")
})

test_that("smd: degenerate cases -> '---'", {
  # zero-variance within-pair differences (d_z)
  dconst <- data.frame(pid = rep(1:5, each = 2), grp = factor(rep(c("A", "B"), 5), levels = c("A", "B")), v = 5)
  smd_const <- fanetc:::.paired_make_smd_fn(dconst, "pid", "grp", "A", "B", "repeated_measure")(data = NULL, variable = "v", by = NULL)$smd
  expect_equal(smd_const, "---")

  # single complete pair -> d_z NA -> "---"
  smd_single <- fanetc:::.paired_make_smd_fn(dsingle, "pid", "grp", "A", "B", "repeated_measure")(data = NULL, variable = "v", by = NULL)$smd
  expect_equal(smd_single, "---")

  # zero complete pairs -> "---"
  smd_zero <- fanetc:::.paired_make_smd_fn(dzero, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "v", by = NULL)$smd
  expect_equal(smd_zero, "---")
})

test_that("smd: other-minus-reference sign is positive when other > reference (continuous)", {
  # Sign sanity check, independent of the closure's own internal computation: when
  # the "other" (non-reference) level is unambiguously and consistently larger, the
  # displayed SMD must be positive under BOTH pairing methods. Realistic (nonzero,
  # comparable-magnitude) within-group variance is used deliberately -- a degenerate
  # near-zero-variance setup makes the marginal pooled-variance SMD numerically
  # unstable (division by a near-zero denominator).
  set.seed(41)
  n_sign <- 30
  dsign <- data.frame(pid = rep(1:n_sign, each = 2), grp = rep(c("A", "B"), n_sign), v = NA_real_)
  dsign$grp <- factor(dsign$grp, levels = c("A", "B"))
  v_a <- rnorm(n_sign, 0, 3)
  dsign$v[dsign$grp == "A"] <- v_a
  dsign$v[dsign$grp == "B"] <- v_a + rnorm(n_sign, 5, 2)  # other = reference + positive shift + noise
  sign_match <- fanetc:::.paired_make_smd_fn(dsign, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "v", by = NULL)$smd
  sign_rm    <- fanetc:::.paired_make_smd_fn(dsign, "pid", "grp", "A", "B", "repeated_measure")(data = NULL, variable = "v", by = NULL)$smd
  expect_false(grepl("^-", sign_match))
  expect_false(grepl("^-", sign_rm))
})

sign_cat_match <- NULL  # populated below, reused by the logical regression-guard test

test_that("smd: categorical (logical) other-minus-reference sign is positive when other prevalence > reference", {
  # Same sign check for a CATEGORICAL (logical) variable -- the negation fix applies
  # to the marginal smd::smd() path used for all categorical variables under both
  # pairing methods, not just continuous ones.
  set.seed(7)
  n_cat <- 30
  dsign_cat <- data.frame(pid = rep(1:n_cat, each = 2), grp = rep(c("A", "B"), n_cat), flag = NA)
  dsign_cat$grp <- factor(dsign_cat$grp, levels = c("A", "B"))
  dsign_cat$flag[dsign_cat$grp == "A"] <- sample(c(TRUE, FALSE), n_cat, TRUE, prob = c(0.2, 0.8))
  dsign_cat$flag[dsign_cat$grp == "B"] <- sample(c(TRUE, FALSE), n_cat, TRUE, prob = c(0.8, 0.2))
  sign_cat_match <<- fanetc:::.paired_make_smd_fn(dsign_cat, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "flag", by = NULL)$smd
  expect_false(grepl("^-", sign_cat_match))
})

test_that("smd: 2-level LOGICAL categorical is still negated (regression guard, positive sign confirmed above)", {
  # Confirm the LOGICAL case is still correctly negated: logical must stay signed,
  # unlike factor/character of the same 2-level cardinality (checked below).
  expect_false(grepl("^-", sign_cat_match))
})

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

test_that("main: table_one_paired() returns a tbl_summary/gtsummary object with expected column order", {
  res <- table_one_paired(tdf, pair_id = pid, group = visit)
  expect_true(inherits(res, "tbl_summary") && inherits(res, "gtsummary"))
  expect_identical(tail(res$table_styling$header$column, 6),
                   c("stat_0", "stat_1", "stat_2", "n_pairs", "smd", "p.value"))
  expect_false("pid" %in% res$table_body$variable)
})

test_that("main: include=c(pid, age) drops pid, keeps age, no error", {
  res_inc <- suppressMessages(table_one_paired(tdf, pair_id = pid, group = visit, include = c(pid, age)))
  expect_true("age" %in% res_inc$table_body$variable)
})

test_that("main: var_name/var_desp forwarded to table_one() datadic labels", {
  dic <- data.frame(nm = c("age", "sex"), lbl = c("Age (years)", "Sex"), stringsAsFactors = FALSE)
  res_dic <- table_one_paired(tdf, pair_id = pid, group = visit, datadic = dic, var_name = nm, var_desp = lbl)
  expect_true("Age (years)" %in% as_tibble(res_dic)[[1]])
})

test_that("main: add_p/add_smd/add_n_pairs/add_overall = FALSE removes those columns", {
  res_off <- table_one_paired(tdf, pair_id = pid, group = visit, add_p = FALSE, add_smd = FALSE, add_n_pairs = FALSE, add_overall = FALSE)
  expect_false(any(c("stat_0", "n_pairs", "smd", "p.value") %in% res_off$table_styling$header$column))
})

## ---- full integration tests ----

set.seed(31)
nb <- 40
big <- data.frame(
  pid = rep(1:nb, each = 2),
  visit = rep(c("Baseline", "Followup"), nb),
  age = NA_real_,
  stage = factor(sample(c("I", "II", "III"), 2 * nb, TRUE)),
  responded = sample(c(TRUE, FALSE), 2 * nb, TRUE)
)
base_age <- rnorm(nb, 55, 8)
big$age[big$visit == "Baseline"] <- base_age
big$age[big$visit == "Followup"] <- base_age + rnorm(nb, 1.5, 3)

res_big <- table_one_paired(big, pair_id = pid, group = visit, pairing_method = "repeated_measure")

test_that("integration: N pairs/SMD/p on label rows only; level rows blank", {
  tb <- res_big$table_body
  lbl <- tb[tb$row_type == "label", ]
  lvl <- tb[tb$row_type == "level", ]
  expect_true(all(!is.na(lbl$n_pairs)) && all(!is.na(lbl$smd)))
  expect_true(all(is.na(lvl$n_pairs)) && all(is.na(lvl$smd)) && all(is.na(lvl$p.value)))
})

test_that("integration: dichotomous (logical) variable gets exactly 1 row with stats filled", {
  tb <- res_big$table_body
  resp_rows <- tb[tb$variable == "responded", ]
  expect_equal(nrow(resp_rows), 1)
  expect_true(!is.na(resp_rows$n_pairs) && !is.na(resp_rows$smd) && !is.na(resp_rows$p.value))
})

test_that("integration: descriptive cells match table_one() on the data without pair_id", {
  # NOTE: table_one() itself drops ALL character columns before summary construction
  # (R/desp_table_gtsummary.R), including a character `group` column, which would
  # make dplyr::pull(df, all_of(group_name)) fail. table_one_paired() never hits this
  # because .paired_prepare_data() always rebuilds `group` as a factor before
  # delegating -- so this direct comparison call must do the same conversion (ref
  # level first) to be a valid comparison at all.
  big_desc <- big[, c("visit", "age", "stage", "responded")]
  big_desc$visit <- factor(big_desc$visit, levels = c("Baseline", "Followup"))
  desc_only <- table_one(big_desc, group = visit, add_p = FALSE, add_overall = FALSE)
  expect_identical(
    res_big$table_body$stat_1[res_big$table_body$variable == "age"],
    desc_only$table_body$stat_1[desc_only$table_body$variable == "age"]
  )
  expect_identical(
    res_big$table_body$stat_2[res_big$table_body$row_type == "level" & res_big$table_body$variable == "stage"],
    desc_only$table_body$stat_2[desc_only$table_body$row_type == "level" & desc_only$table_body$variable == "stage"]
  )
})

test_that("integration: ref_group override flips column order and SMD sign", {
  res_flip <- table_one_paired(big, pair_id = pid, group = visit, pairing_method = "repeated_measure", ref_group = "Followup")
  expect_false(identical(
    res_big$table_body$stat_1[res_big$table_body$variable == "age"],
    res_flip$table_body$stat_1[res_flip$table_body$variable == "age"]
  ))
  smd_default <- as.numeric(res_big$table_body$smd[res_big$table_body$variable == "age"])
  smd_flipped <- as.numeric(res_flip$table_body$smd[res_flip$table_body$variable == "age"])
  expect_equal(smd_default, -smd_flipped)
})

test_that("integration: continuous_stat='mediqr' produces a valid table", {
  res_mediqr <- suppressWarnings(table_one_paired(big, pair_id = pid, group = visit, continuous_stat = "mediqr"))
  expect_s3_class(res_mediqr, "gtsummary")
})

test_that("integration: matching SMD differs numerically from repeated_measure SMD for continuous var", {
  res_matching <- table_one_paired(big, pair_id = pid, group = visit, pairing_method = "matching")
  smd_matching <- as.numeric(res_matching$table_body$smd[res_matching$table_body$variable == "age"])
  smd_default <- as.numeric(res_big$table_body$smd[res_big$table_body$variable == "age"])
  expect_false(isTRUE(all.equal(smd_matching, smd_default)))
})

test_that("integration: degenerate variables produce a valid table through the FULL pipeline", {
  # NOTE: `concordant` must be a FACTOR, not character -- table_one() drops character
  # columns entirely, so a character column here would silently vanish from the table
  # and this would stop testing categorical degenerate behavior at all. Each pair is
  # concordant (same value in both members), but the value varies ACROSS pairs so
  # this is a genuine (not literally constant) factor.
  pair_vals <- rep(c("yes", "no"), 5)
  deg <- data.frame(
    pid = rep(1:10, each = 2),
    visit = rep(c("A", "B"), 10),
    const_val = 5,
    concordant = factor(rep(pair_vals, each = 2))
  )
  deg$visit <- factor(deg$visit, levels = c("A", "B"))
  res_deg <- table_one_paired(deg, pair_id = pid, group = visit)
  expect_s3_class(res_deg, "gtsummary")
  expect_true("concordant" %in% res_deg$table_body$variable)
  expect_true(all(is.na(res_deg$table_body$p.value[res_deg$table_body$row_type == "label"])))
})

test_that("integration: missing/empty pair IDs dropped end-to-end without error", {
  dmiss <- big
  dmiss$pid[c(1, 2)] <- NA
  dmiss$pid[3] <- NA
  res_miss <- suppressMessages(table_one_paired(dmiss, pair_id = pid, group = visit))
  expect_s3_class(res_miss, "gtsummary")
})

test_that("integration: sort_by_p ignored when add_p = FALSE (no error, no p.value column)", {
  res_sort_noP <- table_one_paired(big, pair_id = pid, group = visit, add_p = FALSE, sort_by_p = TRUE)
  expect_false("p.value" %in% res_sort_noP$table_styling$header$column)
})

test_that("integration: post-merge gtsummary compatibility (sort_p, as_tibble, as_flex_table)", {
  expect_s3_class(gtsummary::sort_p(res_big), "gtsummary")
  expect_true(is.data.frame(gtsummary::as_tibble(res_big)))
  if (requireNamespace("flextable", quietly = TRUE)) {
    expect_s3_class(gtsummary::as_flex_table(res_big), "flextable")
  } else {
    skip("flextable not installed")
  }
})

test_that("integration: SMD/p-value footnotes present, differ by pairing_method, name method/reference", {
  res_matching <- table_one_paired(big, pair_id = pid, group = visit, pairing_method = "matching")
  fn_rm <- res_big$table_styling$footnote_header$footnote[res_big$table_styling$footnote_header$column == "smd"]
  fn_match <- res_matching$table_styling$footnote_header$footnote[res_matching$table_styling$footnote_header$column == "smd"]
  expect_true(length(fn_rm) > 0 && nzchar(fn_rm[1]))
  expect_false(identical(fn_rm, fn_match))
  expect_true(grepl("Baseline", fn_rm[1]) && grepl("Followup", fn_rm[1]))
  expect_true(grepl("within-pair", fn_rm[1]))
  expect_true(grepl("pooled-variance", fn_match[1]))

  fn_p <- res_big$table_styling$footnote_header$footnote[res_big$table_styling$footnote_header$column == "p.value"]
  expect_true(length(fn_p) > 0 && !grepl("age.*stage.*responded", fn_p[1]))
  expect_true(grepl("paired t-test", fn_p[1]))
  expect_true(grepl("McNemar", fn_p[1]))
})

test_that("integration: pair_id type (numeric/character/factor) gives identical n_pairs/smd/p", {
  small <- big[big$pid <= 10, ]
  res_num  <- table_one_paired(small, pair_id = pid, group = visit)
  small_chr <- small; small_chr$pid <- as.character(small_chr$pid)
  res_chr  <- table_one_paired(small_chr, pair_id = pid, group = visit)
  small_fct <- small; small_fct$pid <- factor(small_fct$pid)
  res_fct  <- table_one_paired(small_fct, pair_id = pid, group = visit)
  expect_identical(res_num$table_body[c("n_pairs", "smd", "p.value")], res_chr$table_body[c("n_pairs", "smd", "p.value")])
  expect_identical(res_num$table_body[c("n_pairs", "smd", "p.value")], res_fct$table_body[c("n_pairs", "smd", "p.value")])
})

test_that("integration: include=pair_id alone errors with a clear message (zero real variables remain)", {
  # table_one_paired() owns this error explicitly with a clear message, rather than
  # letting gtsummary's own cryptic error ("names must be NULL or a character
  # vector, not an empty integer vector") surface to the user.
  expect_error(
    suppressMessages(table_one_paired(big, pair_id = pid, group = visit, include = pid)),
    "selects no variables"
  )
})

test_that("integration: missing='always' produces a missing row for age with NA n_pairs/smd/p", {
  with_na <- big
  with_na$age[with_na$pid == 1 & with_na$visit == "Baseline"] <- NA
  res_always <- table_one_paired(with_na, pair_id = pid, group = visit, missing = "always")
  miss_row <- res_always$table_body[res_always$table_body$variable == "age" & res_always$table_body$row_type == "missing", ]
  expect_equal(nrow(miss_row), 1)
  expect_true(is.na(miss_row$n_pairs) && is.na(miss_row$smd) && is.na(miss_row$p.value))
})

test_that("integration: datadic labels for pair_id/group do not leak into the table", {
  dic_leak <- data.frame(nm = c("pid", "visit", "age"), lbl = c("PAIR ID LEAK", "VISIT LEAK", "Age (years)"), stringsAsFactors = FALSE)
  res_leak <- table_one_paired(big, pair_id = pid, group = visit, datadic = dic_leak, var_name = nm, var_desp = lbl)
  labels_leak <- as_tibble(res_leak)[[1]]
  expect_false("PAIR ID LEAK" %in% labels_leak)
  expect_false("VISIT LEAK" %in% labels_leak)
  expect_true("Age (years)" %in% labels_leak)
})

test_that("smd: 3+ level categorical SMD is NOT negated (gref-invariant Mahalanobis distance)", {
  # smd::smd() returns a gref-invariant, non-negative Mahalanobis distance for
  # factor/character x (regardless of level count), not a signed difference --
  # negating it (as is correctly done for numeric/logical variables) would produce
  # a meaningless negative value.
  set.seed(43)
  n_multi <- 40
  dsmd_multi <- data.frame(pid = rep(1:n_multi, each = 2), grp = rep(c("A", "B"), n_multi), cat3 = NA)
  dsmd_multi$grp <- factor(dsmd_multi$grp, levels = c("A", "B"))
  dsmd_multi$cat3[dsmd_multi$grp == "A"] <- sample(c("x", "y", "z"), n_multi, TRUE, prob = c(0.6, 0.3, 0.1))
  dsmd_multi$cat3[dsmd_multi$grp == "B"] <- sample(c("x", "y", "z"), n_multi, TRUE, prob = c(0.2, 0.3, 0.5))
  dsmd_multi$cat3 <- factor(dsmd_multi$cat3)
  smd_multi <- fanetc:::.paired_make_smd_fn(dsmd_multi, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "cat3", by = NULL)$smd
  wide_multi <- fanetc:::.paired_wide(dsmd_multi, "pid", "grp", "A", "B", "cat3")
  x_multi <- c(wide_multi$.ref, wide_multi$.other)
  g_multi <- factor(c(rep("A", nrow(wide_multi)), rep("B", nrow(wide_multi))), levels = c("A", "B"))
  expected_multi <- smd::smd(x = x_multi, g = g_multi, gref = 1L)$estimate[1]
  expect_false(grepl("^-", smd_multi))
  expect_equal(smd_multi, formatC(expected_multi, digits = 1L, format = "f"))
})

test_that("smd: 2-level FACTOR categorical SMD is NOT negated either (not just 3+ level)", {
  # smd::smd() returns a gref-invariant, non-negative Mahalanobis distance for
  # factor/character x of ANY level count (including 2), not just 3+ -- only
  # numeric and logical x get a signed estimate from smd::smd(). This is the gap
  # a level-count-based guard would miss.
  set.seed(47)
  n_2f <- 40
  dsmd_2f <- data.frame(pid = rep(1:n_2f, each = 2), grp = rep(c("A", "B"), n_2f), sex = NA)
  dsmd_2f$grp <- factor(dsmd_2f$grp, levels = c("A", "B"))
  dsmd_2f$sex[dsmd_2f$grp == "A"] <- sample(c("F", "M"), n_2f, TRUE, prob = c(0.2, 0.8))
  dsmd_2f$sex[dsmd_2f$grp == "B"] <- sample(c("F", "M"), n_2f, TRUE, prob = c(0.8, 0.2))
  dsmd_2f$sex <- factor(dsmd_2f$sex, levels = c("F", "M"))
  smd_2f <- fanetc:::.paired_make_smd_fn(dsmd_2f, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "sex", by = NULL)$smd
  wide_2f <- fanetc:::.paired_wide(dsmd_2f, "pid", "grp", "A", "B", "sex")
  x_2f <- c(wide_2f$.ref, wide_2f$.other)
  g_2f <- factor(c(rep("A", nrow(wide_2f)), rep("B", nrow(wide_2f))), levels = c("A", "B"))
  expected_2f <- smd::smd(x = x_2f, g = g_2f, gref = 1L)$estimate[1]
  expect_false(grepl("^-", smd_2f))
  expect_equal(smd_2f, formatC(expected_2f, digits = 1L, format = "f"))

  # Raw CHARACTER (not factor) must behave identically to factor -- neither
  # is.numeric() nor is.logical() match a character vector, so it takes the same
  # "not negated" path.
  dsmd_2c <- dsmd_2f
  dsmd_2c$sex <- as.character(dsmd_2c$sex)
  smd_2c <- fanetc:::.paired_make_smd_fn(dsmd_2c, "pid", "grp", "A", "B", "matching")(data = NULL, variable = "sex", by = NULL)$smd
  expect_false(grepl("^-", smd_2c))
  expect_equal(smd_2c, smd_2f)
})


test_that("table_one_paired() resets the RNG to seed 0, matching table_one()'s side effect", {
  # table_one_paired() used to reach table_one() (and its set.seed(0)) via
  # eval(call2("table_one", ...)); the direct .table_one_impl() call must
  # reproduce the same observable RNG side effect.
  n <- 20
  dpaired <- data.frame(pid = rep(1:n, each = 2),
                        visit = rep(c("Baseline", "Followup"), n),
                        age = rnorm(2 * n, 55, 8))
  set.seed(999)
  invisible(table_one_paired(dpaired, pair_id = pid, group = visit,
                             add_p = FALSE, add_smd = FALSE, add_n_pairs = FALSE))
  after <- runif(1)
  set.seed(0)
  expected <- runif(1)
  expect_equal(after, expected)
})
