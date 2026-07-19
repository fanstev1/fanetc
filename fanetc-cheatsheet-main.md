# fanetc function cheatsheet — `main` (origin/master)

- Branch: `origin/master` (aliased "main"; `origin/HEAD -> origin/master`)
- DESCRIPTION `Version: 1.0.0`
- Signatures below are taken verbatim from each function's `\usage{}` block in `git show origin/master:man/<fn>.Rd`; argument meanings are drawn from the same Rd's `\arguments{}`/`\description{}`. `API_REFERENCE.md`/`QUICKREF.md` (already on this branch's working tree) were used only as corroborating cross-checks, not as the signature source.
- Master exports **28** functions (`git show origin/master:NAMESPACE`); all 28 have a matching `man/*.Rd`.

---

## 1. Functions used by the cohort-analysis skills

All 12 fanetc-exported functions the skills call are present on master. `Cs()` is **not** one of them — see the note below.

### construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = NULL, append = FALSE, ..., varname = NULL)
- Builds competing-risk time/event variables from date columns.
- `patid`/`idx_dt`/`evt_dt`/`end_dt` are unquoted column names; `idx_dt` = time zero, `evt_dt` = event-of-interest date (NA if it didn't occur), `end_dt` = last follow-up date (can be NA if the event occurred).
- `...` = additional competing-event date columns, passed as `name = column`; **variable arity** (0..k).
- `evt` factor coding: `0` = censored, `1` = event of interest, `2..k+1` = competing events **in the order given** in `...`. Ties at the same date are resolved event-of-interest first, then competing events in argument order, then censoring.
- `cmprisk_varname` (or its alias `varname`, added in this branch) names the two output columns as `c("time_name", "event_name")`; default `c("evt_time", "evt")`.
- Gotcha: events at time zero are silently set to 0.5 (with a warning); times before the index date become `NA` (with a warning and the offending rows printed).
- `append = TRUE` returns `df` with the new columns bound on; `FALSE` (default) returns only the new columns (plus `patid`).

### construct_surv_var(df, patid, idx_dt, evt_dt, end_dt, surv_varname = NULL, append = FALSE)
- Builds a binary (survival) time/event pair: `evt = 1` if `evt_dt` is non-missing (time = idx_dt→evt_dt), else `evt = 0` (time = idx_dt→end_dt).
- `surv_varname` is an optional length-2 character vector `c("time_name", "event_name")`.
- Same time-zero (→0.5, warning) and negative-time (→NA, warning, rows printed) handling as `construct_cmprisk_var`.

### admin_censor_cmprisk(df, evt_time, evt, adm_cnr_time = NULL, evt_label = NULL, overwrite_var = FALSE)
- Administratively censors a competing-risk time/event pair at `adm_cnr_time`.
- `evt` is the competing-risk **factor** (0=censored, 1=event of interest, other=competing risk(s)).
- New variables get an `_adm` suffix unless `overwrite_var = TRUE` (rewrites `evt_time`/`evt` in place — not recommended in general).
- `evt_label` supplies display labels for the event levels.

### admin_censor_surv(df, evt_time, evt, adm_cnr_time = NULL, overwrite_var = FALSE)
- Same as above but for a binary survival pair; `evt` is an integer 0/1 indicator.
- `_adm` suffix by default; `overwrite_var = TRUE` overwrites in place.

### table_one(df, group, datadic = NULL, var_name, var_desp, include, missing = "ifany", missing_text = "(Missing)", missing_group_exclude = TRUE, add_p = NULL, add_overall = NULL, sort_by_p = FALSE, continuous_stat = c("meansd", "mediqr"), pvalue_fun = format_pvalue)
- Builds a `gtsummary::tbl_summary`-based descriptive table for numeric/logical/factor columns of `df`.
- `group` is NSE (unquoted column), optional; 2+ levels supported.
- `include` restricts the row set (default: all columns except `group`).
- `continuous_stat`: `"meansd"` → mean ± SD with Welch t-test/ANOVA; `"mediqr"` → median (Q1–Q3) with Wilcoxon/Kruskal-Wallis. Categorical vars always use Fisher's exact test (with simulation for complex tables).
- `missing_group_exclude = TRUE` (default) drops rows with a missing `group`; `FALSE` keeps them as an explicit `missing_text` level.
- `add_p`/`add_overall` default to TRUE only when `group` is supplied.
- Gotcha: this signature is substantially enriched vs legacy (see §4) — legacy has none of `include`, `missing*`, `add_p`, `add_overall`, `sort_by_p`, `continuous_stat`, `pvalue_fun`.

### estimate_km(df, evt_time, evt, group, ci_transformation = "log-log", ...)
- Fits Kaplan-Meier via `survival::survfit()`; `evt_time`/`evt`/`group` are unquoted (NSE); `evt` must be a **binary 0/1 indicator**, not a competing-risk factor.
- `ci_transformation` (new on this branch) is passed to `survfit()` as `conf.type` (default `"log-log"`, matching SAS's default).
- Stores the input data in the returned object's `call`, so `run_logrank_test()` can re-evaluate it.

### estimate_cif(df, evt_time, evt, group, ...)
- Fits a multi-state (Aalen-Johansen) cumulative-incidence curve; `evt` is the **competing-risk factor**, event of interest = its first non-reference level.
- Same call-storage trick so `run_gray_test()` can re-evaluate it.

### show_cif(surv_obj, evt_type = 1, evt_label = <recode_factor fn>, add_ci = TRUE, add_atrisk = TRUE, add_legend = FALSE, add_pvalue = TRUE, atrisk_init_pos = NULL, pvalue_pos = c(...), plot_theme = theme_minimal(), x_lab = "Time", y_lab = "Proportion of subjects", x_lim = NULL, y_lim = NULL, x_break = NULL, y_break = NULL, color_scheme = c("brewer","grey","viridis","manual"), color_list = NULL, plot_cdf = FALSE, print_fig = TRUE)
- Plots CIF curve(s) from `estimate_cif()`'s output; `surv_obj` is the first (piped) arg.
- `evt_type` selects which state code(s) to plot; `evt_label` maps state codes to legend/strata labels.
- `color_scheme = "manual"` requires `color_list` (e.g. `list(values = c("red","blue"))`).
- `add_atrisk`/`add_pvalue` overlay the at-risk table / Gray's-test p-value directly on the figure.
- Gotcha: the at-risk table is drawn **outside the panel** — needs adequate `plot.margin` and `panel.clip = "off"` (see the Rd's `\dontrun` example for the `ggplot_gtable`/`grid.draw` recipe).
- `print_fig` (new on this branch) toggles auto-printing to the active device.

### summarize_cif(fit, times = NULL)
- Tabulates the fitted CIF at `times` (default: `pretty(fit$time)`); returns a wide dataframe, one row per time point, one column per state/stratum, cells formatted `"xx.x% [lower, upper]"`.

### summarize_km(fit, times = NULL, failure_fun = FALSE)
- Same idea for a `survfit` KM object; `failure_fun = TRUE` reports `1 - S(t)` instead of `S(t)`.

### add_atrisk(p, surv_obj, x_break = NULL, atrisk_init_pos = NULL, plot_theme = NULL)
- Adds an at-risk table underneath an existing ggplot `p` built from `surv_obj`.
- `x_break` defaults to the plot's x-axis breaks; `atrisk_init_pos` is a vertical offset in text-lines below the panel (auto: 2.23 lines for >1 group, 3.06 for 1 group).
- Gotcha: positions are absolute text-lines, drawn outside the panel — needs sufficient bottom/left `plot.margin` and panel clipping off, same as `show_cif()`.

### extract_atrisk(fit, time.list = NULL, time.scale = 1)
- Returns a dataframe of at-risk counts at `time.list` time points, overall or by stratum.
- `time.scale` divides the survfit's internal time before matching `time.list` (e.g. `365.25` to report on a year scale when `fit` is in days).
- Gotcha: on this branch `time.list` has a default of `NULL`; legacy has no default (see §4).

### Cs(...) — re-export note
- **`Cs()` is not provided by fanetc on this branch.** Master's `DESCRIPTION` has no `Hmisc` dependency at all (Depends is just `R (>= 4.2)`; Imports omits Hmisc), and the master `NAMESPACE` neither imports nor re-exports it.
- If a script only does `library(tidyverse); library(fanetc)` (as the skills templates do), `Cs()` will be **unavailable** on master unless the script or environment separately attaches `Hmisc` (e.g. via `library(Hmisc)`).
- This differs from legacy, where `Hmisc` is a `Depends:` of the package itself (see §4) — attaching `fanetc` there also attaches `Hmisc` to the search path, making `Cs()` incidentally available (not because fanetc exports it).

---

## 2. All remaining exported functions

### Outcome construction
(covered in §1: `construct_cmprisk_var`, `construct_surv_var`)

### Administrative censoring
(covered in §1: `admin_censor_cmprisk`, `admin_censor_surv`)

### Estimation / testing
(estimate_km, estimate_cif covered in §1)

#### run_logrank_test(surv_obj)
- Re-evaluates the `survfit` call embedded in `surv_obj` (e.g. from `estimate_km()`) as `survival::survdiff(..., rho = 0)`; returns the log-rank p-value (numeric scalar).

#### run_gray_test(surv_obj, evt_type = 1:2)
- Re-evaluates the data/time/status/group stored in a `survfitms` object's call (e.g. from `estimate_cif()`) via `cmprsk::cuminc()`; returns Gray's-test p-value(s) for the requested `evt_type` code(s). `evt_type = NULL` returns every event type except the first.

### Plotting
(show_cif, add_atrisk, extract_atrisk covered in §1)

#### show_surv(surv_obj, x_lab = "Time", y_lab = <depends on plot_cdf>, y_lim = NULL, x_break = NULL, y_break = NULL, color_scheme = c("brewer","grey","viridis","manual"), color_list = NULL, plot_theme = theme_minimal(), add_ci = TRUE, add_atrisk = TRUE, add_legend = FALSE, add_pvalue = TRUE, atrisk_init_pos = NULL, pvalue_pos = c(...), plot_cdf = FALSE, print_fig = TRUE)
- KM analogue of `show_cif()`; plots survival (or, with `plot_cdf = TRUE`, failure `1-S(t)`) curves with optional CI ribbons, at-risk table, and log-rank p-value (via `run_logrank_test()`).
- Gotcha: `add_legend` is forced `FALSE` for a single cohort or when the at-risk table is shown (cohorts are then color-coded in the table instead); `add_pvalue` forced `FALSE` for a single cohort.
- `y_lim`'s lower bound is always reset to 0; `NULL` gives `c(0,1)`.

#### geom_ribbon_step(mapping = NULL, data = NULL, ..., direction = "hv", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
- Drop-in replacement for `ggplot2::geom_ribbon()` that draws the ribbon as a step function (`direction` `"hv"` default / `"vh"` / `"mid"`), sorting by x within each group first; NA limits become gaps, as in `geom_ribbon()`. Used for the CI bands in `show_surv()`/`show_cif()`.

#### prepare_survfit(surv_obj)
- Internal-facing but exported helper: converts a `survfit`/`survfitms` object into a nested tibble (one row per stratum, plus state for `survfitms`) with step-curve coordinates (`plot_prob_d`) and raw per-time CI limits (`plot_ci_d`, stepped at plot time by `geom_ribbon_step()`); used by `show_surv()`/`show_cif()`.
- Gotcha: relabels survival's censored/reference placeholder state (`"(s0)"` or `""`) to `"0"` and sets it as the reference level.

### Summary / tables

#### table_one_paired(df, pair_id, group, pairing_method = c("repeated_measure","matching"), ref_group = NULL, datadic = NULL, var_name, var_desp, include, missing = "ifany", missing_text = "(Missing)", add_p = TRUE, add_smd = TRUE, add_n_pairs = TRUE, add_overall = TRUE, sort_by_p = FALSE, continuous_stat = c("meansd","mediqr"), pvalue_fun = format_pvalue)
- **New on this branch** — no legacy equivalent. Companion to `table_one()` for long-format paired/matched data (exactly 2 observed `group` levels per `pair_id`).
- `pairing_method` selects the SMD estimand: `"matching"` → marginal (pooled-variance) SMD via `smd::smd()`; `"repeated_measure"` → Cohen's d_z (within-pair mean diff / within-pair SD of diffs) for continuous vars (categorical vars use marginal SMD either way).
- Paired significance tests: paired t-test/Wilcoxon signed-rank (continuous), McNemar/McNemar-Bowker (categorical), computed pairwise-complete per variable.
- Gotcha: `pair_id` is silently dropped from `include` if listed (with a message) — it's never summarized as a row; degenerate cases (0 complete pairs, zero-variance diffs, all-concordant tables) return `NA` rather than erroring.

#### summarize_coxph(mdl, exponentiate = TRUE, maxlabel = 100, alpha = 0.05)
- Summarizes a fitted `coxph`/`coxph.penal` model: dataframe with `term`, `stat` ("estimate [lower, upper]"), `pval`; multi-df terms get an extra type-3-Wald-p-value row.
- `exponentiate = TRUE` reports hazard ratios (not log-HR).

### Multiple-imputation & regression

#### calculate_type3_mi(mira_obj, vcov_fun = NULL)
- Computes type-3 Wald p-values for a model fitted across multiply-imputed datasets (a `mira` object, e.g. from `with(mice::mice(...), glm(...))`), per Li/Meng/Raghunathan/Rubin (1991).
- `vcov_fun` lets you swap in a robust covariance estimator (e.g. `sandwich::vcovHC`); default uses `stats::vcov`.
- Returns a list, one dataframe per model term (`var, rid, df, stat, chisq_p`), each carrying a `"col_in_X"` attribute.

#### summarize_mi_glm(mira_obj, exponentiate = FALSE, alpha = 0.05, vcov_fun = NULL)
- Pools GLM coefficients across MI datasets via `mitools::MIcombine()`; type-3 p-values from `calculate_type3_mi()`.
- Returns one row per coefficient (`var, stat, pval, est, se, conf_low, conf_high, rid`), plus a type-3 row for multi-df terms.

#### summarize_mi_coxph(cox_mira, exponentiate = TRUE, alpha = 0.05)
- Same idea for Cox models: pools via `mice::pool()` with complete-data df = (events − coefficients).

#### generate_mi_glm_termplot_df(mira_obj, terms = NULL, center_at = NULL, vcov_fun = NULL, ...)
- Builds `stats::termplot()`-style partial-effect data (per term: `x, y, se, conf_low, conf_high`) for a GLM fitted to MI data.
- `center_at` anchors a term's effect at 0 for a given covariate value (factor level, numeric floor, or `FALSE` for logical terms); `NULL` keeps termplot's own centering.

### Descriptive helpers

#### mean_sd(x) / med_iqr(x) / n_avail(x)
- Formatting helpers for `gtsummary::inline_text()` etc.: `"mean ± SD"`, `"median (Q1–Q3)"`, and a comma-formatted count of non-missing `x`, respectively.

#### decimalplaces(x, max_dec = 4L)
- Internal-facing exported helper: returns the most frequent number of decimal digits observed in `x`, capped at `max_dec`.

#### format_pvalue(x, eps = 0.001, trim = TRUE, droptrailing0 = FALSE, pad = FALSE, ...)
- Formats p-values: ≥0.1995 → 2 decimals; <0.1995 → 3 decimals; <0.001 → `"<0.001"`. `...` forwarded to `base::format.pval`.

### Utilities

#### updateWorksheet(wb, sheetName, x, ...)
- Adds (or replaces, by remove-then-add) a worksheet named `sheetName` in an `openxlsx` workbook object `wb`, writing dataframe `x`.

---

## 3. master vs legacy — differences (from master's perspective)

**Functions added in master (absent from legacy):**
- `table_one_paired()` — entirely new; no legacy equivalent, no legacy man page. Confirmed via legacy's `man/` and `NAMESPACE` listings (neither contains it).

**`table_one()` signature enrichment — confirmed via Rd `\usage`:**
- Legacy: `table_one(df, group, datadic = NULL, var_name, var_desp)`.
- Master: adds `include`, `missing = "ifany"`, `missing_text = "(Missing)"`, `missing_group_exclude = TRUE`, `add_p = NULL`, `add_overall = NULL`, `sort_by_p = FALSE`, `continuous_stat = c("meansd","mediqr")`, `pvalue_fun = format_pvalue`. All confirmed present in master's `table_one.Rd` `\usage` and absent from legacy's.

**`estimate_km()` gains `ci_transformation` — confirmed:**
- Legacy: `estimate_km(df, evt_time, evt, group, ...)`.
- Master: `estimate_km(df, evt_time, evt, group, ci_transformation = "log-log", ...)`.

**`construct_cmprisk_var()` gains `varname` — confirmed:**
- Legacy: `construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = NULL, append = FALSE, ...)`.
- Master: same, plus a trailing `varname = NULL` (documented as "Alias of `cmprisk_varname`").

**Additional signature changes found while verifying (not in the original claim list):**
- `extract_atrisk()`: legacy's Rd usage is `extract_atrisk(fit, time.list, time.scale = 1)` — **`time.list` has no default**, i.e. it's a required argument. Master gives it a default: `extract_atrisk(fit, time.list = NULL, time.scale = 1)`.
- `show_cif()`: legacy's Rd usage ends `..., color_list = NULL, plot_cdf = FALSE)` — **no `print_fig` argument**. Master adds `print_fig = TRUE` (matching `show_surv()`, which already had `print_fig` on both branches).

**Functions present only in legacy (absent from master):**
- `factor_desp()`, `logical_desp()`, `numeric_desp()`, `k_sample_test()`, `two_sample_test()` — all confirmed exported in legacy's `NAMESPACE` and documented in legacy's `man/`; none appear in master's `NAMESPACE` or `man/`. These were legacy's manual per-type descriptive-table engine, superseded on master by the `gtsummary`-based `table_one()`.
- Additionally, legacy's `man/` (but *not* its `NAMESPACE`) also documents `factor_dist()`, `fisher_test()`, and `recode_missing()` — these are internal (unexported) legacy helpers, not part of the legacy public API, so they are not counted among the 5 "legacy-only exported functions" above.

**Functions confirmed unchanged (signature-identical) between branches:**
`add_atrisk`, `admin_censor_cmprisk`, `admin_censor_surv`, `calculate_type3_mi`, `construct_surv_var`, `decimalplaces`, `estimate_cif`, `format_pvalue`, `generate_mi_glm_termplot_df`, `mean_sd`, `med_iqr`, `n_avail`, `prepare_survfit`, `run_gray_test`, `run_logrank_test`, `show_surv`, `summarize_cif`, `summarize_coxph`, `summarize_km`, `summarize_mi_coxph`, `summarize_mi_glm`, `updateWorksheet`.

**`Cs()` dependency difference:** see §1 — master has no `Hmisc` dependency at all (Depends/Imports both omit it), whereas legacy's `DESCRIPTION` lists `Depends: Hmisc, magrittr, tidyverse, ...`, so attaching legacy `fanetc` also attaches `Hmisc` (making `Cs()` incidentally reachable). `Cs()` itself is never an export of `fanetc` on either branch.
