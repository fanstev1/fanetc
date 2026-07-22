# fanetc function cheatsheet — `legacy` (origin/fanetc_legacy)

- Branch: `origin/fanetc_legacy` — the version SHORE-5 and HC-and-PGD ran against, installed in Docker image `fanstev1/r-trchr-4.3.0:common`.
- DESCRIPTION `Version: 0.1.0`
- Signatures below are taken verbatim from each function's `\usage{}` block in `git show origin/fanetc_legacy:man/<fn>.Rd`. **8 exported functions on this branch have no `man/*.Rd` at all** (roxygen docs were never regenerated for them) — for those, the signature was read directly from the `function(...)` definition in `git show origin/fanetc_legacy:R/*.R` and is marked "(from R/ source, no Rd)" below.
- Legacy exports **32** functions (`git show origin/fanetc_legacy:NAMESPACE`); legacy's `man/` additionally documents 3 unexported internal helpers (`factor_dist`, `fisher_test`, `recode_missing`) that are not part of the public API.

---

## 1. Functions used by the cohort-analysis skills

All 12 fanetc-exported functions the skills call are present on legacy. `Cs()` is **not** one of them — see the note below.

### construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = NULL, append = FALSE, ...)
- Builds competing-risk time/event variables from date columns. (Rd has no `\arguments{}` block on this branch — description only: "creates time-to-event variables for competing risk data".)
- `patid`/`idx_dt`/`evt_dt`/`end_dt` unquoted column names, same semantics as master: `idx_dt` = time zero, `evt_dt` = event-of-interest date, `end_dt` = last follow-up date.
- `...` = additional competing-event date columns (`name = column`), variable arity.
- `evt` factor coding: `0` = censored, `1` = event of interest, `2..k+1` = competing events in argument order (confirmed against `R/fan_util_fun.R` source, since the Rd omits the details block found on master).
- No `varname` alias on this branch (master-only addition — see §4).

### construct_surv_var(df, patid, idx_dt, evt_dt, end_dt, surv_varname = NULL, append = FALSE)
- Builds a binary survival time/event pair; `evt = 1` iff `evt_dt` non-missing.
- `surv_varname` optional length-2 character vector `c("time_name","event_name")`.
- Signature identical to master.

### admin_censor_cmprisk(df, evt_time, evt, adm_cnr_time = NULL, evt_label = NULL, overwrite_var = FALSE)
- Administratively censors a competing-risk time/event pair at `adm_cnr_time`; `evt` is the competing-risk factor.
- `_adm` suffix unless `overwrite_var = TRUE`. Identical to master.

### admin_censor_surv(df, evt_time, evt, adm_cnr_time = NULL, overwrite_var = FALSE)
- Same for a binary survival pair. Identical to master.

### table_one(df, group, datadic = NULL, var_name, var_desp)
- Legacy's descriptive table — **manual per-type engine**, not gtsummary-based.
- `group` is NSE (unquoted), optional.
- Fixed behavior (no knobs on this branch): continuous vars → mean±SD *and* median(Q1-Q3) with Welch t-test/Wilcoxon (2 groups) or ANOVA/Kruskal-Wallis (>2 groups, via `k_sample_test`/`two_sample_test`); categorical/logical vars → n(%) with Fisher's exact test.
- Gotcha: none of master's `include`, `missing*`, `add_p`, `add_overall`, `sort_by_p`, `continuous_stat`, `pvalue_fun` args exist here — see §4.

### estimate_km(df, evt_time, evt, group, ...)
- Fits KM via `survfit()` with `conf.type = "log-log"` hardcoded (no `ci_transformation` argument on this branch — see §4).
- `evt` must be a binary 0/1 indicator; stores data in the call for `run_logrank_test()`.

### estimate_cif(df, evt_time, evt, group, ...)
- Fits the Aalen-Johansen (Rd on this branch spells it "Andersen-Johansen") multi-state CIF; `evt` is the competing-risk factor.
- Stores data in the call for `run_gray_test()`. Identical signature to master.

### show_cif(surv_obj, evt_type = 1, evt_label = <recode_factor fn>, add_ci = TRUE, add_atrisk = TRUE, add_legend = FALSE, add_pvalue = TRUE, atrisk_init_pos = NULL, pvalue_pos = c(...), plot_theme = theme_minimal(), x_lab = "Time", y_lab = "Proportion of subjects", x_lim = NULL, y_lim = NULL, x_break = NULL, y_break = NULL, color_scheme = c("brewer","grey","viridis","manual"), color_list = NULL, plot_cdf = FALSE)
- Plots CIF curve(s); same behavior as master **minus** `print_fig` (no such argument on this branch — always behaves as if `print_fig = TRUE`; see §4).
- Gotcha: the Rd's `\arguments{}` block on this branch is corrupted/mismatched (e.g. `plot_theme`'s doc text is actually `add_atrisk`'s copy-pasted from another function) — treat the arg *names and defaults* in `\usage` as reliable, but not the per-arg prose in this branch's Rd.
- At-risk table still needs the same `plot.margin`/panel-clip-off treatment as master.

### summarize_cif(fit, times = NULL)
- Tabulates fitted CIF at `times`; identical to master. (Rd on this branch has no `\arguments{}`/`\value{}` block — description/details only.)

### summarize_km(fit, times = NULL, failure_fun = FALSE)
- Tabulates fitted KM at `times`; identical to master. (Rd has no `\arguments{}` block on this branch.)

### add_atrisk(p, surv_obj, x_break = NULL, atrisk_init_pos = NULL, plot_theme = NULL) — (from R/ source, no Rd)
- No `man/add_atrisk.Rd` exists on this branch even though it's exported; signature confirmed from `R/event_time_desp.R`.
- **`atrisk_init_pos` semantics differ from master.** On this branch, when `atrisk_init_pos` is `NULL` it is set to `-0.225 * max(diff(y-range), diff(y-limits))` and used as a `ymin`/`ymax` **negative y-DATA-coordinate offset** below the panel — there is no strata-aware (single-group vs. multi-group) default. This is unlike master (v1.0.0+), where `atrisk_init_pos` is an **absolute text-line count** (`NULL` defaults to 2.23 lines for >1 group, 3.06 lines for a single group), and a negative value there triggers a warning and is ignored. Because the two branches use different units (data coordinates here vs. text-lines on master), **`atrisk_init_pos` values are not portable between versions** — the at-risk table still needs the same panel-clip-off/`plot.margin` treatment as master, but a numeric value tuned on one branch will not behave the same on the other.

### extract_atrisk(fit, time.list, time.scale = 1)
- Returns at-risk counts at `time.list`.
- Gotcha: **`time.list` has no default here** — it is a required argument (unlike master, which defaults it to `NULL`; see §4).

### Cs(...) — re-export note
- **Not exported by `fanetc` on this branch either** — `Cs()` belongs to `Hmisc`, never to `fanetc`'s own `NAMESPACE`.
- However, legacy's `DESCRIPTION` declares `Depends: Hmisc, magrittr, tidyverse, rlang, forcats, lubridate, reshape2, survival, cmprsk, broom, grid, gridExtra, viridis, cowplot, extrafont` — because `Hmisc` is a package `Depends` (not just `Imports`), `library(fanetc)` on this branch also **attaches** `Hmisc` to the search path as a side effect, making `Cs()` callable without a separate `library(Hmisc)`. This is why the existing skills cheatsheet describes `Cs()` as "provided via fanetc" — true only in this indirect, Depends-attachment sense, and true only on legacy (see the master cheatsheet §1, where it does not hold).

---

## 2. All remaining exported functions

### Outcome construction
(covered in §1: `construct_cmprisk_var`, `construct_surv_var`)

### Administrative censoring
(covered in §1: `admin_censor_cmprisk`, `admin_censor_surv`)

### Estimation / testing
(estimate_km, estimate_cif covered in §1)

#### run_logrank_test(surv_obj) — (from R/ source, no Rd)
- No `man/run_logrank_test.Rd` on this branch despite being exported. Re-evaluates the `survfit` call embedded in `surv_obj` as `survival::survdiff(rho = 0)`; returns the log-rank p-value. Confirmed identical to master via `R/event_time_desp.R` source.

#### run_gray_test(surv_obj, evt_type = 1:2) — (from R/ source, no Rd)
- No `man/run_gray_test.Rd` on this branch despite being exported. Gray's test via `cmprsk::cuminc()` on the data/formula stored in `surv_obj`'s call; signature and behavior confirmed identical to master via `R/event_time_desp.R` source.

### Plotting
(show_cif, add_atrisk, extract_atrisk covered in §1)

#### show_surv(surv_obj, x_lab = "Time", y_lab = <depends on plot_cdf>, y_lim = NULL, x_break = NULL, y_break = NULL, color_scheme = c("brewer","grey","viridis","manual"), color_list = NULL, plot_theme = theme_minimal(), add_ci = TRUE, add_atrisk = TRUE, add_legend = FALSE, add_pvalue = TRUE, atrisk_init_pos = NULL, pvalue_pos = c(...), plot_cdf = FALSE, print_fig = TRUE) — (from R/ source, no Rd)
- No `man/show_surv.Rd` on this branch despite being exported. Signature (including `print_fig = TRUE`) confirmed from `R/event_time_desp.R` and is identical to master's.

#### prepare_survfit(surv_obj) — (from R/ source, no Rd)
- No `man/prepare_survfit.Rd` on this branch. Converts a `survfit`/`survfitms` object to the nested tibble used by `show_surv()`/`show_cif()`; confirmed from source, same role as on master.

### Summary / tables

(no `table_one_paired` on legacy — see §4)

#### summarize_coxph(mdl, exponentiate = TRUE, maxlabel = 100, alpha = 0.05)
- Summarizes a `coxph`/`coxph.penal` fit with type-3 Wald p-values. Identical to master. (Rd on this branch has no `\arguments{}` block.)

### Multiple-imputation & regression

#### calculate_type3_mi(mira_obj, vcov_fun = NULL)
- Type-3 Wald p-values for an MI-fitted model. Identical to master. (Rd on this branch has no `\arguments{}` block, and the details text is truncated: "calculates the  3 p-values based on Wald's statistics" — the "type" appears to have been dropped from the roxygen comment.)

#### summarize_mi_glm(mira_obj, exponentiate = FALSE, alpha = 0.05, vcov_fun = NULL) — (from R/ source, no Rd)
- No `man/summarize_mi_glm.Rd` on this branch despite being exported. Signature confirmed from `R/fan_util_fun.R`, identical to master's; the function body was heavily commented-out/in-progress in this branch's source (large blocks of `#`-commented alternative implementation), so treat this function's behavior on legacy as less battle-tested than on master.

#### summarize_mi_coxph(cox_mira, exponentiate = TRUE, alpha = 0.05) — (from R/ source, no Rd)
- No `man/summarize_mi_coxph.Rd` on this branch. Signature confirmed from source, identical to master's; same caveat about commented-out/WIP code in the function body.

#### generate_mi_glm_termplot_df(mira_obj, terms = NULL, center_at = NULL, vcov_fun = NULL, ...) — (from R/ source, no Rd)
- No `man/generate_mi_glm_termplot_df.Rd` on this branch. Signature confirmed from source, identical to master's.

### Descriptive helpers

#### mean_sd(x) / med_iqr(x) / n_avail(x)
- Same formatting helpers as master, but **not** gtsummary-oriented on this branch — they're called directly by `numeric_desp()` (legacy's internal descriptive engine) rather than passed to `gtsummary::inline_text()`.

#### decimalplaces(x, max_dec = 4L)
- Identical to master.

#### format_pvalue(x, eps = 0.001, trim = TRUE, droptrailing0 = FALSE, pad = FALSE, ...)
- Identical signature to master. Rd wording differs slightly ("Annals of Medicine" guideline reference) but the formatting thresholds are the same class of rule.

### Utilities

#### updateWorksheet(wb, sheetName, x, ...)
- Identical to master.

---

## 3. Legacy-only exported functions (legacy's manual descriptive-table engine)

These are absent from master — superseded there by the `gtsummary`-based `table_one()`.

### factor_desp(df, group, includeNA = FALSE)
- Computes frequency/proportion for factor/categorical variables, by `group` if supplied; runs Fisher's exact test across >1 group. Called internally by `table_one()`.

### logical_desp(df, group)
- Same for logical (TRUE/FALSE) variables: frequency/proportion of TRUE, with Fisher's exact test across groups.

### numeric_desp(df, group)
- Reports mean, SD, median, and IQR by group for numeric variables; delegates significance testing to `two_sample_test()` (2 groups) or `k_sample_test()` (>2 groups).

### k_sample_test(df, group)
- One-way ANOVA with unequal variance (when mean is the reported statistic) or Kruskal-Wallis (nonparametric) for comparing >2 groups; returns a 2-tuple of p-values.

### two_sample_test(df, group)
- Welch two-sample t-test or Wilcoxon rank-sum test for comparing exactly 2 groups; returns a 2-tuple of p-values.

**Not counted among the above** (unexported internal helpers, documented in `man/` but absent from legacy's `NAMESPACE`): `factor_dist()` (marginal-table helper for `factor_desp()`'s cross-tab), `fisher_test()` (the Fisher's-exact-test helper called by `logical_desp()`), `recode_missing()` (replaces a missing-value code with `NA`). These have no analogue on master either, but since they were never part of legacy's *public* API they aren't listed as "legacy-only exports."

---

## 4. master vs legacy — differences (from legacy's perspective)

**Functions added in master (absent here):**
- `table_one_paired()` — confirmed absent from this branch's `NAMESPACE` and `man/`.

**`table_one()` — this branch's signature is the older, simpler one:**
- Legacy: `table_one(df, group, datadic = NULL, var_name, var_desp)` — confirmed via this branch's `table_one.Rd` `\usage`.
- Master adds `include`, `missing`, `missing_text`, `missing_group_exclude`, `add_p`, `add_overall`, `sort_by_p`, `continuous_stat`, `pvalue_fun`, and rebuilds the internals on `gtsummary` (vs. this branch's manual `factor_desp`/`logical_desp`/`numeric_desp` dispatch).

**`estimate_km()` — no `ci_transformation` here:**
- Legacy: `estimate_km(df, evt_time, evt, group, ...)` (confirmed via Rd `\usage`); `conf.type = "log-log"` is hardcoded in the source, not user-selectable.
- Master adds `ci_transformation = "log-log"` as a user-facing argument.

**`construct_cmprisk_var()` — no `varname` alias here:**
- Legacy: `construct_cmprisk_var(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname = NULL, append = FALSE, ...)` (confirmed via Rd `\usage`).
- Master adds a trailing `varname = NULL` alias for `cmprisk_varname`.

**Additional signature changes found while verifying (not in the original claim list):**
- `extract_atrisk()`: this branch's Rd usage, `extract_atrisk(fit, time.list, time.scale = 1)`, has **no default for `time.list`** — it must always be supplied. Master gives it `time.list = NULL`.
- `show_cif()`: this branch's Rd usage ends `..., color_list = NULL, plot_cdf = FALSE)` — **no `print_fig` argument**. Master adds `print_fig = TRUE`. (Note `show_surv()` already had `print_fig` on *both* branches — only `show_cif()` differs here.)

**Functions present only on this branch (absent from master):**
- `factor_desp()`, `logical_desp()`, `numeric_desp()`, `k_sample_test()`, `two_sample_test()` — see §3. Confirmed via master's `NAMESPACE`/`man/`, neither of which contains them.

**Functions confirmed unchanged (signature-identical) between branches:**
`add_atrisk`, `admin_censor_cmprisk`, `admin_censor_surv`, `calculate_type3_mi`, `construct_surv_var`, `decimalplaces`, `estimate_cif`, `format_pvalue`, `generate_mi_glm_termplot_df`, `mean_sd`, `med_iqr`, `n_avail`, `prepare_survfit`, `run_gray_test`, `run_logrank_test`, `show_surv`, `summarize_cif`, `summarize_coxph`, `summarize_km`, `summarize_mi_coxph`, `summarize_mi_glm`, `updateWorksheet`.

**`Cs()` dependency difference:** see §1 — on this branch `Hmisc` is a package `Depends`, so `library(fanetc)` incidentally attaches `Hmisc` and makes `Cs()` callable; on master there is no `Hmisc` dependency at all, so `Cs()` is unavailable unless attached separately. `Cs()` is never an export of `fanetc` itself on either branch.

**Documentation-completeness gap specific to this branch:** 8 of legacy's 32 exports have no `man/*.Rd` at all: `add_atrisk`, `generate_mi_glm_termplot_df`, `prepare_survfit`, `run_gray_test`, `run_logrank_test`, `show_surv`, `summarize_mi_coxph`, `summarize_mi_glm` (verified via `comm` between the `NAMESPACE` export list and the `man/` filenames). Their signatures here were instead confirmed directly against `R/*.R` source, as noted per-function above. Master has 1:1 Rd coverage for all 28 exports.
