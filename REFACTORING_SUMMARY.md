# Refactoring Summary: table_one() to gtsummary

## Executive Summary

The `table_one()` function and its supporting functions (`numeric_desp()`,
`logical_desp()`, `factor_desp()`, `factor_dist()`, `two_sample_test()`,
`k_sample_test()`, `fisher_test()`) have been replaced with a rewrite built
on the `gtsummary` package. This is merged to `master` (commit `1eaae1c`,
2026-07-09) — not a proposal.

- **Verified code reduction:** 825 lines across 4 files (the pre-refactor
  `desp_table.R`, `numeric_desp.R`, `logical_desp.R`, `factor_desp.R`, all
  removed from the repo) → 397 lines in 1 file
  (`R/desp_table_gtsummary.R`), a 52% reduction. (An earlier draft of this
  document claimed "75%, 800 → 200 lines" — neither number matched the
  actual files once checked against `git show 7ab169b:R/<file> | wc -l` and
  the current `wc -l`.)
- **Simplified maintenance:** several standalone functions consolidated into
  one function built on a maintained external package.
- **Enhanced functionality:** more export formats, automatic formatting.
- **Better statistical practices:** leverages gtsummary's test selection and
  formatting instead of hand-rolled `try()`-wrapped test logic.

No performance/memory benchmark exists for `table_one()` anywhere in this
repo (`dev-tests/` only benchmarks `construct_surv_var()` and
`construct_cmprisk_var()`), so this document makes no runtime-speed or
memory claims — earlier drafts had a specific ms/MB table that could not be
reproduced or traced to any actual measurement and has been removed.

## What Was Changed

### Pre-Refactor Implementation (removed; line counts verified via
`git show 7ab169b:R/<file> | wc -l`)
```
R/desp_table.R (127 lines)
├── table_one() - Main orchestrator
└── (dispatched to the functions below)

R/numeric_desp.R (271 lines)
├── numeric_desp() - Numeric variable statistics
├── two_sample_test() - Manual 2-group testing
└── k_sample_test() - Manual k-group testing
(n_avail(), mean_sd(), med_iqr() live in fan_util_fun.R and are still
exported today, unchanged in purpose)

R/logical_desp.R (150 lines)
├── logical_desp() - Logical variable statistics
└── fisher_test() - Manual Fisher's exact test

R/factor_desp.R (277 lines)
├── factor_desp() - Factor variable statistics
└── factor_dist() - Cross-tabulation with tests
```

**Total: 825 lines across 4 files**

### Current Implementation
```
R/desp_table_gtsummary.R (397 lines, current master)
├── table_one() - Single unified function
├── format_pvalue() - P-value formatting
└── mean_sd(), med_iqr(), n_avail() - helpers (also used elsewhere)
```

**Total: 397 lines in 1 file**

## Related Documentation

- **REPRODUCING_LEGACY_RESULTS.md** — the diff-verified list of behavior
  changes vs. the pre-refactor `fanetc_legacy` branch; treat this as
  authoritative over anything below that overlaps with it.
- **CODE_COMPARISON.md** — side-by-side old/new code snippets.
- **EXAMPLES.md** — usage examples, checked against the current function.
- **API_REFERENCE.md** — full parameter documentation.
- **QUICKREF.md** — quick lookup / troubleshooting.

(`MIGRATION_GUIDE.md` existed as an earlier draft of a step-by-step "how to
migrate" guide; it was deleted during this audit as redundant with
REPRODUCING_LEGACY_RESULTS.md, and its unique content — a `gtsummary::add_stat()`
Q&A snippet — did not actually run against the current gtsummary version.)

## Key Improvements

### 1. Code Simplicity
**Before:** manual dispatch and binding across multiple functions
```r
num_out_lst <- numeric_desp(df, !!group)
fct_out_lst <- factor_desp(df, !!group)
logic_out_lst <- logical_desp(df, !!group)
out_lst <- num_out_lst %>%
  append(fct_out_lst) %>%
  append(logic_out_lst)
```

**After:** single function call
```r
tbl <- table_one(df, group = sex)
```

### 2. Statistical Testing
**Before:** manual test selection (in `numeric_desp()`/`logical_desp()`)

**After:** automatic via gtsummary, verified against `R/desp_table_gtsummary.R`:
```r
gtsummary::add_p(
  test = list(
    all_continuous() ~ "t.test",       # or "wilcox.test" if continuous_stat = "mediqr"
    all_categorical() ~ "fisher.test"
  )
)
```

### 3. Output Flexibility
**Before:** dataframe only (manually constructed columns)

**After:** multiple formats (verified against gtsummary 2.1.0's actual
exports — `as_latex()` does **not** exist, despite appearing in earlier
drafts of this document):
```r
tbl <- table_one(df, group = sex)
gtsummary::as_gt(tbl)           # HTML
gtsummary::as_kable(tbl)        # Markdown
gtsummary::as_flex_table(tbl)   # Word
gtsummary::as_tibble(tbl)       # Dataframe
gtsummary::as_hux_table(tbl)    # huxtable
gtsummary::as_kable_extra(tbl)  # kableExtra (can go to LaTeX from here)
```

### 4. Automatic Variable Type Detection
**Before:** separate `select_if(is.numeric)`/`is.factor`/`is.logical` dispatch

**After:** automatic via `gtsummary::tbl_summary()`. Note: logical variables
render as a single "dichotomous" row (the count of `TRUE`), not one row per
level — verified by running the function.

### 5. P-value Formatting
**Before:** custom implementation with multiple conditions in `fan_util_fun.R`

**After:** unchanged formatting logic (`format_pvalue()` is still exported
and still used the same way), just passed straight to gtsummary:
```r
table_one(df, group = sex, pvalue_fun = format_pvalue)
```

## Functional Equivalence

| Feature | Pre-refactor | Current |
|---------|----------|-----|
| Numeric summaries | mean/SD *and* median/IQR shown together | One or the other per table, chosen via `continuous_stat` (verified — `table_one()` does not show both at once) |
| Factor summaries (n, %) | yes | yes |
| Logical/binary summaries | yes (TRUE/FALSE rows) | yes, but as a single dichotomous row, not two rows |
| Group comparisons | yes | yes |
| Statistical tests (t-test, Wilcoxon, Fisher) | yes | yes, same tests, auto-selected |
| P-value formatting | yes | yes, same `format_pvalue()` |
| Data dictionary support | yes | yes, same `var_name`/`var_desp` column convention |
| Missing data handling | yes | yes, plus a new `missing_group_exclude` option |
| HTML/Word/Markdown export | no (CSV only) | yes |
| Inline statistics (`inline_text()`) | no | yes |

## Breaking Changes

See **REPRODUCING_LEGACY_RESULTS.md** for the complete, diff-verified list.
Summary:

1. **Return type changed.** `table_one()` now returns a `tbl_summary`/
   `gtsummary` object, not a dataframe. Use `gtsummary::as_tibble()` to get a
   dataframe back.
2. **`library(fanetc)` no longer attaches other packages** (DESCRIPTION has
   no `Depends`, only `Imports`). Scripts relying on `fanetc` to attach
   `dplyr`/`ggplot2`/etc. need their own `library()` calls now.
3. **8 functions removed**: `numeric_desp()`, `logical_desp()`,
   `factor_desp()`, `factor_dist()`, `two_sample_test()`, `k_sample_test()`,
   `fisher_test()`, `recode_missing()`.
4. `construct_surv_var()`/`construct_cmprisk_var()` had two edge-case bugs
   fixed, which can change *numeric results*, not just types, for affected
   rows.

**Not a breaking change, despite earlier drafts of this document saying
otherwise:** the `group` parameter did **not** need to become a required
named argument. `table_one(df, sex)` (positional) still works today,
verified against the current code and `dev-tests/test_backward_compat.R`.

## Benefits Realized

1. **Reduced maintenance burden**: 825 → 397 lines, in one file instead of four.
2. **Fewer bugs**: two known pre-refactor edge-case bugs in
   `construct_surv_var()`/`construct_cmprisk_var()` were fixed as part of
   this overall effort (see REPRODUCING_LEGACY_RESULTS.md).
3. **More export formats**: HTML, Word, Markdown, huxtable, kableExtra (see
   above for the verified list — no direct LaTeX export function).
4. **Leverages the gtsummary ecosystem** going forward instead of
   hand-maintained statistical test logic.

---

**Refactoring merged:** 2026-07-09 (commit `1eaae1c`).
**Status:** Complete and in use on `master`.
**Compatibility:** See REPRODUCING_LEGACY_RESULTS.md for the verified list of
breaking changes and how to reproduce pre-refactor results if needed.
