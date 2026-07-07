# Design: table_one_paired() — descriptive tables for paired data

Date: 2026-07-06
Status: awaiting user review

## Purpose

A companion to `table_one()` for paired/matched data (1:1 matched cohorts,
before/after measurements, matched case–control). The input data frame is in
long format: one row per pair member, a pair-ID column linking members, and a
grouping variable with **exactly 2 levels** identifying the member's arm.
The function produces the usual descriptive table plus paired-appropriate
p-values, standardized mean differences (SMD), and the number of complete
pairs each comparison used.

Support for 3-level matched sets was considered and deliberately dropped
(no classic test exists for multi-category variables in 3-level sets); the
group variable must have exactly 2 observed levels.

## API

New exported function in a **new file** `R/desp_table_paired.R`
(repo convention: new functions never go into existing files):

```r
table_one_paired(df, pair_id, group,
                 datadic = NULL, var_name, var_desp, include,
                 missing = "ifany", missing_text = "(Missing)",
                 add_p = TRUE, add_smd = TRUE, add_n_pairs = TRUE,
                 add_overall = TRUE, sort_by_p = FALSE,
                 continuous_stat = c("meansd", "mediqr"),
                 pvalue_fun = format_pvalue)
```

- `pair_id` and `group` are unquoted column names (NSE), like `group` in
  `table_one()`. Both are required.
- Every argument shared with `table_one()` (`datadic`, `var_name`,
  `var_desp`, `include`, `missing`, `missing_text`, `continuous_stat`,
  `sort_by_p`, `pvalue_fun`) behaves identically to `table_one()`.
- Returns a gtsummary `tbl_summary` object, further modifiable exactly like
  `table_one()` output.

## Input validation (before any summarizing)

1. `group` must have exactly 2 observed (non-missing, non-empty) levels —
   otherwise an informative error.
2. Each pair ID may contribute **at most one row per group level**. Duplicate
   members within a pair/level make the pairing ambiguous — error, naming the
   offending pair IDs.
3. A pair present in only one group level is allowed (kept for descriptives,
   excluded from tests/SMD).
4. Rows with a missing group value are excluded (with a message), since they
   cannot be paired.

## Column layout

| label | Overall | Group level 1 | Group level 2 | N pairs | SMD | p-value |

`add_overall`, `add_n_pairs`, `add_smd`, `add_p` each toggle their column.

## Descriptive columns

Built by **delegating to `table_one()`** with `add_p = FALSE`: the pair-ID
column is carried in the data (needed later by the paired tests) but excluded
from the summary rows. All of table_one's behavior — decimal-place matching
via `decimalplaces()`, `datadic` labels, missing-row display,
`continuous_stat` glue strings, factor-level dropping — is inherited verbatim
rather than duplicated. Descriptives use **all available observations** in
each group (incomplete pairs included).

## Paired p-values

Attached with `gtsummary::add_p()` using a single custom test function
(gtsummary custom-test API: receives `data`, `variable`, `by`, plus the pair
column name; returns a data frame with `p.value` and `method`).

Per variable, the test function:

1. Pivots that variable wide by pair ID (one column per group level).
2. Keeps **complete pairs**: both members present and non-missing for that
   variable (pairwise-complete, so each variable uses all pairs available to
   it).
3. Dispatches:

| Variable type | `continuous_stat = "meansd"` | `continuous_stat = "mediqr"` |
|---|---|---|
| Continuous | Paired t-test (`stats::t.test(paired = TRUE)`) | Wilcoxon signed-rank (`stats::wilcox.test(paired = TRUE)`) |
| Categorical (logical or factor, any number of categories) | `stats::mcnemar.test()` on the k×k pair-level contingency table — McNemar for 2×2, McNemar–Bowker symmetry test for k×k | same |

Notes:
- The pair-level contingency table is built with the union of factor levels
  on both axes so it is always square.
- `stats::mcnemar.test()` default continuity correction applies to the 2×2
  case (base-R default behavior).
- Degenerate cases return `NA` instead of erroring: zero complete pairs,
  all-concordant tables (zero discordant pairs → NaN), constant variables.
- P-values formatted with `pvalue_fun` (default `format_pvalue`), consistent
  with `table_one()`.

## SMD and N-pairs columns

Attached with `gtsummary::add_stat()`, computed on the **same complete pairs
the test used** (per variable):

- **SMD**: `smd::smd(x = variable, g = group)` on the complete-pair rows.
  Handles continuous, binary, and multi-category factors (Yang & Dalton
  Mahalanobis formulation). Displayed to 3 decimal places. `NA` when there
  are no complete pairs.
- **N pairs**: integer count of complete pairs for that variable.

## Dependencies

- `smd` added to DESCRIPTION `Imports` (core to the default output),
  installed from the Posit 2025-03-31 snapshot per `~/.Rprofile`.
- Everything else already in Imports (gtsummary, dplyr, tidyr, rlang).

## Error handling summary

| Condition | Behavior |
|---|---|
| group has ≠ 2 observed levels | error |
| duplicate pair member within a group level | error naming pair IDs |
| rows with missing group | dropped with message |
| pair with only one member | in descriptives; out of tests/SMD |
| variable missing within a pair | that pair out of that variable's test/SMD |
| zero complete pairs / degenerate test | NA p-value / NA SMD, no error |

## Testing

`dev-tests/test_table_one_paired.R`, following the existing dev-test style
(plain Rscript, PASS/FAIL lines, nonzero exit on failure):

1. Paired t / Wilcoxon signed-rank / McNemar / Bowker p-values match direct
   `stats::` calls on manually constructed complete pairs.
2. SMD values match direct `smd::smd()` calls on the same complete pairs.
3. N-pairs column correct with deliberately incomplete pairs (member missing,
   value missing).
4. Descriptive cells match `table_one()` output on the same data.
5. Error paths: 3-level group, duplicate pair member, all-missing group.
6. Both `continuous_stat` modes; `datadic` labels; `include` selection;
   toggling `add_smd`/`add_n_pairs`/`add_overall`/`add_p`.

## Out of scope

- Grouping variables with 3+ levels (explicitly dropped during design).
- Model-based p-values (mixed models, conditional logistic regression).
- Weighted/frequency-matched designs (m:n matching).
