# Design: table_one_paired() — descriptive tables for paired data

Date: 2026-07-06 (revised 2026-07-08 after two external review rounds — Codex)
Status: design finalized; next step is the implementation plan

## Purpose

A companion to `table_one()` for paired data of two kinds, selected by the
`pairing_method` argument:

- `"repeated_measure"`: the same subject measured under two conditions
  (before/after, two timepoints).
- `"matching"`: 1:1 matched cohorts / matched case–control.

The input data frame is in long format: one row per pair member, a pair-ID
column linking members, and a grouping variable with **exactly 2 levels**
identifying the member's arm/condition. The function produces the usual
descriptive table plus paired-appropriate p-values, standardized mean
differences (SMD), and the number of complete pairs each comparison used.

Support for 3-level matched sets was considered and deliberately dropped
(no classic test exists for multi-category variables in 3-level sets); the
group variable must have exactly 2 observed levels.

## API

New exported function in a **new file** `R/desp_table_paired.R`
(repo convention: new functions never go into existing files):

```r
table_one_paired(df, pair_id, group,
                 pairing_method = c("repeated_measure", "matching"),
                 ref_group = NULL,
                 datadic = NULL, var_name, var_desp, include,
                 missing = "ifany", missing_text = "(Missing)",
                 add_p = TRUE, add_smd = TRUE, add_n_pairs = TRUE,
                 add_overall = TRUE, sort_by_p = FALSE,
                 continuous_stat = c("meansd", "mediqr"),
                 pvalue_fun = format_pvalue)
```

- `pair_id` and `group` are unquoted column names (NSE), like `group` in
  `table_one()`. Both are required. `pair_id` may be character, factor, or
  numeric; `group` may be factor, character, logical, or numeric (character
  group is converted to factor internally — see Reference level).
- `pairing_method` selects the SMD estimand (see SMD section); the paired
  tests are identical under both methods.
- `ref_group` names the reference group level. Default (`NULL`): the first
  factor level if `group` is a factor; the most frequent level if `group` is
  character (ties broken by first observed value); the first level after
  default factor conversion (sorted order, e.g. `FALSE` / the smaller
  number) if logical or numeric. A supplied `ref_group` is matched against
  the group's *display levels* after factor conversion — so `ref_group = "0"`
  matches a numeric 0 group and `ref_group = "FALSE"` matches a logical
  group. The reference level is displayed as the first group column, is
  the reference for the SMD sign (SMD = non-reference minus reference), and
  is stated in the SMD footnote.
- Descriptive formatting, labels (`datadic`/`var_name`/`var_desp`),
  missing-value display (`missing`/`missing_text`), `continuous_stat`, and
  `pvalue_fun` behave identically to `table_one()`. Group handling and
  `include` preprocessing follow paired-specific rules (below) and are
  *not* identical.
- `include` is evaluated against the **original** data; `pair_id` is then
  removed from the selection (with a message if it was explicitly listed)
  and `group` is always retained, before delegating to `table_one()`.
- `sort_by_p` is ignored when `add_p = FALSE` (same as `table_one()`).
- `missing_group_exclude` is **intentionally not supported**: a row with a
  missing group value can never be paired, so such rows are always dropped
  (with a message).
- Returns a gtsummary `tbl_summary` object, further modifiable exactly like
  `table_one()` output.

## Input validation (before any summarizing)

Performed in this order:

1. Rows with a missing group value are dropped with a message (they cannot
   be paired).
2. Rows with a missing/empty pair ID (`NA` or `""`) are **dropped with a
   message** (they cannot be paired, and would otherwise pool into a false
   giant "pair"). This happens *before* the duplicate check.
3. `group` must then have exactly 2 observed levels — otherwise an
   informative error. Character group is converted to factor with the
   reference level first; a supplied `ref_group` must be one of the observed
   levels (error otherwise).
4. Each pair ID may contribute **at most one row per group level**.
   Duplicate members within a pair/level make the pairing ambiguous —
   error, naming the offending pair IDs.
5. A pair present in only one group level is allowed (kept for
   descriptives, excluded from tests/SMD).

## Column layout

| label | Overall | reference level | other level | N pairs | SMD | p-value |

`add_overall`, `add_n_pairs`, `add_smd`, `add_p` each toggle their column.
The final order is enforced explicitly with
`gtsummary::modify_column_order()` after all columns are assembled (the
assembly sequence alone does not guarantee it). N pairs, SMD, and p-value
appear on the **variable label row only**; level rows and missing rows of
categorical variables are blank in those columns.

## Descriptive columns

Built by **delegating to `table_one()`** with `add_p = FALSE`, passing the
data **without the pair-ID column** (so pair_id can never leak in as a
summary variable, regardless of its type). All of table_one's behavior —
decimal-place matching via `decimalplaces()`, `datadic` labels, missing-row
display, `continuous_stat` glue strings, factor-level dropping — is
inherited verbatim rather than duplicated. Descriptives use **all available
observations** in each group (incomplete pairs included).

## Paired statistics: integration mechanism

The paired p-values, SMDs, and N-pairs counts are computed **directly from
the validated input data frame** (which table_one_paired holds) and merged
into the gtsummary object with `gtsummary::modify_table_body()`. The merge
is keyed on `variable` **and `row_type == "label"`** (categorical variables
have multiple body rows per variable; level and missing rows are explicitly
left `NA`). The analysis variable set is taken from the label rows of the
table returned by `table_one()` — i.e., only variables that survived
table_one's preprocessing (which drops character and Date columns) get
paired stats. Headers via `modify_header()`, p-value formatting by
attaching `pvalue_fun` as the column's `fmt_fun`, footnotes via
`modify_footnote()`.

This deliberately avoids `add_p()`/`add_stat()` custom-function machinery:
those only see the data stored inside the tbl_summary object, which cannot
contain pair_id without either displaying it or patching
`tbl$inputs$data` (fragile row-alignment surgery). Computing outside and
merging by variable name has no such coupling. `sort_by_p = TRUE` then uses
`gtsummary::sort_p()`, which operates on the `p.value` table-body column as
usual.

## Paired p-values

Per variable, using complete pairs (both members present and non-missing
for that variable — pairwise-complete, so each variable uses all pairs
available to it):

| Variable type | `continuous_stat = "meansd"` | `continuous_stat = "mediqr"` |
|---|---|---|
| Continuous | Paired t-test (`stats::t.test(paired = TRUE)`) | Wilcoxon signed-rank (`stats::wilcox.test(paired = TRUE)`) |
| Categorical (logical or factor, any number of categories) | `stats::mcnemar.test()` on the k×k pair-level contingency table — McNemar for 2×2, McNemar–Bowker symmetry test for k×k | same |

The tests are the same under both `pairing_method` values.

Notes:
- The wide pivot orders the pair members (reference level, other level);
  two-sided p-values do not depend on this, but it fixes the direction of
  any reported statistic.
- The pair-level contingency table is built with the union of factor levels
  on both axes so it is always square.
- `stats::mcnemar.test()` default continuity correction applies to the 2×2
  case (base-R default behavior).
- Degenerate cases return `NA` (displayed as `"---"` by `format_pvalue`)
  instead of erroring: zero complete pairs, all-concordant tables (zero
  discordant pairs → NaN), constant variables, single complete pair.
  Test warnings (e.g., signed-rank ties, zero differences) are suppressed;
  only errors degrade to `NA`.
- The p-value column footnote is **compact, by method class** (e.g.,
  "Continuous: paired t-test; categorical: McNemar / McNemar–Bowker") — not
  a per-variable listing, which would sprawl on large tables. Per-variable
  annotation is used only where a degenerate `NA` needs explaining.
- P-values formatted with `pvalue_fun` (default `format_pvalue`),
  consistent with `table_one()`.

## SMD column

Computed on the **same complete pairs the test used** (per variable). The
estimand depends on `pairing_method`:

- `"matching"`: marginal between-group SMD via
  `smd::smd(x = variable, g = group)` — pooled/average-variance
  denominator, ignoring within-pair covariance. This is the conventional
  matched-cohort balance metric. Handles continuous, binary, and
  multi-category factors (Yang & Dalton Mahalanobis formulation).
- `"repeated_measure"`: for **continuous** variables, the within-pair
  standardized difference (Cohen's d_z): mean of within-pair differences
  (non-reference minus reference) divided by the SD of those differences.
  For **categorical** variables the marginal Yang–Dalton SMD is used in
  both methods (no standard within-pair SMD exists for categorical data).

Sign convention: non-reference minus reference in both methods.

A **footnote on the SMD column** states the method and the reference level,
e.g. "SMD = standardized mean difference, [level B] vs [level A (reference)],
computed on complete pairs; within-pair SD denominator for continuous
variables (repeated measures)" / "...pooled-variance (marginal) SMD
(matched design)". Under `"repeated_measure"` the footnote also notes that
categorical variables use the marginal SMD.

`NA` when there are no complete pairs (or, for d_z, when the difference SD
is zero or fewer than 2 complete pairs exist).

Displayed decimals follow the variable's own precision via
`decimalplaces()`: continuous variables use `decimalplaces(x)` with a floor
of 1 (so integer-valued variables, where `decimalplaces()` returns 0, still
show 1 decimal); categorical variables (logical/factor) always use 1
decimal. (Note: this floor is specific to the SMD column; table_one's
descriptive cells use `decimalplaces()` without a floor.)

## N pairs column

Integer count of complete pairs for that variable (both members present and
non-missing), on the variable label row.

## Dependencies

- `smd` added to DESCRIPTION `Imports` (core to the default output),
  installed from the Posit 2025-03-31 snapshot per `~/.Rprofile`.
- Everything else already in Imports (gtsummary, dplyr, tidyr, rlang).

## Error handling summary

| Condition | Behavior |
|---|---|
| group has ≠ 2 observed levels (after drops) | error |
| `ref_group` not an observed group level | error |
| duplicate pair member within a group level | error naming pair IDs |
| rows with missing group | dropped with message |
| rows with missing/empty pair ID | dropped with message (before duplicate check) |
| pair with only one member | in descriptives; out of tests/SMD |
| variable missing within a pair | that pair out of that variable's test/SMD |
| zero complete pairs / degenerate test | NA p-value / NA SMD, no error; warnings suppressed |

## Testing

`dev-tests/test_table_one_paired.R`, following the existing dev-test style
(plain Rscript, PASS/FAIL lines, nonzero exit on failure):

1. Paired t / Wilcoxon signed-rank / McNemar / Bowker p-values match direct
   `stats::` calls on manually constructed complete pairs.
2. SMD values match direct `smd::smd()` calls (matching) and hand-computed
   Cohen's d_z (repeated_measure, continuous) on the same complete pairs;
   categorical SMD identical under both pairing methods. Displayed decimals
   follow the decimalplaces() rule (integer-valued and categorical
   variables → 1 decimal, others → variable precision).
3. N-pairs column correct with deliberately incomplete pairs (member
   missing, value missing); descriptive Ns and N pairs not conflated when
   many rows are incomplete.
4. Descriptive cells match `table_one()` output on the same data.
5. Error paths: 3-level group, 1-level group, duplicate pair member,
   all-missing group, `ref_group` not an observed level.
6. pair_id types: character, factor, numeric — paired stats identical;
   pair_id never appears as a summary row.
7. group types: factor (incl. unused levels), character, logical, numeric;
   `ref_group` default (first factor level; most frequent for character)
   and explicit override; reference level shown as first group column; SMD
   sign flips when `ref_group` flips.
8. Missing/empty pair IDs dropped with message; duplicates among NA pair
   IDs do not trip the duplicate-pair error.
9. Categorical edge cases: level present in only one arm among complete
   pairs (square union-level table); all-concordant pairs; one observed
   category after complete-pair filtering; zero discordant pairs → NA.
10. Continuous edge cases: all within-pair differences zero (d_z → NA);
    exactly one and exactly two complete pairs.
11. Row placement: N pairs/SMD/p-value on label rows only, blank on level
    and missing rows (with `missing = "ifany"` and `"always"`).
12. Both `continuous_stat` modes; `datadic` labels (including datadic
    entries for pair_id/group not leaking in); `include` selection
    (with and without pair_id/group accidentally listed); toggling
    `add_smd`/`add_n_pairs`/`add_overall`/`add_p`; `sort_by_p = TRUE`
    combined with disabled columns.
13. Footnotes: SMD footnote states method + reference level and differs
    between pairing methods; p-value footnote uses the compact
    method-class format and stays readable on a large table.
14. Include normalization: `include = c(pair_id, group, x)` and
    `include = c(pair_id)` — documented behavior, pair_id never reaches
    `table_one()`.
15. `ref_group` defaults and matching: tied-frequency character group
    (first-observed tie-break); numeric group with `ref_group = "0"`;
    logical group with `ref_group = "FALSE"`.
16. Analysis variable set: original data containing character and Date
    columns — paired stats computed only for variables appearing as label
    rows in the final table.
17. Post-merge gtsummary compatibility: `sort_p()`, `as_tibble()`, and
    `as_flex_table()` all work on the returned object (validates the
    `modify_table_body()` integration).

## Out of scope

- Grouping variables with 3+ levels (explicitly dropped during design).
- Model-based p-values (mixed models, conditional logistic regression).
- Weighted/frequency-matched designs (m:n matching).
- A within-pair SMD for categorical variables (no standard estimand).
