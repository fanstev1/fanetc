# Hand-off note — 2026-07-06

Branch: `fanetc_dev` (GitHub: fanstev1/fanetc). Everything is committed and pushed;
working tree clean. Git identity is configured globally now (Chun-Po Steve Fan
<chunpo.fan@gmail.com>).

## Commit history of this effort

| Commit | What |
|---|---|
| `7c7b475` | table_one() rewritten around gtsummary 2.1.0, backward compatible with the old API; old desp_table functions removed |
| `7c6a1b8` | construct_surv_var/construct_cmprisk_var vectorized in R/construct_event_time.R (165x/6x faster, equivalence-tested vs last-good `7ab169b`); admin_censor_*/summarize_km/cif moved verbatim to R/admin_censor.R / R/summarize_survfit.R; extract_atrisk() wide-format regression fixed |
| `3a56475` | show_surv/show_cif dedup via R/event_time_helpers.R (~190 lines removed); add_atrisk() rebuilt with line-based spacing; show_cif linewidth fix |

## Current behavior worth knowing

- **table_one()** (R/desp_table_gtsummary.R): gtsummary-based; needs **gtsummary,
  cardx, broom >= 1.0.5** at runtime — confirm these exist on Databricks.
- **construct_cmprisk_var()**: old `cmprisk_varname` argument still accepted alongside
  `varname`. Two old bugs deliberately fixed: censored subjects no longer get evt=NA
  when there are zero competing events, and all-dates-missing subjects no longer crash.
- **add_atrisk()** (shared by show_surv and show_cif): at-risk table positions are in
  **text lines below the panel bottom**, not data coordinates. Defaults
  (svglite-measured, default 11pt theme): with >1 group the "At-risk N:" header starts
  on the line right after the x-axis title (offset 2.23); with one group it sits
  exactly one line of whitespace below the title (offset 3.06); rows 1.2 lines apart;
  labels end 2 chars left of the first time point. `atrisk_init_pos` (in lines)
  overrides the default; the "exact" spacing is calibrated to the default theme —
  bigger axis text shifts it.
- Regression suites in `dev-tests/` (Rbuildignored):
  ```
  Rscript dev-tests/test_construct_equiv.R    # construct_* vs old reference + benchmarks
  Rscript dev-tests/test_extract_atrisk.R     # at-risk shape/counts/labels/panel ranges
  Rscript dev-tests/test_show_refactor.R      # 26-check show_surv/show_cif baseline
  Rscript dev-tests/test_table_one.R          # table_one 9 cases
  Rscript dev-tests/test_backward_compat.R    # table_one old-API compatibility
  ```
  Local packages install from the Posit 2025-03-31 snapshot per ~/.Rprofile.
  For spacing work: render with svglite and read the <text> x/y coordinates
  (see dev-tests/test_show_refactor.R and the session notes) instead of eyeballing.

## Folder cleanup + backward-compat audit (2026-07-06, uncommitted at time of writing)

- Deleted R/event_time_desp_revised.R (unexported *_revised duplicates, obsolete now
  that the production functions are fixed) and R/to_be_delete.R (scratch; top-level
  code broke R CMD build). R/ is now 7 files, ~2000 lines.
- Stripped ~200 lines of dead commented-out code (old inline MI blocks in
  fan_util_fun.R, abandoned alternatives in event_time_desp.R).
- **Backward-compat audit** (`dev-tests/test_api_compat.R`): all 27 NAMESPACE exports
  are defined and every argument name from the last-good commit `7ab169b` is still
  accepted, with positional order preserved. Fixes made during the audit:
  construct_cmprisk_var restored the old positional layout
  `(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname, append, ..., varname)`
  (the `..., varname` layout came from the corrupted commits nobody could have run);
  add_atrisk warns and falls back to the default when given an old-style negative
  data-coordinate `atrisk_init_pos`.

## Docs + DESCRIPTION overhaul (2026-07-06)

- Docs regenerated with roxygen2 7.3.3; man/ now matches the code (stale Rd gone,
  fanetc-package.Rd added). Eight exports still have @export but no title/desc, so
  no Rd is generated for them (show_surv, add_atrisk, prepare_survfit,
  run_logrank_test, run_gray_test, summarize_mi_*, generate_mi_glm_termplot_df).
- DESCRIPTION rewritten: License is now **MIT + file LICENSE** (chosen as the
  conventional default — change if another license is preferred); unused deps
  dropped (Hmisc, lubridate, cowplot, extrafont, gridExtra, tidyverse, splines);
  everything real moved Depends -> Imports; broom (>= 1.0.5) + cardx declared in
  Imports because table_one()'s add_p needs them at runtime; mice/mitools/sandwich
  in Suggests (the MI helpers resolve them via require() + the user's search path,
  as they always have); svglite in Suggests.
- **Behavior change to know about:** `library(fanetc)` no longer attaches
  magrittr/tidyverse/ggplot2/survival/etc. Scripts that relied on that side effect
  must library() those packages themselves. Package internals are covered by
  namespace imports (R/fanetc-package.R) — verified by a clean-session smoke test
  of all 15 major code paths with nothing attached, plus all dev-tests/ suites.
- run_logrank_test() fix that came out of this: it re-evaluates the stored survfit
  call in the caller's frame, so `survdiff`/`Surv` are now namespace-qualified in
  the rewritten call (event_time_desp.R ~line 358).

## table_one_paired() — IMPLEMENTED (2026-07-08)

- New feature designed with the user: `table_one_paired(df, pair_id, group, ...)` for
  paired data (long format, pair-ID column, grouping variable with **exactly 2
  levels** — 3-level support was considered and explicitly dropped during design).
- **Full spec (authoritative):** `docs/superpowers/specs/2026-07-06-table-one-paired-design.md`.
  **Implementation plan (task-by-task, with the executable code that was
  actually built):** `docs/superpowers/plans/2026-07-08-table-one-paired-implementation.md`.
  Both went through multiple rounds of external review by Codex (via the
  codex:codex-rescue agent; needs `codex login`) — the design spec caught and
  fixed two critical integration flaws before any code was written; the
  implementation plan caught a real sign-convention bug in the SMD calculation
  (see below) before it shipped.
- Key decisions, all confirmed by the user:
  - Tests: paired t / Wilcoxon signed-rank per `continuous_stat`;
    `stats::mcnemar.test` on the k×k pair table (McNemar/Bowker) for categorical.
    Descriptives use all rows; tests/SMD use per-variable complete pairs; N-pairs
    column reports the count.
  - `pairing_method = c("repeated_measure", "matching")`: matching → marginal
    smd::smd (pooled variance, **negated** — see gotcha below); repeated_measure
    → Cohen's d_z (within-pair SD) for continuous, marginal SMD for categorical
    in both modes. Footnote states method + reference level.
  - `ref_group` argument: default first factor level / most-frequent for character
    (first-observed tie-break) / sorted-first for logical-numeric; sets first group
    column and SMD sign (non-reference minus reference).
  - SMD digits: decimalplaces(x) with floor of 1 for continuous; 1 decimal for
    categorical.
  - **Integration mechanism (as actually built — differs from an earlier spec
    draft):** gtsummary 2.1.0 has no `modify_column_order()` function (confirmed
    absent from its exports), so the originally-planned manual
    `modify_table_body()` merge couldn't work. The shipped mechanism uses
    `gtsummary::add_stat()`/`add_p()` with **closures that ignore their own
    `data` argument** and instead close over `table_one_paired()`'s own
    validated data frame (which does contain `pair_id`) — so `pair_id` never
    needs to ride inside the tbl_summary object at all. `add_stat()`'s default
    `location = "label"` places N pairs/SMD on label rows only for free, and
    calling `add_overall()` → `add_stat()` (N pairs) → `add_stat()` (SMD) →
    `add_p()` in that order produces the correct final column order with no
    separate reordering step. `table_one(add_p = FALSE)` still builds the
    descriptive table and still never receives the `pair_id` column.
  - Missing group rows and NA/empty pair IDs dropped with messages (before the
    duplicate-pair-member validation, which errors); `include` normalized
    against the original data with `pair_id` stripped and `group` always kept,
    erroring with a clear message (not gtsummary's cryptic one) if nothing
    real remains to summarize.
- New code lives in `R/desp_table_paired.R` (repo convention); tests in
  `dev-tests/test_table_one_paired.R`; `smd` package added to DESCRIPTION
  Imports (Posit 2025-03-31 snapshot).
- **Gotcha for anyone touching the SMD code:** `smd::smd()`'s own sign
  convention is *reference minus other*, the opposite of this design's
  *non-reference minus reference* — verified empirically (not just from its
  docs) during the plan's review. `.paired_make_smd_fn()` negates the raw
  `smd::smd()` result for exactly this reason; Cohen's d_z needs no such
  negation (already correctly signed by construction). If this negation is
  ever "cleaned up," every `pairing_method = "matching"` SMD and every
  categorical SMD will silently flip sign.
- Status: implementation complete, built via subagent-driven-development
  (fresh implementer + reviewer subagent pair per task, all 8 tasks approved).
  Code in `R/desp_table_paired.R`; tests in `dev-tests/test_table_one_paired.R`
  (84 passing checks, 1 skip for optional `flextable`); docs regenerated via
  roxygen2 (`NAMESPACE` exports `table_one_paired`, `man/table_one_paired.Rd`
  created); full dev-tests suite green and a clean-session install smoke test
  passes. Not yet pushed to origin — the final whole-branch code review is
  still pending.

## Next steps (priority order)

1. Root *.md files (REFACTORING_SUMMARY.md, MIGRATION_GUIDE.md, ...) contain
   unverified metrics (line counts, performance table) — trim or rewrite.
2. Longer term: convert dev-tests/ into a proper testthat suite.
3. Smaller review findings not yet addressed: show_surv silently resets user-supplied
   `y_lim` to c(0,1); `grepl("0", state)` in prepare_survfit would misclassify a state
   named "10"; show_cif @param docs are copy-paste errors (partially fixed).

## Gotchas

- `git show <commit>:R/fan_util_fun.R` does not parse for a3f7f70..7c7b475 (corrupted
  construct_surv_var); use `7ab169b` when pulling reference versions.
- The at-risk table renders outside the panel: figures need `plot.margin` with enough
  bottom/left room and the panel clip turned off (`ggplot_gtable` + `layout$clip`
  trick in the show_cif roxygen example). ~70px left margin suffices now.
