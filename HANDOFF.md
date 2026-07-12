# Hand-off note — v1.0 (2026-07-09)

`master` now contains the full merge of `fanetc_dev`: the `table_one()` gtsummary
rewrite, vectorized `construct_surv_var()`/`construct_cmprisk_var()`, the
`show_surv()`/`show_cif()` dedup, the new `table_one_paired()` function, a
`testthat` conversion of the old `dev-tests/` scripts, and a post-merge
verification pass that fixed 3 known bugs plus everything `R CMD check`
flagged as an ERROR. Not yet pushed to origin, not yet tagged — that's next.

**If you need the exact pre-merge code** (e.g. to reproduce an old analysis's
results byte-for-byte), it's preserved unchanged on branch `fanetc_legacy`
(commit `7ab169b`). See `REPRODUCING_LEGACY_RESULTS.md` at the repo root for
the full list of breaking changes and install instructions — most notably,
`library(fanetc)` no longer auto-attaches `tidyverse`/`Hmisc`/etc, and 8 old
`desp_table` helper functions were removed.

## Current behavior worth knowing

- **table_one()** (`R/desp_table_gtsummary.R`): gtsummary-based; needs
  **gtsummary, cardx, broom >= 1.0.5** at runtime.
- **table_one_paired()** (`R/desp_table_paired.R`): companion function for
  paired/matched long-format data (pair-ID column, grouping variable with
  exactly 2 observed levels). See "table_one_paired()" section below.
- **construct_cmprisk_var()**: old `cmprisk_varname` argument still accepted
  alongside `varname`. Two old bugs deliberately fixed relative to the
  pre-refactor version: censored subjects no longer get `evt=NA` when there
  are zero competing events, and all-dates-missing subjects no longer crash.
- **add_atrisk()** (shared by show_surv and show_cif): at-risk table
  positions are in **text lines below the panel bottom**, not data
  coordinates. Defaults (svglite-measured, default 11pt theme): with >1
  group the "At-risk N:" header starts on the line right after the x-axis
  title (offset 2.23); with one group it sits exactly one line of whitespace
  below the title (offset 3.06); rows 1.2 lines apart; labels end 2 chars
  left of the first time point. `atrisk_init_pos` (in lines) overrides the
  default; the "exact" spacing is calibrated to the default theme — bigger
  axis text shifts it.
- **show_surv()**: a user-supplied `y_lim` is now respected for survival
  curves (previously silently reset to `c(0,1)` — fixed post-merge).
- **prepare_survfit()**: the censored/reference state placeholder that
  `survival::survfit()` emits is literally `"(s0)"`, not `"0"` — matched
  exactly (not via a substring match) so a real competing-risk state named
  e.g. `"10"` is never misclassified as censored (fixed post-merge; the
  previous `grepl("0", state)` had this bug).
- **Test suite**: `tests/testthat/` (standard `devtools::test()` /
  `R CMD check` layout; `dev-tests/` no longer exists, fully converted).
  ```
  Rscript -e 'devtools::test()'
  ```
  257 checks, 1 skip (flextable not installed). Local packages install from
  the Posit 2025-03-31 snapshot per `~/.Rprofile`.
- **`R CMD check` status**: 0 ERRORs, 0 WARNINGs, 3 NOTEs (openxlsx not
  installed in this environment; the broom/cardx unused-Imports false
  positive; the NSE "no visible binding" NOTE — see "Known debt" below).
  The documentation-coverage WARNINGs were fixed post-v1.0.0. Run via:
  ```
  R CMD build . --no-build-vignettes
  _R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual --no-vignettes fanetc_0.1.0.tar.gz
  ```
  (`_R_CHECK_FORCE_SUGGESTS_=false` is needed because `mice`/`mitools`/
  `openxlsx`/`sandwich` aren't installed in every dev environment; they're
  optional `Suggests`.)

## table_one_paired()

- `table_one_paired(df, pair_id, group, pairing_method = c("repeated_measure",
  "matching"), ref_group = NULL, ...)` for paired data (long format,
  pair-ID column, grouping variable with **exactly 2 levels**).
- Design spec: `docs/superpowers/specs/2026-07-06-table-one-paired-design.md`.
  Implementation plan: `docs/superpowers/plans/2026-07-08-table-one-paired-implementation.md`.
  Both went through multiple rounds of external review by Codex.
- Tests: paired t-test / Wilcoxon signed-rank per `continuous_stat`;
  `stats::mcnemar.test()` on the k×k pair table (McNemar/Bowker) for
  categorical. Descriptives use all rows; tests/SMD use per-variable
  complete pairs; the N-pairs column reports the count.
- `pairing_method`: `"matching"` → marginal `smd::smd()` (pooled variance);
  `"repeated_measure"` → Cohen's d_z (within-pair SD) for continuous
  variables, marginal SMD for categorical variables in both modes. A
  footnote states the method and reference level.
- **Gotcha for anyone touching the SMD code:** `smd::smd()`'s sign
  convention depends on variable **type**, not level count. For numeric or
  logical `x` it returns a signed "reference minus other" estimate that
  flips sign with `gref` — the opposite of this design's "non-reference
  minus reference" convention, so `.paired_make_smd_fn()` negates it. For
  factor or character `x` of *any* level count (2, 3, or more), it instead
  returns the Yang–Dalton Mahalanobis distance, which is gref-invariant and
  always non-negative — negating that would produce a spurious negative
  "SMD," so those are left un-negated. The guard is
  `signed_estimate <- is_cont || is.logical(x_var)`. This went through two
  incorrect fix attempts (unconditional negation, then a level-count-based
  guard) before landing on the type-based rule — verified against live
  `smd::smd()` output for all six type/level combinations, independently,
  twice (once directly, once by an external Codex review running the same
  checks itself). If this is ever "simplified," every `pairing_method =
  "matching"` SMD and every categorical SMD will silently flip sign.
- Status: implementation complete (8 tasks via subagent-driven-development),
  internally reviewed (3 rounds, caught the SMD bug above), and externally
  reviewed by Codex twice — the first attempt couldn't execute R at all
  (read-only sandbox blocks R's temp directory), the second (with
  `--write`) independently ran `smd::smd()`, the paired tests, and the full
  test suite itself and found no blocking issues. 91 of the merged suite's
  234 checks cover this function specifically.

## Known debt (pre-existing, not introduced by this merge)

`R CMD check` still reports, unfixed:
- ~~8 exported functions with no roxygen docs; 9 documented functions
  missing `@param` entries~~ — **fixed post-v1.0.0**: every exported
  function now has a full roxygen block (written from the actual function
  bodies, not guessed), `man/` regenerated with roxygen2 7.3.3, and the
  "missing documentation entries" / "Rd \usage" WARNINGs are gone. Raw `%`
  characters in roxygen text (which comment out the rest of the line in the
  generated Rd) were escaped as `\%` — markdown mode is off, so `%` is NOT
  auto-escaped; keep using `\%` in new roxygen text.
- **NSE "no visible binding for global variable" NOTEs** across most of the
  older MI/survival-summary functions (`summarize_mi_coxph`,
  `summarize_mi_glm`, `summarize_coxph`, `summarize_km`, `summarize_cif`,
  `calculate_type3_mi`, `generate_mi_glm_termplot_df`, plus a few in the
  newer `prepare_survfit`/`show_cif`/`show_surv`/`table_one` themselves) —
  standard tidyverse-NSE pattern, fixable via `utils::globalVariables()` or
  `.data$` pronouns.
- ~~`summarize_mi_coxph`/`summarize_mi_glm` call `require(mitools)` /
  `require(sandwich)` directly inside package functions~~ — **fixed
  post-v1.0.0**: all MI helpers now use `::`-qualified calls plus a
  `check_mi_packages()` guard, and `tests/testthat/test-summarize_mi.R`
  pins down that they work with mice/mitools/sandwich detached. Root cause
  worth knowing: `getfit()` and `pool()` live in **mice**, not mitools, so
  the old `require(mitools)` never provided them — the functions only ever
  worked when the user happened to have `library(mice)` attached.
- `broom`/`cardx` are declared in `Imports` but never referenced via `::`
  in `fanetc`'s own code — they're genuinely needed at runtime (by
  `gtsummary::add_p()` internally), just not directly called, so this NOTE
  is a false positive rather than a real problem.

The MI functions have test coverage (`tests/testthat/test-summarize_mi.R`;
`mice`/`mitools`/`sandwich` are installed locally from the Posit snapshot)
and all exported functions are documented, so the only remaining debt is
the NSE NOTEs and the broom/cardx false positive. Worth cleaning up before
a stricter `R CMD check --as-cran` matters.

## Gotchas

- `git show <commit>:R/fan_util_fun.R` does not parse for `a3f7f70..7c7b475`
  (corrupted construct_surv_var in that range); use `7ab169b` (== the tip of
  `fanetc_legacy`) when pulling pre-refactor reference versions.
- The at-risk table renders outside the panel: figures need `plot.margin`
  with enough bottom/left room and the panel clip turned off
  (`ggplot_gtable` + `layout$clip` trick in the show_cif roxygen example).
  ~70px left margin suffices now.
- Two roxygen examples (`admin_censor_cmprisk`, `show_cif`) fetch a live URL
  and are wrapped in `\dontrun{}` for that reason — they're not exercised by
  `R CMD check` or by the test suite.
