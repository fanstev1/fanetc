# Hand-off note — updated 2026-07-19 (geom_ribbon_step)

## Maintainability refactor (2026-07-19)

- All `R/` files received a behavior-preserving maintainability refactor following
  `docs/superpowers/plans/2026-07-19-maintainability-refactor.md`; exported APIs
  remain unchanged.
- The `lazyeval` and `reshape2` dependencies were removed.
- Two sanctioned crash-to-works improvements are included: `add_atrisk()` no
  longer errors when its theme text settings are `NULL`, and
  `admin_censor_cmprisk()` no longer crashes for every non-`NULL`
  `adm_cnr_time` (v1.0.0's broken braced pipe produced "`:=` can only be used
  within dynamic dots").
- `decimalplaces()`'s rewrite is more robust at its edges, discovered during
  the branch's final whole-branch review: (1) an all-scientific-notation
  fractional input, which previously returned a zero-length `integer(0)` via
  out-of-bounds indexing, now returns `0L`; (2) non-finite input (`Inf`,
  `-Inf`, `NaN`), which previously errored ("missing value where TRUE/FALSE
  needed"), now returns `0L`; (3) a non-integer `max_dec` (e.g. `2.9`), which
  previously could return a fractional decimal-place count via `pmin.int()`,
  is now coerced with `as.integer()` first. No call site in this package ever
  exercises any of these three inputs (`numeric(0)`, the only edge that
  already returned `0L` pre-rewrite, is unaffected). All three are pinned in
  `tests/testthat/test-decimalplaces.R`.
- The new `show_cif_shapes_baseline.rds`, `atrisk_baseline.rds`, and
  `summarize_baseline.rds` fixtures live under `tests/testthat/fixtures/` with
  their respective `make_*_baseline.R` scripts. Regenerate them only from the
  pre-refactor code identified by those scripts, never from current code.

## Latest: geom_ribbon_step() — step-function CI ribbons (2026-07-19)

Merged to `master` and pushed to origin (`0c67296..3131279`, merge commit
`3131279`; 4 feature commits + spec/plan docs).

- **What**: `geom_ribbon_step()` (`R/geom_ribbon_step.R`) is an exported
  drop-in for `ggplot2::geom_ribbon()` that draws the ribbon as a step
  function — `direction = "hv"` (default, right-continuous; what survival/ROC
  curves need), `"vh"`, or `"mid"`, mirroring `geom_step()`. Internals: pure
  helper `stairstep_ribbon()` + ggproto `StatStepRibbon`; only the wrapper is
  exported.
- **Behavior contract worth knowing**: data are sorted by x within each group
  before stepping (input order never matters). NA `ymin`/`ymax` rows reach
  `GeomRibbon` un-dropped and warning-free, drawing as gaps — this works
  because `StatStepRibbon` declares only `"x"` in `required_aes` and checks
  ymin/ymax itself in `setup_data()`. Do NOT "fix" `required_aes` to
  `c("x","ymin","ymax")`: the default `Stat$compute_layer()` would then strip
  NA rows with a warning and break geom_ribbon() parity. Deliberate limits
  (documented in the Rd): no orientation/flipped-aes support, and ymin/ymax
  must be aesthetics, not constant layer params.
- **Refactor**: `prepare_survfit()$plot_ci_d` now holds **raw** per-time CI
  limits (`time, conf_low, conf_high`, sorted); the manual pre-stepping that
  used to live there is gone, and `show_surv()`/`show_cif()` draw CI bands
  with `geom_ribbon_step()`. This is a documented behavior change of the
  exported `prepare_survfit()` (roxygen + `fanetc-cheatsheet-main.md`
  updated).
- **Tests**: `test-geom_ribbon_step.R` (unit: all three directions vs the
  original index logic, sort invariance, per-group stepping, NA gaps, 0/1-row
  lifecycle) and `test-show_ci_ribbon.R` (equivalence: ribbons match the old
  manual stepping for KM survival, KM failure/`plot_cdf`, and CIF, plus an
  exact `tolerance = 0` comparison against
  `tests/testthat/fixtures/ribbon_step_baseline.rds`).
- **Fixture rule**: `ribbon_step_baseline.rds` was captured by running the
  actual PRE-refactor master code in a temp worktree. If it ever needs
  regeneration, do it only from pre-refactor code (`0c67296` or earlier),
  never from current code — regenerating from current code would silently
  reintroduce the circular-oracle problem the fixture exists to prevent.
- **Process artifacts**: spec
  `docs/superpowers/specs/2026-07-18-geom-ribbon-step-design.md`, plan
  `docs/superpowers/plans/2026-07-18-geom-ribbon-step.md`, execution ledger
  `.superpowers/sdd/progress.md`. Implemented task-by-task by Codex
  (`gpt-5.6-sol`); dual independent whole-branch reviews (Claude + Codex)
  both ended "Ready to merge: Yes" after one fix round (Codex caught the
  circular test oracle).
- **Codex environment gotchas** (verified this session): on this machine's
  ChatGPT-account auth the only accepted top model id is `gpt-5.6-sol`
  (`gpt-5.x-codex` ids are rejected), and the Codex write sandbox cannot
  touch `.git` — have Codex implement/test and put the intended commit in its
  report; the coordinator commits.

# Prior hand-off — v1.0 (2026-07-09)

`master` contains the full merge of `fanetc_dev`: the `table_one()` gtsummary
rewrite, vectorized `construct_surv_var()`/`construct_cmprisk_var()`, the
`show_surv()`/`show_cif()` dedup, the new `table_one_paired()` function, a
`testthat` conversion of the old `dev-tests/` scripts, and a post-merge
verification pass that fixed 3 known bugs plus everything `R CMD check`
flagged as an ERROR. v1.0.0 was released 2026-07-11 and pushed to origin
(tag sits at `81aa601`, which also added the verified R-version floors —
see `R_VERSION_COMPATIBILITY.md`).

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
  328 checks, 1 skip (flextable not installed) as of 2026-07-19. Local
  packages install from the Posit 2025-03-31 snapshot per `~/.Rprofile`.
- **`R CMD check` status**: 0 ERRORs, 0 WARNINGs, 1 NOTE (openxlsx not
  installed in this environment — purely environmental). The
  documentation-coverage WARNINGs and the NSE/unused-Imports NOTEs were
  all fixed post-v1.0.0 (see "Resolved debt" below). Run via:
  ```
  R CMD build . --no-build-vignettes
  _R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual --no-vignettes fanetc_1.0.0.tar.gz
  ```
  (`_R_CHECK_FORCE_SUGGESTS_=false` is needed because `mice`/`mitools`/
  `openxlsx`/`sandwich` aren't installed in every dev environment; they're
  optional `Suggests`.)
- **R version requirements**: DESCRIPTION declares `R (>= 4.2)` and
  `survival (>= 3.7-0)` — both empirically verified floors, not guesses
  (gtsummary/cardx require R >= 4.2; survival 3.6-4 and older break
  `extract_atrisk()` on R 4.3.x). Full evidence and the per-R-version
  verification results are in `R_VERSION_COMPATIBILITY.md` at the repo root.

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

## Resolved debt (post-v1.0.0 cleanup, 2026-07-11)

The whole pre-existing debt list from the v1.0 merge is now cleared —
`R CMD check` is 0 ERRORs / 0 WARNINGs / 1 environmental NOTE:
- **MI-helper `require()` landmine** — all MI helpers use `::`-qualified
  calls behind a `check_mi_packages()` guard, and
  `tests/testthat/test-summarize_mi.R` pins down that they work with
  mice/mitools/sandwich detached. Root cause worth knowing: `getfit()` and
  `pool()` live in **mice**, not mitools, so the old `require(mitools)`
  never provided them — the functions only ever worked when the user
  happened to have `library(mice)` attached.
- **Documentation coverage** — every exported function has a full roxygen
  block (written from the actual function bodies, not guessed); the
  "missing documentation entries" / "Rd \usage" WARNINGs are gone. Raw `%`
  characters in roxygen text comment out the rest of the line in the
  generated Rd — markdown mode is off, so `%` is NOT auto-escaped; keep
  writing `\%` in new roxygen text.
- **NSE "no visible binding" NOTEs** — genuine function calls got
  `@importFrom stats ...` in `R/fanetc-package.R`; pipeline column names
  (plus the magrittr dot and `%$%` element names) are declared via
  `utils::globalVariables()` in the same file. New pipeline column names
  that R CMD check flags should be appended to that vector.
- **broom/cardx "unused Imports" NOTE** — they are needed at runtime by
  `gtsummary::add_p()` but never called directly; the never-invoked
  `ignore_unused_imports()` in `R/fanetc-package.R` references their
  namespaces to keep the check quiet. Don't delete it, and don't move
  broom/cardx to Suggests.

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
