# Reproducing results from before fanetc v1.0

`main` (formerly `master`) now points at the merged `fanetc_dev` work and will be
tagged `v1.0.0`. The exact code that existed on `main` immediately before that
merge is preserved, unchanged, on the branch **`fanetc_legacy`** (commit
`7ab169b9f42db72c205d9e0a9554429132d34d8c`). If a script or analysis was run
against `fanetc` before this merge, use `fanetc_legacy` to reproduce it exactly
— do not assume the new `main` will give identical output, even where the
function signature looks the same. See "What actually changed" below.

## How to install the legacy version

**From GitHub** (works once `fanetc_legacy` is pushed to origin — it is
currently local-only; ask for it to be pushed, or use the local-checkout
method below in the meantime):

```r
remotes::install_github("fanstev1/fanetc", ref = "fanetc_legacy")
# or pin to the exact commit, which never moves even if the branch is later updated:
remotes::install_github("fanstev1/fanetc", ref = "7ab169b9f42db72c205d9e0a9554429132d34d8c")
```

**From a local clone** (works right now, no push required):

```r
# from a shell, in your local clone of the repo:
# git checkout fanetc_legacy
remotes::install_local("/Users/sfan/Documents/projects/fanetc", force = TRUE)
```

Or without touching your working copy's checked-out branch:

```r
remotes::install_git(
  "file:///Users/sfan/Documents/projects/fanetc",
  ref = "fanetc_legacy"
)
```

**Package repo:** install from `https://packagemanager.posit.co/cran/2025-03-31`
per `~/.Rprofile`, same as always — this hasn't changed.

## What actually changed between `fanetc_legacy` and v1.0 `main`

Verified directly against the diff (`git diff master fanetc_dev`), not from
notes or memory.

1. **`library(fanetc)` no longer attaches other packages.** `fanetc_legacy`'s
   `DESCRIPTION` has `Depends: Hmisc, magrittr, tidyverse, rlang, forcats,
   lubridate, reshape2, survival, cmprsk, broom, grid, gridExtra, viridis,
   cowplot, extrafont` — so `library(fanetc)` used to also attach all of
   those (and everything `tidyverse` itself attaches: dplyr, ggplot2, tidyr,
   purrr, stringr, readr, tibble...). v1.0 `main` has **no `Depends`**, only
   `Imports`, and drops several of those packages entirely (Hmisc, lubridate,
   cowplot, extrafont, gridExtra, tidyverse itself, splines). **Any old
   script that calls `dplyr::filter()`, `%>%`, `ggplot()`, etc. without its
   own explicit `library()` call — relying on `fanetc` to have attached it —
   will fail on v1.0 `main` with "could not find function" errors.** This is
   the single most likely cause of an old script breaking.

2. **8 functions were removed**: `factor_desp()`, `factor_dist()`,
   `fisher_test()`, `k_sample_test()`, `logical_desp()`, `numeric_desp()`,
   `recode_missing()`, `two_sample_test()`. The first five and the last were
   publicly exported in `fanetc_legacy`; `factor_dist()`, `fisher_test()`,
   and `recode_missing()` were internal (unexported) but still callable via
   `fanetc:::factor_dist()` etc. if any old script did that. These were the
   old low-level building blocks of `table_one()`, superseded by a
   `gtsummary`-based rewrite. If any script called these directly (rather
   than only through `table_one()`), it will error on v1.0 `main`.

3. **`table_one()` was rewritten around `gtsummary` 2.1.0.** The argument
   names were kept backward-compatible, but the rewrite changes the
   underlying engine entirely — the **returned object and its printed/knit
   output format differ** (now a `gtsummary` table object rather than
   whatever the old implementation returned). Any downstream code that
   post-processed `table_one()`'s return value (rather than just knitting
   it) should be re-checked against `fanetc_legacy`'s output, not assumed
   compatible.

4. **`construct_surv_var()` / `construct_cmprisk_var()` were vectorized and
   two edge-case bugs were fixed**: censored subjects no longer get `evt =
   NA` when there are zero competing-risk events, and subjects with all
   dates missing no longer crash the function. If an old analysis's results
   depended on either of those two prior (buggy) behaviors — e.g. a censored
   subject with zero competing events being silently coded `NA` — v1.0
   `main` will compute a different answer for those specific rows.

5. **At-risk table positioning in `show_surv()`/`show_cif()` was rebuilt**
   (line-based spacing instead of the old positioning logic). Plots from
   v1.0 `main` will not pixel-match old plots even for identical data;
   re-render from `fanetc_legacy` if an exact figure needs to be reproduced.

6. **License field**: `fanetc_legacy` has the placeholder `License: What
   license is it under?`; v1.0 `main` has `MIT + file LICENSE`. Not a
   behavior change, but a licensing terms change worth knowing about if this
   package is redistributed.

## What did *not* change (safe to assume compatible)

- `add_atrisk()`'s newer positional convention added a fallback: an
  old-style negative data-coordinate `atrisk_init_pos` still works on v1.0
  `main`, with a warning, by falling back to the default.
- `construct_cmprisk_var()` kept its original positional argument order
  `(df, patid, idx_dt, evt_dt, end_dt, cmprisk_varname, append, ..., varname)`
  — old positional calls still work.
- All other exported function names, signatures, and (as far as tested)
  output shapes are unchanged — see `dev-tests/test_api_compat.R` and
  `dev-tests/test_backward_compat.R` in the repo for the specific
  compatibility checks that were run.

## Recommendation

If you have an existing analysis script or R Markdown report that must
produce byte-identical results to a previous run, pin its environment (e.g.
`renv.lock`, or a comment noting the commit) to `fanetc_legacy` at
`7ab169b9f42db72c205d9e0a9554429132d34d8c` rather than tracking `main` going
forward. New analyses should use v1.0 `main`.
