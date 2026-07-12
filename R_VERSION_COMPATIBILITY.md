# R version compatibility verification — fanetc 1.0.0 (2026-07-11)

Two independent verification passes (Claude locally + an external Codex run)
of the `fanetc_1.0.0` tarball under R 4.4.2, R 4.3.0, and R 4.0.3, using the
pinned Posit snapshot `https://packagemanager.posit.co/cran/2025-03-31` for
all dependency installs. Method: `R CMD build` + `_R_CHECK_FORCE_SUGGESTS_=false
R CMD check --no-manual --no-vignettes`, each R version with its own freshly
built package library.

## Verdict summary

| R version | Verdict | Tests (in-check) | Notes |
|-----------|---------|------------------|-------|
| 4.4.2 | **PASS** (verified twice, independently) | 200 pass / 0 fail / 4 skip | 0E / 0W / 1 environmental NOTE (openxlsx not installed) |
| 4.3.0 | **PASS only with survival >= 3.7-0** | with bundled survival 3.5-5: 6 FAIL; with snapshot survival 3.8-3: 200 pass / 0 fail / 4 skip | see below |
| 4.0.3 | **IMPOSSIBLE — cannot install** | n/a | gtsummary 2.1.0 and cardx 0.2.3 both require R >= 4.2 |

The in-check baseline is 200/0/4, not the 257/0/1 seen with `devtools::test()`
from a source checkout: inside `R CMD check` 3 extra tests correctly skip
themselves (2 need git commit `7ab169b`, 1 needs a source checkout — a tarball
has neither).

## R 4.3.0: the survival version landmine

All 6 failures are in `tests/testthat/test-extract_atrisk.R`: R 4.3.0 bundles
the recommended package **survival 3.5-5**, whose `summary.survfit()` returns
`n.risk` in a different shape, so the `matrix(..., dimnames = list(time.list,
strata_lab))` reshape inside `extract_atrisk()` dies with
"length of 'dimnames' [2] not equal to array extent". (`add_atrisk()`,
`show_surv()`, `show_cif()` all sit on top of `extract_atrisk()`.)

Exact minimum found by installing every intermediate survival release from
source under R 4.3.0 and running the extract_atrisk tests:

| survival | CRAN date | extract_atrisk tests |
|----------|-----------|----------------------|
| 3.5-5 (bundled with R 4.3.0) | 2023-03-12 | 6 failed / 2 passed |
| 3.5-7 | 2023-08-14 | 6 failed / 2 passed |
| 3.5-8 | 2024-02-14 | 6 failed / 2 passed |
| 3.6-4 | 2024-04-24 | 6 failed / 2 passed |
| **3.7-0** | 2024-06-05 | **0 failed / 13 passed** |
| 3.8-3 (current snapshot) | 2024-12-17 | 0 failed / 13 passed |

**The floor is exactly survival >= 3.7-0.** The only other 4.3.0-vs-4.4.2
difference is one benign testthat warning: `stats::fisher.test()`'s
"'hybrid' is ignored for a 2 x 2 table" surfacing through
`gtsummary::add_p()` — R 4.4 simply stopped emitting it.

### Which Posit snapshot should R 4.3.0 users pin?

**The one already pinned — `2025-03-31` — is sufficient. No change needed.**
Verified empirically: that snapshot serves survival **3.8-3** for R 4.3.0 as

- a macOS arm64 binary (`bin/macosx/big-sur-arm64/contrib/4.3/survival_3.8-3.tgz`,
  installed and tested here), and
- the source tarball (`src/contrib/survival_3.8-3.tar.gz`, built and tested here).

The only requirement is that R 4.3.0 users actually `install.packages("survival")`
from the snapshot (into their user library) instead of relying on the copy
bundled with R. If a different snapshot is ever needed, any date on or after
**2024-06-06** (the day after survival 3.7-0 reached CRAN) contains a
sufficient survival; `2025-03-31` is the closest-to-current choice and is
what the project already uses via `~/.Rprofile`.

### Would installing survival from source work for R 4.3.0 users?

**Yes — demonstrated, not assumed.** survival is C-only (no Fortran), so a
source install needs just a C toolchain, not gfortran. On this machine (no
gfortran installed) all five candidate versions above, including 3.8-3 pulled
from the pinned snapshot's own `src/contrib`, compiled cleanly under R 4.3.0
with Xcode's clang and passed the tests. Practical requirements per platform:

- macOS: Xcode Command Line Tools (`xcode-select --install`); gfortran NOT needed.
- Linux (incl. Databricks): gcc, normally already present; P3M also serves
  prebuilt Linux binaries for R 4.3 so source builds are rarely necessary.
- Windows: Rtools.

Install into the user library (the default for non-admin users); it shadows
the bundled 3.5-5. Upgrading survival in place does not break base R.

## R 4.0.3: cannot be installed

The package's own code is fine on old R — no native pipe `|>` or lambda
`\(...)` anywhere in `R/` (two independent scans). But at the pinned snapshot
the dependency stack makes installation impossible:

| Package | Snapshot version | Declared minimum R |
|---------|------------------|--------------------|
| gtsummary (direct Import) | 2.1.0 | **>= 4.2** |
| cardx (direct Import) | 0.2.3 | **>= 4.2** |
| cards (recursive, via both) | 0.5.1 | >= 4.1 |

Both analyses (mine via `available.packages()` against the snapshot, Codex's
via installed-package metadata) independently reached the same verdict.
`install.packages()` on R 4.0.3 would refuse gtsummary/cardx outright.

## DESCRIPTION changes (applied 2026-07-11)

The original `Depends: R (>= 3.5.0)` was demonstrably false. DESCRIPTION now
declares the verified floors:

```
Depends:
    R (>= 4.2)
Imports:
    ...
    survival (>= 3.7-0),
    ...
```

Without the survival floor, an R 4.3.x user who never upgrades the bundled
survival gets runtime breakage in `extract_atrisk()`/`add_atrisk()`/
`show_surv()`/`show_cif()` that `install.packages("fanetc")` would not warn
about; with the floor, installation pulls a new-enough survival automatically.

## Reproduction notes

- R 4.4.2: system R (arm64). R 4.3.0: rig-installed framework at
  `/Library/Frameworks/R.framework/Versions/4.3-arm64`.
- Fresh 4.3.0 library: all 23 Imports/Suggests installed from the snapshot as
  macOS arm64 binaries with zero failures.
- Codex's independent run reproduced the 4.4.2 result exactly and the 4.0.3
  metadata verdict; its 4.3.0 leg was blocked by its sandbox's network policy
  (could not reach packagemanager.posit.co), so the 4.3.0 empirical results
  come from the local run only.
