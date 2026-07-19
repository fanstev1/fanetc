# fanetc Maintainability Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement every accepted finding from the 2026-07-19 dual (Claude + Codex, both independent) maintainability review: simplify, streamline and make transparent all 10 files under `R/`, with byte-identical behavior.

**Architecture:** Pure refactor — no exported API changes, no new features. Every task follows pin-then-change: characterization tests or fixtures are captured from the *pre-task* code first (and shown green), then the refactor is applied, then the same tests must still pass. The existing 328-test suite plus the master-captured ribbon baseline (`tolerance = 0`) is the standing oracle for all plotting code.

**Tech Stack:** R (>= 4.2), devtools/testthat (edition 3), rlang tidy-eval, ggplot2, gtsummary. Packages install from the Posit snapshot 2025-03-31 (already configured in `~/.Rprofile`); no new dependencies — two are removed (`lazyeval`, `reshape2`).

## Global Constraints

- **Branch:** create `refactor/maintainability-review` from `master` before Task 1: `git checkout master && git pull && git checkout -b refactor/maintainability-review`.
- **Behavior preservation is absolute.** All existing tests must pass unmodified. `tests/testthat/fixtures/ribbon_step_baseline.rds` must NEVER be regenerated (it is captured from pre-geom_ribbon_step master code).
- **Exported signatures are frozen** — `tests/testthat/test-api_compat.R` pins them. Do not add/remove/reorder exported formals. In particular: `show_cif()` keeps its unused `plot_cdf` formal; `table_one()`'s `set.seed(0)` side effect stays.
- **New fixtures** introduced by this plan (Tasks 2, 5, 8) are captured from the code as it stands at the START of the task (pre-refactor), via a committed generation script under `tests/testthat/fixtures/`. Each script gets a header comment: `# Regenerate ONLY from code at or before the commit that introduced this fixture.`
- **Style:** match each file's surrounding style (`foo<- function`, `arg= value` spacing in legacy files; tidyverse spacing in newer files like `desp_table_paired.R`). Roxygen markdown is OFF for this package: escape `%` as `\%` in roxygen text.
- **Test commands:** full suite `Rscript -e 'devtools::test()'` (expect 0 FAIL, 0 WARN; 1 pre-existing flextable SKIP is fine). Filtered: `Rscript -e 'devtools::test(filter = "<name>")'`. Tests attach packages via `helper-setthat/helper-setup.R` — do not add `library()` calls to test files.
- **Do not run `devtools::document()`** except where a task says to (Tasks 7, 8 and 13). If it rewrites `man/fanetc-package.Rd` as a side effect, restore that file with `git checkout -- man/fanetc-package.Rd` unless the task changed package-level roxygen.
- **Messages/warnings/errors keep their exact current wording** unless a task explicitly says otherwise (tests and users may match on them).
- Commit after every task with the message given in the task.

## Explicitly Out of Scope (reviewed, deliberately skipped)

- Removing `show_cif()`'s dead `plot_cdf` formal (API-frozen).
- Removing/redesigning `table_one()`'s `set.seed(0)` (documented, deliberate).
- Merging the `summarize_mi_glm()`/`summarize_mi_coxph()` pooling pipelines beyond the two small shared helpers in Task 11 (their pooling outputs genuinely differ; Codex rated the full merge medium-risk for little gain).
- Package-wide `::`-qualification pass and removal of `ignore_unused_imports()` (standard documented idiom; churn without payoff).
- "Fixing" `summarize_cif()`'s double rounding (`round(x, 3)*100` then `formatC(digits = 1)`) or its `(s0)`-placeholder handling — both are behavior, and behavior is frozen. Task 8 adds clarifying comments only.

---

### Task 1: `event_time_desp.R` mechanical cleanup + `show_surv()` streamlining

**Files:**
- Modify: `R/event_time_desp.R`
- Test: existing suite only (`test-show_refactor.R`, `test-show_ci_ribbon.R`, `test-event_time_bugfixes.R` cover every touched path; the ribbon baseline pins plot output at `tolerance = 0`)

**Interfaces:**
- Consumes: nothing from other tasks.
- Produces: the file state Tasks 2–4 edit further. No name changes.

- [ ] **Step 1: Delete dead commented-out code**

Delete these comment lines (content shown so they can be located after line drift; do NOT delete explanatory prose comments):

- `# state    = rep(surv_obj$state, each= length(surv_obj$time)),` (~line 114)
- `# if (class(out$strata) != "factor") out$strata <- factor(out$strata)` (~line 128)
- `# stemp <- rep(1, length(surv_obj$time)) # same length as stime` (~line 137)
- `# stemp <- rep(1:nstrat, surv_obj$strata) # same length as stime` (~line 141)
- `# dplyr::group_by(strata, state) %>%` (~line 160)
- `# dplyr::group_by(strata) %>%` (~line 180)
- `# x_lim= NULL,` (~line 445)
- `#  expand= c(0.01, 0.005),` (~line 507)
- `# pval<- format_pvalue(pval)` (~line 540)
- `# print(out, vp= viewport(width = unit(6.5, "inches"), height = unit(6.5, "inches")))` (~line 555)
- The 4-line block starting `# x_lab<- if (is.null(x_lab)) "Time" else x_lab` (~lines 761–764)
- `# limits = x_lim,` (~line 788) and `# limits= y_lim,` (~line 793)
- `# evt_label= identity, # identity function` (~line 713)

- [ ] **Step 2: Modernize idioms (behavior-identical)**

In `prepare_survfit()`:

```r
# line ~157: class check
out <- if (inherits(surv_obj, "survfitms")) {
# line ~129:
if (!is.factor(out$state)) out$state <- relevel(factor(out$state), ref = "0")
```

Replace all three `dplyr::select(one_of(...))` calls (lines ~167, ~196, ~226) with `all_of()`:

```r
dplyr::select(all_of(c("time", "prob"))) %>%
# and
dplyr::select(all_of(c("time", "conf_low", "conf_high"))) %>%
```

In `show_surv()`:

```r
# ~463-465
  add_pvalue <- if (!"strata" %in% names(surv_obj)) FALSE else add_pvalue
  add_legend <- if (!"strata" %in% names(surv_obj) || add_atrisk) FALSE else add_legend
# ~468
  if (color_scheme=='manual' && is.null(color_list)) stop("Please provide a list of color value(s).")
# ~518 (mutate_at -> across)
      plot_ci_d<- plot_ci_d %>%
        mutate(across(starts_with('conf'), function(x) 1-x)) %>%
        rename(conf_high= conf_low,
               conf_low = conf_high)
```

In `show_cif()`:

```r
# ~752-754
  add_pvalue<- if (nlevels(plot_prob_d$strata)==1) FALSE else add_pvalue
  add_legend<- if (nlevels(plot_prob_d$strata)==1 &&
                   nlevels(plot_prob_d$state) ==1) FALSE else add_legend
# ~757
  if (color_scheme=='manual' && is.null(color_list)) stop("Please provide a list of color value(s) when a manual color scheme is specified.")
# ~796
  out<- if (!is.null(x_lim) || !is.null(y_lim)) out + coord_cartesian(xlim= x_lim, ylim = y_lim, clip = "on") else out
```

(Leave the three-way `if` plotting branches alone — Task 2 replaces them wholesale. Do not convert their `&`/`>` conditions here.)

- [ ] **Step 3: Collapse `show_surv()`'s four-way y_lim branch and remove the dead NULL check**

Replace the whole `if (!plot_cdf & !is.null(y_lim)) { ... } else if ... { ... }` block (~lines 472–484) with:

```r
  curve_name<- if (plot_cdf) "failure" else "survival"
  if (is.null(y_lim)) {
    y_lim<- c(0, 1)
    message("The parameter y_lim was set to y_lim= c(0, 1) for ", curve_name, " function.")
  } else {
    y_lim<- c(0, max(y_lim, na.rm= TRUE))
    message("The lower limit of y-axis was reset to 0 for ", curve_name, " function.")
  }
```

(The concatenated message text is byte-identical to the four originals.)

Replace the y-truncation block (~494–498) — the `group_by()` fed straight into ggplot data and did nothing:

```r
  if (y_lim[2] < 1) {
    plot_prob_d <- dplyr::mutate(plot_prob_d, prob = pmin(prob, y_lim[2], na.rm = TRUE))
  }
```

Replace line ~535 (`y_lim` can no longer be NULL here — the else branch was dead):

```r
  out <- out + coord_cartesian(ylim = y_lim, clip = "on")
```

- [ ] **Step 4: Run the full suite**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 FAIL, 0 WARN (the three ribbon-baseline assertions in `test-show_ci_ribbon.R` are the critical gate).

- [ ] **Step 5: Commit**

```bash
git add R/event_time_desp.R
git commit -m "refactor: modernize event_time_desp idioms; collapse show_surv y_lim branches"
```

---

### Task 2: Collapse `show_cif()`'s three parallel plotting branches

**Files:**
- Create: `tests/testthat/fixtures/make_show_cif_shapes_baseline.R` (generation script)
- Create: `tests/testthat/fixtures/show_cif_shapes_baseline.rds` (run the script BEFORE refactoring)
- Create: `tests/testthat/test-show_cif_shapes.R`
- Modify: `R/event_time_desp.R` (`show_cif()` only)

**Interfaces:**
- Consumes: Task 1's file state.
- Produces: `show_cif()` with a single `series_col` mapping; later tasks don't depend on it.

- [ ] **Step 1: Write the fixture generation script**

`tests/testthat/fixtures/make_show_cif_shapes_baseline.R`:

```r
# Captures show_cif() layer data for the three strata/state display shapes.
# Regenerate ONLY from code at or before the commit that introduced this
# fixture (i.e. BEFORE the show_cif() branch-collapse refactor).
# Run from the package root: Rscript tests/testthat/fixtures/make_show_cif_shapes_baseline.R
devtools::load_all(".")
library(ggplot2)

set.seed(42)
lung_cr<- survival::lung
lung_cr$status2<- factor(replace(lung_cr$status - 1, sample(nrow(lung_cr), 60), 2))

fit_overall<- estimate_cif(lung_cr, time, status2)             # 1 stratum
fit_grouped<- estimate_cif(lung_cr, time, status2, group= sex) # 2 strata

step_layer_data<- function(p) {
  ld<- lapply(seq_along(p$layers), function(i)
    if (inherits(p$layers[[i]]$geom, "GeomStep")) layer_data(p, i))
  ld[!vapply(ld, is.null, logical(1))][[1]]
}
ribbon_layer_data<- function(p) {
  ld<- lapply(seq_along(p$layers), function(i)
    if (inherits(p$layers[[i]]$geom, "GeomRibbon")) layer_data(p, i))
  ld[!vapply(ld, is.null, logical(1))][[1]]
}
capture<- function(p) {
  s<- step_layer_data(p); r<- ribbon_layer_data(p)
  list(step  = data.frame(x= s$x, y= s$y, group= s$group, colour= s$colour),
       ribbon= data.frame(x= r$x, ymin= r$ymin, ymax= r$ymax, group= r$group, fill= r$fill))
}

common<- function(fit, evt) suppressMessages(
  show_cif(fit, evt_type = evt, add_ci = TRUE, add_atrisk = FALSE,
           add_pvalue = FALSE, add_legend = FALSE, print_fig = FALSE))

baseline<- list(
  one_stratum_multi_state = capture(common(fit_overall, evt= c(1, 2))),
  multi_strata_one_state  = capture(common(fit_grouped, evt= 1)),
  multi_strata_multi_state= capture(common(fit_grouped, evt= c(1, 2)))
)
saveRDS(baseline, "tests/testthat/fixtures/show_cif_shapes_baseline.rds", version = 2)
cat("Wrote show_cif_shapes_baseline.rds:", length(baseline), "shapes\n")
```

- [ ] **Step 2: Generate the fixture from the CURRENT (pre-refactor) code**

Run: `Rscript tests/testthat/fixtures/make_show_cif_shapes_baseline.R`
Expected: `Wrote show_cif_shapes_baseline.rds: 3 shapes`

- [ ] **Step 3: Write the pinning test**

`tests/testthat/test-show_cif_shapes.R`:

```r
# Pins show_cif() step + ribbon layer data for all three strata/state display
# shapes against a baseline captured before the branch-collapse refactor.

set.seed(42)
lung_cr<- survival::lung
lung_cr$status2<- factor(replace(lung_cr$status - 1, sample(nrow(lung_cr), 60), 2))
fit_overall<- estimate_cif(lung_cr, time, status2)
fit_grouped<- estimate_cif(lung_cr, time, status2, group= sex)

shapes_baseline<- readRDS(testthat::test_path("fixtures", "show_cif_shapes_baseline.rds"))

layer_by_geom<- function(p, geom_class) {
  ld<- lapply(seq_along(p$layers), function(i)
    if (inherits(p$layers[[i]]$geom, geom_class)) layer_data(p, i))
  ld[!vapply(ld, is.null, logical(1))][[1]]
}
capture<- function(p) {
  s<- layer_by_geom(p, "GeomStep"); r<- layer_by_geom(p, "GeomRibbon")
  list(step  = data.frame(x= s$x, y= s$y, group= s$group, colour= s$colour),
       ribbon= data.frame(x= r$x, ymin= r$ymin, ymax= r$ymax, group= r$group, fill= r$fill))
}
cif_plot<- function(fit, evt) suppressMessages(
  show_cif(fit, evt_type = evt, add_ci = TRUE, add_atrisk = FALSE,
           add_pvalue = FALSE, add_legend = FALSE, print_fig = FALSE))

test_that("show_cif() layer data matches the pre-refactor baseline for all three shapes", {
  expect_equal(capture(cif_plot(fit_overall, c(1, 2))),
               shapes_baseline$one_stratum_multi_state, tolerance = 0)
  expect_equal(capture(cif_plot(fit_grouped, 1)),
               shapes_baseline$multi_strata_one_state, tolerance = 0)
  expect_equal(capture(cif_plot(fit_grouped, c(1, 2))),
               shapes_baseline$multi_strata_multi_state, tolerance = 0)
})
```

- [ ] **Step 4: Verify the pinning test is green on the UNrefactored code**

Run: `Rscript -e 'devtools::test(filter = "show_cif_shapes")'`
Expected: PASS (3 assertions). If it fails, fix the test/fixture — do not proceed.

- [ ] **Step 5: Commit the pin**

```bash
git add tests/testthat/fixtures/make_show_cif_shapes_baseline.R tests/testthat/fixtures/show_cif_shapes_baseline.rds tests/testthat/test-show_cif_shapes.R
git commit -m "test: pin show_cif layer data for all three strata/state shapes"
```

- [ ] **Step 6: Refactor `show_cif()`**

Replace the three-branch `geom_step` block (`out<- ggplot()` through the closing `}` of the third branch, ~766–782) with:

```r
  # one display series drives group/colour/fill in both layers; which column
  # supplies it depends on how many strata and states are being shown
  series_col<- if (nlevels(plot_prob_d$strata)==1 && nlevels(plot_prob_d$state)>1) {
    "state_label"
  } else if (nlevels(plot_prob_d$strata)>1 && nlevels(plot_prob_d$state)==1) {
    "strata"
  } else {
    "state_strata"
  }

  out<- ggplot() +
    geom_step(data= plot_prob_d,
              aes(x= time, y= prob,
                  group= .data[[series_col]], color= .data[[series_col]]),
              linewidth= 1.1, show.legend = add_legend)
```

Replace the three-branch ribbon block inside `if (add_ci) { ... }` (~798–843) with:

```r
  if (add_ci) {
    plot_ci_d<- cmprisk_mat %>%
      dplyr::select(strata, state, state_label, state_strata, plot_ci_d) %>%
      unnest(cols = c(plot_ci_d))

    out<- out +
      geom_ribbon_step(data= plot_ci_d,
                  aes(x= time, ymin= conf_low, ymax= conf_high,
                      group= .data[[series_col]], fill= .data[[series_col]]),
                  alpha= .2, show.legend = FALSE) +
      scale_pair$fill
  }
```

Keep everything between the two blocks (scales, coord) exactly where it is: the ribbon layer must still be added AFTER `scale_x/scale_y/coord`, as today.

- [ ] **Step 7: Run the gates**

Run: `Rscript -e 'devtools::test(filter = "show_cif_shapes")'` — Expected: PASS, `tolerance = 0`.
Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN (includes the master ribbon baseline).

- [ ] **Step 8: Commit**

```bash
git add R/event_time_desp.R
git commit -m "refactor: collapse show_cif's three plotting branches into one series mapping"
```

---

### Task 3: Decompose `prepare_survfit()` into file-level helpers

**Files:**
- Modify: `R/event_time_desp.R` (`prepare_survfit()` region only)
- Test: existing suite (`test-show_ci_ribbon.R` baseline + `test-show_refactor.R` pin the nested output exactly)

**Interfaces:**
- Consumes: Task 1/2 file state.
- Produces: internal (non-exported) helpers `.survfit_strata(surv_obj, times_per_state = 1L)`, `.prepare_km_data(surv_obj)`, `.prepare_multistate_data(surv_obj)`. `prepare_survfit(surv_obj)` keeps its exported signature and its exact return value.

- [ ] **Step 1: Add the helpers above `prepare_survfit()` and shrink it to a dispatcher**

Replace everything from `prepare_survfit <- function(surv_obj) {` down to its closing `return(out)` `}` with:

```r
# strata factor for the tidy rows of a survfit/survfitms object. For
# multi-state fits the per-stratum block repeats once per state
# (times_per_state = length(surv_obj$states)); plain survival fits use 1.
.survfit_strata<- function(surv_obj, times_per_state= 1L) {
  if (is.null(surv_obj$strata)) {
    factor(rep(1, length(surv_obj$time) * times_per_state), 1, labels = "Overall")
  } else {
    strata_lab <- sapply(strsplit(names(surv_obj$strata), "="), function(x) x[2])
    factor(rep(rep(seq_along(surv_obj$strata), surv_obj$strata), times_per_state),
           seq_along(surv_obj$strata), strata_lab)
  }
}

# tidy per-time rows of a survfitms (competing risks) object
.prepare_multistate_data<- function(surv_obj) {
  out <- dplyr::tibble(
    strata = .survfit_strata(surv_obj, times_per_state = length(surv_obj$states)),
    state = rep(
      # survival::survfit() labels the pre-event/reference state "(s0)" (or, on some
      # versions, "") regardless of the original event factor's level names -- match
      # that literal placeholder, not any state whose name merely contains "0"
      # (e.g. a real competing-risk state named "10" must not be swept in here).
      replace(surv_obj$state, nchar(surv_obj$state) == 0 | surv_obj$state == "(s0)", "0"),
      each = length(surv_obj$time)
    ),
    time = rep(surv_obj$time, length(surv_obj$state)),
    prob = as.numeric(surv_obj$pstate),
    conf_low = as.numeric(surv_obj$lower),
    conf_high = as.numeric(surv_obj$upper)
  )
  if (!is.factor(out$state)) out$state <- relevel(factor(out$state), ref = "0")
  out
}

# tidy per-time rows of a plain survfit (Kaplan-Meier) object
.prepare_km_data<- function(surv_obj) {
  dplyr::tibble(
    strata = .survfit_strata(surv_obj),
    time = surv_obj$time,
    prob = surv_obj$surv,
    conf_low = surv_obj$lower,
    conf_high = surv_obj$upper,
    n_risk = surv_obj$n.risk, # immediately before time t
    n_event = surv_obj$n.event,
    n_censor = surv_obj$n.censor
  )
}
```

then keep the existing roxygen block and rebuild `prepare_survfit()` as:

```r
prepare_survfit <- function(surv_obj) {
  out <- if (inherits(surv_obj, "survfitms")) {
    surv_obj %>%
      .prepare_multistate_data() %>%
      tidyr::nest(.by = c(strata, state)) %>%
      dplyr::mutate(
        plot_prob_d = purrr::map2(
          state, data,
          function(state, df) {
            df %>%
              dplyr::select(all_of(c("time", "prob"))) %>%
              dplyr::bind_rows(
                dplyr::tribble(
                  ~time, ~prob,
                  0, as.numeric(state == "0")
                )
              ) %>%
              dplyr::arrange(time, if (state != "0") prob else desc(prob))
          }
        )
      )
  } else {
    surv_obj %>%
      .prepare_km_data() %>%
      tidyr::nest(.by = strata) %>%
      dplyr::mutate(
        data = purrr::map(
          data,
          function(df) {
            remove <- duplicated(df$prob)
            if (remove[length(remove)]) remove[length(remove)] <- FALSE
            df[!remove, ]
          }
        ),
        plot_prob_d = purrr::map(
          data,
          function(df) {
            df <- df %>%
              dplyr::select(all_of(c("time", "prob")))

            df <- if (is.na(match(0, df$time))) {
              # if time 0 is not included in the estimated time-prob (i.e., no
              # events occur at time 0), then add time = 0 and prob = 1
              df %>%
                dplyr::bind_rows(
                  dplyr::tribble(
                    ~time, ~prob,
                    0, 1
                  )
                )
            } else {
              df
            }

            dplyr::arrange(df, time)
          }
        )
      )
  }

  out %>%
    dplyr::mutate(
      plot_ci_d = purrr::map(
        data,
        function(df) {
          # raw per-time CI limits; geom_ribbon_step() steps them at plot time
          df %>%
            dplyr::select(all_of(c("time", "conf_low", "conf_high"))) %>%
            dplyr::arrange(time)
        }
      )
    )
}
```

Equivalence notes for the implementer: the old `rep(1:nstrat, surv_obj$strata)` and `mapply`-free construction are reproduced exactly by `.survfit_strata()`; `nstrat` was only ever used to build the factor and is gone.

- [ ] **Step 2: Run the gates**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 FAIL, 0 WARN — `test-show_refactor.R` compares `prepare_survfit()` output structurally and `test-show_ci_ribbon.R`/`test-show_cif_shapes.R` pin the downstream plots at `tolerance = 0`.

- [ ] **Step 3: Commit**

```bash
git add R/event_time_desp.R
git commit -m "refactor: lift prepare_survfit strata/data helpers to file level"
```

---

### Task 4: Shared survfit call builder for `estimate_km()` / `estimate_cif()`

**Files:**
- Modify: `R/event_time_desp.R` (`estimate_km()`, `estimate_cif()`)
- Create: `tests/testthat/test-estimate_call.R`

**Interfaces:**
- Consumes: Task 1–3 file state.
- Produces: internal helper `.survfit_call(df, evt_time, evt, group, extra_args = list())` returning an unevaluated `survfit(...)` call with the data frame embedded. `run_logrank_test()`/`run_gray_test()` continue to consume `fit$call` unchanged.

- [ ] **Step 1: Write the pinning test (against CURRENT code)**

`tests/testthat/test-estimate_call.R`:

```r
# Pins the stored-call contract that run_logrank_test()/run_gray_test() rely
# on: unqualified survfit/Surv symbols, the formula as written, the data frame
# embedded literally, and extra arguments carried through.

test_that("estimate_km() stores the historical call shape", {
  fit <- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
  expect_identical(deparse(fit$call[[1]]), "survfit")
  expect_identical(deparse(fit$call[[2]]), "Surv(time, status) ~ sex")
  expect_true(is.data.frame(fit$call$data))
  expect_identical(fit$call$conf.type, "log-log")

  fit0 <- estimate_km(survival::lung, evt_time = time, evt = status)
  expect_identical(deparse(fit0$call[[2]]), "Surv(time, status) ~ 1")

  fit_ci <- estimate_km(survival::lung, evt_time = time, evt = status, ci_transformation = "plain")
  expect_identical(fit_ci$call$conf.type, "plain")
})

test_that("estimate_cif() stores the historical call shape", {
  set.seed(42)
  lung_cr <- survival::lung
  lung_cr$status2 <- factor(replace(lung_cr$status - 1, sample(nrow(lung_cr), 60), 2))
  fit <- estimate_cif(lung_cr, time, status2, group = sex)
  expect_identical(deparse(fit$call[[1]]), "survfit")
  expect_identical(deparse(fit$call[[2]]), "Surv(time, status2) ~ sex")
  expect_true(is.data.frame(fit$call$data))
})

test_that("the stored call re-evaluates to the same fit (run_*_test contract)", {
  fit <- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
  refit <- eval(fit$call)
  expect_equal(refit$surv, fit$surv)
  expect_silent(pv <- run_logrank_test(fit))
  expect_true(is.numeric(pv) && length(pv) == 1)
})
```

- [ ] **Step 2: Verify green on the UNrefactored code**

Run: `Rscript -e 'devtools::test(filter = "estimate_call")'`
Expected: PASS. (If `deparse(fit$call[[2]])` differs, adjust the expectation to the actual current value — the point is pinning current behavior.) Commit:

```bash
git add tests/testthat/test-estimate_call.R
git commit -m "test: pin the survfit call shape stored by estimate_km/estimate_cif"
```

- [ ] **Step 3: Refactor**

Add above `estimate_km()`:

```r
# Build a survfit() call with the data frame embedded literally, so downstream
# helpers (run_logrank_test(), run_gray_test()) can re-evaluate fit$call. The
# stored call keeps unqualified survfit/Surv symbols, exactly as the historical
# versions produced via substitute().
.survfit_call<- function(df, evt_time, evt, group, extra_args= list()) {
  rhs<- if (rlang::quo_is_missing(group)) 1 else rlang::quo_get_expr(group)
  rlang::expr(
    survfit(Surv(!!rlang::quo_get_expr(evt_time), !!rlang::quo_get_expr(evt)) ~ !!rhs,
            data = !!df,
            !!!extra_args)
  )
}
```

Replace the body of `estimate_km()` (keep signature and roxygen):

```r
estimate_km<- function(df, evt_time, evt, group, ci_transformation = "log-log", ...) {
  cl<- .survfit_call(
    df, rlang::enquo(evt_time), rlang::enquo(evt), rlang::enquo(group),
    extra_args = c(list(conf.type = ci_transformation),
                   lapply(rlang::enquos(...), rlang::quo_get_expr))
  )
  eval(cl)
}
```

Replace the body of `estimate_cif()` (keep signature and roxygen):

```r
estimate_cif<- function(df, evt_time, evt, group, ...) {
  cl<- .survfit_call(
    df, rlang::enquo(evt_time), rlang::enquo(evt), rlang::enquo(group),
    extra_args = lapply(rlang::enquos(...), rlang::quo_get_expr)
  )
  eval(cl)
}
```

- [ ] **Step 4: Run the gates**

Run: `Rscript -e 'devtools::test(filter = "estimate_call")'` — Expected: PASS.
Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN (log-rank/Gray's-test paths and all plot baselines exercise the stored call end to end).

- [ ] **Step 5: Commit**

```bash
git add R/event_time_desp.R
git commit -m "refactor: share one survfit call builder between estimate_km and estimate_cif"
```

---

### Task 5: `extract_atrisk()` / `add_atrisk()` — unify strata paths, fix theme fallback

**Files:**
- Create: `tests/testthat/fixtures/make_atrisk_baseline.R` + `tests/testthat/fixtures/atrisk_baseline.rds` (captured BEFORE refactor)
- Modify: `tests/testthat/test-extract_atrisk.R` (append tests)
- Modify: `R/event_time_desp.R` (`extract_atrisk()`, `add_atrisk()`)

**Interfaces:**
- Consumes: Task 1–4 file state; `estimate_km()` from Task 4.
- Produces: same exported signatures; `add_atrisk()` additionally tolerates `plot_theme` objects whose `$text` element is NULL (previously a length-zero-condition error — this is the plan's one deliberate behavior improvement, on an input that previously crashed).

- [ ] **Step 1: Fixture generation script**

`tests/testthat/fixtures/make_atrisk_baseline.R`:

```r
# Captures extract_atrisk() output (stratified and overall). Regenerate ONLY
# from code at or before the commit that introduced this fixture.
# Run from the package root: Rscript tests/testthat/fixtures/make_atrisk_baseline.R
devtools::load_all(".")
fit_grp<- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
fit_all<- estimate_km(survival::lung, evt_time = time, evt = status)
baseline<- list(
  strata_default = extract_atrisk(fit_grp),
  strata_times   = extract_atrisk(fit_grp, time.list = c(100, 300, 500)),
  overall_default= extract_atrisk(fit_all),
  overall_scaled = extract_atrisk(fit_all, time.list = 0:2, time.scale = 365.25)
)
saveRDS(baseline, "tests/testthat/fixtures/atrisk_baseline.rds", version = 2)
cat("Wrote atrisk_baseline.rds\n")
```

- [ ] **Step 2: Generate from CURRENT code**

Run: `Rscript tests/testthat/fixtures/make_atrisk_baseline.R` — Expected: `Wrote atrisk_baseline.rds`

- [ ] **Step 3: Append pinning + bug-fix tests to `tests/testthat/test-extract_atrisk.R`**

```r
test_that("extract_atrisk() matches the pre-refactor baseline exactly", {
  atrisk_baseline<- readRDS(testthat::test_path("fixtures", "atrisk_baseline.rds"))
  fit_grp<- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
  fit_all<- estimate_km(survival::lung, evt_time = time, evt = status)
  expect_equal(extract_atrisk(fit_grp), atrisk_baseline$strata_default, tolerance = 0)
  expect_equal(extract_atrisk(fit_grp, time.list = c(100, 300, 500)),
               atrisk_baseline$strata_times, tolerance = 0)
  expect_equal(extract_atrisk(fit_all), atrisk_baseline$overall_default, tolerance = 0)
  expect_equal(extract_atrisk(fit_all, time.list = 0:2, time.scale = 365.25),
               atrisk_baseline$overall_scaled, tolerance = 0)
})

test_that("add_atrisk() falls back to font defaults when plot_theme has no text element", {
  fit<- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
  p<- suppressMessages(show_surv(fit, add_ci = FALSE, add_atrisk = FALSE,
                                 add_pvalue = FALSE, print_fig = FALSE))
  expect_s3_class(add_atrisk(p, fit, plot_theme = theme()), "ggplot")
})
```

- [ ] **Step 4: Run — expect a SPLIT result**

Run: `Rscript -e 'devtools::test(filter = "extract_atrisk")'`
Expected: the baseline test PASSES (pin is green pre-refactor); the `theme()` test FAILS with `argument is of length zero` — that is the bug Task 5 fixes (red before green). Commit the pin:

```bash
git add tests/testthat/fixtures/make_atrisk_baseline.R tests/testthat/fixtures/atrisk_baseline.rds tests/testthat/test-extract_atrisk.R
git commit -m "test: pin extract_atrisk output; add failing test for NULL-text theme in add_atrisk"
```

- [ ] **Step 5: Refactor `extract_atrisk()`**

Replace the whole function body (keep roxygen and signature):

```r
extract_atrisk <- function(fit, time.list = NULL, time.scale = 1) {
  if (is.null(time.list)) {
    time.list <- pretty(c(fit$t0, max(fit$time / time.scale)))
  }
  time.list <- sort(c(fit$t0, time.list[time.list > fit$t0 & time.list <= max(fit$time / time.scale)]))

  has_strata <- "strata" %in% names(fit)
  strata_lab <- if (has_strata) {
    sapply(strsplit(names(fit$strata), "="), function(x) x[2])
  } else {
    "Overall"
  }

  x <- data.frame(
    time = fit$time / time.scale,
    n.risk = if (is.matrix(fit$n.risk)) rowSums(fit$n.risk) else fit$n.risk,
    strata = if (has_strata) {
      factor(rep(seq_along(fit$strata), fit$strata), seq_along(fit$strata), labels = strata_lab)
    } else {
      factor(rep(1, length(fit$time)), 1, labels = "Overall")
    }
  )

  # right-continuous piecewise-constant at-risk process per stratum
  # Steve note: have to fix `rule` for data with delayed entries:
  # approxfun(x= time, y= n.risk, method= "constant", rule = 1:1, f= 1)
  atRiskPts <- vapply(
    split(x, x$strata),
    function(df) {
      ff <- approxfun(x = df$time, y = df$n.risk, method = "constant", rule = 2:1, f = 1)
      as.integer(replace(ff(time.list), is.na(ff(time.list)), 0))
    },
    integer(length(time.list))
  )
  atRiskPts <- matrix(atRiskPts, nrow = length(time.list),
                      dimnames = list(NULL, strata_lab))

  # wide, plain data.frame (time + one integer column per stratum) -- the shape
  # add_atrisk() consumes
  data.frame(time = time.list, atRiskPts, check.names = FALSE, row.names = NULL)
}
```

(`rowSums` replaces `apply(., 1, sum)`; `rep(seq_along, ...)` replaces the `mapply(rep, ...)` construction; `vapply` over `split()` replaces `rapply` + manual matrix bookkeeping. To avoid calling `ff()` twice, the implementer may hoist `out <- ff(time.list)` — either form is fine.)

- [ ] **Step 6: Fix `add_atrisk()` font extraction and x_break handling**

Replace the font block:

```r
  # ---- get font information ----
  theme_text<- if (is.null(plot_theme)) NULL else plot_theme$text
  font_family<- if (is.null(theme_text$family) || trimws(theme_text$family) == "") "Arial" else theme_text$family
  font_face  <- if (is.null(theme_text$face)   || trimws(theme_text$face) == "") "plain" else theme_text$face
  font_size  <- if (is.null(theme_text$size)) 11 else theme_text$size
```

(`||` short-circuits, so a NULL `family`/`face`/whole-`text` element now falls back instead of erroring — the vectorized `|` produced a length-zero `if` condition.)

Replace the x_break block:

```r
  # I need to calculate the number of at-risk at the x_break
  x_range<- layer_scales(p)$x$range$range
  x_break<- if (is.null(x_break)) {
    layer_scales(p)$x$get_breaks(x_range)
  } else {
    x_break[x_break >= min(x_range) & x_break <= max(x_range)]
  }
  x_break<- x_break[!is.na(x_break)]
```

- [ ] **Step 7: Run the gates**

Run: `Rscript -e 'devtools::test(filter = "extract_atrisk")'` — Expected: ALL pass now (including the theme test).
Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN.

- [ ] **Step 8: Commit**

```bash
git add R/event_time_desp.R
git commit -m "refactor: unify extract_atrisk strata paths; fix add_atrisk NULL-text theme fallback"
```

---

### Task 6: Explicit scale-function dispatch in `event_time_color_scales()`

**Files:**
- Modify: `R/event_time_helpers.R`
- Test: existing suite (palette assertions in `test-show_refactor.R` / ribbon baselines)

**Interfaces:**
- Consumes: nothing new.
- Produces: same function signature and return; `viridis::` calls become explicit (Task 13 drops the now-unneeded imports).

- [ ] **Step 1: Replace `event_time_color_scales()`**

```r
# colour/fill scale pair for a color scheme. show_surv() uses grey_end = 0.75 with no
# guide argument; show_cif() uses grey_end = 0.65 and passes guide_legend(title = "")
# positionally, as its original inline code did. The "manual" scheme takes its
# arguments verbatim from color_list and (as always) ignores the extra guide.
event_time_color_scales <- function(color_scheme, color_list, grey_end = 0.75, blank_guide_title = FALSE) {
  extra <- if (blank_guide_title) list(guide_legend(title = "")) else list()
  fns <- switch(color_scheme,
    "brewer"  = list(colour = scale_color_brewer, fill = scale_fill_brewer,
                     args = c(list(palette = "Set1"), extra)),
    "grey"    = list(colour = scale_color_grey, fill = scale_fill_grey,
                     args = c(list(start = 0, end = grey_end), extra)),
    "viridis" = list(colour = viridis::scale_color_viridis, fill = viridis::scale_fill_viridis,
                     args = c(list(option = "viridis", begin = .2, end = .85, discrete = TRUE), extra)),
    "manual"  = list(colour = scale_color_manual, fill = scale_fill_manual,
                     args = color_list)
  )
  list(colour = do.call(fns$colour, fns$args), fill = do.call(fns$fill, fns$args))
}
```

(Preserves the original quirk exactly: `manual` never receives `extra`. `do.call` remains only to splice the argument list — the functions themselves are now named, visible to readers and to `R CMD check`.)

- [ ] **Step 2: Run the full suite**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 FAIL, 0 WARN (colour/fill hex values in the plot baselines are the gate).

- [ ] **Step 3: Commit**

```bash
git add R/event_time_helpers.R
git commit -m "refactor: dispatch color scales via explicit function objects"
```

---

### Task 7: `admin_censor.R` — dedupe, drop `lazyeval`, fix docs

**Files:**
- Create: `tests/testthat/test-admin_censor.R`
- Modify: `R/admin_censor.R`
- Modify: `DESCRIPTION` (remove `lazyeval` from Imports)
- Modify: `tests/testthat/helper-setup.R` (drop the `library(lazyeval)` attach + its comment)
- Modify: `tests/testthat/fixtures/ref_construct.R` (`lazyeval::as_name` → `rlang::as_name`)
- Modify (generated): `man/admin_censor_cmprisk.Rd` via `devtools::document()`

**Interfaces:**
- Consumes: nothing new.
- Produces: same exported signatures/behavior; package no longer depends on lazyeval anywhere (code, tests, fixtures).

- [ ] **Step 1: Write characterization tests (against CURRENT code)**

`tests/testthat/test-admin_censor.R`:

```r
# Characterization tests written before the admin_censor dedupe refactor:
# they pin current behavior exactly.

test_that("admin_censor_surv() censors events after the cutoff into *_adm columns", {
  out<- admin_censor_surv(survival::aml, evt_time= time, evt= status, adm_cnr_time= 30)
  expect_equal(out$time_adm, replace(survival::aml$time, survival::aml$time > 30, 30))
  expect_equal(out$status_adm,
               replace(survival::aml$status,
                       survival::aml$time > 30 & survival::aml$status != 0, 0))
  expect_equal(out$time, survival::aml$time)     # originals untouched
  expect_equal(out$status, survival::aml$status)
})

test_that("admin_censor_surv() without a cutoff returns the data unchanged", {
  expect_identical(admin_censor_surv(survival::aml, evt_time= time, evt= status),
                   survival::aml)
})

test_that("admin_censor_surv(overwrite_var= TRUE) overwrites in place", {
  out<- admin_censor_surv(survival::aml, evt_time= time, evt= status,
                          adm_cnr_time= 30, overwrite_var= TRUE)
  expect_equal(out$time, replace(survival::aml$time, survival::aml$time > 30, 30))
  expect_false("time_adm" %in% names(out))
})

test_that("admin_censor_cmprisk() recodes post-cutoff events to censored", {
  cr<- data.frame(ftime= c(5, 15, 25, 40), fstatus= factor(c("1", "0", "2", "1")))
  out<- admin_censor_cmprisk(cr, ftime, fstatus, adm_cnr_time= 20)
  expect_equal(out$ftime_adm, c(5, 15, 20, 20))
  expect_equal(out$fstatus_adm, factor(c("1", "0", "0", "0"), levels= levels(cr$fstatus)))
})

test_that("admin_censor_cmprisk() applies evt_label as factor labels", {
  cr<- data.frame(ftime= c(5, 15, 25, 40), fstatus= factor(c("1", "0", "2", "1")))
  out<- admin_censor_cmprisk(cr, ftime, fstatus, adm_cnr_time= 20,
                             evt_label= c("0"= "Event free", "1"= "Event", "2"= "Competing event"))
  expect_equal(out$fstatus_adm,
               factor(c("Event", "Event free", "Event free", "Event free"),
                      levels= c("Event free", "Event", "Competing event")))
})

test_that("admin_censor_cmprisk() requires a censor time", {
  cr<- data.frame(ftime= 1, fstatus= factor("1"))
  expect_error(admin_censor_cmprisk(cr, ftime, fstatus), "No administrative censor time")
})
```

- [ ] **Step 2: Verify green on the UNrefactored code, commit the pin**

Run: `Rscript -e 'devtools::test(filter = "admin_censor")'` — Expected: PASS.

```bash
git add tests/testthat/test-admin_censor.R
git commit -m "test: characterize admin_censor_surv/admin_censor_cmprisk behavior"
```

- [ ] **Step 3: Refactor `R/admin_censor.R`**

Delete the boxed banner comment inside `admin_censor_surv()` (lines 25–32, `#####...` through `#####...` — it duplicates the roxygen). Replace both function bodies:

```r
# output column names: the originals when overwriting, otherwise "<name>_adm"
.admin_censor_names<- function(evt_time, evt, overwrite_var) {
  suffix<- if (overwrite_var) "" else "_adm"
  c(time= paste0(rlang::as_name(evt_time), suffix),
    evt = paste0(rlang::as_name(evt), suffix))
}

admin_censor_surv <- function(df, evt_time, evt, adm_cnr_time = NULL, overwrite_var = FALSE) {
  evt_time <- enquo(evt_time)
  evt <- enquo(evt)

  if (is.null(adm_cnr_time)) return(df)

  nm<- .admin_censor_names(evt_time, evt, overwrite_var)
  df %>%
    mutate(
      !!nm[["evt"]] := replace(!!evt, !!evt_time > adm_cnr_time & !!evt != 0, 0),
      !!nm[["time"]] := replace(!!evt_time, !!evt_time > adm_cnr_time, adm_cnr_time)
    )
}
```

```r
admin_censor_cmprisk <- function(df, evt_time, evt, adm_cnr_time = NULL, evt_label = NULL, overwrite_var = FALSE) {
  evt_time <- enquo(evt_time)
  evt <- enquo(evt)

  if (is.null(adm_cnr_time)) stop("No administrative censor time is given.")

  nm<- .admin_censor_names(evt_time, evt, overwrite_var)
  df %>%
    mutate(
      !!nm[["evt"]] := {
        censored<- replace(!!evt, !!evt_time > adm_cnr_time & !!evt != "0", "0")
        if (is.null(evt_label)) censored else factor(censored, names(evt_label), labels = evt_label)
      },
      !!nm[["time"]] := replace(!!evt_time, !!evt_time > adm_cnr_time, adm_cnr_time)
    )
}
```

The `.admin_censor_names()` helper goes at the top of the file, before the first roxygen block. Note the mutate assignment ORDER (event first, then time) is preserved — the event recode must see the UNcensored times.

- [ ] **Step 4: Fix the `evt_label` roxygen (copy-paste error) and document**

Replace the `@param evt_label` line in `admin_censor_cmprisk()`'s roxygen:

```r
#' @param evt_label an optional named vector mapping event codes (its names) to display labels; when given, the censored event variable is returned as a factor with these labels.
```

Run: `Rscript -e 'devtools::document()'` — regenerates `man/admin_censor_cmprisk.Rd`. If `man/fanetc-package.Rd` is touched, restore it: `git checkout -- man/fanetc-package.Rd`.

- [ ] **Step 5: Remove lazyeval everywhere**

- `DESCRIPTION`: delete the line `    lazyeval,` from Imports.
- `tests/testthat/helper-setup.R`: delete `library(lazyeval)` and the comment lines that explain the lazyeval/rlang masking order (they reference a conflict that no longer exists).
- `tests/testthat/fixtures/ref_construct.R` line ~93: change `lazyeval::as_name` → `rlang::as_name` (drop-in equivalent for quosures; the trailing comment stays).

- [ ] **Step 6: Run the gates**

Run: `Rscript -e 'devtools::test(filter = "admin_censor")'` — Expected: PASS.
Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN (`test-construct_equiv.R` exercises `ref_construct.R`).
Run: `grep -rn "lazyeval" R/ tests/ DESCRIPTION NAMESPACE` — Expected: no matches.

- [ ] **Step 7: Commit**

```bash
git add R/admin_censor.R DESCRIPTION tests/testthat/helper-setup.R tests/testthat/fixtures/ref_construct.R man/admin_censor_cmprisk.Rd
git commit -m "refactor: dedupe admin_censor naming; drop lazyeval dependency; fix evt_label docs"
```

---

### Task 8: `summarize_survfit.R` — unify KM/CIF pipelines, drop `reshape2`

**Files:**
- Create: `tests/testthat/fixtures/make_summarize_baseline.R` + `tests/testthat/fixtures/summarize_baseline.rds` (captured BEFORE refactor)
- Create: `tests/testthat/test-summarize_survfit.R`
- Modify: `R/summarize_survfit.R`
- Modify: `R/fanetc-package.R` (remove `@importFrom reshape2 melt dcast`)
- Modify: `DESCRIPTION` (remove `reshape2` from Imports)

**Interfaces:**
- Consumes: `estimate_km()`/`estimate_cif()` (Task 4).
- Produces: same exported signatures; `summarize_cif()` still returns a plain `data.frame` with the same column names and order (dcast contract), `summarize_km()` still returns a tibble.

- [ ] **Step 1: Fixture generation script**

`tests/testthat/fixtures/make_summarize_baseline.R`:

```r
# Captures summarize_km()/summarize_cif() output for stratified and overall
# fits. Regenerate ONLY from code at or before the commit that introduced this
# fixture (i.e. BEFORE the reshape2 -> tidyr refactor).
# Run from the package root: Rscript tests/testthat/fixtures/make_summarize_baseline.R
devtools::load_all(".")

fit_grp<- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
fit_all<- estimate_km(survival::lung, evt_time = time, evt = status)

set.seed(42)
lung_cr<- survival::lung
lung_cr$status2<- factor(replace(lung_cr$status - 1, sample(nrow(lung_cr), 60), 2))
cif_grp<- estimate_cif(lung_cr, time, status2, group = sex)
cif_all<- estimate_cif(lung_cr, time, status2)

baseline<- list(
  km_strata      = summarize_km(fit_grp),
  km_strata_times= summarize_km(fit_grp, times = c(0, 250, 500, 750)),
  km_overall     = summarize_km(fit_all),
  km_failure     = summarize_km(fit_grp, failure_fun = TRUE),
  cif_strata     = summarize_cif(cif_grp),
  cif_overall    = summarize_cif(cif_all),
  cif_times      = summarize_cif(cif_grp, times = c(0, 250, 500, 750))
)
saveRDS(baseline, "tests/testthat/fixtures/summarize_baseline.rds", version = 2)
cat("Wrote summarize_baseline.rds:", length(baseline), "tables\n")
```

- [ ] **Step 2: Generate from CURRENT code**

Run: `Rscript tests/testthat/fixtures/make_summarize_baseline.R` — Expected: `Wrote summarize_baseline.rds: 7 tables`

- [ ] **Step 3: Pinning test**

`tests/testthat/test-summarize_survfit.R`:

```r
# Pins summarize_km()/summarize_cif() output -- values, column names, column
# order, row order AND class -- against a baseline captured before the
# reshape2 -> tidyr refactor.

summarize_baseline<- readRDS(testthat::test_path("fixtures", "summarize_baseline.rds"))

fit_grp<- estimate_km(survival::lung, evt_time = time, evt = status, group = sex)
fit_all<- estimate_km(survival::lung, evt_time = time, evt = status)

set.seed(42)
lung_cr<- survival::lung
lung_cr$status2<- factor(replace(lung_cr$status - 1, sample(nrow(lung_cr), 60), 2))
cif_grp<- estimate_cif(lung_cr, time, status2, group = sex)
cif_all<- estimate_cif(lung_cr, time, status2)

test_that("summarize_km() matches the pre-refactor baseline exactly", {
  expect_equal(summarize_km(fit_grp), summarize_baseline$km_strata, tolerance = 0)
  expect_equal(summarize_km(fit_grp, times = c(0, 250, 500, 750)),
               summarize_baseline$km_strata_times, tolerance = 0)
  expect_equal(summarize_km(fit_all), summarize_baseline$km_overall, tolerance = 0)
  expect_equal(summarize_km(fit_grp, failure_fun = TRUE),
               summarize_baseline$km_failure, tolerance = 0)
})

test_that("summarize_cif() matches the pre-refactor baseline exactly (incl. class and column order)", {
  expect_equal(summarize_cif(cif_grp), summarize_baseline$cif_strata, tolerance = 0)
  expect_identical(class(summarize_cif(cif_grp)), class(summarize_baseline$cif_strata))
  expect_identical(names(summarize_cif(cif_grp)), names(summarize_baseline$cif_strata))
  expect_equal(summarize_cif(cif_all), summarize_baseline$cif_overall, tolerance = 0)
  expect_equal(summarize_cif(cif_grp, times = c(0, 250, 500, 750)),
               summarize_baseline$cif_times, tolerance = 0)
})
```

- [ ] **Step 4: Verify green on the UNrefactored code, commit the pin**

Run: `Rscript -e 'devtools::test(filter = "summarize_survfit")'` — Expected: PASS.

```bash
git add tests/testthat/fixtures/make_summarize_baseline.R tests/testthat/fixtures/summarize_baseline.rds tests/testthat/test-summarize_survfit.R
git commit -m "test: pin summarize_km/summarize_cif output before reshape2 removal"
```

- [ ] **Step 5: Refactor `summarize_km()`**

Replace the whole function body (keep roxygen/signature):

```r
summarize_km<- function(fit, times= NULL, failure_fun= FALSE) {

  ss<- summary(fit, times= if (is.null(times)) pretty(fit$time) else times, extend = TRUE)

  if (failure_fun) {
    # failure function: 1 - S(t); the CI bounds swap sides
    ss[c("surv", "lower", "upper")]<- list(1 - ss$surv, 1 - ss$upper, 1 - ss$lower)
  }

  has_strata<- "strata" %in% names(ss)

  dplyr::tibble(
    strata   = if (has_strata) gsub("^.*=", "", ss$strata) else "Overall",
    time     = ss$time,
    surv     = ss$surv,
    conf_low = ss$lower,
    conf_high= ss$upper
  ) %>%
    dplyr::mutate(
      stat = case_when(
        is.na(surv) ~ "---",
        is.na(conf_low) & is.na(conf_high) ~ sprintf("%3.1f%% [---]", surv * 100),
        is.na(conf_low)  ~ sprintf("%3.1f%% [---, %3.1f%%]", surv * 100, conf_high * 100),
        is.na(conf_high) ~ sprintf("%3.1f%% [%3.1f%%, ---]", surv * 100, conf_low * 100),
        TRUE ~ sprintf("%3.1f%% [%3.1f%%, %3.1f%%]", surv * 100, conf_low * 100, conf_high * 100)
      )
    ) %>%
    pivot_wider(id_cols = time, names_from = strata, values_from = stat)
}
```

(`case_when` evaluates top-down, so the repeated `!is.na(surv) &` guards were redundant; the `%$%`/`{ if }` blocks collapse into one tibble build.)

- [ ] **Step 6: Refactor `summarize_cif()`**

Replace the whole function body (keep roxygen/signature):

```r
summarize_cif<- function(fit, times= NULL) {
  ss <- summary(fit, times = if (is.null(times)) pretty(fit$time) else times, extend = TRUE)
  # relabel survfit's placeholder censor state ("" on some versions) as "0";
  # NB the historical code wrote ss$state, which reached ss$states only via
  # partial matching -- keep the semantics, spell it out
  state_levels <- replace(ss$states, sapply(ss$states, nchar) == 0, "0")
  colnames(ss$pstate) <- colnames(ss$lower) <- colnames(ss$upper) <- state_levels

  has_strata<- "strata" %in% names(fit)
  id_cols<- c(if (has_strata) "strata", "times")

  long<- purrr::map2(
    list(pstate= ss$pstate, conf_low= ss$lower, conf_high= ss$upper),
    c("pstate", "conf_low", "conf_high"),
    function(mat, var) {
      d<- as.data.frame(mat)
      d$times<- ss$time
      if (has_strata) d$strata<- ss$strata
      tidyr::pivot_longer(d, cols = all_of(state_levels),
                          names_to = "states", values_to = var)
    }
  ) %>%
    reduce(full_join, by = c(id_cols, "states")) %>%
    mutate(across(all_of(c("pstate", "conf_low", "conf_high")),
                  function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= ""))) %>%
    mutate(stat= paste0(pstate, " [", conf_low, ", ", conf_high, "]"))

  wide<- tidyr::pivot_wider(long, id_cols = "times",
                            names_from = all_of(c("states", if (has_strata) "strata")),
                            values_from = "stat", names_sep = "_")
  wide<- wide[order(wide$times), ]

  # match reshape2::dcast(times ~ states + strata)'s column order:
  # states-major (colnames order), strata varying fastest (survfit level order)
  ordered_cols<- if (has_strata) {
    as.vector(t(outer(state_levels, levels(factor(ss$strata)),
                      function(s, g) paste(s, g, sep = "_"))))
  } else {
    state_levels
  }
  as.data.frame(wide[, c("times", ordered_cols)])
}
```

Notes for the implementer:
- `as.data.frame()` at the end is REQUIRED: `dcast()` returned a plain data.frame; `pivot_wider()` returns a tibble. The pinning test's `expect_identical(class(...))` enforces this.
- The double rounding `round(x, 3)*100` + `formatC(digits = 1)` is intentional legacy behavior — do not "fix" it.
- `levels(factor(ss$strata))`: `ss$strata` is already a factor for stratified fits, so this is just `levels(ss$strata)` with a belt for character input; either is acceptable if the pin passes.

- [ ] **Step 7: Remove the reshape2 dependency**

- `R/fanetc-package.R`: delete the line `#' @importFrom reshape2 melt dcast`.
- `DESCRIPTION`: delete the line `    reshape2,` from Imports.
- Run: `Rscript -e 'devtools::document()'` (regenerates NAMESPACE; restore `man/fanetc-package.Rd` if rewritten: `git checkout -- man/fanetc-package.Rd`).

- [ ] **Step 8: Run the gates — with a contingency**

Run: `Rscript -e 'devtools::test(filter = "summarize_survfit")'`
Expected: PASS at `tolerance = 0`.
**Contingency:** if the column-order assertion fails with the same names in a different order, the dcast order was strata-major instead — swap the `outer()` construction to `as.vector(t(outer(levels(factor(ss$strata)), state_levels, function(g, s) paste(s, g, sep = "_"))))` and re-run. The fixture is the authority; do not edit the fixture.

Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN.
Run: `grep -rn "reshape2\|melt\|dcast" R/ DESCRIPTION NAMESPACE` — Expected: no matches.

- [ ] **Step 9: Commit**

```bash
git add R/summarize_survfit.R R/fanetc-package.R DESCRIPTION NAMESPACE
git commit -m "refactor: unify summarize_km/summarize_cif pipelines; drop reshape2 dependency"
```

---

### Task 9: Flatten `table_one()`

**Files:**
- Modify: `R/desp_table_gtsummary.R` (`table_one()` only)
- Test: existing `test-table_one.R`, `test-backward_compat.R`, `test-table_one_paired.R`

**Interfaces:**
- Consumes: nothing new.
- Produces: same exported signature/behavior; body restructured. Task 10 splits this flattened body into `.table_one_impl()` — apply this task first.

- [ ] **Step 1: Delete dead code**

Delete the commented blocks at ~144–147 (`# df <- df %>% ... mutate(!!group_name := group_col)`), ~192 (`# include = if (has_group) ...`), ~214–215 (`# group_name <- ...` / `# n_groups_val <- ...`), ~232–235 (`#   test.args = list(...)`), and the four example-call comment lines after the function (~265–268, `# table_one(df)` etc.).

- [ ] **Step 2: Restructure the group handling (kill the parallel bookkeeping)**

Replace the block from `# Remove observations with missing group variable...` through the `group_col<- if (missing_group_exclude) ... }` assignment (~139–161) with:

```r
  # Remove observations with a missing group value (or keep them as an
  # explicit level); group_col is then simply the resulting column
  if (has_group) {
    group_name <- rlang::quo_name(group)
    if (missing_group_exclude) {
      df <- dplyr::filter(df, !is.na(!!group))
    } else {
      df <- dplyr::mutate(df, !!group := forcats::fct_na_value_to_level(!!group, level = missing_text))
    }
    group_col <- df[[group_name]]
  }
```

- [ ] **Step 3: Modernize the data-prep verbs**

```r
  df <- df %>%
      dplyr::ungroup() %>%
      # Remove character and date variables
      dplyr::select(dplyr::where(~ !is.character(.) && !inherits(., "Date"))) %>%
      # Drop unused factor levels
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), forcats::fct_drop))
```

and in the include block change `has_include & has_group` → `has_include && has_group`, `has_include & !has_group` → `has_include && !has_group`. In `tbl_summary(by = ...)` use `group_name` instead of re-deriving `rlang::quo_name(group)`.

- [ ] **Step 4: Replace the three `%>% { if } ` pipeline stages with sequential statements**

Replace everything from `) %>%` after the `tbl_summary(...)` call (i.e. the three brace stages, ~209–260) with — `tbl_summary(...)` now ends with a plain `)`:

```r
  if (add_p && has_group) {
    # Determine number of groups to select appropriate statistical tests
    n_groups_val <- length(unique(group_col))

    # For continuous: choose based on continuous_stat
    cont_test <- if (continuous_stat == "meansd") {
      if (n_groups_val == 2) "t.test" else "oneway.test"
    } else {
      if (n_groups_val == 2) "wilcox.test" else "kruskal.test"
    }

    test_args <- c(
      list(all_categorical() ~ list(hybrid = TRUE, simulate.p.value = TRUE)),
      if (continuous_stat == "meansd") list(all_continuous() ~ list(var.equal = FALSE))
    )

    tbl <- gtsummary::add_p(
      tbl,
      test = list(
        all_continuous() ~ cont_test,
        all_categorical() ~ "fisher.test"
      ),
      pvalue_fun = pvalue_fun,
      test.args = test_args
    )
  }

  if (add_overall && has_group) tbl <- gtsummary::add_overall(tbl)
  if (sort_by_p && add_p && has_group) tbl <- gtsummary::sort_p(tbl)

  tbl
```

(`c(list(...), if (cond) list(...))` replaces the `%>% append(...)` construction; `NULL` from a false `if` vanishes inside `c()`.)

- [ ] **Step 5: Run the gates**

Run: `Rscript -e 'devtools::test(filter = "table_one")'` — Expected: PASS (runs both table_one and table_one_paired files).
Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN.

- [ ] **Step 6: Commit**

```bash
git add R/desp_table_gtsummary.R
git commit -m "refactor: flatten table_one group handling and modifier pipeline"
```

---

### Task 10: `.table_one_impl()` split + paired-helper simplification

**Files:**
- Modify: `R/desp_table_gtsummary.R` (split `table_one()` into wrapper + `.table_one_impl()`)
- Modify: `R/desp_table_paired.R` (`.paired_safe_pvalue()`, lazy closures, replace the `call2()` block)
- Test: existing `test-table_one.R`, `test-table_one_paired.R`, `test-api_compat.R`, `test-backward_compat.R`

**Interfaces:**
- Consumes: Task 9's flattened `table_one()`.
- Produces: internal `.table_one_impl(df, group_name = NULL, include_quo = NULL, datadic = NULL, name_col = "var_name", desp_col = "var_desp", missing = "ifany", missing_text = "(Missing)", missing_group_exclude = TRUE, add_p = NULL, add_overall = NULL, sort_by_p = FALSE, continuous_stat = "meansd", pvalue_fun = format_pvalue)`. `table_one()` keeps its exact exported signature; `table_one_paired()` calls the impl directly (no more `call2()`/`eval()`).

- [ ] **Step 1: Split `table_one()`**

In `R/desp_table_gtsummary.R`, turn the (Task-9-flattened) body into `.table_one_impl()` and a thin wrapper. The wrapper (keeps the current roxygen block and `@export`):

```r
table_one <- function(df,
                      group,
                      datadic = NULL,
                      var_name,
                      var_desp,
                      include,
                      missing = "ifany",
                      missing_text = "(Missing)",
                      missing_group_exclude = TRUE,
                      add_p = NULL,
                      add_overall = NULL,
                      sort_by_p = FALSE,
                      continuous_stat = c("meansd", "mediqr"),
                      pvalue_fun = format_pvalue) {

  set.seed(0) # For reproducibility of Fisher's exact test p-values
  continuous_stat <- match.arg(continuous_stat)

  group <- rlang::enquo(group)
  include <- rlang::enquo(include)
  var_name <- rlang::enquo(var_name)
  var_desp <- rlang::enquo(var_desp)

  .table_one_impl(
    df,
    group_name = if (rlang::quo_is_missing(group)) NULL else rlang::quo_name(group),
    include_quo = if (rlang::quo_is_missing(include)) NULL else include,
    datadic = datadic,
    name_col = if (rlang::quo_is_missing(var_name)) "var_name" else rlang::as_name(var_name),
    desp_col = if (rlang::quo_is_missing(var_desp)) "var_desp" else rlang::as_name(var_desp),
    missing = missing, missing_text = missing_text,
    missing_group_exclude = missing_group_exclude,
    add_p = add_p, add_overall = add_overall, sort_by_p = sort_by_p,
    continuous_stat = continuous_stat, pvalue_fun = pvalue_fun
  )
}
```

`.table_one_impl()` (non-exported, placed directly below the wrapper; body is Task 9's flattened logic with quosure references replaced by resolved values):

```r
# Implementation behind table_one(), taking resolved values: group_name/name_col/
# desp_col are strings (or NULL), include_quo is a tidyselect quosure or NULL.
# table_one_paired() calls this directly, bypassing NSE argument re-capture.
.table_one_impl <- function(df,
                            group_name = NULL,
                            include_quo = NULL,
                            datadic = NULL,
                            name_col = "var_name",
                            desp_col = "var_desp",
                            missing = "ifany",
                            missing_text = "(Missing)",
                            missing_group_exclude = TRUE,
                            add_p = NULL,
                            add_overall = NULL,
                            sort_by_p = FALSE,
                            continuous_stat = "meansd",
                            pvalue_fun = format_pvalue) {

  has_group <- !is.null(group_name)

  if (!is.null(datadic) && !all(c(name_col, desp_col) %in% names(datadic))) {
    stop("`datadic` must contain columns `", name_col, "` and `", desp_col, "`")
  }

  # Set defaults for add_p and add_overall
  if (is.null(add_p)) add_p <- has_group
  if (is.null(add_overall)) add_overall <- has_group

  # Data preparation
  df <- df %>%
      dplyr::ungroup() %>%
      # Remove character and date variables
      dplyr::select(dplyr::where(~ !is.character(.) && !inherits(., "Date"))) %>%
      # Drop unused factor levels
      dplyr::mutate(dplyr::across(dplyr::where(is.factor), forcats::fct_drop))

  if (!is.null(include_quo)) {
    include_loc <- tidyselect::eval_select(include_quo, df)
    df <- if (has_group) {
      group_loc <- match(group_name, names(df))
      df[, sort(unique(c(group_loc, include_loc)))]
    } else {
      df[, include_loc]
    }
  }

  # Remove observations with a missing group value (or keep them as an
  # explicit level); group_col is then simply the resulting column
  if (has_group) {
    if (missing_group_exclude) {
      df <- dplyr::filter(df, !is.na(.data[[group_name]]))
    } else {
      df <- dplyr::mutate(df, !!rlang::sym(group_name) := forcats::fct_na_value_to_level(.data[[group_name]], level = missing_text))
    }
    group_col <- df[[group_name]]
  }

  # gtsummary requires glue strings for statistics; med_q1_q3 uses type-1
  # quantiles to match the historical med_iqr() output
  continuous_glue <- if (continuous_stat == "meansd") {
    "{mean} ± {sd}"
  } else {
    "{median_type1} ({q1_type1} – {q3_type1})"
  }
  n_continuous_stats <- if (continuous_stat == "meansd") 2L else 3L

  # Calculate decimal places for each numeric variable using decimalplaces()
  # This ensures formatting matches the actual data precision
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  digits_list <- list()

  for (var in numeric_vars) {
    dec <- decimalplaces(df[[var]])
    digits_list[[var]] <- rep(dec, n_continuous_stats)
  }

  # For categorical variables: count and percentage
  cat_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.logical(x))]
  for (var in cat_vars) {
    digits_list[[var]] <- c(0, 1)
  }

  # Build the base tbl_summary
  tbl <- gtsummary::tbl_summary(
    data = df,
    by = if (has_group) group_name else NULL,
    missing = missing,
    missing_text = missing_text,
    statistic = list(
      all_continuous() ~ continuous_glue,
      all_categorical(dichotomous = FALSE) ~ "{n} ({p}%)",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    digits = digits_list,
    type = list(
      all_categorical(dichotomous = FALSE) ~ "categorical",
      all_dichotomous() ~ "dichotomous"
    ),
    label = if (!is.null(datadic)) {
      # gtsummary requires a named list (not a named vector)
      as.list(setNames(datadic[[desp_col]], datadic[[name_col]]))
    } else NULL
  )

  if (add_p && has_group) {
    # Determine number of groups to select appropriate statistical tests
    n_groups_val <- length(unique(group_col))

    # For continuous: choose based on continuous_stat
    cont_test <- if (continuous_stat == "meansd") {
      if (n_groups_val == 2) "t.test" else "oneway.test"
    } else {
      if (n_groups_val == 2) "wilcox.test" else "kruskal.test"
    }

    test_args <- c(
      list(all_categorical() ~ list(hybrid = TRUE, simulate.p.value = TRUE)),
      if (continuous_stat == "meansd") list(all_continuous() ~ list(var.equal = FALSE))
    )

    tbl <- gtsummary::add_p(
      tbl,
      test = list(
        all_continuous() ~ cont_test,
        all_categorical() ~ "fisher.test"
      ),
      pvalue_fun = pvalue_fun,
      test.args = test_args
    )
  }

  if (add_overall && has_group) tbl <- gtsummary::add_overall(tbl)
  if (sort_by_p && add_p && has_group) tbl <- gtsummary::sort_p(tbl)

  tbl
}
```

The rest of the body (glue strings, digits loops, `tbl_summary()` call, sequential `add_p`/`add_overall`/`sort_p` blocks, final `tbl`) moves verbatim from Task 9's version. Equivalence notes: `match(group_name, names(df))` reproduces `eval_select(group, df)`'s position; `filter(!is.na(.data[[group_name]]))` ≡ `filter(!is.na(!!group))` for a data-frame column; `set.seed(0)` fires in the wrapper at the same effective point as before (before any computation).

- [ ] **Step 2: Replace `table_one_paired()`'s constructed call**

In `R/desp_table_paired.R`, replace the `te_call <- rlang::call2(...)` block through `tbl <- eval(te_call, envir = environment())` (~lines 296–306) with:

```r
  tbl <- .table_one_impl(
    desc_data,
    group_name = group_name,
    datadic = datadic,
    name_col = if (rlang::quo_is_missing(var_name_q)) "var_name" else rlang::as_name(var_name_q),
    desp_col = if (rlang::quo_is_missing(var_desp_q)) "var_desp" else rlang::as_name(var_desp_q),
    missing = missing, missing_text = missing_text,
    add_p = FALSE, add_overall = FALSE,
    continuous_stat = continuous_stat
  )
```

- [ ] **Step 3: Add `.paired_safe_pvalue()` and slim the two test closures**

In `R/desp_table_paired.R`, above `.paired_make_cont_test_fn`:

```r
# run a paired-test expression: warnings muffled, errors -> NA, and NaN from
# degenerate tests (e.g. zero-discordant-pair denominators) normalized to
# NA_real_, as the design specifies
.paired_safe_pvalue <- function(expr) {
  p <- tryCatch(
    withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) NA_real_
  )
  if (!is.finite(p)) NA_real_ else p
}
```

Replace the two closures' bodies:

```r
.paired_make_cont_test_fn <- function(paired_df, pair_id_name, group_name, ref_level, other_level, continuous_stat) {
  function(data, variable, by, ...) {
    wide <- .paired_wide(paired_df, pair_id_name, group_name, ref_level, other_level, variable)
    p <- .paired_safe_pvalue(
      if (continuous_stat == "meansd") {
        stats::t.test(wide$.other, wide$.ref, paired = TRUE)$p.value
      } else {
        stats::wilcox.test(wide$.other, wide$.ref, paired = TRUE)$p.value
      }
    )
    dplyr::tibble(p.value = p)
  }
}

.paired_make_cat_test_fn <- function(paired_df, pair_id_name, group_name, ref_level, other_level) {
  function(data, variable, by, ...) {
    wide <- .paired_wide(paired_df, pair_id_name, group_name, ref_level, other_level, variable)
    p <- .paired_safe_pvalue({
      lv <- union(as.character(unique(wide$.ref)), as.character(unique(wide$.other)))
      ref_f   <- factor(as.character(wide$.ref),   levels = lv)
      other_f <- factor(as.character(wide$.other), levels = lv)
      stats::mcnemar.test(table(ref_f, other_f))$p.value
    })
    dplyr::tibble(p.value = p)
  }
}
```

- [ ] **Step 4: Make closure creation lazy**

In `table_one_paired()`, delete the four eager `*_fn <- .paired_make_*(...)` assignments (~308–311) and move each into its `if` block:

```r
  if (add_overall) tbl <- gtsummary::add_overall(tbl)

  if (add_n_pairs) {
    n_pairs_fn <- .paired_make_n_pairs_fn(data, pair_id_name, group_name, ref_level, other_level)
    tbl <- gtsummary::add_stat(tbl, fns = gtsummary::everything() ~ n_pairs_fn)
    tbl <- gtsummary::modify_header(tbl, n_pairs ~ "**N pairs**")
  }

  if (add_smd) {
    smd_fn <- .paired_make_smd_fn(data, pair_id_name, group_name, ref_level, other_level, pairing_method)
    tbl <- gtsummary::add_stat(tbl, fns = gtsummary::everything() ~ smd_fn)
    ...  # existing modify_header/footnote lines unchanged
  }

  if (add_p) {
    cont_test_fn <- .paired_make_cont_test_fn(data, pair_id_name, group_name, ref_level, other_level, continuous_stat)
    cat_test_fn  <- .paired_make_cat_test_fn(data, pair_id_name, group_name, ref_level, other_level)
    tbl <- gtsummary::add_p(...)  # existing call unchanged
    ...
  }
```

(The `...` lines here mean "keep the existing statements between/after, unchanged" — only the closure constructions move.)

- [ ] **Step 5: Run the gates**

Run: `Rscript -e 'devtools::test(filter = "table_one")'` — Expected: PASS (both files; the paired suite covers NA/NaN p-value normalization).
Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN (`test-api_compat.R` confirms the `table_one()` signature is untouched).

- [ ] **Step 6: Commit**

```bash
git add R/desp_table_gtsummary.R R/desp_table_paired.R
git commit -m "refactor: split table_one into wrapper + impl; simplify paired test plumbing"
```

---

### Task 11: `fan_util_fun.R` — `decimalplaces()` rewrite, MI cleanup, misc

**Files:**
- Create: `tests/testthat/test-decimalplaces.R`
- Modify: `R/fan_util_fun.R`
- Test: `test-summarize_mi.R`, `test-table_one.R` (decimalplaces feeds table_one digits)

**Interfaces:**
- Consumes: nothing new.
- Produces: same exported signatures; internal helpers `.mi_term_index(type3_list)` and `.bind_type3_rows(main, type3)`.

- [ ] **Step 1: Write `decimalplaces()` pinning tests (against CURRENT code)**

`tests/testthat/test-decimalplaces.R`:

```r
# Pins decimalplaces()'s historical digit rule before its rewrite: most
# frequent decimal count wins; ties go to the LARGER count; capped at max_dec.

test_that("decimalplaces() pins the historical digit rule", {
  expect_identical(decimalplaces(c(1, 2, 3)), 0L)
  expect_identical(decimalplaces(numeric(0)), 0L)
  expect_identical(decimalplaces(c(NA_real_, NA_real_)), 0L)
  expect_identical(decimalplaces(c(1.5, 2.5, 3.25)), 1L)
  expect_identical(decimalplaces(c(1.25, 2.75, 3.5)), 2L)
  expect_identical(decimalplaces(c(1.5, 2.25)), 2L)          # tie -> larger count
  expect_identical(decimalplaces(c(1.12345, 2.12345)), 4L)   # capped at max_dec
  expect_identical(decimalplaces(c(1.12345, 2.12345), max_dec = 6L), 5L)
  expect_identical(decimalplaces(c(1.10, 2.20)), 1L)         # trailing zeros stripped
  expect_identical(decimalplaces(c(0.5, 1, 2)), 1L)          # integers ignored when any fractional
})
```

- [ ] **Step 2: Verify green on CURRENT code, commit the pin**

Run: `Rscript -e 'devtools::test(filter = "decimalplaces")'` — Expected: PASS. If any expectation disagrees with current behavior, change the EXPECTATION (this is a pin), then commit:

```bash
git add tests/testthat/test-decimalplaces.R
git commit -m "test: pin decimalplaces digit rule before rewrite"
```

- [ ] **Step 3: Rewrite `decimalplaces()`**

Replace the function body (keep roxygen/signature); delete the commented line 31 with it:

```r
decimalplaces <- function(x, max_dec= 4L) {
  frac<- round(x[!is.na(x)] %% 1, 10)
  frac<- frac[frac != 0]
  if (length(frac) == 0) return(0L)

  # decimal digits of each fractional part, trailing zeros stripped; values
  # whose character form has no "." (scientific notation) are ignored
  txt<- gsub("0+$", "", as.character(frac))
  parts<- strsplit(txt, ".", fixed = TRUE)
  n_dec<- nchar(vapply(parts[lengths(parts) == 2L], `[`, "", 2L))
  if (length(n_dec) == 0) return(0L)

  counts<- table(n_dec)
  modes<- as.integer(names(counts)[counts == max(counts)])
  pmin.int(as.integer(max_dec), max(modes))
}
```

Deliberate edge normalization (document in the commit message): when every fractional part renders in scientific notation (e.g. all `1e-08`), the old code returned a zero-length `integer(0)` via out-of-bounds indexing; the new code returns `0L`. All sane inputs are pinned identical by Step 1.

- [ ] **Step 4: MI-helper cleanup**

In `calculate_type3_mi()`:
- Delete the dead line `df<- sapply(split(varseq, varseq), length)` (~185).
- Split the combined assignment and rename `tmp` → `first_fit`:

```r
  first_fit<- mice::getfit(mira_obj, 1L)
  x<- model.matrix(first_fit)
```

- Update the one use: `attr(first_fit$terms, "term.labels")[i]` (~line 223).
- Delete the commented line `# x<- model.matrix(mira_obj$analyses[[1]])` (~182).

Add above `summarize_mi_glm()`:

```r
# shared by summarize_mi_glm()/summarize_mi_coxph(): design-matrix term index
# carried in the "col_in_X" attributes, and the final type3-row append/ordering
.mi_term_index<- function(type3_list) {
  dplyr::bind_rows(lapply(type3_list, function(x) attr(x, "col_in_X")))
}
.bind_type3_rows<- function(main, type3) {
  main %>%
    dplyr::bind_rows(type3) %>%
    dplyr::arrange(rid, var) %>%
    dplyr::select(var, stat, pval, dplyr::everything())
}
```

In `summarize_mi_glm()`: replace the `out_tmp<- out %>% lapply(...) %>% bind_rows()` block with `out_tmp<- .mi_term_index(out)`; replace the final `glm_out %>% bind_rows(type3_out) %>% arrange(rid, var) %>% select(var, stat, pval, everything())` with `.bind_type3_rows(glm_out, type3_out)`; add this comment directly above the `pval= type3_out$pval[charmatch(...)]` line:

```r
           # match each coefficient row to its type-3 variable: strip glm's
           # "TRUE" suffix on logical dummies, then prefix-match (charmatch)
           # against the type-3 variable names
```

In `summarize_mi_coxph()`: same two replacements (`.mi_term_index(out)`; `.bind_type3_rows(cox_out, type3_out)`); delete the commented lines `# pool() %>%` (keep the mice-issue URL comment), `# as.data.frame() %>%`, `# rownames_to_column("var") %>%`.

In `generate_mi_glm_termplot_df()`: rename the `mapply` argument `tt` → `term_id` (declaration at the `mapply(function(df, cc, var, tt)` line, the `mm[, tt == varseq]` use, and the `tt= terms` argument → `term_id= terms`) so it no longer shadows the `terms()` object `tt`; delete the 4 commented verification lines (~426–429); add above the `names(tmp)<- gsub(...)` line:

```r
    # termplot() names its per-term data frames c("x", "y"); rename "x" to the
    # term's carrier variable so model.matrix() finds it below
```

- [ ] **Step 5: `summarize_coxph()` class checks + `updateWorksheet()`**

In `summarize_coxph()`:

```r
  if (!inherits(mdl, c("coxph", "coxph.penal"))) stop("Not a coxph or coxph.penal object.")
  ...
  if (inherits(mdl, "coxph.penal")) {
    out<- rename(out, se= 'se(coef)')
  } else if (identical(class(mdl), "coxph")) {
    out<- rename(out, se= 'se(coef)', p= 'Pr(>|z|)')
  }
```

(`identical(class(mdl), "coxph")` preserves the old `all(class(mdl)== "coxph")` semantics exactly — a subclassed cox model fell through both branches and must continue to.) Delete the commented line `# names(out)[grep(...)]<- "p"` (~94). Also change `df= if (any(class(mdl)=="coxph.penal") && ...)` → `df= if (inherits(mdl, "coxph.penal") && ...)` (~127), and `dplyr::select(one_of(c("term", "stat", "pval")))` → `dplyr::select(all_of(c("term", "stat", "pval")))` (~108).

Replace `updateWorksheet()`'s body (keep roxygen/signature). NOTE: openxlsx is not installed locally and this function has no tests — the change must stay strictly mechanical:

```r
updateWorksheet<- function(wb, sheetName, x, ...) {
  sheet_exists<- sheetName %in% names(wb)

  if (sheet_exists) {
    sheetOrder<- openxlsx::worksheetOrder(wb)
    names(sheetOrder)<- names(wb)
    openxlsx::removeWorksheet(wb, sheetName)
  }

  openxlsx::addWorksheet(wb, sheetName)
  openxlsx::writeData(wb, sheetName, x)

  if (sheet_exists) openxlsx::worksheetOrder(wb)<- sheetOrder[names(wb)]
  wb
}
```

- [ ] **Step 6: Run the gates**

Run: `Rscript -e 'devtools::test(filter = "decimalplaces")'` — Expected: PASS.
Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN (`test-summarize_mi.R` pins the MI output columns; `test-table_one.R` exercises decimalplaces through the digits path).

- [ ] **Step 7: Commit**

```bash
git add R/fan_util_fun.R tests/testthat/test-decimalplaces.R
git commit -m "refactor: rewrite decimalplaces transparently; dedupe MI summary helpers (all-scientific-fraction edge now returns 0L, was integer(0))"
```

---

### Task 12: Shared finalization in `construct_surv_var()` / `construct_cmprisk_var()`

**Files:**
- Modify: `R/construct_event_time.R`
- Test: existing `test-construct_equiv.R`, `test-backward_compat.R`

**Interfaces:**
- Consumes: nothing new.
- Produces: internal helper `.finalize_event_vars(df, patid_quo, time2evt, evt, varname, append)`; both exported constructors keep their signatures and outputs.

- [ ] **Step 1: Add the shared helper**

In `R/construct_event_time.R`, directly below `fix_nonpositive_times()`:

```r
# shared tail of construct_surv_var()/construct_cmprisk_var(): assemble the
# patid/time2evt/evt tibble, apply the non-positive-time fixes (whose printed
# diagnostics use these pre-rename column names), rename to the requested
# variable names, and append to df or return standalone
.finalize_event_vars <- function(df, patid_quo, time2evt, evt, varname, append) {
  out <- dplyr::tibble(
    !!rlang::as_name(patid_quo) := dplyr::pull(df, !!patid_quo),
    time2evt = time2evt,
    evt = evt
  )
  out$time2evt <- fix_nonpositive_times(out$time2evt, out)
  names(out)[2:3] <- varname
  if (append) dplyr::bind_cols(df, out[varname]) else out
}
```

- [ ] **Step 2: Use it in both constructors**

In `construct_surv_var()`, replace the tail (from `out <- dplyr::tibble(` through the final `if (append) ...` line) with:

```r
  varname <- if (is.null(surv_varname)) c("evt_time", "evt") else surv_varname
  .finalize_event_vars(df, patid, time2evt, evt, varname, append)
```

(The original computed `varname` after the tibble build; the fix/rename/append semantics are order-independent because `fix_nonpositive_times()` runs before the rename in both versions.)

In `construct_cmprisk_var()`, replace the tail (from `out <- dplyr::tibble(` through the final `if (append) ...` line; `varname` is already computed earlier in that function) with:

```r
  .finalize_event_vars(df, patid, time2evt, evt, varname, append)
```

- [ ] **Step 3: Run the gates**

Run: `Rscript -e 'devtools::test(filter = "construct")'` — Expected: PASS (the equivalence suite compares against `fixtures/ref_construct.R` reference output, including the zero/negative-time warning + printout paths).
Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN.

- [ ] **Step 4: Commit**

```bash
git add R/construct_event_time.R
git commit -m "refactor: share event-variable finalization between construct_surv_var and construct_cmprisk_var"
```

---

### Task 13: Namespace hygiene, docs regeneration, final verification

**Files:**
- Modify: `R/fanetc-package.R`, `R/desp_table_gtsummary.R` (roxygen import lines only)
- Modify (generated): `NAMESPACE`, `man/*.Rd` as emitted by `devtools::document()`
- Modify: `fanetc-cheatsheet-main.md`, `HANDOFF.md`

**Interfaces:**
- Consumes: all prior tasks.
- Produces: the finished branch, ready for review/merge.

- [ ] **Step 1: Trim imports**

- `R/fanetc-package.R`: change the viridis line — `event_time_color_scales()` now calls `viridis::` explicitly (Task 6), so delete `#' @importFrom viridis scale_color_viridis scale_colour_viridis scale_fill_viridis` entirely. (viridis stays in DESCRIPTION Imports; `::` satisfies the dependency check.)
- `R/desp_table_gtsummary.R`: delete the three function-level roxygen import lines above `table_one()` — `#' @importFrom gtsummary tbl_summary add_p add_overall modify_header as_flex_table`, `#' @importFrom dplyr select mutate filter across where pull`, `#' @importFrom rlang enquo quo_is_missing quo_name` — all redundant with the package-level `@import dplyr/gtsummary` and rlang usages already namespaced or imported in `fanetc-package.R`.

- [ ] **Step 2: Regenerate docs**

Run: `Rscript -e 'devtools::document()'`
Expected: NAMESPACE loses the viridis/reshape2 importFrom lines (reshape2 went in Task 8); no exported alias changes. Inspect `git diff NAMESPACE` — only removals of `importFrom(viridis,...)`, `importFrom(reshape2,...)` (and nothing else) are acceptable.

- [ ] **Step 3: Full check**

Run: `Rscript -e 'devtools::test()'` — Expected: 0 FAIL, 0 WARN.
Run: `Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "never")'`
Expected vs the pre-branch baseline (1 WARNING / 5 NOTEs): the WARNING (flextable in tests) and 4 NOTEs persist (environmental/pre-existing), but the `checking R code for possible problems ... NOTE` about `class() != "factor"` in `event_time_desp.R` is GONE. Any NEW warning or note is a task failure — fix before proceeding.

- [ ] **Step 4: Update the cheatsheet and handoff note**

- `fanetc-cheatsheet-main.md`: no exported API changed; update only the `prepare_survfit` entry if it mentions internal structure, and add one line under a "internals" note that `estimate_km`/`estimate_cif` share `.survfit_call()`. Keep it brief.
- `HANDOFF.md`: add a dated section "Maintainability refactor (2026-07-19)" listing: behavior-preserving refactor of all `R/` files per `docs/superpowers/plans/2026-07-19-maintainability-refactor.md`; dependencies removed (lazyeval, reshape2); the one deliberate behavior improvement (`add_atrisk()` NULL-text theme no longer errors) and the `decimalplaces()` `integer(0)` → `0L` edge normalization; the three new fixture files and their regeneration rule (pre-refactor code only).

- [ ] **Step 5: Commit**

```bash
git add R/fanetc-package.R R/desp_table_gtsummary.R NAMESPACE man/ fanetc-cheatsheet-main.md HANDOFF.md
git commit -m "chore: trim redundant imports; regenerate docs; record refactor in handoff"
```

- [ ] **Step 6: Final branch verification**

Run: `Rscript -e 'devtools::test()'` one last time on the branch tip — Expected: 0 FAIL, 0 WARN.
Then stop: merging is a user decision (finishing-a-development-branch options will be presented after review).
