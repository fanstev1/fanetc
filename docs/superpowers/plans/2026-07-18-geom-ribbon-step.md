# geom_ribbon_step() Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `geom_ribbon_step()` — a drop-in `ggplot2::geom_ribbon()` replacement that draws step-function ribbons — and use it for the CI bands in `show_surv()`/`show_cif()`, replacing the manual pre-stepping in `prepare_survfit()`.

**Architecture:** A new file `R/geom_ribbon_step.R` holds an internal pure helper `stairstep_ribbon()` (index-expansion of sorted x/ymin/ymax data, mirroring ggplot2's internal `stairstep()`), an internal ggproto `StatStepRibbon` whose `compute_group()` sorts by x then calls the helper, and the exported wrapper `geom_ribbon_step()` that builds a `layer(stat = StatStepRibbon, geom = GeomRibbon)`. `prepare_survfit()` then stops pre-stepping its `plot_ci_d` tibbles, and the four `geom_ribbon()` call sites in `show_surv()`/`show_cif()` switch to `geom_ribbon_step()`.

**Tech Stack:** R package (fanetc), ggplot2 ggproto extension, roxygen2 docs (markdown OFF), testthat 3, devtools.

**Spec:** `docs/superpowers/specs/2026-07-18-geom-ribbon-step-design.md` — read it before starting.

## Global Constraints

- Run everything from the package root: `/Users/sfan/Documents/projects/fanetc`.
- R packages install from the Posit snapshot `packagemanager.posit.co/cran/2025-03-31` (already configured in `~/.Rprofile`); you should not need to install anything.
- Roxygen **markdown is off** in this package: no backticks/markdown in roxygen comments; use `\code{}`; escape any literal `%` as `\%`.
- DESCRIPTION declares `R (>= 4.2)`. Don't use newer-R idioms (no lambda shorthand `\(x)` in package code; match existing style).
- Match existing code style in `R/event_time_desp.R` (e.g. `foo<- function`, `%>%` pipes, `dplyr::`-qualified verbs in package code). Tests may call attached packages unqualified — `tests/testthat/helper-setup.R` already attaches ggplot2, dplyr, tidyr, survival, magrittr, etc.
- **Surgical changes only**: touch exactly the lines each task lists. Do not reformat, "improve", or re-comment adjacent code in `R/event_time_desp.R`.
- Keep `R CMD check` at 0 errors / 0 warnings / 1 (environment-specific) NOTE.
- NA confidence limits must flow through to `GeomRibbon` un-dropped and warning-free (they render as gaps). This is why `StatStepRibbon` declares only `x` in `required_aes` — see the spec's Error handling section. Do not "fix" this to `c("x", "ymin", "ymax")`.
- Every commit message ends with `Co-Authored-By:` trailer per repo convention (see `git log`).

---

### Task 1: `stairstep_ribbon()` helper

**Files:**
- Create: `R/geom_ribbon_step.R`
- Create: `tests/testthat/test-geom_ribbon_step.R`

**Interfaces:**
- Consumes: nothing (pure function).
- Produces: internal `stairstep_ribbon(data, direction = c("hv", "vh", "mid"))` — takes a data.frame with numeric columns `x`, `ymin`, `ymax` (other columns allowed), returns a data.frame with the same columns where x/ymin/ymax are expanded to step coordinates; sorts by `x` first; 0/1-row input returned unchanged. Task 2's `StatStepRibbon$compute_group()` calls it.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-geom_ribbon_step.R`:

```r
# Unit tests for geom_ribbon_step() and its stairstep_ribbon() helper
# (spec: docs/superpowers/specs/2026-07-18-geom-ribbon-step-design.md).

# The original manual "hv" index logic (step_ribbon() / the old
# prepare_survfit() pre-stepping), kept as the independent reference.
ref_step_hv <- function(df) {
  df <- df[order(df$x), ]
  nn <- nrow(df)
  ys <- rep(1:nn, each = 2)[-2 * nn]
  xs <- c(1, rep(2:nn, each = 2))
  data.frame(x = df$x[xs], ymin = df$ymin[ys], ymax = df$ymax[ys])
}

roc <- data.frame(
  x    = c(0, .1, .25, .5, 1),
  ymin = c(0, .2, .45, .7, .95),
  ymax = c(.05, .4, .65, .85, 1)
)

test_that("stairstep_ribbon() hv matches the original step_ribbon() index logic", {
  out <- fanetc:::stairstep_ribbon(roc)
  ref <- ref_step_hv(roc)
  expect_equal(nrow(out), 2 * nrow(roc) - 1)
  expect_equal(out$x, ref$x)
  expect_equal(out$ymin, ref$ymin)
  expect_equal(out$ymax, ref$ymax)
})

test_that("stairstep_ribbon() sorts by x first", {
  set.seed(1)
  shuffled <- roc[sample(nrow(roc)), ]
  expect_equal(fanetc:::stairstep_ribbon(shuffled), fanetc:::stairstep_ribbon(roc))
})

test_that("stairstep_ribbon() vh jumps at the current x", {
  out <- fanetc:::stairstep_ribbon(roc, direction = "vh")
  nn <- nrow(roc)
  xs <- rep(1:nn, each = 2)[-2 * nn]
  ys <- c(1, rep(2:nn, each = 2))
  expect_equal(out$x, roc$x[xs])
  expect_equal(out$ymin, roc$ymin[ys])
  expect_equal(out$ymax, roc$ymax[ys])
})

test_that("stairstep_ribbon() mid jumps midway between adjacent x values", {
  out <- fanetc:::stairstep_ribbon(roc, direction = "mid")
  nn <- nrow(roc)
  mids <- (roc$x[-1] + roc$x[-nn]) / 2
  expect_equal(out$x, c(roc$x[1], rep(mids, each = 2), roc$x[nn]))
  expect_equal(out$ymin, rep(roc$ymin, each = 2))
  expect_equal(out$ymax, rep(roc$ymax, each = 2))
})

test_that("stairstep_ribbon() carries other columns along by row", {
  withcols <- transform(roc, fill = "grey20", PANEL = 1L)
  out <- fanetc:::stairstep_ribbon(withcols)
  expect_equal(out$fill, rep("grey20", 2 * nrow(roc) - 1))
  expect_equal(out$PANEL, rep(1L, 2 * nrow(roc) - 1))
})

test_that("stairstep_ribbon() returns 0- and 1-row input unchanged", {
  expect_equal(fanetc:::stairstep_ribbon(roc[1, ]), roc[1, ], ignore_attr = TRUE)
  expect_equal(nrow(fanetc:::stairstep_ribbon(roc[0, ])), 0)
})

test_that("stairstep_ribbon() rejects an invalid direction", {
  expect_error(fanetc:::stairstep_ribbon(roc, direction = "diagonal"))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "geom_ribbon_step")'`
Expected: FAIL — every test errors with `object 'stairstep_ribbon' not found`.

- [ ] **Step 3: Write the implementation**

Create `R/geom_ribbon_step.R`:

```r
# geom_ribbon_step(): a drop-in replacement for ggplot2::geom_ribbon() that
# draws the ribbon as a step function, for confidence bands around step curves
# (Kaplan-Meier, cumulative incidence, ROC).

# Expand x/ymin/ymax data to step-function coordinates, mirroring ggplot2's
# internal stairstep() (geom_step) but carrying ymin and ymax instead of y.
# Sorts by x first; all other columns are carried along by row-indexing.
stairstep_ribbon <- function(data, direction = c("hv", "vh", "mid")) {
  direction <- match.arg(direction)
  data <- data[order(data$x), , drop = FALSE]
  n <- nrow(data)
  if (n <= 1) return(data)

  if (direction == "hv") {
    # y-levels held over each x-interval, jump at the next x (right-continuous)
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
  } else if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  } else { # mid: jump midway between adjacent x values
    xs <- rep(1:(n - 1), each = 2)
    ys <- rep(1:n, each = 2)
  }

  other_cols <- setdiff(names(data), c("x", "ymin", "ymax"))
  if (direction == "mid") {
    mid_x <- (data$x[-1] + data$x[-n]) / 2
    out <- data[c(1, xs, n), other_cols, drop = FALSE]
    out$x <- c(data$x[1], rep(mid_x, each = 2), data$x[n])
  } else {
    out <- data[xs, other_cols, drop = FALSE]
    out$x <- data$x[xs]
  }
  out$ymin <- data$ymin[ys]
  out$ymax <- data$ymax[ys]
  rownames(out) <- NULL
  out
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "geom_ribbon_step")'`
Expected: all tests PASS, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/geom_ribbon_step.R tests/testthat/test-geom_ribbon_step.R
git commit -m "feat: add stairstep_ribbon() step-expansion helper

Co-Authored-By: Codex (GPT-5) <noreply@openai.com>"
```

---

### Task 2: `StatStepRibbon` + exported `geom_ribbon_step()`

**Files:**
- Modify: `R/geom_ribbon_step.R` (append below `stairstep_ribbon()`)
- Modify: `tests/testthat/test-geom_ribbon_step.R` (append)
- Generated: `NAMESPACE` (gains `export(geom_ribbon_step)`), `man/geom_ribbon_step.Rd` — via `devtools::document()`, do not hand-edit.

**Interfaces:**
- Consumes: `stairstep_ribbon(data, direction)` from Task 1.
- Produces: exported `geom_ribbon_step(mapping = NULL, data = NULL, ..., direction = "hv", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)` returning a ggplot2 layer; internal ggproto object `StatStepRibbon`. Task 3's `show_surv()`/`show_cif()` call `geom_ribbon_step()` with the same arguments they currently give `geom_ribbon()` plus nothing else.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-geom_ribbon_step.R`:

```r
# ---- geom_ribbon_step() layer behavior ----

test_that("geom_ribbon_step() is a drop-in for geom_ribbon() with stepped output", {
  p <- ggplot(roc, aes(x, ymin = ymin, ymax = ymax)) +
    geom_ribbon_step(alpha = .3, fill = "steelblue")
  ld <- layer_data(p)
  ref <- ref_step_hv(roc)
  expect_equal(ld$x, ref$x)
  expect_equal(ld$ymin, ref$ymin)
  expect_equal(ld$ymax, ref$ymax)
  expect_equal(unique(ld$fill), "steelblue")
})

test_that("geom_ribbon_step() output does not depend on input row order", {
  set.seed(1)
  shuffled <- roc[sample(nrow(roc)), ]
  p_sorted <- ggplot(roc, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step()
  p_shuffled <- ggplot(shuffled, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step()
  expect_equal(layer_data(p_shuffled), layer_data(p_sorted))
})

test_that("geom_ribbon_step() steps each group independently", {
  two <- rbind(transform(roc, g = "a"),
               transform(roc, ymin = ymin / 2, ymax = ymax / 2, g = "b"))
  p <- ggplot(two, aes(x, ymin = ymin, ymax = ymax, fill = g)) + geom_ribbon_step()
  ld <- layer_data(p)
  for (i in 1:2) {
    ref <- ref_step_hv(two[two$g == c("a", "b")[i], ])
    grp <- ld[ld$group == i, ]
    expect_equal(grp$x, ref$x)
    expect_equal(grp$ymin, ref$ymin)
    expect_equal(grp$ymax, ref$ymax)
  }
})

test_that("geom_ribbon_step() keeps NA limits as gaps, without warnings", {
  withna <- roc
  withna$ymin[3] <- NA
  p <- ggplot(withna, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step()
  expect_silent(ld <- layer_data(p))
  # hv holds row 3 over two step segments, so its NA appears twice
  expect_equal(nrow(ld), 2 * nrow(withna) - 1)
  expect_equal(sum(is.na(ld$ymin)), 2)
})

test_that("geom_ribbon_step() supports direction = 'vh' and 'mid' at layer level", {
  p_vh <- ggplot(roc, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step(direction = "vh")
  expect_equal(layer_data(p_vh)$ymax,
               fanetc:::stairstep_ribbon(roc, direction = "vh")$ymax)
  p_mid <- ggplot(roc, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_step(direction = "mid")
  expect_equal(layer_data(p_mid)$x,
               fanetc:::stairstep_ribbon(roc, direction = "mid")$x)
})

test_that("geom_ribbon_step() rejects an invalid direction at construction", {
  expect_error(geom_ribbon_step(direction = "diagonal"))
})

test_that("geom_ribbon_step() errors informatively when ymin/ymax are not mapped", {
  p <- ggplot(roc, aes(x)) + geom_ribbon_step()
  expect_error(ggplot_build(p), "ymin")
})
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run: `Rscript -e 'devtools::test(filter = "geom_ribbon_step")'`
Expected: Task 1 tests PASS; every new test errors with `could not find function "geom_ribbon_step"`.

- [ ] **Step 3: Write the implementation**

Append to `R/geom_ribbon_step.R`:

```r
# Stat powering geom_ribbon_step(). Only "x" is declared required so the
# default Stat$compute_layer() NA-removal leaves ymin/ymax untouched: NA
# confidence limits must reach GeomRibbon un-dropped and warning-free, where
# they become gaps in the ribbon -- exactly what geom_ribbon() does with
# pre-stepped data (see the old prepare_survfit() pipeline). setup_data()
# supplies the ymin/ymax presence check instead.
StatStepRibbon <- ggproto("StatStepRibbon", Stat,
  required_aes = "x",
  setup_data = function(data, params) {
    if (!all(c("ymin", "ymax") %in% names(data))) {
      stop("geom_ribbon_step() requires the ymin and ymax aesthetics.",
           call. = FALSE)
    }
    data
  },
  compute_group = function(data, scales, direction = "hv") {
    stairstep_ribbon(data, direction = direction)
  }
)

#' @title geom_ribbon_step
#'
#' @details
#' A drop-in replacement for \code{ggplot2::geom_ribbon()} that draws the
#' ribbon as a step function instead of interpolating linearly between points,
#' for confidence bands around step curves such as Kaplan-Meier, cumulative
#' incidence or ROC curves. Within each group the data are sorted by x before
#' the steps are built, so the input row order does not matter. Rows with NA
#' ymin/ymax are kept and yield gaps in the ribbon, as in geom_ribbon().
#'
#' @param mapping,data,na.rm,show.legend,inherit.aes as in \code{ggplot2::geom_ribbon()}
#' @param ... other arguments passed on to \code{ggplot2::layer()}, e.g. the
#'   fill, alpha, colour, linetype or outline.type of the ribbon, as in
#'   \code{ggplot2::geom_ribbon()}
#' @param direction the step direction, as in \code{ggplot2::geom_step()}:
#'   "hv" (default) holds each value over the following x-interval and jumps at
#'   the next x (right-continuous, as survival and ROC curves need); "vh" jumps
#'   at the current x; "mid" jumps midway between adjacent x values
#' @return a ggplot2 layer
#' @examples
#' library(ggplot2)
#' roc <- data.frame(fpr      = c(0, .1, .25, .5, 1),
#'                   tpr      = c(.1, .3, .55, .8, 1),
#'                   tpr_low  = c(0, .2, .45, .7, .95),
#'                   tpr_high = c(.15, .4, .65, .85, 1))
#' ggplot(roc, aes(fpr, ymin = tpr_low, ymax = tpr_high)) +
#'   geom_ribbon_step(alpha = 0.3) +
#'   geom_step(aes(y = tpr))
#' @export
geom_ribbon_step<- function(mapping = NULL, data = NULL, ...,
                            direction = "hv",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  direction <- match.arg(direction, c("hv", "vh", "mid"))
  layer(
    data = data,
    mapping = mapping,
    stat = StatStepRibbon,
    geom = GeomRibbon,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(direction = direction, na.rm = na.rm, ...)
  )
}
```

- [ ] **Step 4: Regenerate docs**

Run: `Rscript -e 'devtools::document()'`
Expected: `NAMESPACE` gains `export(geom_ribbon_step)`; `man/geom_ribbon_step.Rd` is created. No roxygen warnings.

- [ ] **Step 5: Run tests to verify they pass**

Run: `Rscript -e 'devtools::test(filter = "geom_ribbon_step")'`
Expected: all tests PASS, 0 failures, 0 warnings.

- [ ] **Step 6: Commit**

```bash
git add R/geom_ribbon_step.R tests/testthat/test-geom_ribbon_step.R NAMESPACE man/geom_ribbon_step.Rd
git commit -m "feat: add geom_ribbon_step(), a step-function geom_ribbon()

Co-Authored-By: Codex (GPT-5) <noreply@openai.com>"
```

---

### Task 3: use `geom_ribbon_step()` in `show_surv()` / `show_cif()`

**Files:**
- Modify: `R/event_time_desp.R` (prepare_survfit's plot_ci_d block ~lines 218-238; its roxygen @details ~line 87-92; show_surv ribbon call ~line 531; show_cif ribbon calls ~lines 813, 825, 837 — locate by content, line numbers may have drifted)
- Create: `tests/testthat/test-show_ci_ribbon.R`
- Modify: `fanetc-cheatsheet-main.md` (add geom_ribbon_step entry; update prepare_survfit entry)
- Generated: `man/prepare_survfit.Rd` via `devtools::document()`.

**Interfaces:**
- Consumes: exported `geom_ribbon_step(...)` from Task 2 (called with the exact aes/params the old `geom_ribbon()` calls used).
- Produces: `prepare_survfit()`'s `plot_ci_d` list-column now holds RAW per-stratum tibbles with columns `time, conf_low, conf_high` sorted by time (no pre-stepping). No signature changes anywhere.

- [ ] **Step 1: Write the failing integration tests**

Create `tests/testthat/test-show_ci_ribbon.R`:

```r
# Integration tests: the CI ribbons drawn by show_surv()/show_cif() via
# geom_ribbon_step() must be identical to the old manual pre-stepping that
# lived in prepare_survfit()
# (spec: docs/superpowers/specs/2026-07-18-geom-ribbon-step-design.md).

# The old manual "hv" stepping formerly applied per stratum in prepare_survfit().
old_step_ci <- function(df) {
  nn <- nrow(df)
  ys <- rep(1:nn, each = 2)[-2 * nn]
  xs <- c(1, rep(2:nn, each = 2))
  dplyr::tibble(
    time      = df$time[xs],
    conf_low  = df$conf_low[ys],
    conf_high = df$conf_high[ys]
  )
}

ribbon_layer_data <- function(p) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)))
  expect_length(i, 1)
  layer_data(p, i)
}

fit <- estimate_km(lung, evt_time = time, evt = status, group = sex)

set.seed(42)
lung_cr <- lung
lung_cr$cr_evt <- factor(ifelse(lung$status == 2, sample(1:2, nrow(lung), TRUE, prob = c(.7, .3)), 0))
fit_cr <- estimate_cif(lung_cr, evt_time = time, evt = cr_evt, group = sex)

# raw (unstepped) per-stratum CI limits, as post-refactor prepare_survfit()
# stores them in plot_ci_d
km_ci_raw <- prepare_survfit(fit) %>%
  dplyr::select(strata, plot_ci_d) %>%
  tidyr::unnest(cols = c(plot_ci_d))

test_that("show_surv() survival-function ribbon equals the old manual stepping", {
  p <- suppressMessages(show_surv(fit, add_ci = TRUE, add_atrisk = FALSE,
                                  add_pvalue = FALSE, print_fig = FALSE))
  ld <- ribbon_layer_data(p)
  strata_levels <- levels(km_ci_raw$strata)
  expect_equal(length(unique(ld$group)), length(strata_levels))
  for (i in seq_along(strata_levels)) {
    ref <- old_step_ci(km_ci_raw[km_ci_raw$strata == strata_levels[i], ])
    grp <- ld[ld$group == i, ]
    expect_equal(grp$x, ref$time)
    expect_equal(grp$ymin, ref$conf_low)
    expect_equal(grp$ymax, ref$conf_high)
  }
})

test_that("show_surv() failure-function (plot_cdf = TRUE) ribbon equals the old pipeline", {
  p <- suppressMessages(show_surv(fit, plot_cdf = TRUE, add_ci = TRUE, add_atrisk = FALSE,
                                  add_pvalue = FALSE, print_fig = FALSE))
  ld <- ribbon_layer_data(p)
  strata_levels <- levels(km_ci_raw$strata)
  for (i in seq_along(strata_levels)) {
    stepped <- old_step_ci(km_ci_raw[km_ci_raw$strata == strata_levels[i], ])
    grp <- ld[ld$group == i, ]
    # the old pipeline stepped first, then took 1 - x and swapped low/high
    expect_equal(grp$x, stepped$time)
    expect_equal(grp$ymin, 1 - stepped$conf_high)
    expect_equal(grp$ymax, 1 - stepped$conf_low)
  }
  expect_true(all(ld$ymin <= ld$ymax, na.rm = TRUE))
})

test_that("show_cif() ribbon equals the old manual stepping", {
  p <- suppressMessages(show_cif(fit_cr, add_ci = TRUE, add_atrisk = FALSE,
                                 add_pvalue = FALSE, print_fig = FALSE))
  ld <- ribbon_layer_data(p)
  cif_ci_raw <- prepare_survfit(fit_cr) %>%
    dplyr::filter(state %in% 1) %>%  # show_cif() default evt_type = 1
    dplyr::select(strata, plot_ci_d) %>%
    tidyr::unnest(cols = c(plot_ci_d))
  strata_levels <- levels(droplevels(cif_ci_raw$strata))
  expect_equal(length(unique(ld$group)), length(strata_levels))
  for (i in seq_along(strata_levels)) {
    ref <- old_step_ci(cif_ci_raw[cif_ci_raw$strata == strata_levels[i], ])
    grp <- ld[ld$group == i, ]
    expect_equal(grp$x, ref$time)
    expect_equal(grp$ymin, ref$conf_low)
    expect_equal(grp$ymax, ref$conf_high)
  }
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::test(filter = "show_ci_ribbon")'`
Expected: FAIL — `prepare_survfit()` still returns pre-stepped `plot_ci_d`, so the reference double-steps and row counts/values mismatch (e.g. `expect_equal(grp$x, ref$time)` failures).

- [ ] **Step 3: Refactor `prepare_survfit()`**

In `R/event_time_desp.R`, replace the `plot_ci_d` mutate block (currently lines 218-237):

```r
  out <- out %>%
    dplyr::mutate(
      plot_ci_d = purrr::map(
        data,
        function(df) {
          nn <- nrow(df)
          # check http://stackoverflow.com/questions/33874909/how-do-i-add-shading-and-color-to-the-confidence-intervals-in-ggplot-2-generated
          ys <- rep(1:nn, each = 2)[-2 * nn]
          xs <- c(1, rep(2:nn, each = 2))

          df %$%
            dplyr::tibble(
              time = time[xs],
              conf_low = conf_low[ys],
              conf_high = conf_high[ys]
            ) # %>%
          # filter(!(is.na(conf_low) & is.na(conf_high)))
        }
      )
    )
```

with:

```r
  out <- out %>%
    dplyr::mutate(
      plot_ci_d = purrr::map(
        data,
        function(df) {
          # raw per-time CI limits; geom_ribbon_step() steps them at plot time
          df %>%
            dplyr::select(one_of("time", "conf_low", "conf_high")) %>%
            dplyr::arrange(time)
        }
      )
    )
```

And in `prepare_survfit()`'s roxygen block (~line 87), replace the sentence

```
#' tidy estimates (data), the step-curve coordinates including a time-0 anchor
#' (plot_prob_d), and the confidence-band coordinates (plot_ci_d). The
```

with:

```
#' tidy estimates (data), the step-curve coordinates including a time-0 anchor
#' (plot_prob_d), and the raw per-time confidence limits (plot_ci_d), which
#' show_surv() and show_cif() draw as step ribbons via geom_ribbon_step(). The
```

- [ ] **Step 4: Switch the four ribbon call sites**

In `show_surv()` (~line 531), change only the function name:

```r
    out<- out +
      geom_ribbon_step(data= plot_ci_d,
                  aes(x= time, ymin= conf_low, ymax= conf_high, fill= strata),
                  alpha= .2, show.legend = FALSE) +
      scale_pair$fill
```

In `show_cif()` (~lines 813, 825, 837), change `geom_ribbon(` to `geom_ribbon_step(` in each of the three branches, leaving every aes and argument untouched. Nothing else in either function changes.

- [ ] **Step 5: Regenerate docs**

Run: `Rscript -e 'devtools::document()'`
Expected: `man/prepare_survfit.Rd` regenerated; no other files change.

- [ ] **Step 6: Run the integration tests**

Run: `Rscript -e 'devtools::test(filter = "show_ci_ribbon")'`
Expected: all 3 tests PASS, 0 failures, 0 warnings.

- [ ] **Step 7: Run the full suite (regression gate)**

Run: `Rscript -e 'devtools::test()'`
Expected: everything green — in particular `test-show_refactor.R` (p-value placement, palettes) and `test-event_time_bugfixes.R` must not change.

- [ ] **Step 8: Update the cheatsheet**

In `fanetc-cheatsheet-main.md`, under `### Plotting` (section 2), add after the `show_surv` entry:

```markdown
#### geom_ribbon_step(mapping = NULL, data = NULL, ..., direction = "hv", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
- Drop-in replacement for `ggplot2::geom_ribbon()` that draws the ribbon as a step function (`direction` `"hv"` default / `"vh"` / `"mid"`), sorting by x within each group first; NA limits become gaps, as in `geom_ribbon()`. Used for the CI bands in `show_surv()`/`show_cif()`.
```

and replace the `prepare_survfit` description line

```markdown
- Internal-facing but exported helper: converts a `survfit`/`survfitms` object into a nested tibble (one row per stratum, plus state for `survfitms`) with step-curve and CI-band coordinates; used by `show_surv()`/`show_cif()`.
```

with:

```markdown
- Internal-facing but exported helper: converts a `survfit`/`survfitms` object into a nested tibble (one row per stratum, plus state for `survfitms`) with step-curve coordinates (`plot_prob_d`) and raw per-time CI limits (`plot_ci_d`, stepped at plot time by `geom_ribbon_step()`); used by `show_surv()`/`show_cif()`.
```

- [ ] **Step 9: Commit**

```bash
git add R/event_time_desp.R tests/testthat/test-show_ci_ribbon.R man/prepare_survfit.Rd fanetc-cheatsheet-main.md
git commit -m "refactor: draw show_surv()/show_cif() CI bands with geom_ribbon_step()

prepare_survfit() now stores raw per-time CI limits in plot_ci_d; the
stepping moved into StatStepRibbon. Ribbon output is bit-identical
(covered by test-show_ci_ribbon.R).

Co-Authored-By: Codex (GPT-5) <noreply@openai.com>"
```

---

### Task 4: full verification

**Files:** none created; may touch generated docs only if `R CMD check` flags something.

**Interfaces:**
- Consumes: the completed Tasks 1-3.
- Produces: a verified clean package state (the release gate).

- [ ] **Step 1: Run the full test suite**

Run: `Rscript -e 'devtools::test()'`
Expected: 0 failures, 0 warnings across every file.

- [ ] **Step 2: Run R CMD check**

Run: `Rscript -e 'devtools::check()'` (allow ~5-10 minutes)
Expected: **0 errors, 0 warnings, 1 NOTE** (the pre-existing environment-specific NOTE only — see `R_VERSION_COMPATIBILITY.md` context; any NEW note/warning is a regression to fix).

- [ ] **Step 3: Verify the example renders**

Run:
```bash
Rscript -e 'devtools::load_all(quiet = TRUE); library(ggplot2);
roc <- data.frame(fpr = c(0, .1, .25, .5, 1), tpr = c(.1, .3, .55, .8, 1),
                  tpr_low = c(0, .2, .45, .7, .95), tpr_high = c(.15, .4, .65, .85, 1));
p <- ggplot(roc, aes(fpr, ymin = tpr_low, ymax = tpr_high)) + geom_ribbon_step(alpha = .3) + geom_step(aes(y = tpr));
ggsave(file.path(Sys.getenv("TMPDIR", "/tmp"), "geom_ribbon_step_demo.png"), p, width = 5, height = 4, dpi = 96)'
```
Expected: PNG written with no errors/warnings; visually a stepped CI band hugging a step curve.

- [ ] **Step 4: Commit anything outstanding (should be nothing)**

Run: `git status --short` — expected clean. If check regenerated docs, commit them with message `docs: regenerate after R CMD check` plus the Co-Authored-By trailer.
