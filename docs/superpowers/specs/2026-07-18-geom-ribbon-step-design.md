# geom_ribbon_step() design

**Date:** 2026-07-18
**Status:** Approved for implementation

## Goal

Add `geom_ribbon_step()` to fanetc: a drop-in replacement for
`ggplot2::geom_ribbon()` that draws the ribbon as a step function, then use it
inside `show_surv()` and `show_cif()` to replace the manual step-ribbon
construction currently buried in `prepare_survfit()`.

Motivating use case: confidence bands around step curves (ROC CI bands,
Kaplan-Meier / cumulative-incidence CI bands), where the band must be held
constant over each x-interval instead of interpolated linearly.

## Background

The package already implements the "hv" stepping trick manually in
`prepare_survfit()` (`R/event_time_desp.R:220-237`): for `n` sorted rows it
builds index vectors

```r
ys <- rep(1:nn, each = 2)[-2 * nn]   # value held over each interval
xs <- c(1, rep(2:nn, each = 2))      # x repeated to make the vertical jump
```

and materializes a pre-stepped `plot_ci_d` tibble per stratum, which
`show_surv()` and `show_cif()` then draw with a plain `geom_ribbon()`. This
works but forces every call site to pre-transform its data and handle grouping
manually.

## Design

### New file: `R/geom_ribbon_step.R`

Three components:

1. **`stairstep_ribbon(data, direction)`** — internal helper. Generalizes the
   index trick above to both `ymin` and `ymax`, mirroring ggplot2's internal
   `stairstep()` logic for all three directions:
   - `"hv"` (default): value held over each x-interval, jump at the next x
     (right-continuous; correct for ROC and survival curves). Indices exactly
     as in the Background section.
   - `"vh"`: jump at the current x, then horizontal (indices swapped).
   - `"mid"`: jumps at midpoints between consecutive x values.

   All other columns (`fill`, `colour`, `group`, `alpha`, `PANEL`, ...) are
   carried along by row-indexing so grouped and faceted plots work. Groups with
   0 or 1 rows are returned unchanged (nothing to step).

2. **`StatStepRibbon`** — internal ggproto Stat.
   - `required_aes = c("x", "ymin", "ymax")`.
   - `compute_group()` **sorts by `x` first** (input order must not matter),
     then calls `stairstep_ribbon()`. ggplot2 splits by group/panel before
     `compute_group()` runs, so each series is stepped independently.

3. **`geom_ribbon_step()`** — the only exported symbol. Signature mirrors
   `geom_ribbon()`:

   ```r
   geom_ribbon_step(mapping = NULL, data = NULL, ...,
                    direction = "hv", na.rm = FALSE,
                    show.legend = NA, inherit.aes = TRUE)
   ```

   Builds `layer(stat = StatStepRibbon, geom = GeomRibbon, ...)`, so all of
   `geom_ribbon()`'s visual parameters (`fill`, `alpha`, `colour`, `linetype`,
   `outline.type`, ...) pass through unchanged. Example usage:

   ```r
   ggplot(roc, aes(fpr, ymin = tpr_low, ymax = tpr_high)) +
     geom_ribbon_step(alpha = 0.3, fill = "steelblue") +
     geom_step(aes(y = tpr))
   ```

### Error handling

- Invalid `direction` rejected via `match.arg(direction, c("hv", "vh", "mid"))`.
- Missing `ymin`/`ymax` aesthetics produce ggplot2's standard required-aesthetic
  error (via `required_aes`).
- `na.rm` is forwarded to the layer as usual; `GeomRibbon` handles NA rows the
  same way `geom_ribbon()` does.

### Deliberate limitation (YAGNI)

No `orientation`/flipped-aes support (no horizontal ribbons via
`y`/`xmin`/`xmax`). ROC and survival bands never need it; it can be added later
without breaking the API.

### Refactor: `show_surv()` / `show_cif()` use the new geom

`prepare_survfit()` and `plot_ci_d` have no consumers outside
`R/event_time_desp.R`, so the change is contained:

1. **`prepare_survfit()`**: the `plot_ci_d` mapping keeps only the column
   selection — plain sorted `time, conf_low, conf_high` per stratum. The manual
   `ys`/`xs` stepping is deleted (it now lives in `StatStepRibbon`).
2. **`show_surv()`** (`R/event_time_desp.R:531`) and **`show_cif()`**
   (`R/event_time_desp.R:813,825,837`): `geom_ribbon()` becomes
   `geom_ribbon_step()`. Aesthetics and grouping are unchanged; the stat steps
   each strata/state group independently.

Safety notes established during review:

- `show_surv(plot_cdf = TRUE)` transforms the CI columns (`1 - x` plus
  low/high swap) after unnesting; that transform commutes with stepping, so
  moving the stepping later preserves output exactly.
- The survfit data is already sorted by time today; the stat's explicit sort
  makes that guarantee structural instead of incidental.

### Package plumbing

- Roxygen docs following package conventions (markdown **off** — escape any
  literal `%` as `\%`); `@export` only `geom_ribbon_step`.
- `devtools::document()` to regenerate NAMESPACE and Rd files.
- New Rd page gets an example; keep `R CMD check` at 0 errors / 0 warnings.

## Testing

New file `tests/testthat/test-geom_ribbon_step.R` plus additions for the show
functions. The ribbon path of `show_surv()`/`show_cif()` currently has **no**
coverage (all existing tests pass `add_ci = FALSE`).

Unit tests on the geom (via `ggplot2::layer_data()` on small fixtures):

1. `"hv"` output matches the original `step_ribbon()` index logic on a small
   ROC-style fixture; row count is `2n - 1`.
2. `"vh"` and `"mid"` produce the expected coordinates.
3. **Unsorted input yields output identical to sorted input** (the sort-first
   requirement).
4. Two groups in one layer (mapped `fill`/`group`) are stepped independently.
5. 0- and 1-row groups pass through without error.
6. Invalid `direction` errors.

Integration tests on the show functions:

7. `show_surv(add_ci = TRUE)` and `show_cif(add_ci = TRUE)` on multi-stratum
   fits built the same way `test-show_refactor.R` builds them
   (`estimate_km(lung, ...)` and `estimate_cif()` on a seeded competing-risk
   version of `survival::lung`): the ribbon layer's `x`/`ymin`/`ymax` from
   `layer_data()` are **identical** to the old manual pre-stepping, recomputed
   independently inside the test with the original index logic.
8. `show_surv()` equivalence in test 7 is run for **both** curve types:
   - survival function (`plot_cdf = FALSE`): ribbon matches the old manual
     stepping of `conf_low`/`conf_high` directly;
   - failure function (`plot_cdf = TRUE`): ribbon matches the old pipeline's
     output — manual stepping combined with the `1 - x` transform and
     low/high swap — and stays valid (`ymin <= ymax` everywhere).
9. The existing suite (p-value placement, palettes, y_lim behavior) stays
   green.

Final verification: `devtools::test()`, then `R CMD check` clean.
