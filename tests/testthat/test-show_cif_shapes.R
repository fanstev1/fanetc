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
