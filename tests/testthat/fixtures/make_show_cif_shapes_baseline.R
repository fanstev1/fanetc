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
