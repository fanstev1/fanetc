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
