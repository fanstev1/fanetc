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
