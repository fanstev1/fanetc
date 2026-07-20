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
