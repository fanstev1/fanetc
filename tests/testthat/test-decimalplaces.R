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
