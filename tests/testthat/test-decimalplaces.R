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

test_that("decimalplaces() is robust to non-finite input (rewrite-only edge, documented in HANDOFF.md)", {
  # Pre-rewrite: any((y %% 1) != 0) hit NA for Inf/NaN fractional parts and
  # errored ("missing value where TRUE/FALSE needed"). No call site in this
  # package ever passes such input; the rewrite returns a value instead.
  expect_identical(decimalplaces(Inf), 0L)
  expect_identical(decimalplaces(-Inf), 0L)
  expect_identical(decimalplaces(NaN), 0L)
  # Inf/NaN mixed with real fractional values does not disturb the mode count
  expect_identical(decimalplaces(c(1.5, Inf)), 1L)
})

test_that("decimalplaces() coerces a non-integer max_dec to integer (rewrite-only edge)", {
  # Pre-rewrite: pmin.int(max_dec, ...) with a fractional max_dec (an
  # unintended input -- "decimal places" cannot be fractional) could return a
  # fractional result, e.g. pmin.int(2.9, 5) == 2.9. as.integer(max_dec) now
  # truncates first. No call site in this package ever passes a non-integer
  # max_dec (only the 4L default and integer literals appear).
  expect_identical(decimalplaces(1.234, max_dec = 2.9), 2L)
})
