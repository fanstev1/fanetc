# Converted from dev-tests/test_table_one.R.
# The original script was a "run and print" smoke test (no PASS/FAIL tracking,
# just eyeballed output via run_test()/print(as_tibble(res))); the only implicit
# check was "does this call pattern run without error". Preserved here as
# expect_no_error() for each usage pattern, plus a real assertion added for the
# format_pvalue() spot check (which previously only printed its output).

set.seed(0)
df <- data.frame(
  age = round(rnorm(150, mean = 50, sd = 12), 1),
  weight = round(rnorm(150, mean = 72, sd = 15)),
  stage = factor(sample(c("I", "II", "III"), 150, replace = TRUE)),
  diabetic = sample(c(TRUE, FALSE), 150, replace = TRUE),
  sex = factor(sample(c("F", "M"), 150, replace = TRUE)),
  arm = factor(sample(c("A", "B", "C"), 150, replace = TRUE))
)
df$age[c(3, 10)] <- NA
df$stage[5] <- NA
df$sex[c(7, 20)] <- NA

test_that("1. basic usage: table_one(df)", {
  expect_no_error(suppressWarnings(table_one(df)))
})

test_that("2. grouped 2 levels: table_one(df, group = sex)", {
  expect_no_error(suppressWarnings(table_one(df, group = sex)))
})

test_that("3. grouped 3 levels: table_one(df, group = arm)", {
  expect_no_error(suppressWarnings(table_one(df, group = arm)))
})

test_that("4. mediqr: table_one(df, group = sex, continuous_stat = 'mediqr')", {
  expect_no_error(suppressWarnings(table_one(df, group = sex, continuous_stat = "mediqr")))
})

test_that("5. include: table_one(df, group = sex, include = c(age, stage))", {
  expect_no_error(suppressWarnings(table_one(df, group = sex, include = c(age, stage))))
})

test_that("6. datadic: table_one(df, group = sex, datadic = datadic)", {
  datadic <- data.frame(
    var_name = c("age", "weight", "stage", "diabetic", "sex", "arm"),
    var_desp = c("Age (years)", "Weight (kg)", "Tumor stage", "Diabetes", "Sex", "Treatment arm")
  )
  expect_no_error(suppressWarnings(table_one(df, group = sex, datadic = datadic)))
})

test_that("7. keep missing group level: missing_group_exclude = FALSE", {
  expect_no_error(suppressWarnings(table_one(df, group = sex, missing_group_exclude = FALSE)))
})

test_that("8. sort_by_p = TRUE", {
  expect_no_error(suppressWarnings(table_one(df, group = sex, sort_by_p = TRUE)))
})

test_that("9. format_pvalue spot checks", {
  expect_equal(
    format_pvalue(c(0.5, 0.234, 0.1994, 0.05, 0.0012, 0.0009, NA)),
    c("0.50", "0.23", "0.199", "0.050", "0.001", "<0.001", "---")
  )
})
