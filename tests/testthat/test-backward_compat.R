# Converted from dev-tests/test_backward_compat.R.
# Checks that table_one()'s old call patterns (positional group, positional/named
# datadic, custom var_name/var_desp columns as symbols/strings) still work
# unchanged after the gtsummary refactor, alongside the new-style calls.

set.seed(0)
df <- data.frame(
  age = round(rnorm(150, 50, 12), 1),
  income = round(100 * (rnorm(150) + 5)),
  sex = factor(sample(c("F", "M"), 150, TRUE)),
  dm = sample(c(TRUE, FALSE), 150, TRUE)
)

# datadic in the OLD default column layout
datadic <- data.frame(var_name = c("age", "income", "sex", "dm"),
                      var_desp = c("Age (years)", "Household income", "Sex", "Diabetes"),
                      stringsAsFactors = FALSE)
# datadic with CUSTOM column names (old var_name/var_desp arguments)
dic2 <- data.frame(nm = datadic$var_name, lbl = datadic$var_desp, stringsAsFactors = FALSE)

expect_has_label <- function(res, label) {
  expect_true(label %in% as_tibble(res)[[1]])
}

test_that("old: positional group table_one(df, sex) runs without error", {
  expect_no_error(suppressWarnings(table_one(df, sex)))
})

test_that("old: named datadic table_one(df, sex, datadic = datadic)", {
  res <- suppressWarnings(table_one(df, sex, datadic = datadic))
  expect_has_label(res, "Household income")
})

test_that("old: positional datadic table_one(df, sex, datadic)", {
  res <- suppressWarnings(table_one(df, sex, datadic))
  expect_has_label(res, "Household income")
})

test_that("old: custom cols (symbols) table_one(df, sex, dic2, nm, lbl)", {
  res <- suppressWarnings(table_one(df, sex, dic2, nm, lbl))
  expect_has_label(res, "Household income")
})

test_that("old: custom cols (named) datadic = dic2, var_name = nm, var_desp = lbl", {
  res <- suppressWarnings(table_one(df, sex, datadic = dic2, var_name = nm, var_desp = lbl))
  expect_has_label(res, "Household income")
})

test_that("old: custom cols (strings) var_name = 'nm', var_desp = 'lbl'", {
  res <- suppressWarnings(table_one(df, sex, datadic = dic2, var_name = "nm", var_desp = "lbl"))
  expect_has_label(res, "Household income")
})

test_that("old: no group with datadic table_one(df, datadic = datadic)", {
  res <- suppressWarnings(table_one(df, datadic = datadic))
  expect_has_label(res, "Age (years)")
})

test_that("bad datadic columns fail with an informative error", {
  expect_error(table_one(df, sex, datadic = dic2), "must contain columns")
})

test_that("new: include named table_one(df, group = sex, include = c(age, dm))", {
  expect_no_error(suppressWarnings(table_one(df, group = sex, include = c(age, dm))))
})

test_that("new: everything default table_one(df, group = sex)", {
  expect_no_error(suppressWarnings(table_one(df, group = sex)))
})
