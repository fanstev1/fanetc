suppressPackageStartupMessages({
  library(dplyr); library(forcats); library(rlang); library(tidyselect); library(gtsummary)
})
setwd("/Users/sfan/Documents/projects/fanetc")
src <- readLines("R/fan_util_fun.R")
starts <- grep("^[a-zA-Z_.0-9]+ *<- *function", src)
dp <- grep("^decimalplaces *<- *function", src)
eval(parse(text = src[dp:(min(starts[starts > dp]) - 1)]))
source("R/desp_table_gtsummary.R")

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

ok <- TRUE
check <- function(label, expr, expect_label = NULL) {
  res <- tryCatch(suppressWarnings(expr), error = function(e) e)
  if (inherits(res, "error")) {
    cat("FAIL:", label, "->", conditionMessage(res), "\n"); ok <<- FALSE
  } else if (!is.null(expect_label) &&
             !expect_label %in% as_tibble(res)[[1]]) {
    cat("FAIL:", label, "-> label", shQuote(expect_label), "not found in output\n"); ok <<- FALSE
  } else cat("PASS:", label, "\n")
  invisible(res)
}

# Old call patterns (from obsolete desp_table.R examples & summary docs)
check("old: positional group        table_one(df, sex)", table_one(df, sex))
check("old: named datadic           table_one(df, sex, datadic = datadic)",
      table_one(df, sex, datadic = datadic), expect_label = "Household income")
check("old: positional datadic      table_one(df, sex, datadic)",
      table_one(df, sex, datadic), expect_label = "Household income")
check("old: custom cols (symbols)   table_one(df, sex, dic2, nm, lbl)",
      table_one(df, sex, dic2, nm, lbl), expect_label = "Household income")
check("old: custom cols (named)     table_one(df, sex, datadic = dic2, var_name = nm, var_desp = lbl)",
      table_one(df, sex, datadic = dic2, var_name = nm, var_desp = lbl),
      expect_label = "Household income")
check("old: custom cols (strings)   var_name = 'nm', var_desp = 'lbl'",
      table_one(df, sex, datadic = dic2, var_name = "nm", var_desp = "lbl"),
      expect_label = "Household income")
check("old: no group with datadic   table_one(df, datadic = datadic)",
      table_one(df, datadic = datadic), expect_label = "Age (years)")

# Bad datadic columns should fail with a clear message
res <- tryCatch(table_one(df, sex, datadic = dic2), error = function(e) e)
cat(if (inherits(res, "error") && grepl("must contain columns", conditionMessage(res)))
      "PASS: informative error for wrong datadic columns\n"
    else {ok <- FALSE; "FAIL: expected informative datadic column error\n"})

# New-style calls must still work unchanged
check("new: include named           table_one(df, group = sex, include = c(age, dm))",
      table_one(df, group = sex, include = c(age, dm)))
check("new: everything default      table_one(df, group = sex)", table_one(df, group = sex))

cat(if (ok) "\nALL PASS\n" else "\nFAILURES PRESENT\n")
