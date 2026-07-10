suppressPackageStartupMessages({
  library(dplyr)
  library(forcats)
  library(rlang)
  library(tidyselect)
  library(gtsummary)
})

setwd("/Users/sfan/Documents/projects/fanetc")

# decimalplaces() lives in fan_util_fun.R; source only that function to avoid
# pulling in heavy deps
src <- readLines("R/fan_util_fun.R")
# decimalplaces is lines ~11-35; find its start and next top-level function
starts <- grep("^[a-zA-Z_.0-9]+ *<- *function", src)
dp_start <- grep("^decimalplaces *<- *function", src)
dp_end <- min(starts[starts > dp_start]) - 1
eval(parse(text = src[dp_start:dp_end]))

source("R/desp_table_gtsummary.R")

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

run_test <- function(label, expr) {
  cat("\n=== ", label, " ===\n", sep = "")
  res <- tryCatch(expr, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n"); NULL
  }, warning = function(w) {
    cat("WARNING:", conditionMessage(w), "\n")
    suppressWarnings(withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning")))
  })
  if (!is.null(res) && inherits(res, "gtsummary")) {
    print(as_tibble(res), n = 40)
  }
  invisible(res)
}

run_test("1. Basic usage: table_one(df)", table_one(df))
run_test("2. Grouped 2 levels: table_one(df, group = sex)", table_one(df, group = sex))
run_test("3. Grouped 3 levels: table_one(df, group = arm)", table_one(df, group = arm))
run_test("4. mediqr: table_one(df, group = sex, continuous_stat = 'mediqr')",
         table_one(df, group = sex, continuous_stat = "mediqr"))
run_test("5. include: table_one(df, group = sex, include = c(age, stage))",
         table_one(df, group = sex, include = c(age, stage)))

datadic <- data.frame(
  var_name = c("age", "weight", "stage", "diabetic", "sex", "arm"),
  var_desp = c("Age (years)", "Weight (kg)", "Tumor stage", "Diabetes", "Sex", "Treatment arm")
)
run_test("6. datadic: table_one(df, group = sex, datadic = datadic)",
         table_one(df, group = sex, datadic = datadic))

run_test("7. keep missing group level: missing_group_exclude = FALSE",
         table_one(df, group = sex, missing_group_exclude = FALSE))

run_test("8. sort_by_p = TRUE", table_one(df, group = sex, sort_by_p = TRUE))

run_test("9. format_pvalue spot checks", {
  cat(format_pvalue(c(0.5, 0.234, 0.1994, 0.05, 0.0012, 0.0009, NA)), "\n")
  NULL
})
