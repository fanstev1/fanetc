suppressPackageStartupMessages({
  library(dplyr); library(forcats); library(rlang); library(tidyselect)
  library(gtsummary); library(tidyr)
})
setwd("/Users/sfan/Documents/projects/fanetc")
src <- readLines("R/fan_util_fun.R")
starts <- grep("^[a-zA-Z_.0-9]+ *<- *function", src)
dp <- grep("^decimalplaces *<- *function", src)
eval(parse(text = src[dp:(min(starts[starts > dp]) - 1)]))
source("R/desp_table_gtsummary.R")
source("R/desp_table_paired.R")

ok <- TRUE
check <- function(label, cond) {
  if (isTRUE(cond)) cat("PASS:", label, "\n")
  else { cat("FAIL:", label, "\n"); ok <<- FALSE }
}
check_err <- function(label, expr, pattern) {
  res <- tryCatch({ force(expr); NULL }, error = function(e) e)
  if (!is.null(res) && grepl(pattern, conditionMessage(res))) cat("PASS:", label, "\n")
  else { cat("FAIL:", label, "-> ", if (is.null(res)) "no error thrown" else conditionMessage(res), "\n"); ok <<- FALSE }
}

## ---- .paired_resolve_ref_level() ----

check("ref: factor default is first level",
      .paired_resolve_ref_level(factor(c("B", "A", "B"), levels = c("B", "A")), NULL, c("B", "A")) == "B")
check("ref: character default is most frequent",
      .paired_resolve_ref_level(c("x", "y", "y", "y"), NULL, c("x", "y")) == "y")
check("ref: character tie broken by first-observed",
      .paired_resolve_ref_level(c("y", "x", "y", "x"), NULL, c("x", "y")) == "y")
check("ref: logical default is sorted-first (FALSE)",
      .paired_resolve_ref_level(c(TRUE, FALSE, TRUE), NULL, c("FALSE", "TRUE")) == "FALSE")
check("ref: numeric default is sorted-first (smaller)",
      .paired_resolve_ref_level(c(2, 1, 2), NULL, c("1", "2")) == "1")
check("ref: explicit ref_group honored",
      .paired_resolve_ref_level(c("x", "y"), "y", c("x", "y")) == "y")
check_err("ref: ref_group not observed errors",
      .paired_resolve_ref_level(c("x", "y"), "z", c("x", "y")),
      "not one of the observed")

## ---- .paired_prepare_data() ----

df1 <- data.frame(pid = c(1, 1, 2, 2, 3), grp = c("A", "B", "A", "B", "A"), val = 1:5)
p1 <- .paired_prepare_data(df1, "pid", "grp", NULL)
check("prep: unpaired singleton kept in data", nrow(p1$data) == 5)
check("prep: ref/other levels resolved", p1$ref_level == "A" && p1$other_level == "B")
check("prep: group column is a 2-level factor with ref first",
      is.factor(p1$data$grp) && levels(p1$data$grp)[1] == "A")

df_missgrp <- data.frame(pid = c(1, 1, 2), grp = c("A", NA, "B"), val = 1:3)
p2 <- suppressMessages(.paired_prepare_data(df_missgrp, "pid", "grp", NULL))
check("prep: missing-group row dropped", nrow(p2$data) == 2)

df_misspid <- data.frame(pid = c(1, NA, 2, 2), grp = c("A", "A", "A", "B"), val = 1:4)
p3 <- suppressMessages(.paired_prepare_data(df_misspid, "pid", "grp", NULL))
check("prep: missing-pair_id row dropped", nrow(p3$data) == 3)

df_emptypid <- data.frame(pid = c("1", "", "2", "2"), grp = c("A", "A", "A", "B"), val = 1:4)
p4 <- suppressMessages(.paired_prepare_data(df_emptypid, "pid", "grp", NULL))
check("prep: empty-string pair_id row dropped", nrow(p4$data) == 3)

df_emptypid_f <- data.frame(pid = factor(c("1", "", "2", "2")), grp = c("A", "A", "A", "B"), val = 1:4)
p4f <- suppressMessages(.paired_prepare_data(df_emptypid_f, "pid", "grp", NULL))
check("prep: empty-level FACTOR pair_id row dropped too", nrow(p4f$data) == 3)

check_err("prep: 3 group levels errors",
      .paired_prepare_data(data.frame(pid = 1:3, grp = c("A", "B", "C"), val = 1:3), "pid", "grp", NULL),
      "exactly 2 observed levels")
check_err("prep: 1 group level errors",
      .paired_prepare_data(data.frame(pid = 1:3, grp = c("A", "A", "A"), val = 1:3), "pid", "grp", NULL),
      "exactly 2 observed levels")
check_err("prep: all-missing group errors (0 levels)",
      .paired_prepare_data(data.frame(pid = 1:3, grp = c(NA, NA, NA), val = 1:3), "pid", "grp", NULL),
      "exactly 2 observed levels")
check_err("prep: duplicate pair member errors",
      .paired_prepare_data(data.frame(pid = c(1, 1, 1), grp = c("A", "A", "B"), val = 1:3), "pid", "grp", NULL),
      "Duplicate pair ID")
check_err("prep: bad ref_group errors",
      .paired_prepare_data(df1, "pid", "grp", "Z"),
      "not one of the observed")

check("prep: pair_id as character works",
      { d <- df1; d$pid <- as.character(d$pid); nrow(.paired_prepare_data(d, "pid", "grp", NULL)$data) == 5 })
check("prep: pair_id as factor works",
      { d <- df1; d$pid <- factor(d$pid); nrow(.paired_prepare_data(d, "pid", "grp", NULL)$data) == 5 })

if (ok) cat("\nALL PASS\n") else { cat("\nFAILURES PRESENT\n"); quit(status = 1) }
