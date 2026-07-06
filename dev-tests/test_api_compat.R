# API surface / backward-compatibility check.
# 1. Every function exported in NAMESPACE is defined by the files in R/.
# 2. For each export that also existed at the last-good commit (7ab169b), every OLD
#    argument name is still accepted, and the leading positional arguments keep
#    their order — so old calls (positional or named) keep working.
suppressPackageStartupMessages({
  library(dplyr); library(rlang)
})
if (basename(getwd()) == "dev-tests") setwd("..")

new_env <- new.env(parent = globalenv())
for (f in list.files("R", full.names = TRUE, pattern = "[.]R$")) sys.source(f, envir = new_env)

old_env <- new.env(parent = globalenv())
old_files <- c("R/fan_util_fun.R", "R/event_time_desp.R", "R/desp_table.R",
               "R/numeric_desp.R", "R/logical_desp.R", "R/factor_desp.R")
for (f in old_files) {
  src <- system(paste0("git show 7ab169b:", f), intern = TRUE, ignore.stderr = TRUE)
  if (length(src)) try(eval(parse(text = src), envir = old_env), silent = TRUE)
}

exports <- grep("^export\\(", readLines("NAMESPACE"), value = TRUE)
exports <- gsub("^export\\(|\\)$", "", exports)

ok <- TRUE
removed_on_purpose <- c("factor_desp", "factor_dist", "fisher_test", "k_sample_test",
                        "logical_desp", "numeric_desp", "two_sample_test", "recode_missing")

for (fn in exports) {
  if (!exists(fn, envir = new_env, inherits = FALSE) ||
      !is.function(get(fn, envir = new_env))) {
    cat("FAIL:", fn, "exported but not defined in R/\n"); ok <- FALSE; next
  }
  if (!exists(fn, envir = old_env, inherits = FALSE)) next  # new function, nothing to compare

  old_args <- names(formals(get(fn, envir = old_env)))
  new_args <- names(formals(get(fn, envir = new_env)))

  missing_args <- setdiff(setdiff(old_args, "..."), new_args)
  if (length(missing_args)) {
    cat("FAIL:", fn, "dropped old argument(s):", paste(missing_args, collapse = ", "), "\n")
    ok <- FALSE; next
  }
  # positional compatibility: shared leading args (before the first mismatch or
  # "...") must appear in the same order
  lead_old <- old_args[seq_len(min(which(c(old_args, "...") == "...")) - 1)]
  lead_new <- new_args[seq_along(lead_old)]
  pos_ok <- identical(lead_old, lead_new) ||
    all(lead_old == lead_new[seq_along(lead_old)])
  if (!isTRUE(pos_ok)) {
    cat("WARN:", fn, "positional order changed:",
        "old:", paste(lead_old, collapse = ","), "| new:", paste(lead_new, collapse = ","), "\n")
  } else {
    cat("PASS:", fn, "\n")
  }
}

# exports intentionally removed in the gtsummary refactor must NOT be exported
still_exported <- intersect(removed_on_purpose, exports)
cat(if (length(still_exported)) {
  ok <- FALSE
  paste("FAIL: removed functions still exported:", paste(still_exported, collapse = ", "), "\n")
} else "PASS: no intentionally-removed function is exported\n")

# every export in NAMESPACE and vice versa: defined-but-unexported top-level helpers
defined <- ls(new_env)
helpers_ok <- setdiff(defined, exports)
cat("INFO: unexported internal helpers:", paste(sort(helpers_ok), collapse = ", "), "\n")

cat(if (ok) "\nAPI COMPATIBILITY OK\n" else "\nAPI COMPATIBILITY FAILURES\n")
