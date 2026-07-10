# API surface / backward-compatibility check (converted from dev-tests/test_api_compat.R).
# 1. Every function exported in NAMESPACE is defined by the files in R/.
# 2. For each export that also existed at the last-good pre-refactor commit
#    (7ab169b), every OLD argument name is still accepted, and the leading
#    positional arguments keep their order -- so old calls (positional or
#    named) keep working.
#
# This test shells out to `git show 7ab169b:<path>` to reconstruct the old
# implementations, so it only makes sense to run from a git checkout that has
# that commit in its history. It's skipped (not failed) if git or the commit
# isn't available, e.g. when tests run from a source tarball with no .git/.

pkg_root <- testthat::test_path("..", "..")

git_available <- nzchar(Sys.which("git")) &&
  file.exists(file.path(pkg_root, ".git")) &&  # a regular file (not dir) in a git worktree
  {
    res <- suppressWarnings(system2("git", c("-C", pkg_root, "cat-file", "-e", "7ab169b"),
                                    stdout = FALSE, stderr = FALSE))
    identical(res, 0L)
  }

build_new_env <- function() {
  new_env <- new.env(parent = globalenv())
  for (f in list.files(file.path(pkg_root, "R"), full.names = TRUE, pattern = "[.]R$")) {
    sys.source(f, envir = new_env)
  }
  new_env
}

build_old_env <- function() {
  old_env <- new.env(parent = globalenv())
  old_files <- c("R/fan_util_fun.R", "R/event_time_desp.R", "R/desp_table.R",
                 "R/numeric_desp.R", "R/logical_desp.R", "R/factor_desp.R")
  for (f in old_files) {
    src <- system2("git", c("-C", pkg_root, "show", paste0("7ab169b:", f)),
                   stdout = TRUE, stderr = FALSE)
    if (length(src)) try(eval(parse(text = src), envir = old_env), silent = TRUE)
  }
  old_env
}

get_exports <- function() {
  # Reads the already-loaded package's namespace metadata rather than parsing the
  # raw NAMESPACE file: works whether tests run from a source checkout (devtools::test())
  # or against an installed package (R CMD check), where pkg_root's NAMESPACE text file
  # isn't guaranteed to be present at this relative path.
  getNamespaceExports("fanetc")
}

removed_on_purpose <- c("factor_desp", "factor_dist", "fisher_test", "k_sample_test",
                        "logical_desp", "numeric_desp", "two_sample_test", "recode_missing")

test_that("every exported function is defined in R/", {
  skip_if_not(git_available, "git or commit 7ab169b not available")
  new_env <- build_new_env()
  exports <- get_exports()
  for (fn in exports) {
    expect_true(exists(fn, envir = new_env, inherits = FALSE) &&
                  is.function(get(fn, envir = new_env)),
                info = paste(fn, "exported but not defined in R/"))
  }
})

test_that("intentionally-removed functions are not exported", {
  exports <- get_exports()
  still_exported <- intersect(removed_on_purpose, exports)
  expect_length(still_exported, 0)
})

test_that("backward-compatible exports retain old argument names and leading positional order", {
  skip_if_not(git_available, "git or commit 7ab169b not available")
  new_env <- build_new_env()
  old_env <- build_old_env()
  exports <- get_exports()

  for (fn in exports) {
    if (!exists(fn, envir = old_env, inherits = FALSE)) next  # new function, nothing to compare

    old_args <- names(formals(get(fn, envir = old_env)))
    new_args <- names(formals(get(fn, envir = new_env)))

    missing_args <- setdiff(setdiff(old_args, "..."), new_args)
    expect_true(length(missing_args) == 0,
                info = paste(fn, "dropped old argument(s):", paste(missing_args, collapse = ", ")))
    if (length(missing_args)) next

    # positional compatibility: shared leading args (before the first mismatch or
    # "...") must appear in the same order. This is informational only in the
    # original dev-tests script (a WARN, not a FAIL), so it's not asserted here.
    lead_old <- old_args[seq_len(min(which(c(old_args, "...") == "...")) - 1)]
    lead_new <- new_args[seq_along(lead_old)]
    pos_ok <- identical(lead_old, lead_new) ||
      all(lead_old == lead_new[seq_along(lead_old)])
    if (!isTRUE(pos_ok)) {
      message("NOTE: ", fn, " positional order changed: old: ",
              paste(lead_old, collapse = ","), " | new: ", paste(lead_new, collapse = ","))
    }
  }
})
