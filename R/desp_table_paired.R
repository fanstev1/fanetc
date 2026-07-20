#---- table_one_paired(): descriptive tables for paired/matched data ----

.paired_resolve_ref_level <- function(group_vals, ref_group, observed) {
  if (!is.null(ref_group)) {
    ref_chr <- as.character(ref_group)
    if (!ref_chr %in% observed) {
      stop("`ref_group` (", ref_chr, ") is not one of the observed `group` levels: ",
           paste(observed, collapse = ", "))
    }
    return(ref_chr)
  }

  if (is.factor(group_vals)) return(observed[1])

  if (is.character(group_vals)) {
    first_seen <- unique(group_vals)
    counts <- table(group_vals)[first_seen]
    return(first_seen[which.max(counts)])
  }

  # logical or numeric: sorted-first (FALSE before TRUE; smaller number first)
  as.character(sort(unique(group_vals))[1])
}

.paired_prepare_data <- function(df, pair_id_name, group_name, ref_group) {
  df <- as.data.frame(df)

  group_vals <- df[[group_name]]
  miss_group <- is.na(group_vals)
  if (any(miss_group)) {
    message(sum(miss_group), " row(s) with missing `", group_name,
            "` dropped (a missing group value cannot be paired).")
    df <- df[!miss_group, , drop = FALSE]
  }

  pid_vals <- df[[pair_id_name]]
  # work on the character representation so this catches empty strings
  # regardless of whether pair_id is character, factor, or numeric
  pid_chr <- as.character(pid_vals)
  miss_pid <- is.na(pid_vals) | (!is.na(pid_chr) & pid_chr == "")
  if (any(miss_pid)) {
    message(sum(miss_pid), " row(s) with missing/empty `", pair_id_name,
            "` dropped (a missing pair ID cannot be paired).")
    df <- df[!miss_pid, , drop = FALSE]
  }

  group_vals <- df[[group_name]]
  if (is.factor(group_vals)) group_vals <- droplevels(group_vals)
  observed <- if (is.factor(group_vals)) levels(group_vals) else sort(unique(as.character(group_vals)))
  if (length(observed) != 2L) {
    stop("`group` must have exactly 2 observed levels after dropping missing rows; found ",
         length(observed), ": ", paste(observed, collapse = ", "))
  }

  ref <- .paired_resolve_ref_level(group_vals, ref_group, observed)
  other <- setdiff(observed, ref)
  df[[group_name]] <- factor(as.character(group_vals), levels = c(ref, other))

  dup_mask <- duplicated(df[c(pair_id_name, group_name)])
  if (any(dup_mask)) {
    stop("Each pair ID may have at most one row per `group` level. Duplicate pair ID(s): ",
         paste(unique(df[[pair_id_name]][dup_mask]), collapse = ", "))
  }

  list(data = df, ref_level = ref, other_level = other)
}

.paired_wide <- function(paired_df, pair_id_name, group_name, ref_level, other_level, var) {
  wide <- tidyr::pivot_wider(
    paired_df[c(pair_id_name, group_name, var)],
    id_cols = dplyr::all_of(pair_id_name),
    names_from = dplyr::all_of(group_name),
    values_from = dplyr::all_of(var)
  )
  names(wide)[names(wide) == ref_level]   <- ".ref"
  names(wide)[names(wide) == other_level] <- ".other"
  wide[stats::complete.cases(wide[c(".ref", ".other")]), , drop = FALSE]
}

# run a paired-test expression: warnings muffled, errors -> NA, and NaN from
# degenerate tests (e.g. zero-discordant-pair denominators) normalized to
# NA_real_, as the design specifies
.paired_safe_pvalue <- function(expr) {
  p <- tryCatch(
    withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) NA_real_
  )
  if (!is.finite(p)) NA_real_ else p
}

.paired_make_cont_test_fn <- function(paired_df, pair_id_name, group_name, ref_level, other_level, continuous_stat) {
  function(data, variable, by, ...) {
    wide <- .paired_wide(paired_df, pair_id_name, group_name, ref_level, other_level, variable)
    p <- .paired_safe_pvalue(
      if (continuous_stat == "meansd") {
        stats::t.test(wide$.other, wide$.ref, paired = TRUE)$p.value
      } else {
        stats::wilcox.test(wide$.other, wide$.ref, paired = TRUE)$p.value
      }
    )
    dplyr::tibble(p.value = p)
  }
}

.paired_make_cat_test_fn <- function(paired_df, pair_id_name, group_name, ref_level, other_level) {
  function(data, variable, by, ...) {
    wide <- .paired_wide(paired_df, pair_id_name, group_name, ref_level, other_level, variable)
    p <- .paired_safe_pvalue({
      lv <- union(as.character(unique(wide$.ref)), as.character(unique(wide$.other)))
      ref_f   <- factor(as.character(wide$.ref),   levels = lv)
      other_f <- factor(as.character(wide$.other), levels = lv)
      stats::mcnemar.test(table(ref_f, other_f))$p.value
    })
    dplyr::tibble(p.value = p)
  }
}

.paired_make_n_pairs_fn <- function(paired_df, pair_id_name, group_name, ref_level, other_level) {
  function(data, variable, by, ...) {
    wide <- .paired_wide(paired_df, pair_id_name, group_name, ref_level, other_level, variable)
    dplyr::tibble(n_pairs = formatC(nrow(wide), format = "d"))
  }
}

.paired_make_smd_fn <- function(paired_df, pair_id_name, group_name, ref_level, other_level, pairing_method) {
  function(data, variable, by, ...) {
    wide <- .paired_wide(paired_df, pair_id_name, group_name, ref_level, other_level, variable)
    x_var <- paired_df[[variable]]
    is_cont <- is.numeric(x_var)

    val <- if (pairing_method == "repeated_measure" && is_cont) {
      if (nrow(wide) < 2) {
        NA_real_
      } else {
        diff <- wide$.other - wide$.ref
        sd_diff <- stats::sd(diff)
        if (is.na(sd_diff) || isTRUE(sd_diff == 0)) NA_real_ else mean(diff) / sd_diff
      }
    } else {
      if (nrow(wide) == 0) {
        NA_real_
      } else {
        x <- c(wide$.ref, wide$.other)
        g <- factor(c(rep(ref_level, nrow(wide)), rep(other_level, nrow(wide))), levels = c(ref_level, other_level))
        res <- tryCatch(smd::smd(x = x, g = g, gref = 1L), error = function(e) NULL)
        # smd::smd() returns a SIGNED estimate ("reference minus other", the
        # opposite of this design's "non-reference minus reference" convention)
        # only for numeric (continuous) and logical x -- logical is coerced to
        # 0/1 internally. For factor/character x of ANY level count (including
        # 2 levels), smd::smd() instead returns the Yang-Dalton Mahalanobis
        # distance, which is gref-invariant and always non-negative (verified
        # empirically: identical estimate under gref=1 and gref=2 for both
        # 2-level and 3+-level factor/character variables) -- negating that
        # would produce a spurious negative "SMD," so it is left as-is.
        signed_estimate <- is_cont || is.logical(x_var)
        if (is.null(res)) NA_real_ else if (signed_estimate) -res$estimate[1] else res$estimate[1]
      }
    }

    dec <- if (is_cont) max(1L, decimalplaces(x_var)) else 1L
    dplyr::tibble(smd = if (is.na(val)) "---" else formatC(val, digits = dec, format = "f"))
  }
}

#' @title table_one_paired
#'
#' @description
#' Descriptive table for paired/matched data: a companion to \code{table_one()} for
#' data in long format with a pair-ID column and a grouping variable with exactly
#' 2 observed levels (e.g. before/after measurements, or 1:1 matched cohorts).
#' Adds paired-appropriate p-values, standardized mean differences (SMD), and the
#' number of complete pairs used for each variable.
#'
#' @details
#' Descriptive cells are produced by delegating to \code{table_one()} (which the
#' \code{pair_id} column is never passed to, so it can never be summarized as a
#' row). Paired statistics are computed directly from the validated input data and
#' attached with \code{gtsummary::add_stat()}/\code{gtsummary::add_p()}.
#'
#' **Paired significance tests** (pairwise-complete per variable):
#' - Continuous: paired t-test (\code{continuous_stat = "meansd"}) or Wilcoxon
#'   signed-rank test (\code{"mediqr"}).
#' - Categorical (logical or factor, any number of categories): McNemar's test
#'   (2 categories) or the McNemar-Bowker symmetry test (more than 2 categories),
#'   via \code{stats::mcnemar.test()} on the union-of-levels square table.
#'
#' **SMD**, selected by \code{pairing_method}:
#' - \code{"matching"}: marginal (pooled-variance) SMD via \code{smd::smd()} --
#'   the conventional matched-cohort balance metric.
#' - \code{"repeated_measure"}: Cohen's d_z (within-pair mean difference divided
#'   by the within-pair SD of differences) for continuous variables; categorical
#'   variables use the marginal SMD in both methods (no standard within-pair
#'   categorical estimand exists).
#'
#' Both are computed on the same complete pairs the significance test used, with
#' sign convention non-reference minus reference. Degenerate cases (zero complete
#' pairs, zero-variance differences, all-concordant categorical tables) return
#' \code{NA} (displayed as \code{"---"} for SMD, formatted by \code{pvalue_fun}
#' for p-values) instead of erroring.
#'
#' @param df Dataframe in long format: one row per pair member.
#' @param pair_id Column identifying the pair (unquoted). May be character,
#'        factor, or numeric. Rows with a missing/empty pair ID are dropped.
#' @param group Column identifying the pair member's arm/condition (unquoted).
#'        Must have exactly 2 observed levels after missing-row drops.
#' @param pairing_method One of "repeated_measure" or "matching"; selects the
#'        SMD estimand (see Details). The significance tests are the same under
#'        both.
#' @param ref_group Reference group level (optional). Default: first factor
#'        level if \code{group} is a factor; most frequent level (ties broken by
#'        first-observed) if character; sorted-first if logical/numeric. Sets the
#'        first group column and the SMD sign (non-reference minus reference).
#' @param datadic Optional data dictionary with variable descriptions; see
#'        \code{table_one()}.
#' @param var_name Column of \code{datadic} holding variable names; see
#'        \code{table_one()}.
#' @param var_desp Column of \code{datadic} holding display labels; see
#'        \code{table_one()}.
#' @param include Vector of variable names to include (optional). Evaluated
#'        against the original data; \code{pair_id} is dropped from the
#'        selection if listed (with a message), \code{group} is always kept.
#' @param missing Type of missing data display: "ifany" (default), "no", or
#'        "always".
#' @param missing_text Text to display for missing count. Default: "(Missing)".
#' @param add_p Logical. Add paired p-values (default: TRUE).
#' @param add_smd Logical. Add the SMD column (default: TRUE).
#' @param add_n_pairs Logical. Add the N-pairs column (default: TRUE).
#' @param add_overall Logical. Add an overall column (default: TRUE).
#' @param sort_by_p Logical. Sort rows by p-value; ignored when
#'        \code{add_p = FALSE} (default: FALSE).
#' @param continuous_stat One of "meansd" or "mediqr"; see \code{table_one()}.
#' @param pvalue_fun Function to format p-values (default: \code{format_pvalue}).
#'
#' @return A tbl_summary object (gtsummary class).
#'
#' @examples
#' library(gtsummary)
#' set.seed(0)
#' n <- 40
#' df <- data.frame(
#'   pid = rep(1:n, each = 2),
#'   visit = rep(c("Baseline", "Followup"), n),
#'   age = rnorm(2 * n, 55, 8)
#' )
#' table_one_paired(df, pair_id = pid, group = visit)
#'
#' @export
table_one_paired <- function(df, pair_id, group,
                              pairing_method = c("repeated_measure", "matching"),
                              ref_group = NULL,
                              datadic = NULL, var_name, var_desp, include,
                              missing = "ifany", missing_text = "(Missing)",
                              add_p = TRUE, add_smd = TRUE, add_n_pairs = TRUE,
                              add_overall = TRUE, sort_by_p = FALSE,
                              continuous_stat = c("meansd", "mediqr"),
                              pvalue_fun = format_pvalue) {

  pairing_method  <- match.arg(pairing_method)
  continuous_stat <- match.arg(continuous_stat)

  pair_id_q  <- rlang::enquo(pair_id)
  group_q    <- rlang::enquo(group)
  include_q  <- rlang::enquo(include)
  var_name_q <- rlang::enquo(var_name)
  var_desp_q <- rlang::enquo(var_desp)

  pair_id_name <- rlang::as_name(pair_id_q)
  group_name   <- rlang::as_name(group_q)

  prep <- .paired_prepare_data(df, pair_id_name, group_name, ref_group)
  data <- prep$data
  ref_level   <- prep$ref_level
  other_level <- prep$other_level

  has_include <- !rlang::quo_is_missing(include_q)
  if (has_include) {
    include_loc   <- tidyselect::eval_select(include_q, df)
    include_names <- names(df)[include_loc]
    if (pair_id_name %in% include_names) {
      message("`pair_id` column '", pair_id_name,
              "' was listed in `include`; ignoring it (pair IDs are never summarized)")
      include_names <- setdiff(include_names, pair_id_name)
    }
    include_names <- union(include_names, group_name)
  } else {
    include_names <- setdiff(names(data), pair_id_name)
  }

  if (length(setdiff(include_names, group_name)) == 0L) {
    stop("`include` selects no variables to summarize after removing `pair_id` ",
         "(only `group` remains). Include at least one real variable.")
  }

  desc_data <- data[, include_names, drop = FALSE]

  tbl <- .table_one_impl(
    desc_data,
    group_name = group_name,
    datadic = datadic,
    name_col = if (rlang::quo_is_missing(var_name_q)) "var_name" else rlang::as_name(var_name_q),
    desp_col = if (rlang::quo_is_missing(var_desp_q)) "var_desp" else rlang::as_name(var_desp_q),
    missing = missing, missing_text = missing_text,
    add_p = FALSE, add_overall = FALSE,
    continuous_stat = continuous_stat
  )

  if (add_overall) tbl <- gtsummary::add_overall(tbl)

  if (add_n_pairs) {
    n_pairs_fn <- .paired_make_n_pairs_fn(data, pair_id_name, group_name, ref_level, other_level)
    tbl <- gtsummary::add_stat(tbl, fns = gtsummary::everything() ~ n_pairs_fn)
    tbl <- gtsummary::modify_header(tbl, n_pairs ~ "**N pairs**")
  }

  if (add_smd) {
    smd_fn <- .paired_make_smd_fn(data, pair_id_name, group_name, ref_level, other_level, pairing_method)
    tbl <- gtsummary::add_stat(tbl, fns = gtsummary::everything() ~ smd_fn)
    tbl <- gtsummary::modify_header(tbl, smd ~ "**SMD**")
    smd_note <- if (pairing_method == "matching") {
      paste0("SMD = standardized mean difference, ", other_level, " vs ", ref_level,
             " (reference); pooled-variance (marginal) estimate on complete pairs (matched design).")
    } else {
      paste0("SMD = standardized mean difference, ", other_level, " vs ", ref_level,
             " (reference), computed on complete pairs; within-pair SD denominator for ",
             "continuous variables (repeated measures); categorical variables use the ",
             "marginal (pooled-variance) estimate.")
    }
    tbl <- gtsummary::modify_footnote_header(tbl, footnote = smd_note, columns = "smd")
  }

  if (add_p) {
    cont_test_fn <- .paired_make_cont_test_fn(data, pair_id_name, group_name, ref_level, other_level, continuous_stat)
    cat_test_fn  <- .paired_make_cat_test_fn(data, pair_id_name, group_name, ref_level, other_level)
    tbl <- gtsummary::add_p(
      tbl,
      test = list(gtsummary::all_continuous()  ~ cont_test_fn,
                  gtsummary::all_categorical() ~ cat_test_fn),
      pvalue_fun = pvalue_fun
    )
    p_note <- paste0(
      "Continuous variables: ",
      if (continuous_stat == "meansd") "paired t-test" else "Wilcoxon signed-rank test",
      "; categorical variables: McNemar's test (2 categories) or McNemar-Bowker test ",
      "(more than 2 categories). Tests use pairwise-complete pairs for each variable."
    )
    tbl <- gtsummary::modify_footnote_header(tbl, footnote = p_note, columns = "p.value")
    if (sort_by_p) tbl <- gtsummary::sort_p(tbl)
  }

  tbl
}
