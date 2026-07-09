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

.paired_make_cont_test_fn <- function(paired_df, pair_id_name, group_name, ref_level, other_level, continuous_stat) {
  function(data, variable, by, ...) {
    wide <- .paired_wide(paired_df, pair_id_name, group_name, ref_level, other_level, variable)
    p <- tryCatch(
      withCallingHandlers({
        if (continuous_stat == "meansd") {
          stats::t.test(wide$.other, wide$.ref, paired = TRUE)$p.value
        } else {
          stats::wilcox.test(wide$.other, wide$.ref, paired = TRUE)$p.value
        }
      }, warning = function(w) invokeRestart("muffleWarning")),
      error = function(e) NA_real_
    )
    # mcnemar.test()/t.test() degenerate cases can return NaN (e.g. division by a
    # zero-discordant-pairs denominator) rather than NA_real_; the design specifies
    # NA, so normalize here rather than leaving NaN in the table body.
    if (!is.finite(p)) p <- NA_real_
    dplyr::tibble(p.value = p)
  }
}

.paired_make_cat_test_fn <- function(paired_df, pair_id_name, group_name, ref_level, other_level) {
  function(data, variable, by, ...) {
    wide <- .paired_wide(paired_df, pair_id_name, group_name, ref_level, other_level, variable)
    p <- tryCatch(
      withCallingHandlers({
        lv <- union(as.character(unique(wide$.ref)), as.character(unique(wide$.other)))
        ref_f   <- factor(as.character(wide$.ref),   levels = lv)
        other_f <- factor(as.character(wide$.other), levels = lv)
        stats::mcnemar.test(table(ref_f, other_f))$p.value
      }, warning = function(w) invokeRestart("muffleWarning")),
      error = function(e) NA_real_
    )
    if (!is.finite(p)) p <- NA_real_
    dplyr::tibble(p.value = p)
  }
}
