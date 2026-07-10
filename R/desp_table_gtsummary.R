#---- Refactored table summary functions using gtsummary package ----

#' @title table_one
#'
#' @description
#' Main function that generates professional summary statistics tables for continuous, logical,
#' and factor variables following statistical guidelines. Replaces the previous manual implementation
#' with gtsummary package, providing a cleaner API and automatic statistical testing.
#'
#' @details
#' \code{table_one} calculates summary statistics for continuous, logical, and factor variables.
#' If a group variable is provided, it also assesses between-group differences using appropriate
#' statistical tests based on the number of groups:
#'
#' **For continuous variables** (test follows the chosen \code{continuous_stat}):
#' - \code{continuous_stat = "meansd"}: Welch t-test (2 groups) or one-way ANOVA (>2 groups);
#'   summary statistic is Mean +/- SD
#' - \code{continuous_stat = "mediqr"}: Wilcoxon rank-sum test (2 groups) or Kruskal-Wallis
#'   test (>2 groups); summary statistic is Median (Q1 - Q3), using type-1 quantiles
#' - Decimal places automatically match the variable's actual precision using decimalplaces()
#'
#' **For categorical variables (including logical/binary):**
#' - Fisher's exact test (with simulation for complex tables)
#' - Summary statistics: n (%)
#'
#' The input data frame should only contain numeric, logical, and factor variables.
#' Factor variables with only two levels should be converted to logical variables.
#' Date and datetime variables should be removed.
#'
#' @param df Dataframe consisting of numeric, logical, and factor variables
#' @param group Name of the grouping variable (optional). Can have 2 or more levels.
#' @param datadic Optional data dictionary with variable descriptions. By default it should
#'                 have columns var_name (original variable names) and var_desp (display
#'                 labels); use the \code{var_name}/\code{var_desp} arguments to point to
#'                 differently named columns
#' @param var_name Column of \code{datadic} holding the variable names (unquoted or string;
#'        default \code{var_name}). Kept for backward compatibility with the pre-gtsummary API.
#' @param var_desp Column of \code{datadic} holding the display labels (unquoted or string;
#'        default \code{var_desp}). Kept for backward compatibility with the pre-gtsummary API.
#' @param include Vector of variable names to include (optional; default includes all except group)
#' @param missing Type of missing data display: "ifany" (default), "no", or "always"
#' @param missing_text Text to display for missing count. Default: "(Missing)"
#' @param missing_group_exclude Logical. If TRUE (default), observations with a missing group
#'        value are excluded; if FALSE, they are kept as an explicit factor level named
#'        \code{missing_text}
#' @param add_p Logical. Add p-values for between-group comparisons (default: TRUE if group specified)
#' @param add_overall Logical. Add column with overall statistics (default: TRUE if group specified)
#' @param sort_by_p Logical. Sort rows by p-value (default: FALSE)
#' @param continuous_stat One of "meansd" or "mediqr". 
#'        "meansd" gives mean +/- SD; "mediqr" gives median (Q1 - Q3).
#' @param pvalue_fun Function to format p-values (default: format_pvalue)
#'
#' @return A tbl_summary object (gtsummary class) that can be further modified or converted to other formats
#'
#' @examples
#' library(gtsummary)
#' set.seed(0)
#' df <- data.frame(
#'   age = rnorm(150, mean = c(45, 50, 55), sd = 12),
#'   weight = rnorm(150, mean = c(70, 72, 75), sd = 15),
#'   group = factor(rep(c("A", "B", "C"), 50))
#' )
#'
#' # Basic usage
#' table_one(df)
#'
#' # With grouping variable (handles 2+ levels automatically)
#' table_one(df, group = group, add_p = TRUE)
#'
#' # With data dictionary
#' datadic <- data.frame(
#'   var_name = c("age", "weight"),
#'   var_desp = c("Age (years)", "Weight (kg)")
#' )
#' table_one(df, group = group, datadic = datadic, add_p = TRUE)
#'
#' @export
#' @importFrom gtsummary tbl_summary add_p add_overall modify_header as_flex_table
#' @importFrom dplyr select mutate filter across where pull
#' @importFrom rlang enquo quo_is_missing quo_name
#'
table_one <- function(df,
                      group,
                      datadic = NULL,
                      var_name,
                      var_desp,
                      include,
                      missing = "ifany",
                      missing_text = "(Missing)",
                      missing_group_exclude = TRUE,
                      add_p = NULL,
                      add_overall = NULL,
                      sort_by_p = FALSE,
                      continuous_stat = c("meansd", "mediqr"),
                      pvalue_fun = format_pvalue) {

  set.seed(0) # For reproducibility of Fisher's exact test p-values
  continuous_stat <- match.arg(continuous_stat)

  # Capture group variable (for NSE)
  group <- rlang::enquo(group)
  include <- rlang::enquo(include)

  has_group <- !rlang::quo_is_missing(group)
  has_include <- !rlang::quo_is_missing(include)

  # datadic column selectors (unquoted symbols or strings, as in the old API)
  var_name <- rlang::enquo(var_name)
  var_desp <- rlang::enquo(var_desp)
  name_col <- if (rlang::quo_is_missing(var_name)) "var_name" else rlang::as_name(var_name)
  desp_col <- if (rlang::quo_is_missing(var_desp)) "var_desp" else rlang::as_name(var_desp)
  if (!is.null(datadic) && !all(c(name_col, desp_col) %in% names(datadic))) {
    stop("`datadic` must contain columns `", name_col, "` and `", desp_col, "`")
  }

  # Set defaults for add_p and add_overall
  if (is.null(add_p)) add_p <- has_group
  if (is.null(add_overall)) add_overall <- has_group

  # Data preparation
  df <- df %>%
      dplyr::ungroup() %>%
      # Remove character and date variables
      dplyr::select(dplyr::where(~ !is.character(.) & !inherits(., "Date"))) %>%
      # Drop unused factor levels
      dplyr::mutate_if(is.factor, forcats::fct_drop)
  
  df <- if (has_include & has_group) {
    include_loc <- tidyselect::eval_select(include, df)
    group_loc <- tidyselect::eval_select(group, df)
    df[, sort(unique(c(group_loc, include_loc)))]
  } else if (has_include & !has_group) {
    include_loc <- tidyselect::eval_select(include, df)
    df[, include_loc]
  } else {
    df # everything
  }

  # Remove observations with missing group variable from summary if present
  if (has_group) {
    group_name <- rlang::quo_name(group)
    group_col <- dplyr::pull(df, all_of(group_name))

    # df <- df %>%
    #   dplyr::select(-all_of(group_name)) %>%
    #   dplyr::filter(!is.na(group_col)) %>%
    #   dplyr::mutate(!!group_name := group_col)
    df <- df %>% {
        if (missing_group_exclude) {
          dplyr::filter(., !is.na(!!group))
        } else {
          dplyr::mutate(., !!group := forcats::fct_na_value_to_level(!!group, level = missing_text))
        }
    }

    group_col<- if (missing_group_exclude) {
        group_col[!is.na(group_col)]
    } else {
      forcats::fct_na_value_to_level(group_col, level = missing_text)
    }
  }

  # gtsummary requires glue strings for statistics; med_q1_q3 uses type-1
  # quantiles to match the historical med_iqr() output
  continuous_glue <- if (continuous_stat == "meansd") {
    "{mean} \u00B1 {sd}"
  } else {
    "{median_type1} ({q1_type1} \u2013 {q3_type1})"
  }
  n_continuous_stats <- if (continuous_stat == "meansd") 2L else 3L

  # Calculate decimal places for each numeric variable using decimalplaces()
  # This ensures formatting matches the actual data precision
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  digits_list <- list()

  for (var in numeric_vars) {
    dec <- decimalplaces(df[[var]])
    digits_list[[var]] <- rep(dec, n_continuous_stats)
  }

  # For categorical variables: count and percentage
  cat_vars <- names(df)[sapply(df, function(x) is.factor(x) | is.logical(x))]
  for (var in cat_vars) {
    digits_list[[var]] <- c(0, 1)
  }

  # Build the base tbl_summary
  tbl <- gtsummary::tbl_summary(
    data = df,
    by = if (has_group) rlang::quo_name(group) else NULL,
    # include = if (has_group) -all_of(rlang::quo_name(group)) else everything(),
    missing = missing,
    missing_text = missing_text,
    statistic = list(
      all_continuous() ~ continuous_glue,
      all_categorical(dichotomous = FALSE) ~ "{n} ({p}%)",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    digits = digits_list,
    type = list(
      all_categorical(dichotomous = FALSE) ~ "categorical",
      all_dichotomous() ~ "dichotomous"
    ),
    label = if (!is.null(datadic)) {
      # gtsummary requires a named list (not a named vector)
      as.list(setNames(datadic[[desp_col]], datadic[[name_col]]))
    } else NULL
  ) %>%
    # Add p-values if comparing groups
    {
      if (add_p & has_group) {
        # Determine number of groups to select appropriate statistical tests
        # group_name <- rlang::quo_name(group)
        # n_groups_val <- length(unique(dplyr::pull(df, all_of(group_name))))
        n_groups_val <- length(unique(group_col))

        # For continuous: choose based on continuous_stat
        cont_test <- if (continuous_stat == "meansd") {
          if (n_groups_val == 2) "t.test" else "oneway.test"
        } else {
          if (n_groups_val == 2) "wilcox.test" else "kruskal.test"
        }

        gtsummary::add_p(
          .,
          test = list(
            all_continuous() ~ cont_test,
            all_categorical() ~ "fisher.test"
          ),
          pvalue_fun = pvalue_fun,
        #   test.args = list(
        #     all_continuous() ~ list(var.equal = FALSE),
        #     all_categorical() ~ list(hybrid = TRUE, simulate.p.value = TRUE)
        #   ),
          test.args = list(all_categorical() ~ list(hybrid = TRUE, simulate.p.value = TRUE)) %>% 
          append(
             if (continuous_stat == "meansd") list(all_continuous() ~ list(var.equal = FALSE)) else list()
          )
        )
      } else {
        .
      }
    } %>%
    # Add overall column
    {
      if (add_overall & has_group) {
        gtsummary::add_overall(.)
      } else {
        .
      }
    } %>%
    # Sort by p-value if requested
    {
      if (sort_by_p & add_p & has_group) {
        gtsummary::sort_p(.)
      } else {
        .
      }
    }

  tbl
}

# table_one(df)
# table_one(df, group = sex)
# table_one(df, include = c(male, age))
# table_one(df, group = sex, include = c(group, age))

# Type-1 quantile statistics referenced by name in the tbl_summary() glue
# string, matching the historical med_iqr() output
median_type1 <- function(x) stats::quantile(x, probs = 0.5, type = 1, names = FALSE, na.rm = TRUE)
q1_type1 <- function(x) stats::quantile(x, probs = 0.25, type = 1, names = FALSE, na.rm = TRUE)
q3_type1 <- function(x) stats::quantile(x, probs = 0.75, type = 1, names = FALSE, na.rm = TRUE)

#' @title format_pvalue
#'
#' @description
#' Format p-values according to statistical guidelines (similar to Annals of Medicine format).
#' This function is optimized for use with gtsummary but maintains compatibility with the
#' original implementation.
#'
#' @details
#' P-values >= 0.1995 are formatted to 2 decimal places.
#' P-values < 0.1995 are formatted to 3 decimal places.
#' P-values < 0.001 are displayed as "<0.001".
#'
#' @param x Numeric vector of p-values
#' @param eps Threshold for scientific notation (default: 0.001)
#' @param trim Logical. Remove leading spaces (default: TRUE)
#' @param droptrailing0 Logical. Remove trailing zeros (default: FALSE)
#' @param pad Logical. Add padding (default: FALSE)
#' @param ... Additional arguments passed to base::format.pval
#'
#' @return Character vector of formatted p-values
#' @export
#'
format_pvalue <- function(x, eps = 0.001, trim = TRUE,
                          droptrailing0 = FALSE,
                          pad = FALSE, ...) {
  p <- vector("character", length = length(x))

  large <- !is.na(x) & x >= 0.1995
  p[large] <- base::format.pval(x[large],
    digits = 1,
    eps = 0.1995,
    na.form = "---",
    nsmall = 2,
    trim = trim,
    drop0trailing = droptrailing0,
    scientific = FALSE, ...
  )

  p[!large] <- base::format.pval(x[!large],
    digits = 1,
    eps = eps,
    na.form = "---",
    nsmall = 3,
    trim = trim,
    drop0trailing = droptrailing0,
    scientific = FALSE, ...
  )

  if (pad) p <- gsub("^([^<])", "  \\1", p)
  p
}


#' @title Helper functions for custom statistics
#'
#' @description
#' These functions work with gtsummary's inline_text() and other functions to extract
#' specific statistics from the summary table.
#'

#' @title mean_sd
#' @description Format mean and standard deviation
#' @param x Numeric variable
#' @return Character string with formatted mean +/- SD
#' @export
mean_sd <- function(x) {
  if (length(x[!is.na(x)]) == 0) {
    return("---")
  } else {
    dec <- decimalplaces(x)
    if (length(x[!is.na(x)]) == 1) {
      m <- mean(x, na.rm = TRUE)
      return(formatC(m, digits = dec, format = "f"))
    } else {
      m <- mean(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      return(paste0(
        formatC(m, digits = dec, format = "f"),
        " \u00B1 ",
        formatC(s, digits = dec, format = "f")
      ))
    }
  }
}

#' @title med_iqr
#' @description Format median and interquartile range
#' @param x Numeric variable
#' @return Character string with formatted median (Q1-Q3)
#' @export
med_iqr <- function(x) {
  if (length(x[!is.na(x)]) == 0) {
    return("---")
  }

  dec <- decimalplaces(x)
  med <- quantile(x, probs = 0.5, type = 1, na.rm = TRUE)
  q1 <- quantile(x, probs = 0.25, type = 1, na.rm = TRUE)
  q3 <- quantile(x, probs = 0.75, type = 1, na.rm = TRUE)

  if (length(x[!is.na(x)]) == 1) {
    return(formatC(med, digits = dec, format = "f"))
  } else {
    return(paste0(
      formatC(med, digits = dec, format = "f"),
      " (",
      formatC(q1, digits = dec, format = "f"),
      " \u2013 ",
      formatC(q3, digits = dec, format = "f"),
      ")"
    ))
  }
}

#' @title n_avail
#' @description Count available (non-missing) observations
#' @param x Variable
#' @return Formatted count of non-missing values
#' @export
n_avail <- function(x) {
  formatC(sum(!is.na(x)), digits = 0, format = "d", big.mark = ",")
}
