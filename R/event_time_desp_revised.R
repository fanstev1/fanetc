#---- Revised event-time analysis functions using survminer approach ----

#' @title show_surv_revised
#'
#' @description
#' Generates a professional Kaplan-Meier survival plot with confidence intervals and at-risk table.
#' Uses the survminer approach with the at-risk table positioned below the x-axis label for cleaner visualization.
#'
#' @details
#' This function creates a ggplot object showing the estimated survival function with optional confidence intervals,
#' at-risk numbers, log-rank p-values, and stratification by group.
#' The at-risk table is positioned as a separate panel below the main plot, similar to survminer's ggsurvplot.
#'
#' @param surv_obj A survfit object from survival::survfit()
#' @param x_lab Character. Label for x-axis (default: 'Time')
#' @param y_lab Character. Label for y-axis (default: 'The freedom from death')
#' @param y_lim Numeric vector of length 2. Y-axis limits
#' @param x_break Numeric vector. Breaks for x-axis. If NULL, uses pretty breaks
#' @param y_break Numeric vector. Breaks for y-axis. If NULL, uses pretty breaks
#' @param color_scheme One of "brewer", "grey", "viridis", "manual" (default: "brewer")
#' @param color_list List of colors for manual color scheme
#' @param plot_theme ggplot2 theme object (default: theme_minimal())
#' @param add_ci Logical. Add confidence intervals (default: TRUE)
#' @param add_atrisk Logical. Add at-risk table below plot (default: TRUE)
#' @param add_legend Logical. Add legend (default: FALSE)
#' @param add_pvalue Logical. Add log-rank p-value (default: TRUE)
#' @param pvalue_pos Character. Position for p-value: "topleft", "topright", "bottomleft", "bottomright", 
#'                   "left", "right", "top", "bottom" (default: "topleft")
#' @param plot_cdf Logical. Plot cumulative failure instead of survival (default: FALSE)
#' @param print_fig Logical. Print the figure (default: TRUE)
#' @param atrisk_font_size Numeric. Font size for at-risk table (default: 10)
#'
#' @return A ggplot object or a list with "plot" and "atrisk_table" if add_atrisk=TRUE
#' @export
#' @importFrom ggplot2 ggplot geom_step scale_x_continuous scale_y_continuous coord_cartesian annotation_custom gpar textGrob
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid tableGrob
#'
show_surv_revised <- function(surv_obj,
                              x_lab = 'Time',
                              y_lab = NULL,
                              y_lim = NULL,
                              x_break = NULL,
                              y_break = NULL,
                              color_scheme = c("brewer", "grey", "viridis", "manual"),
                              color_list = NULL,
                              plot_theme = theme_minimal(),
                              add_ci = TRUE,
                              add_atrisk = TRUE,
                              add_legend = FALSE,
                              add_pvalue = TRUE,
                              pvalue_pos = c("topleft", "topright", "bottomleft", "bottomright", "left", "right", "top", "bottom"),
                              plot_cdf = FALSE,
                              print_fig = TRUE,
                              atrisk_font_size = 10) {

  # Set default y-axis label
  if (is.null(y_lab)) {
    y_lab <- if (plot_cdf) 'Cumulative Incidence' else 'Survival Probability'
  }

  # Determine if stratified and set add_legend accordingly
  has_strata <- any(names(surv_obj) == 'strata')
  if (!has_strata) {
    add_pvalue <- FALSE
    add_legend <- FALSE
  }

  color_scheme <- match.arg(color_scheme)
  pvalue_pos <- match.arg(pvalue_pos)

  if (color_scheme == 'manual' & is.null(color_list)) {
    stop("Please provide a list of color value(s).")
  }

  # Apply color schemes
  fill_fun <- switch(color_scheme,
    'brewer' = quote(scale_fill_brewer(palette = "Set1")),
    'grey' = quote(scale_fill_grey(start = 0, end = 0.65)),
    'viridis' = quote(scale_fill_viridis_d(option = "viridis", begin = .2, end = .85)),
    'manual' = do.call(scale_fill_manual, color_list)
  )

  color_fun <- switch(color_scheme,
    'brewer' = quote(scale_color_brewer(palette = "Set1")),
    'grey' = quote(scale_color_grey(start = 0, end = 0.65)),
    'viridis' = quote(scale_color_viridis_d(option = "viridis", begin = .2, end = .85)),
    'manual' = do.call(scale_color_manual, color_list)
  )

  # Prepare data
  surv_mat <- prepare_survfit(surv_obj)

  plot_prob_d <- surv_mat %>%
    dplyr::select(strata, plot_prob_d) %>%
    tidyr::unnest(cols = c(plot_prob_d)) %>%
    dplyr::mutate(prob = if (plot_cdf) 1 - prob else prob)

  # Apply y-axis limits to data
  if (!is.null(y_lim)) {
    if (!plot_cdf) {
      y_lim <- c(0, 1)
    } else {
      y_lim <- c(0, max(y_lim, na.rm = TRUE))
    }
    plot_prob_d <- plot_prob_d %>%
      dplyr::mutate(prob = pmin(prob, y_lim[2], na.rm = TRUE))
  } else {
    y_lim <- c(0, 1)
  }

  # Create main plot
  out <- ggplot() +
    geom_step(
      data = plot_prob_d,
      aes(x = time, y = prob, group = strata, color = strata),
      size = 1.1,
      show.legend = add_legend
    ) +
    eval(color_fun) +
    scale_x_continuous(
      name = x_lab,
      breaks = if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
      expand = c(0.01, 0.005),
      labels = function(x) scales::comma(x, accuracy = 1)
    )

  # Add confidence intervals
  if (add_ci) {
    plot_ci_d <- surv_mat %>%
      dplyr::select(strata, plot_ci_d) %>%
      tidyr::unnest(cols = c(plot_ci_d))

    if (plot_cdf) {
      plot_ci_d <- plot_ci_d %>%
        dplyr::mutate_at(dplyr::vars(dplyr::starts_with('conf')), function(x) 1 - x) %>%
        dplyr::rename(conf_high = conf_low, conf_low = conf_high)
    }

    out <- out +
      geom_ribbon(
        data = plot_ci_d,
        aes(x = time, ymin = conf_low, ymax = conf_high, fill = strata),
        alpha = .2,
        show.legend = FALSE
      ) +
      eval(fill_fun)
  }

  # Add y-axis
  out <- out +
    scale_y_continuous(
      name = y_lab,
      breaks = if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
      expand = c(0.005, 0),
      labels = function(x) scales::percent(x, accuracy = 1),
      limits = y_lim
    ) +
    coord_cartesian(ylim = y_lim, clip = "on")

  # Add p-value if comparing groups
  if (add_pvalue & has_strata) {
    pval <- run_logrank_test(surv_obj) %>%
      format_pvalue()
    pval <- ifelse(trimws(pval) == "<0.001", "Log-rank p< 0.001", paste0("Log-rank p= ", pval))

    nudge <- 0.01
    position_list <- list(
      topleft = list(x = nudge, y = 1 - nudge, hjust = 0, vjust = 1),
      topright = list(x = 1 - nudge, y = 1 - nudge, hjust = 1, vjust = 1),
      bottomleft = list(x = nudge, y = nudge, hjust = 0, vjust = 0),
      bottomright = list(x = 1 - nudge, y = nudge, hjust = 1, vjust = 0),
      left = list(x = nudge, y = 0.5, hjust = 0, vjust = 0.5),
      right = list(x = 1 - nudge, y = 0.5, hjust = 1, vjust = 0.5),
      top = list(x = 0.5, y = 1 - nudge, hjust = 0.5, vjust = 1),
      bottom = list(x = 0.5, y = nudge, hjust = 0.5, vjust = 0)
    )

    pos <- position_list[[pvalue_pos]]

    out <- out +
      annotation_custom(
        grob = textGrob(
          label = pval,
          x = pos$x,
          y = pos$y,
          hjust = pos$hjust,
          vjust = pos$vjust,
          gp = gpar(
            family = "Inconsolata",
            fontface = "italic",
            fontsize = if (is.null(plot_theme$text$size)) 11 else plot_theme$text$size
          )
        )
      )
  }

  out <- out + plot_theme

  # Create at-risk table if requested
  atrisk_grob <- NULL
  if (add_atrisk) {
    # Get x-axis breaks for at-risk table
    x_breaks_atrisk <- if (is.null(x_break)) {
      ggplot2::ggplot_build(out)$layout$panel_scales_x[[1]]$get_breaks(
        ggplot2::ggplot_build(out)$layout$panel_scales_x[[1]]$range$range
      )
    } else {
      x_break
    }

    x_breaks_atrisk <- x_breaks_atrisk[x_breaks_atrisk >= 0]

    # Extract at-risk numbers
    atrisk_data <- extract_atrisk(surv_obj, time.list = x_breaks_atrisk)

    # Transpose the at-risk table so strata are rows and time points are columns
    atrisk_table <- atrisk_data %>%
      dplyr::mutate(dplyr::across(-time, as.integer)) %>%
      dplyr::mutate(dplyr::across(-time, function(x) formatC(x, digits = 0, format = "d")))

    # Create transposed table with strata as rows and time as columns
    strata_names <- colnames(atrisk_table)[-1]  # Exclude "time" column
    time_values <- atrisk_table$time

    # Create matrix with strata as rows and time points as columns
    atrisk_matrix <- as.matrix(atrisk_table[, -1])  # Remove time column
    rownames(atrisk_matrix) <- strata_names
    colnames(atrisk_matrix) <- as.character(time_values)

    # Convert to data frame for tableGrob
    atrisk_transposed <- as.data.frame(atrisk_matrix)
    atrisk_transposed <- cbind(Stratum = rownames(atrisk_transposed), atrisk_transposed)
    rownames(atrisk_transposed) <- NULL

    # Create table as grob
    atrisk_grob <- grid::tableGrob(
      atrisk_transposed,
      rows = NULL,
      theme = gridExtra::ttheme_minimal(
        base_size = atrisk_font_size,
        core = list(
          fg_params = list(col = "black"),
          bg_params = list(fill = NA, col = NA)
        ),
        colhead = list(
          fg_params = list(col = "black", fontface = "bold"),
          bg_params = list(fill = NA, col = NA)
        )
      )
    )
  }

  # Return result
  result <- out

  if (is.null(atrisk_grob)) {
    if (print_fig) print(out)
    return(out)
  } else {
    # Create combined plot with at-risk table below
    combined <- gridExtra::arrangeGrob(
      out + theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank()),
      atrisk_grob,
      nrow = 2,
      heights = c(4, 1)
    )

    if (print_fig) grid::grid.draw(combined)

    return(list(plot = out, atrisk_table = atrisk_transposed, combined = combined))
  }
}


#' @title show_cif_revised
#'
#' @description
#' Generates a professional cumulative incidence function (CIF) plot with confidence intervals and at-risk table.
#' Uses the survminer approach with the at-risk table positioned below the x-axis label.
#'
#' @details
#' This function creates a ggplot object showing cumulative incidence functions for competing risks data with
#' optional confidence intervals, at-risk numbers, Gray's test p-values, and stratification.
#'
#' @param surv_obj A survfit object from survival::survfit() with competing risks
#' @param evt_type Numeric. Event type(s) to display (default: 1)
#' @param evt_label Function. Function to label event types
#' @param x_lab Character. X-axis label (default: 'Time')
#' @param y_lab Character. Y-axis label (default: 'Cumulative Incidence')
#' @param x_lim Numeric vector of length 2. X-axis limits
#' @param y_lim Numeric vector of length 2. Y-axis limits
#' @param x_break Numeric vector. Breaks for x-axis
#' @param y_break Numeric vector. Breaks for y-axis
#' @param color_scheme One of "brewer", "grey", "viridis", "manual"
#' @param color_list List of colors for manual scheme
#' @param plot_theme ggplot2 theme object
#' @param add_ci Logical. Add confidence intervals (default: TRUE)
#' @param add_atrisk Logical. Add at-risk table (default: TRUE)
#' @param add_legend Logical. Add legend (default: FALSE)
#' @param add_pvalue Logical. Add Gray's test p-value (default: TRUE)
#' @param pvalue_pos Character. Position for p-value
#' @param print_fig Logical. Print the figure (default: TRUE)
#' @param atrisk_font_size Numeric. Font size for at-risk table (default: 10)
#'
#' @return A ggplot object or list with plot and at-risk table
#' @export
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr mutate across select
#' @importFrom gridExtra tableGrob ttheme_minimal arrangeGrob
#' @importFrom grid textGrob gpar
'
#'
show_cif_revised <- function(surv_obj,
                            evt_type = 1,
                            evt_label = function(x) {
                              dplyr::recode_factor(x,
                                `1` = "Event of Interest",
                                `2` = "Competing Event",
                                .default = "Event Free"
                              )
                            },
                            x_lab = 'Time',
                            y_lab = 'Cumulative Incidence',
                            x_lim = NULL,
                            y_lim = NULL,
                            x_break = NULL,
                            y_break = NULL,
                            color_scheme = c("brewer", "grey", "viridis", "manual"),
                            color_list = NULL,
                            plot_theme = theme_minimal(),
                            add_ci = TRUE,
                            add_atrisk = TRUE,
                            add_legend = FALSE,
                            add_pvalue = TRUE,
                            pvalue_pos = c("bottomright", "topleft", "topright", "bottomleft", "left", "right", "top", "bottom"),
                            print_fig = TRUE,
                            atrisk_font_size = 10) {

  color_scheme <- match.arg(color_scheme)
  pvalue_pos <- match.arg(pvalue_pos)

  if (color_scheme == 'manual' & is.null(color_list)) {
    stop("Please provide a list of color value(s) when using manual color scheme.")
  }

  # Apply color schemes
  color_fun <- switch(color_scheme,
    'brewer' = scale_color_brewer(palette = "Set1"),
    'grey' = scale_color_grey(start = 0, end = 0.65),
    'viridis' = scale_color_viridis_d(option = "viridis", begin = .2, end = .85),
    'manual' = do.call(scale_color_manual, color_list)
  )

  fill_fun <- switch(color_scheme,
    'brewer' = scale_fill_brewer(palette = "Set1"),
    'grey' = scale_fill_grey(start = 0, end = 0.65),
    'viridis' = scale_fill_viridis_d(option = "viridis", begin = .2, end = .85),
    'manual' = do.call(scale_fill_manual, color_list)
  )

  # Prepare data
  cmprisk_mat <- prepare_survfit(surv_obj) %>%
    dplyr::filter(state %in% evt_type) %>%
    dplyr::mutate(
      state_label = evt_label(state),
      state_label = forcats::fct_drop(state_label),
      state = forcats::fct_drop(state),
      state_strata = interaction(state_label, strata, drop = TRUE, sep = ": ")
    )

  plot_prob_d <- cmprisk_mat %>%
    dplyr::select(strata, state, state_label, state_strata, plot_prob_d) %>%
    tidyr::unnest(cols = c(plot_prob_d))

  has_strata <- nlevels(plot_prob_d$strata) > 1
  has_states <- nlevels(plot_prob_d$state) > 1

  add_pvalue <- if (!has_strata) FALSE else add_pvalue
  add_legend <- if (!has_strata & !has_states) FALSE else add_legend

  # Create main plot
  out <- ggplot() +
    {
      if (!has_strata & has_states) {
        geom_step(
          data = plot_prob_d,
          aes(x = time, y = prob, group = state_label, color = state_label),
          linewidth = 1.1,
          show.legend = add_legend
        )
      } else if (has_strata & !has_states) {
        geom_step(
          data = plot_prob_d,
          aes(x = time, y = prob, group = strata, color = strata),
          linewidth = 1.1,
          show.legend = add_legend
        )
      } else {
        geom_step(
          data = plot_prob_d,
          aes(x = time, y = prob, group = state_strata, color = state_strata),
          linewidth = 1.1,
          show.legend = add_legend
        )
      }
    } +
    color_fun +
    scale_x_continuous(
      name = x_lab,
      breaks = if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
      expand = c(0.01, 0.005),
      labels = function(x) scales::comma(x, accuracy = 1)
    ) +
    scale_y_continuous(
      name = y_lab,
      breaks = if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
      expand = c(0.01, 0),
      labels = function(x) scales::percent(x, accuracy = 1)
    ) +
    {
      if (!is.null(x_lim) | !is.null(y_lim)) {
        coord_cartesian(xlim = x_lim, ylim = y_lim, clip = "on")
      } else {
        coord_cartesian(ylim = c(0, 1), clip = "on")
      }
    }

  # Add confidence intervals
  if (add_ci) {
    plot_ci_d <- cmprisk_mat %>%
      dplyr::select(strata, state, state_label, state_strata, plot_ci_d) %>%
      tidyr::unnest(cols = c(plot_ci_d))

    out <- out +
      {
        if (!has_strata & has_states) {
          geom_ribbon(
            data = plot_ci_d,
            aes(x = time, ymin = conf_low, ymax = conf_high, group = state_label, fill = state_label),
            alpha = .2,
            show.legend = FALSE
          )
        } else if (has_strata & !has_states) {
          geom_ribbon(
            data = plot_ci_d,
            aes(x = time, ymin = conf_low, ymax = conf_high, group = strata, fill = strata),
            alpha = .2,
            show.legend = FALSE
          )
        } else {
          geom_ribbon(
            data = plot_ci_d,
            aes(x = time, ymin = conf_low, ymax = conf_high, group = state_strata, fill = state_strata),
            alpha = .2,
            show.legend = FALSE
          )
        }
      } +
      fill_fun
  }

  # Add p-value
  if (add_pvalue & has_strata) {
    pval <- run_gray_test(surv_obj, evt_type = evt_type) %>%
      format_pvalue()
    pval <- ifelse(trimws(pval) == "<0.001", "Gray's p< 0.001", paste0("Gray's p= ", pval))

    nudge <- 0.01
    position_list <- list(
      topleft     = list(x = nudge,     y = 1 - nudge, hjust = 0,   vjust = 1),
      topright    = list(x = 1 - nudge, y = 1 - nudge, hjust = 1,   vjust = 1),
      bottomleft  = list(x = nudge,     y = nudge,     hjust = 0,   vjust = 0),
      bottomright = list(x = 1 - nudge, y = nudge,     hjust = 1,   vjust = 0),
      left        = list(x = nudge,     y = 0.5,       hjust = 0,   vjust = 0.5),
      right       = list(x = 1 - nudge, y = 0.5,       hjust = 1,   vjust = 0.5),
      top         = list(x = 0.5,       y = 1 - nudge, hjust = 0.5, vjust = 1),
      bottom      = list(x = 0.5,       y = nudge,     hjust = 0.5, vjust = 0)
    )

    pos <- position_list[[pvalue_pos]]

    out <- out +
      annotation_custom(
        grob = textGrob(
          label = pval,
          x = pos$x,
          y = pos$y,
          hjust = pos$hjust,
          vjust = pos$vjust,
          gp = gpar(
            family = "Inconsolata",
            fontface = "italic",
            fontsize = if (is.null(plot_theme$text$size)) 11 else plot_theme$text$size
          )
        )
      )
  }

  out <- out + plot_theme

  # Create at-risk table if requested
  atrisk_grob <- NULL
  if (add_atrisk) {
    x_breaks_atrisk <- if (is.null(x_break)) {
      scales::pretty_breaks(6)(c(0, max(plot_prob_d$time, na.rm = TRUE)))
    } else {
      x_break
    }

    x_breaks_atrisk <- x_breaks_atrisk[x_breaks_atrisk >= 0 & x_breaks_atrisk <= max(plot_prob_d$time, na.rm = TRUE)]

    atrisk_data <- extract_atrisk(surv_obj, time.list = x_breaks_atrisk)

    # Transpose the at-risk table: strata as rows, time points as columns
    atrisk_transposed <- atrisk_data %>%
      tidyr::pivot_longer(-time, names_to = "strata", values_to = "n") %>%
      tidyr::pivot_wider(names_from = time, values_from = n, names_prefix = "") %>%
      dplyr::mutate(dplyr::across(-strata, as.integer)) %>%
      dplyr::mutate(dplyr::across(-strata, function(x) formatC(x, digits = 0, format = "d")))

    # Rename strata column for clarity
    colnames(atrisk_transposed)[1] <- "Stratum"

    atrisk_grob <- gridExtra::tableGrob(
      atrisk_transposed,
      rows = NULL,
      theme = gridExtra::ttheme_minimal(
        base_size = atrisk_font_size,
        core = list(
          fg_params = list(col = "black"),
          bg_params = list(fill = NA, col = NA)
        ),
        colhead = list(
          fg_params = list(col = "black", fontface = "bold"),
          bg_params = list(fill = NA, col = NA)
        )
      )
    )
  }

  # Return result
  if (is.null(atrisk_grob)) {
    if (print_fig) print(out)
    return(out)
  } else {
    combined <- gridExtra::arrangeGrob(
        out,
    #   out + theme(axis.title.x = element_blank(),
    #               axis.text.x = element_blank()),
      atrisk_grob,
      nrow = 2,
      heights = c(4, 1)
    )

    if (print_fig) grid::grid.draw(combined)

    return(list(plot = out, atrisk_table = atrisk_transposed, combined = combined))
  }
}


#' @title construct_cmprisk_var_revised
#'
#' @description
#' Creates time-to-event variables for competing risk analysis from date variables.
#' Requires an index date (time zero), event date, competing event date(s), and last follow-up date.
#'
#' @details
#' This function processes dates to create:
#' - `evt_time`: the time from index to first event (either event of interest or competing event)
#' - `evt`: a factor indicating the event type (0=censored, 1=event of interest, 2+=competing events)
#'
#' The function handles:
#' - Events occurring before end of follow-up
#' - Censoring at the last follow-up date
#' - Multiple competing events (requires separate date columns for each)
#' - Validation of date ranges and consistency
#' - Flags records with events at index date (sets evt_time to 0.5)
#' - Flags records with events before index date
#'
#' @param df Input data frame
#' @param patid Name of patient ID column (unquoted)
#' @param idx_dt Name of index date column (unquoted) - time zero (no missing values allowed)
#' @param evt_dt Name of event date column (unquoted) - event of interest (missing if event doesn't occur)
#' @param end_dt Name of end of follow-up date column (unquoted) (can be missing if event occurred)
#' @param ... Additional competing event date columns (unquoted), passed as name=column
#' @param varname Character vector of output variable names: c("time_var_name", "event_var_name")
#'                If NULL, uses c("evt_time", "evt")
#' @param append Logical. If TRUE, appends to original data; if FALSE, returns only new variables
#'
#' @return A data frame with patient ID and event time / event indicator variables
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' test_df <- data.frame(
#'   patid = 1:100,
#'   idx_dt = as.Date("2020-01-01"),
#'   evt_dt = c(NA, sample(0:200, 99)) + as.Date("2020-01-01"),
#'   cmp_evt_dt = c(NA, sample(50:250, 99)) + as.Date("2020-01-01"),
#'   end_dt = sample(100:300, 100) + as.Date("2020-01-01")
#' )
#'
#' # Basic usage
#' result <- construct_cmprisk_var_revised(
#'   test_df,
#'   patid = patid,
#'   idx_dt = idx_dt,
#'   evt_dt = evt_dt,
#'   end_dt = end_dt,
#'   cmp_evt_1 = cmp_evt_dt
#' )
#'
#' # With custom variable names
#' result <- construct_cmprisk_var_revised(
#'   test_df,
#'   patid = patid,
#'   idx_dt = idx_dt,
#'   evt_dt = evt_dt,
#'   end_dt = end_dt,
#'   cmp_evt_1 = cmp_evt_dt,
#'   varname = c("ftime", "fstatus")
#' )
#' }
#'
#' @export
#' @importFrom rlang enquo quo_is_missing quo_name quo_get_expr
#' @importFrom dplyr select mutate filter bind_cols tibble
#'
construct_cmprisk_var_revised <- function(df,
                                         patid,
                                         idx_dt,
                                         evt_dt,
                                         end_dt,
                                         ...,
                                         varname = NULL,
                                         append = FALSE) {

  # Capture variable names using non-standard evaluation
  patid <- rlang::enquo(patid)
  idx_dt <- rlang::enquo(idx_dt)
  evt_dt <- rlang::enquo(evt_dt)
  end_dt <- rlang::enquo(end_dt)
  cmp_evt_list <- rlang::enquos(...)

  # Validate inputs
  if (rlang::quo_is_missing(idx_dt)) stop("Index date (idx_dt) is required.")
  if (rlang::quo_is_missing(evt_dt)) stop("Event date (evt_dt) is required.")
  if (rlang::quo_is_missing(end_dt)) stop("End of follow-up date (end_dt) is required.")
  if (rlang::quo_is_missing(patid)) stop("Patient ID (patid) is required.")

  # Get variable names
  patid_name <- rlang::quo_name(patid)
  idx_dt_name <- rlang::quo_name(idx_dt)
  evt_dt_name <- rlang::quo_name(evt_dt)
  end_dt_name <- rlang::quo_name(end_dt)

  # Set output variable names
  if (is.null(varname)) {
    evt_time_name <- "evt_time"
    evt_name <- "evt"
  } else {
    evt_time_name <- varname[1]
    evt_name <- varname[2]
  }

  # Initialize result data frame
  result <- df %>%
    dplyr::select(!!patid) %>%
    dplyr::mutate(
      evt_time = NA_real_,
      evt = NA_integer_,
      flag_event_at_index = FALSE,
      flag_event_before_index = FALSE,
      flag_insufficient_info = FALSE
    )

  # Process each row to determine first event
  for (i in 1:nrow(df)) {
    # Get dates for this row
    idx_date <- df[[idx_dt_name]][i]
    evt_date <- df[[evt_dt_name]][i]
    end_date <- df[[end_dt_name]][i]

    # Collect all event dates (event of interest, competing events, end of follow-up)
    event_dates <- list(
      "event_of_interest" = evt_date,
      "end_of_followup" = end_date
    )

    # Add competing events
    for (j in seq_along(cmp_evt_list)) {
      event_dates[[paste0("competing_", j)]] <- df[[rlang::quo_name(cmp_evt_list[[j]])]][i]
    }

    # Find non-NA dates
    valid_dates <- event_dates[!sapply(event_dates, is.na)]

    if (length(valid_dates) == 0) {
      # No events occurred - this shouldn't happen if end_dt is provided
      result$evt[i] <- NA_integer_
      result$evt_time[i] <- NA_real_
      result$flag_insufficient_info[i] <- TRUE
      warning("Some rows have no sufficient information to determine event status - check input data")
      next
    }

    # Find the earliest date
    earliest_date <- as.Date(min(unlist(valid_dates), na.rm = TRUE))
    earliest_event <- names(valid_dates)[which.min(unlist(valid_dates))]

    # Determine event type
    result$evt[i] <- if (earliest_event == "event_of_interest") {
      1L
    } else if (grepl("^competing_", earliest_event)) {
      # Extract competing event number
      1L + as.integer(gsub("competing_", "", earliest_event)) # Competing events start at 2
    } else if (earliest_event == "end_of_followup") {
      0L  # Right censored
    }

    # Calculate time to event
    evt_time_days <- as.numeric(earliest_date - idx_date)
    result$evt_time[i] <- evt_time_days

    # Flag special cases
    if (evt_time_days == 0) {
      result$flag_event_at_index[i] <- TRUE
      result$evt_time[i] <- 0.5  # Set to 0.5 as per requirements
    } else if (evt_time_days < 0) {
      result$flag_event_before_index[i] <- TRUE
      result$evt[i] <- NA_integer_
      result$evt_time[i] <- NA_real_ # Set to NA for invalid cases
      warning(paste0("Subject ", df[[patid_name]][i], ": Event occurred before index date (evt_time = ", evt_time_days, ")"))
    }
  }

  # Convert event status to factor
  result[[evt_name]] <- factor(result[[evt_name]],
      levels = 0L:(length(cmp_evt_list) + 1L),
      labels = 0L:(length(cmp_evt_list) + 1L)
  )

  flag_entry <- result$flag_event_at_index | result$flag_event_before_index | result$flag_insufficient_info
  if (any(flag_entry)) {
      cat("Warning: Some rows have error flags:\n")
      print(
          result %>%
              filter(flag_entry) %>%
              dplyr::left_join(df, by = patid_name) %>%
              select(!!patid, !!idx_dt, !!evt_dt, !!end_dt, !!!cmp_evt_list,
                  evt_time, evt, starts_with("flag_"))
      )
  }

  # Rename columns
  colnames(result)[colnames(result) == "evt_time"] <- evt_time_name
  colnames(result)[colnames(result) == "evt"] <- evt_name

  # Return result
  if (append) {
    dplyr::full_join(df, dplyr::select(result, -starts_with("flag_")), by = patid_name)
  } else {
    dplyr::select(result, -starts_with("flag_"))
  }
}
