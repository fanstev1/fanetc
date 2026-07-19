#' @title extract_atrisk
#'
#' @details
#' The function creates a dataframe containing the number of at risk patients at time-, overall or by strata
#'
#' @param fit a survfit object
#' @param time.list a numeric vector specifying the time points at which the number of at-risk subjects is calculated.
#' @param time.scale a divisor applied to the survfit times before matching \code{time.list} (e.g. 365.25 to report counts on a year scale when the fit is in days; default 1)
#' @return A dataframe containing the number of at risk patients at time-, overall or by strata
#' @export
extract_atrisk <- function(fit, time.list = NULL, time.scale = 1) {
  if (is.null(time.list)) {
    time.list <- c(fit$t0, max(fit$time / time.scale))
    time.list <- pretty(time.list)
  }
  time.list <- sort(c(fit$t0, time.list[time.list > fit$t0 & time.list <= max(fit$time / time.scale)]))

  if (any(names(fit) == "strata")) {
    strata_lab <- sapply(strsplit(names(fit$strata), "="), function(x) x[2])

    x <- data.frame(
      time = fit$time / time.scale,
      n.risk = if (is.matrix(fit$n.risk)) apply(fit$n.risk, 1, sum) else fit$n.risk,
      strata = factor(
        unlist(
          mapply(rep, x = seq_along(fit$strata), each = fit$strata, SIMPLIFY = FALSE)
        ),
        seq_along(fit$strata),
        labels = strata_lab
      )
    )

    # create a list of right continuous piecewise contant function for the at risk process in each strata
    # Steve note: have to fix the following line `rule` for data with delayed entries.
    # approxfun(x= time, y= n.risk, method= "constant", rule = 1:1, f= 1)
    atRisk <- lapply(
      split(x, x$strata),
      function(df) {
        with(
          df,
          approxfun(x = time, y = n.risk, method = "constant", rule = 2:1, f = 1)
        )
      }
    )

    atRiskPts <- rapply(atRisk,
      function(ff) {
        out <- ff(time.list)
        replace(out, is.na(out), 0)
      },
      how = "unlist"
    ) %>%
      matrix(
        nrow = length(time.list), byrow = FALSE,
        dimnames = list(time.list, strata_lab)
      )

    # wide, plain data.frame (time + one integer column per stratum) -- the shape
    # add_atrisk() consumes
    atRiskPts <- replace(atRiskPts, is.na(atRiskPts), 0)
    storage.mode(atRiskPts) <- "integer"
    atRiskPts <- data.frame(time = time.list, atRiskPts, check.names = FALSE, row.names = NULL)
  } else {
    # no strata - single group
    x <- data.frame(
      time = fit$time / time.scale,
      n.risk = if (is.matrix(fit$n.risk)) apply(fit$n.risk, 1, sum) else fit$n.risk,
      strata = factor(1, 1, labels = "Overall")
    )

    atRisk <- with(x, approxfun(x = time, y = n.risk, method = "constant", rule = 2:1, f = 1))
    atRiskPts <- atRisk(time.list)
    atRiskPts <- data.frame(
      time = time.list,
      Overall = as.integer(replace(atRiskPts, is.na(atRiskPts), 0))
    )
  }
  return(atRiskPts)
}




#' @title prepare_survfit
#'
#' @details
#' The function converts a survfit (Kaplan-Meier) or survfitms (multi-state /
#' competing risks) object into a nested tibble with one row per stratum (and, for
#' survfitms, per state), used by show_surv() and show_cif(). Each row carries the
#' tidy estimates (data), the step-curve coordinates including a time-0 anchor
#' (plot_prob_d), and the raw per-time confidence limits (plot_ci_d), which
#' show_surv() and show_cif() draw as step ribbons via geom_ribbon_step(). The
#' censored/reference placeholder state that survfit emits ("(s0)", or "" on some
#' versions) is relabeled "0" and set as the reference level.
#'
#' @param surv_obj a survfit or survfitms object
#' @return a nested tibble with columns strata (plus state for survfitms), data, plot_prob_d and plot_ci_d
#' @export
prepare_survfit <- function(surv_obj) {
  prepare_cmprisk <- function(surv_obj) {
    # set up strata
    nstrat <- if (is.null(surv_obj$strata)) 1 else length(surv_obj$strata)
    if (is.null(surv_obj$strata)) {
      stemp <- rep(1, length(surv_obj$time) * length(surv_obj$states)) # same length as stime
      stemp <- factor(stemp, 1, labels = c("Overall"))
    } else {
      strata_lab <- sapply(strsplit(names(surv_obj$strata), "="), function(x) x[2])
      stemp <- rep(rep(1:nstrat, surv_obj$strata), length(surv_obj$states)) # same length as stime
      stemp <- factor(stemp, 1:nstrat, strata_lab)
    }

    out <- dplyr::tibble(
      strata = stemp,
      state = rep(
        # survival::survfit() labels the pre-event/reference state "(s0)" (or, on some
        # versions, "") regardless of the original event factor's level names -- match
        # that literal placeholder, not any state whose name merely contains "0"
        # (e.g. a real competing-risk state named "10" must not be swept in here).
        replace(surv_obj$state, nchar(surv_obj$state) == 0 | surv_obj$state == "(s0)", "0"),
        each = length(surv_obj$time)
      ),
      time = rep(surv_obj$time, length(surv_obj$state)),
      prob = as.numeric(surv_obj$pstate),
      conf_low = as.numeric(surv_obj$lower),
      conf_high = as.numeric(surv_obj$upper)
    )
    if (!is.factor(out$state)) out$state <- relevel(factor(out$state), ref = "0")
    out
  }

  prepare_surv <- function(surv_obj) {
    # set up strata
    nstrat <- if (is.null(surv_obj$strata)) 1 else length(surv_obj$strata)
    if (is.null(surv_obj$strata)) {
      stemp <- factor(rep(1, length(surv_obj$time)), 1, labels = c("Overall"))
    } else {
      strata_lab <- sapply(strsplit(names(surv_obj$strata), "="), function(x) x[2])
      stemp <- factor(rep(1:nstrat, surv_obj$strata), 1:nstrat, strata_lab)
    }

    dplyr::tibble(
      strata = stemp,
      time = surv_obj$time,
      prob = surv_obj$surv,
      conf_low = surv_obj$lower,
      conf_high = surv_obj$upper,
      n_risk = surv_obj$n.risk, # immediately before time t
      n_event = surv_obj$n.event,
      n_censor = surv_obj$n.censor
    )
  }

  out <- if (inherits(surv_obj, "survfitms")) {
    surv_obj %>%
      prepare_cmprisk() %>%
      tidyr::nest(.by = c(strata, state)) %>%
      dplyr::mutate(
        plot_prob_d = purrr::map2(
          state, data,
          function(state, df) {
            df %>%
              dplyr::select(all_of(c("time", "prob"))) %>%
              dplyr::bind_rows(
                dplyr::tribble(
                  ~time, ~prob,
                  0, as.numeric(state == "0")
                )
              ) %>%
              dplyr::arrange(time, if (state != "0") prob else desc(prob))
          }
        )
      )
  } else {
    surv_obj %>%
      prepare_surv() %>%
      tidyr::nest(.by = strata) %>%
      dplyr::mutate(
        data = purrr::map(
          data,
          function(df) {
            remove <- duplicated(df$prob)
            if (remove[length(remove)]) remove[length(remove)] <- FALSE
            df[!remove, ]
          }
        ),
        plot_prob_d = purrr::map(
          data,
          function(df) {
            df <- df %>%
              dplyr::select(all_of(c("time", "prob")))

            df <- if (is.na(match(0, df$time))) {
              # if time 0 is not included in the estimate
              # time-prob (i.e., no events occur at time 0),
              # then add time = 0 and prob = 1
              df %>%
                dplyr::bind_rows(
                  dplyr::tribble(
                    ~time, ~prob,
                    0, 1
                  )
                )
            } else {
              df
            }

            dplyr::arrange(df, time)
          }
        )
      )
  }

  out <- out %>%
    dplyr::mutate(
      plot_ci_d = purrr::map(
        data,
        function(df) {
          # raw per-time CI limits; geom_ribbon_step() steps them at plot time
          df %>%
            dplyr::select(all_of(c("time", "conf_low", "conf_high"))) %>%
            dplyr::arrange(time)
        }
      )
    )
  return(out)
}

#' @title add_atrisk
#'
#' @details
#' The function adds an at-risk table underneath a survival or cumulative-incidence
#' plot. The horizontal positions are data coordinates (the time points); the
#' vertical positions are absolute text lines below the panel bottom, so the row
#' spacing does not change with figure or panel size. The annotations are drawn
#' outside the panel, so the figure needs a sufficient bottom/left plot.margin and
#' the panel clip turned off to show them (see the show_cif() example).
#'
#' @param p a ggplot object showing the survival or cumulative-incidence curves
#' @param surv_obj the survfit object the plot was built from
#' @param x_break a numeric vector of time points at which the at-risk counts are displayed (default: the plot's x-axis breaks)
#' @param atrisk_init_pos position of the "At-risk N:" header, in text lines below the panel bottom. Default: with more than one group the header starts on the line right after the x-axis title (2.23); with a single group it sits one full line of whitespace below it (3.06)
#' @param plot_theme a ggplot2 theme object; its text family, face and size are used for the table text (default: plain 11pt Arial)
#' @return a ggplot object with the at-risk table added
#' @export
add_atrisk<- function(p, surv_obj, x_break= NULL, atrisk_init_pos= NULL, plot_theme = NULL) {

  # ---- get font information ----
  if (is.null(plot_theme)) {
    font_family<- "Arial"
    font_face  <- "plain"
    font_size  <- 11
  } else {
    font_family<- if (is.null(plot_theme$text$family) | trimws(plot_theme$text$family) == "") "Arial" else plot_theme$text$family
    font_face  <- if (is.null(plot_theme$text$face) | trimws(plot_theme$text$face) == "") "plain" else plot_theme$text$face
    font_size  <- if (is.null(plot_theme$text$size)) 11 else plot_theme$text$size
  }

  #---- get parameters required for where to include the at-risk table ----#
  atrisk_row_inc<- 1.2 # lines between at-risk rows

  # backward compatibility: atrisk_init_pos used to be a (negative) y data
  # coordinate; it is now the number of text lines below the panel bottom
  if (!is.null(atrisk_init_pos) && atrisk_init_pos < 0) {
    warning("atrisk_init_pos is now the number of text lines below the panel bottom ",
            "(positive; e.g. 3). Ignoring the old-style negative value and using the default.")
    atrisk_init_pos<- NULL
  }

  # I need to calculate the number of at-risk at the x_break
  x_break     <- if (is.null(x_break)) {
    layer_scales(p)$x$get_breaks(layer_scales(p)$x$range$range)
  } else {
    x_break[x_break >= min(layer_scales(p)$x$range$range) & x_break<= max(layer_scales(p)$x$range$range)]
  }

  x_break<- if (any(is.na(x_break))) x_break[!is.na(x_break)] else x_break


  risk_tbl<- extract_atrisk(surv_obj, time.list= x_break)
  nstrata <- ncol(risk_tbl)-1

  # header position in text lines below the panel bottom (device-independent).
  # svglite measurement (default theme): the x-axis tick labels + title occupy the
  # first 2.06 lines, and text is 11/13.2 = 0.83 line tall. With >1 group the header
  # starts on the line right after the axis title (2.06 + 0.17); with a single group
  # it sits exactly one full line of whitespace below it (2.06 + 1)
  if (is.null(atrisk_init_pos)) atrisk_init_pos<- if (nstrata> 1) 2.23 else 3.06

  strata_col<- unique(layer_data(p)$colour)
  strata_col<- if (nstrata==1) "black" else if (length(strata_col)< nstrata) rep_len(strata_col, nstrata) else strata_col

  # where there are no strata, the numbers at-risk are displayed at the same level as
  # 'At-risk N:'; otherwise each stratum gets its own row below it
  row_lines<- if (nstrata==1) atrisk_init_pos else atrisk_init_pos + seq_len(nstrata)*atrisk_row_inc

  # one grob per table column: the x position comes from annotation_custom (data
  # coordinates), the y positions are absolute text lines below the panel bottom
  # so the spacing does not change with figure or panel size; the labels end
  # 2 characters left of the first time point so they clear its centered counts
  label_col<- textGrob(
    label= c("At-risk N:",
             if (nstrata> 1) paste0(colnames(risk_tbl)[-1], ":")),
    x= unit(0, "npc") - unit(2, "char"),
    y= unit(0, "npc") - unit(c(atrisk_init_pos, if (nstrata> 1) row_lines), "lines"),
    hjust= 1, vjust= 1,
    gp= gpar(fontfamily= font_family, fontsize= font_size,
             col= c("black", if (nstrata> 1) strata_col),
             fontface= c("bold", if (nstrata> 1) rep("plain", nstrata))))

  out<- p + annotation_custom(label_col, xmin= 0, xmax= 0, ymin= -Inf, ymax= Inf)

  for (i in seq_along(risk_tbl$time)) {
    count_col<- textGrob(
      label= formatC(unlist(risk_tbl[i, -1]), digits= 0, format= "d", big.mark= ",", flag= "#"),
      y= unit(0, "npc") - unit(row_lines, "lines"),
      hjust= 0.5, vjust= 1,
      gp= gpar(fontfamily= font_family, fontsize= font_size, col= strata_col))
    out<- out + annotation_custom(count_col,
                                  xmin= risk_tbl$time[i], xmax= risk_tbl$time[i],
                                  ymin= -Inf, ymax= Inf)
  }
  out
}

#' @title estimate_km
#'
#' @details
#' The function analyzes the data (df) using Kaplan-Meier survival method with pointwise 95\% CI estimated using log-log
#' transformation (same as SAS's default). The function store the input data in the call(), which can be used in
#' run_logrank_test().
#'
#' @param df a dataframe containing the analysis variables
#' @param evt_time the unquoted name of the event/censoring time variable
#' @param evt the unquoted name of the event indicator variable, as accepted by \code{survival::Surv()}
#' @param group the unquoted name of an optional grouping variable; omit it for a single overall curve
#' @param ci_transformation the confidence-interval transformation passed to \code{survfit()} as conf.type (default "log-log")
#' @param ... additional arguments passed to \code{survival::survfit()}
#' @return a survfit object whose call embeds the input data, so run_logrank_test() can re-evaluate it
#' @export
estimate_km<- function(df, evt_time, evt, group, ci_transformation = "log-log", ...) {

  evt_time <- rlang::enquo(evt_time)
  evt <- rlang::enquo(evt)
  group <- rlang::enquo(group)
  args_surfit <- rlang::enquos(...)

  out <- if (quo_is_missing(group)) {
    rlang::quo_squash(
      rlang::quo(
        substitute(
          survfit(Surv(!!evt_time, !!evt) ~ 1,
            data = df,
            conf.type = ci_type,
            !!!args_surfit
          ),
          list(
            df = df,
            ci_type = ci_transformation
          )
        )
      )
    )
  } else {
    rlang::quo_squash(
      rlang::quo(
        substitute(
          survfit(Surv(!!evt_time, !!evt) ~ !!group,
            data = df,
            conf.type = ci_type,
            !!!args_surfit
          ),
          list(
            df = df,
            ci_type = ci_transformation
          )
        )
      )
    )
  }
  return(eval(eval(out)))
}

#' @title run_logrank_test
#'
#' @details
#' The function re-evaluates the survfit call stored in a survfit object (e.g. one
#' created by estimate_km(), which embeds the data in the call) as
#' \code{survival::survdiff()} with rho= 0, and returns the log-rank test p-value.
#'
#' @param surv_obj a survfit object whose call embeds the data and a grouping variable, e.g. from estimate_km()
#' @return the log-rank p-value (a numeric scalar)
#' @export
run_logrank_test<- function(surv_obj) {

  tmp<- surv_obj$call
  # namespace the survival symbols: the call is evaluated in the caller's
  # frame, where survival may not be attached now that it is only an import
  tmp[[1]]<- quote(survival::survdiff)
  if (is.call(tmp[[2]]) && identical(tmp[[2]][[2]][[1]], as.name("Surv")))
    tmp[[2]][[2]][[1]]<- quote(survival::Surv)
  tmp$rho<- 0
  tmp$conf.type<- NULL
  test<- eval(tmp, parent.frame())

  pval<- pchisq(test$chisq, df= length(test$n) - 1, lower.tail = FALSE)
  pval
}


#' @title show_surv
#'
#' @details
#' The function shows Kaplan-Meier survival (or failure) curves with and without
#' strata, with optional confidence-interval ribbons, an at-risk table and the
#' log-rank p-value.
#'
#' @param surv_obj a survfit object, as returned by estimate_km().
#' @param x_lab the x-axis label.
#' @param y_lab the y-axis label; the default depends on plot_cdf.
#' @param y_lim a numeric vector of length 2 specifying the y-axis limits; the lower limit is always reset to 0, and NULL gives c(0, 1). Curves are truncated at an upper limit below 1.
#' @param x_break a numeric vector specifying the x-axis break points (default: automatic).
#' @param y_break a numeric vector specifying the y-axis break points (default: automatic).
#' @param color_scheme the color palette used for strata: "brewer", "grey", "viridis", or "manual".
#' @param color_list a list of scale arguments, required only when color_scheme= 'manual' (e.g. list(values= c('red', 'blue'))).
#' @param plot_theme a ggplot2 theme object applied to the plot.
#' @param add_ci a logical parameter indicating whether confidence interval ribbons should be added to the plot.
#' @param add_atrisk a logical parameter indicating whether at-risk table should be added to the figure.
#' @param add_legend a logical parameter indicating whether legend should be added to the figure; forced to FALSE for a single cohort or when the at-risk table is shown (the table is color-coded by cohort instead).
#' @param add_pvalue a logical parameter indicating whether the log-rank p-value (from run_logrank_test()) should be added to the plot; forced to FALSE for a single cohort.
#' @param atrisk_init_pos position of the "At-risk N:" header, in text lines below the panel bottom. Default: with more than one group the header starts on the line right after the x-axis title (2.23); with a single group it sits one full line of whitespace below it (3.06)
#' @param pvalue_pos a character parameter indicating where the p-value should be added to the plot.
#' @param plot_cdf a logical parameter indicating whether the failure function 1 - S(t) should be plotted instead of the survival function.
#' @param print_fig a logical parameter indicating whether the figure should be printed to the active device.
#' @return A ggplot object.
#' @export
show_surv<- function(surv_obj,
                     x_lab= 'Time',
                     y_lab= if (plot_cdf) 'The proportion of deceased subjects' else 'The freedom from death',
                     y_lim= NULL,
                     x_break= NULL,
                     y_break= NULL,
                     color_scheme= c("brewer", "grey", "viridis", "manual"),
                     color_list= NULL, #required only if color_scheme= 'manual'. eg color_list= list(values= c('red', 'blue'))
                     plot_theme= theme_minimal(),

                     add_ci= TRUE,
                     add_atrisk= TRUE,
                     add_legend= FALSE,
                     add_pvalue= TRUE,
                     atrisk_init_pos= NULL,
                     pvalue_pos= c("topleft", "topright", "bottomleft", "bottomright", "left", "right", "top", "bottom"),
                     plot_cdf= FALSE,
                     print_fig = TRUE) {

  # no need to add pvalues for a single cohort
  add_pvalue <- if (!"strata" %in% names(surv_obj)) FALSE else add_pvalue
  # no need to add legend if it is a single cohort or add risk (when it is >1 cohorts). The at-risk table will be color-coded to indicate cohort
  add_legend <- if (!"strata" %in% names(surv_obj) || add_atrisk) FALSE else add_legend

  color_scheme <- match.arg(color_scheme)
  if (color_scheme=='manual' && is.null(color_list)) stop("Please provide a list of color value(s).")

  scale_pair<- event_time_color_scales(color_scheme, color_list)

  curve_name<- if (plot_cdf) "failure" else "survival"
  if (is.null(y_lim)) {
    y_lim<- c(0, 1)
    message("The parameter y_lim was set to y_lim= c(0, 1) for ", curve_name, " function.")
  } else {
    y_lim<- c(0, max(y_lim, na.rm= TRUE))
    message("The lower limit of y-axis was reset to 0 for ", curve_name, " function.")
  }

  #---- prepare survfit for plot ----
  surv_mat<- prepare_survfit(surv_obj)

  plot_prob_d<- surv_mat %>%
    dplyr::select(strata, plot_prob_d) %>%
    tidyr::unnest(cols = c(plot_prob_d)) %>%
    dplyr::mutate(prob= if (plot_cdf) 1-prob else prob)

  if (y_lim[2] < 1) {
    plot_prob_d <- dplyr::mutate(plot_prob_d, prob = pmin(prob, y_lim[2], na.rm = TRUE))
  }

  out<- ggplot() +
    geom_step(data= plot_prob_d,
              aes(x= time, y= prob, group= strata, color= strata),
              linewidth = 1.1, show.legend = add_legend) +
    scale_pair$colour +
    scale_x_continuous(name  = x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                       expand= c(0.01, 0),
                       labels= function(x) scales::number(x, accuracy = 1))

  if (add_ci) {
    plot_ci_d<- surv_mat %>%
      dplyr::select(strata, plot_ci_d) %>%
      unnest(cols = c(plot_ci_d))

    if (plot_cdf) {
      plot_ci_d<- plot_ci_d %>%
        mutate(across(starts_with('conf'), function(x) 1-x)) %>%
        rename(conf_high= conf_low,
               conf_low = conf_high)
    }

    out<- out +
      geom_ribbon_step(data= plot_ci_d,
                  aes(x= time, ymin= conf_low, ymax= conf_high, fill= strata),
                  alpha= .2, show.legend = FALSE) +
      scale_pair$fill
  }

  out<- out + scale_y_continuous(name  = y_lab,
                                 breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                                 expand= c(0.005, 0),
                                 labels= function(x) scales::percent(x, accuracy = 1))

  out <- out + coord_cartesian(ylim = y_lim, clip = "on")

  if (add_pvalue) {
    pval<- run_logrank_test(surv_obj) %>%
      format_pvalue()
    pval<- ifelse(trimws(pval)=="<0.001", "Log-rank p< 0.001", paste0("Log-rank p= ", pval) )

    out<- annotate_pvalue(out, pval, match.arg(pvalue_pos), plot_theme)
  }

  if (add_atrisk) out<- add_atrisk(out,
                                   surv_obj = surv_obj,
                                   x_break = x_break,
                                   atrisk_init_pos= atrisk_init_pos,
                                   plot_theme = plot_theme)

  out<- out + plot_theme

  if (print_fig) print(out)
  return(out)
}




#' @title estimate_cif
#'
#' @details
#' The function analyzes the competing data (df) using Aalen-Johansen method in estimating cumulative incidence
#' function. The function store the input data in the call(), which can be used in run_gray_test().
#'
#' @param df a dataframe containing the analysis variables
#' @param evt_time the unquoted name of the event/censoring time variable
#' @param evt the unquoted name of the event-status variable (typically a factor whose first level codes censoring), as accepted by \code{survival::Surv()} for multi-state data
#' @param group the unquoted name of an optional grouping variable; omit it for a single overall estimate
#' @param ... additional arguments passed to \code{survival::survfit()}
#' @return a survfitms object whose call embeds the input data, so run_gray_test() can re-evaluate it
#' @export
estimate_cif<- function(df, evt_time, evt, group, ...) {

  evt_time<- enquo(evt_time)
  evt     <- enquo(evt)
  group   <- enquo(group)

  out<- if (quo_is_missing(group)) {
    substitute(survfit(Surv(evt_time, evt) ~ 1, data= df, ...),
               list(evt_time= quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    df      = df))
  } else {
    substitute(survfit(Surv(evt_time, evt) ~ grp, data= df, ...),
               list(evt_time= quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    grp     = quo_get_expr(group),
                    df      = df))
  }
  out<- eval(out)
  out
}


#' @title run_gray_test
#'
#' @details
#' The function runs Gray's test for equality of the cumulative incidence functions
#' across groups (via \code{cmprsk::cuminc()}), re-using the data and the time,
#' status and group variables stored in the call of a survfitms object created by
#' estimate_cif().
#'
#' @param surv_obj a survfitms object whose call embeds the data and a grouping variable, e.g. from estimate_cif()
#' @param evt_type the event-type code(s) whose p-value(s) to return (default 1:2); NULL returns every event type except the first
#' @return a numeric vector of Gray's test p-values, one per requested event type
#' @export
run_gray_test<- function(surv_obj, evt_type= 1:2) {

  df<- as.list(eval(surv_obj$call$data, parent.frame()))
  df<- all.vars(surv_obj$call$formula) %>%
    setNames(c("ftime", "fstatus", "group")) %>%
    lapply(function(x) df[[x]])

  test<- do.call('cuminc', df)
  nn<- rownames(test$Tests)
  pval<- test$Tests[ (if ( !is.null(evt_type) ) match(evt_type, nn) else -1), "pv"]
  pval
}

#' @title show_cif
#'
#' @details
#' The function shows the cumulative incidence function for competing risks with and without strata.
#'
#' @param surv_obj a survfitms object, as returned by estimate_cif().
#' @param evt_type the event-type code(s) (as encoded in surv_obj's states) to plot the cumulative incidence for.
#' @param evt_label a function (or named vector) mapping event-type codes to display labels for the legend/strata.
#' @param add_ci a logical parameter indicating whether confidence interval ribbons should be added to the plot.
#' @param add_atrisk a logical parameter indicating whether at-risk table should be added to the figure.
#' @param add_legend a logical parameter indicating whether legend should be added to the figure.
#' @param add_pvalue a logical parameter (default= FALSE) indiciates if a p-value should be added to the plot.
#' @param atrisk_init_pos position of the "At-risk N:" header, in text lines below the panel bottom. Default: with more than one group the header starts on the line right after the x-axis title (2.23); with a single group it sits one full line of whitespace below it (3.06)
#' @param pvalue_pos a character parameter indicating where the p-value should be added to the plot.
#' @param plot_theme a ggplot2 theme object applied to the plot.
#' @param x_lab the x-axis label.
#' @param y_lab the y-axis label.
#' @param x_lim a numeric vector of length 2 specifying the x-axis limits.
#' @param y_lim a numeric vector of length 2 specifying the y-axis limits.
#' @param x_break a numeric vector specifying the x-axis break points (default: automatic).
#' @param y_break a numeric vector specifying the y-axis break points (default: automatic).
#' @param color_scheme the color palette used for strata: "brewer", "grey", "viridis", or "manual".
#' @param color_list a list of scale arguments, required only when color_scheme= 'manual' (e.g. list(values= c('red', 'blue'))).
#' @param plot_cdf Not used
#' @param print_fig a logical parameter indicating whether the figure should be printed to the active device.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' # requires internet access to fetch the example dataset
#' my_plot_theme<- theme_bw() +
#' theme(axis.title  = element_text(size= 14, family="Arial"),
#'       axis.title.x= element_text(margin= unit(c(t= 1, r = 0, b = 0, l = 0), "lines"), family="Arial"),
#'       axis.title.y= element_text(margin= unit(c(t= 0, r = 1, b = 0, l = 0), "lines"), family="Arial"),
#'       axis.text   = element_text(size= 12, family="Arial"),
#'       # set the axis line (i.e. bty= "l" in traditional plot)
#'       axis.line.x = element_line(color="black", size = 1),
#'       axis.line.y = element_line(color="black", size = 1),
#'
#'       # specify the legend (e.g. position, color, justification)
#'       legend.title= element_blank(),
#'       legend.background= element_rect(fill= "transparent"),
#'       legend.text      = element_text(family= "Arial", size = 12),
#'       legend.margin    = margin(t = 0, r = 0, b = 0, l = 0,"bigpts"),
#'       legend.key       = element_rect(fill= "transparent", color= "transparent", linetype= "blank"),
#'       legend.key.size  = unit(24, "bigpts"),
#'       # legend.position= c(0.98, 0.02),
#'       # legend.justification= c(1, 0),
#'
#'       #eliminates background, gridlines, and chart border
#'       panel.border = element_blank(),
#'       # plot.background = element_blank(),
#'       # panel.grid.major = element_blank(),
#'       panel.grid.minor = element_blank(),
#'       panel.spacing = unit(1, "lines"),
#'
#'       strip.background = element_rect(fill = "black", color= "white"),
#'       strip.text = element_text(family = "Arial", color= "white", size = 12, face= "bold"),
#'
#'       # plot margin. Sufficient margins must be specified for printing at-risk patients.
#'       plot.margin= unit(c(top= 0.1, right= 0.05, bottom= .3, left= .2), "npc"))
#'
#' # read in the example data
#' cmprisk_df<- read.csv2("http://www.stat.unipg.it/luca/misc/bmt.csv")
#' admin_censor_cmprisk(cmprisk_df, ftime, status, evt_label = c("0"= "Event free", "1"= "Event", "2"= "Competing event"), adm_cnr_time= 10)
#' p<- cmprisk_df %>%
#' admin_censor_cmprisk(ftime, status,
#'                      adm_cnr_time= 30,
#'                      overwrite_var = TRUE) %>%
#'   estimate_cif(ftime, status) %>%
#'   show_cif(evt_type = 1,
#'            plot_theme= my_plot_theme,
#'            x_break= seq(0, 10, by= 1),
#'            y_break= seq(0, .8, by= .1),
#'            add_pvalue = TRUE,
#'            add_atrisk = TRUE,
#'            add_ci = TRUE,
#'            add_legend = FALSE,
#'            color_scheme = "manual",
#'            color_list = list(values= c('blue', 'blue')),
#'            pvalue_pos= "topleft")
#'
#' #---- To show the at risk information ----
#' gt <- ggplot_gtable(ggplot_build(p))
#' gt$layout$clip[gt$layout$name == 'panel'] <- "off"
#' grid.draw(gt)
#' }
#' @export
show_cif<- function(surv_obj,
                    evt_type = 1,
                    evt_label= function(x) {
                      recode_factor(x,
                                    `1`= "Event",
                                    `2`= "Competing event",
                                    .default= "Event free")
                    },
                    add_ci= TRUE,
                    add_atrisk= TRUE,
                    add_legend= FALSE,
                    add_pvalue= TRUE,
                    atrisk_init_pos= NULL,
                    pvalue_pos= c("bottomright", "topleft", "topright", "bottomleft", "left", "right", "top", "bottom"),

                    plot_theme= theme_minimal(),
                    x_lab= 'Time',
                    y_lab= 'Proportion of subjects',
                    x_lim= NULL,
                    y_lim= NULL,
                    x_break= NULL,
                    y_break= NULL,
                    color_scheme= c("brewer", "grey", "viridis", "manual"),
                    color_list= NULL, #required only if color_scheme= 'manual'. eg color_list= list(values= c('red', 'blue'))

                    plot_cdf= FALSE,
                    print_fig = TRUE) {

  #---- prepare survfit for plot ----
  cmprisk_mat<- prepare_survfit(surv_obj)
  cmprisk_mat<- cmprisk_mat %>%
    filter(state %in% evt_type) %>%
    mutate(state_label = evt_label(state),
           state_label = fct_drop(state_label),
           state       = fct_drop((state)),
           state_strata= interaction(state_label, strata, drop= TRUE, sep= ": "))

  plot_prob_d<- cmprisk_mat %>%
    dplyr::select(strata, state, state_label, state_strata, plot_prob_d) %>%
    unnest(cols = c(plot_prob_d))

  add_pvalue<- if (nlevels(plot_prob_d$strata)==1) FALSE else add_pvalue
  add_legend<- if (nlevels(plot_prob_d$strata)==1 &&
                   nlevels(plot_prob_d$state) ==1) FALSE else add_legend

  color_scheme<- match.arg(color_scheme)
  if (color_scheme=='manual' && is.null(color_list)) stop("Please provide a list of color value(s) when a manual color scheme is specified.")

  scale_pair<- event_time_color_scales(color_scheme, color_list, grey_end = 0.65, blank_guide_title = TRUE)

  out<- ggplot()
  out<- if (nlevels(plot_prob_d$strata)==1 & nlevels(plot_prob_d$state)>1) {
    out +
      geom_step(data= plot_prob_d,
                aes(x= time, y= prob, group= state_label, color= state_label),
                linewidth= 1.1, show.legend = add_legend)
  } else if (nlevels(plot_prob_d$strata)>1 & nlevels(plot_prob_d$state)==1) {
    out +
      geom_step(data= plot_prob_d,
                aes(x= time, y= prob, group= strata, color= strata),
                linewidth= 1.1, show.legend = add_legend)
  } else {
    out +
      geom_step(data= plot_prob_d,
                aes(x= time, y= prob, group= state_strata, color= state_strata),
                linewidth= 1.1, show.legend = add_legend)
  }
  out<- out +
    scale_pair$colour +
    scale_x_continuous(name  = x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                       expand= c(0.01, 0.005),
                       labels= function(x) scales::comma(x, accuracy = 1)) +
    scale_y_continuous(name  = y_lab,
                       breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                       expand= c(0.01, 0),
                       labels= function(x) scales::percent(x, accuracy = 1))

  out<- if (!is.null(x_lim) || !is.null(y_lim)) out + coord_cartesian(xlim= x_lim, ylim = y_lim, clip = "on") else out

  if (add_ci) {
    plot_ci_d<- cmprisk_mat %>%
      dplyr::select(strata, state, state_label, state_strata, plot_ci_d) %>%
      unnest(cols = c(plot_ci_d))

    out<- if (nlevels(plot_prob_d$strata)==1 & nlevels(plot_prob_d$state)>1) {

      out +
        geom_ribbon_step(data= plot_ci_d,
                    aes(x   = time,
                        ymin= conf_low,
                        ymax= conf_high,
                        group= state_label,
                        fill= state_label),
                    alpha= .2,
                    show.legend = FALSE)

    } else if (nlevels(plot_prob_d$strata)>1 & nlevels(plot_prob_d$state)==1) {

      out +
        geom_ribbon_step(data= plot_ci_d,
                    aes(x= time,
                        ymin = conf_low,
                        ymax = conf_high,
                        group= strata,
                        fill = strata),
                    alpha= .2,
                    show.legend = FALSE)

    } else {

      out +
        geom_ribbon_step(data= plot_ci_d,
                    aes(x= time,
                        ymin= conf_low,
                        ymax= conf_high,
                        group= state_strata,
                        fill = state_strata),
                    alpha= .2,
                    show.legend = FALSE)

    }

    out<- out + scale_pair$fill

  }

  if (add_pvalue) {
    pval<- run_gray_test(surv_obj, evt_type = evt_type) %>%
      format_pvalue()
    pval<- ifelse(trimws(pval)=="<0.001", "Gray's p< 0.001", paste0("Gray's p= ", pval) )

    out<- annotate_pvalue(out, pval, match.arg(pvalue_pos), plot_theme)
  }

  if (add_atrisk) out<- add_atrisk(out,
                                   surv_obj = surv_obj,
                                   x_break = x_break,
                                   atrisk_init_pos= atrisk_init_pos,
                                   plot_theme = plot_theme)

  out<- out + plot_theme

  if (print_fig) print(out)
  return(out)
}
