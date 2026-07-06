#' @title extract_atrisk
#'
#' @details
#' The function creates a dataframe containing the number of at risk patients at time-, overall or by strata
#'
#' @param fit a survfit object
#' @param time.list a numeric vector specifying the time points at which the number of at-risk subjects is calculated.
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

    # wide, plain data.frame (time + one integer column per stratum) — the shape
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
      # state    = rep(surv_obj$state, each= length(surv_obj$time)),
      state = rep(
        replace(surv_obj$state, nchar(surv_obj$state) == 0 | grepl("0", surv_obj$state), "0"),
        each = length(surv_obj$time)
      ),
      time = rep(surv_obj$time, length(surv_obj$state)),
      prob = as.numeric(surv_obj$pstate),
      conf_low = as.numeric(surv_obj$lower),
      conf_high = as.numeric(surv_obj$upper)
    )
    # if (class(out$strata) != "factor") out$strata <- factor(out$strata)
    if (class(out$state) != "factor") out$state <- relevel(factor(out$state), ref = "0")
    out
  }

  prepare_surv <- function(surv_obj) {
    # set up strata
    nstrat <- if (is.null(surv_obj$strata)) 1 else length(surv_obj$strata)
    if (is.null(surv_obj$strata)) {
      # stemp <- rep(1, length(surv_obj$time)) # same length as stime
      stemp <- factor(rep(1, length(surv_obj$time)), 1, labels = c("Overall"))
    } else {
      strata_lab <- sapply(strsplit(names(surv_obj$strata), "="), function(x) x[2])
      # stemp <- rep(1:nstrat, surv_obj$strata) # same length as stime
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

  out <- if (any(class(surv_obj) == "survfitms")) {
    surv_obj %>%
      prepare_cmprisk() %>%
      # dplyr::group_by(strata, state) %>%
      tidyr::nest(.by = c(strata, state)) %>%
      dplyr::mutate(
        plot_prob_d = purrr::map2(
          state, data,
          function(state, df) {
            df %>%
              dplyr::select(one_of("time", "prob")) %>%
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
      # dplyr::group_by(strata) %>%
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
              dplyr::select(one_of("time", "prob"))

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
          nn <- nrow(df)
          # check http://stackoverflow.com/questions/33874909/how-do-i-add-shading-and-color-to-the-confidence-intervals-in-ggplot-2-generated
          ys <- rep(1:nn, each = 2)[-2 * nn]
          xs <- c(1, rep(2:nn, each = 2))

          df %$%
            dplyr::tibble(
              time = time[xs],
              conf_low = conf_low[ys],
              conf_high = conf_high[ys]
            ) # %>%
          # filter(!(is.na(conf_low) & is.na(conf_high)))
        }
      )
    )
  return(out)
}

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
#' The function analyzes the data (df) using Kaplan-Meier survival method with pointwise 95% CI estimated using log-log
#' transformation (same as SAS's defualt). The function store the input data in the call(), which can be used in
#' run_logrank_test().
#'
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

#' @export
run_logrank_test<- function(surv_obj) {

  tmp<- surv_obj$call
  tmp[[1]]<- as.name("survdiff")
  tmp$rho<- 0
  tmp$conf.type<- NULL
  test<- eval(tmp, parent.frame())

  pval<- pchisq(test$chisq, df= length(test$n) - 1, lower.tail = FALSE)
  pval
}


#' @export
show_surv<- function(surv_obj,
                     x_lab= 'Time',
                     y_lab= if (plot_cdf) 'The proportion of deceased subjects' else 'The freedom from death',
                     # x_lim= NULL,
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
  add_pvalue <- if (all(names(surv_obj) != 'strata')) FALSE else add_pvalue
  # no need to add legend if it is a single cohort or add risk (when it is >1 cohorts). The at-risk table will be color-coded to indicate cohort
  add_legend <- if (all(names(surv_obj) != 'strata') | add_atrisk) FALSE else add_legend

  color_scheme <- match.arg(color_scheme)
  if (color_scheme=='manual' & is.null(color_list)) stop("Please provide a list of color value(s).")

  scale_pair<- event_time_color_scales(color_scheme, color_list)

  if (!plot_cdf & !is.null(y_lim)) {
    y_lim<- c(0, 1)
    message("The parameter y_lim was reset to y_lim= c(0, 1) for survival function.")
  } else if (plot_cdf & !is.null(y_lim)) {
    y_lim<- c(0, max(y_lim, na.rm= TRUE))
    message("The lower limit of y-axis was reset to 0 for failure function.")
  } else if (!plot_cdf & is.null(y_lim)) {
    y_lim<- c(0, 1)
    message("The parameter y_lim was set to y_lim= c(0, 1) for survival function.")
  } else if (plot_cdf & is.null(y_lim)) {
    y_lim<- c(0, 1)
    message("The parameter y_lim was set to y_lim= c(0, 1) for failure function.")
  }

  #---- prepare survfit for plot ----
  surv_mat<- prepare_survfit(surv_obj)

  plot_prob_d<- surv_mat %>%
    dplyr::select(strata, plot_prob_d) %>%
    tidyr::unnest(cols = c(plot_prob_d)) %>%
    dplyr::mutate(prob= if (plot_cdf) 1-prob else prob)

  if (y_lim[2] < 1) {
    plot_prob_d <- plot_prob_d %>%
      dplyr::mutate(prob = pmin(prob, y_lim[2], na.rm = TRUE)) %>%
      dplyr::group_by(strata)
  }

  out<- ggplot() +
    geom_step(data= plot_prob_d,
              aes(x= time, y= prob, group= strata, color= strata),
              linewidth = 1.1, show.legend = add_legend) +
    scale_pair$colour +
    scale_x_continuous(name  = x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                      #  expand= c(0.01, 0.005),
                       expand= c(0.01, 0),
                       labels= function(x) scales::number(x, accuracy = 1))

  if (add_ci) {
    plot_ci_d<- surv_mat %>%
      dplyr::select(strata, plot_ci_d) %>%
      unnest(cols = c(plot_ci_d))

    if (plot_cdf) {
      plot_ci_d<- plot_ci_d %>%
        mutate_at(vars(starts_with('conf')), function(x) 1-x) %>%
        rename(conf_high= conf_low,
               conf_low = conf_high)
    }

    out<- out +
      geom_ribbon(data= plot_ci_d,
                  aes(x= time, ymin= conf_low, ymax= conf_high, fill= strata),
                  alpha= .2, show.legend = FALSE) +
      scale_pair$fill
  }

  out<- out + scale_y_continuous(name  = y_lab,
                                 breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                                 expand= c(0.005, 0),
                                 labels= function(x) scales::percent(x, accuracy = 1))

  out <- out + if (!is.null(y_lim)) coord_cartesian(ylim = y_lim, clip = "on") else coord_cartesian(clip = "on")

  if (add_pvalue) {
    pval<- run_logrank_test(surv_obj) %>%
      format_pvalue()
    # pval<- format_pvalue(pval)
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
  # print(out, vp= viewport(width = unit(6.5, "inches"), height = unit(6.5, "inches")))
  return(out)
}




#' @title estimate_cif
#'
#' @details
#' The function analyzes the competing data (df) using Andersen-Johansen method in estimating cumulative incidence
#' function.The function store the input data in the call(), which can be used in run_gray_test().
#'
#'
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
#' @param surv_obj a survfitms subject.
#' @param x_lab an integer vector indicating right censoring (0= censored; 1= event of interest; other= competing risk(s)).
#' @param y_lab a numeric vector specifying the time point at which administrative censoring is applied.
#' @param x_lim a numeric vector specifying the time point at which administrative censoring is applied.
#' @param y_lim a logical scalar (default= FALSE) indiciates if the existing time-to-event variables should be overwritten.
#' @param color_list input data
#' @param plot_theme a numeric vector recording the time points at which the event occurs.
#' @param add_ci an integer vector indicating right censoring (0= censored; 1= event of interest; other= competing risk(s)).
#' @param add_atrisk a logical parameter indicating whether at-risk table should be added to the figure.
#' @param add_legend a logical parameter indicating whether legend should be added to the figure.
#' @param add_pvalue a logical parameter (default= FALSE) indiciates if a p-value should be added to the plot.
#' @param pvalue_pos a character parameter indicating where the p-value should be added to the plot.
#' @param atrisk_init_pos position of the "At-risk N:" header, in text lines below the panel bottom. Default: with more than one group the header starts on the line right after the x-axis title (2.23); with a single group it sits one full line of whitespace below it (3.06)
#' @param plot_cdf Not used
#' @return A ggplot object.
#' @examples
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
#' @export
show_cif<- function(surv_obj,
                    evt_type = 1,
                    # evt_label= identity, # identity function
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
  add_legend<- if ((nlevels(plot_prob_d$strata)==1 &
                    nlevels(plot_prob_d$state) ==1 )) FALSE else add_legend

  color_scheme<- match.arg(color_scheme)
  if (color_scheme=='manual' & is.null(color_list)) stop("Please provide a list of color value(s) when a manual color scheme is specified.")

  scale_pair<- event_time_color_scales(color_scheme, color_list, grey_end = 0.65, blank_guide_title = TRUE)

  # x_lab<- if (is.null(x_lab)) "Time" else x_lab
  # y_lab<- if (is.null(y_lab)) "Proportion of subjects" else y_lab
  # x_break<- if (is.null(x_break)) scales::pretty_breaks(6) else x_break
  # y_break<- if (is.null(y_break)) scales::pretty_breaks(6) else y_break

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
                       # limits = x_lim,
                       labels= function(x) scales::comma(x, accuracy = 1)) +
    scale_y_continuous(name  = y_lab,
                       breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                       expand= c(0.01, 0),
                       # limits= y_lim,
                       labels= function(x) scales::percent(x, accuracy = 1))

  out<- if (!is.null(x_lim) | !is.null(y_lim)) out + coord_cartesian(xlim= x_lim, ylim = y_lim, clip = "on") else out

  if (add_ci) {
    plot_ci_d<- cmprisk_mat %>%
      dplyr::select(strata, state, state_label, state_strata, plot_ci_d) %>%
      unnest(cols = c(plot_ci_d))

    out<- if (nlevels(plot_prob_d$strata)==1 & nlevels(plot_prob_d$state)>1) {

      out +
        geom_ribbon(data= plot_ci_d,
                    aes(x   = time,
                        ymin= conf_low,
                        ymax= conf_high,
                        group= state_label,
                        fill= state_label),
                    alpha= .2,
                    show.legend = FALSE)

    } else if (nlevels(plot_prob_d$strata)>1 & nlevels(plot_prob_d$state)==1) {

      out +
        geom_ribbon(data= plot_ci_d,
                    aes(x= time,
                        ymin = conf_low,
                        ymax = conf_high,
                        group= strata,
                        fill = strata),
                    alpha= .2,
                    show.legend = FALSE)

    } else {

      out +
        geom_ribbon(data= plot_ci_d,
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
