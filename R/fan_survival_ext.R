construct_surv_var<- function(idx_dt, evt_dt, end_dt, surv_varname= NULL) {

  # date of origin in R:   1970-01-01
  # date of origin in SAS: 1960-01-01

  ## Excel is said to use 1900-01-01 as day 1 (Windows default) or
  ## 1904-01-01 as day 0 (Mac default), but this is complicated by Excel
  ## thinking 1900 was a leap year.
  ## So for recent dates from Windows Excel
  #       as.Date(35981, origin="1899-12-30") # 1998-07-05
  ## and Mac Excel
  #       as.Date(34519, origin="1904-01-01") # 1998-07-05

  idx_dt<- enquo(idx_dt)
  evt_dt<- enquo(evt_dt)
  end_dt<- enquo(end_dt)

  if (quo_is_missing(idx_dt)) stop("No index date (time zero).")
  if (quo_is_missing(evt_dt)) stop("No event date.")
  if (quo_is_missing(end_dt)) stop("No date of the end of follow-up.")

  idx_dt<- as.Date(as.character(idx_dt), origin= "1970-01-01")
  evt_dt<- as.Date(as.character(!!evt_dt), origin= "1970-01-01")
  end_dt<- as.Date(as.character(!!end_dt), origin= "1970-01-01")

  evt<- ifelse(is.na(evt_dt), 0, 1)
  time2evt<- ifelse(is.na(evt_dt), end_dt - idx_dt, evt_dt - idx_dt)

  if (any(time2evt==0)) {
    time2evt<- replace(time2evt, time2evt==0, 0.5)
    warning("Event at time zero")
  }

  if (any(time2evt<0)) {
    time2evt<- replace(time2evt, time2evt<0, NA)
    warning("Negative time-to-event!?")
  }

  out<- data.frame(evt_time= time2evt, evt= evt)
  if (!is.null(surv_varname)) names(out)<- surv_varname

  out
}

# surv_var<- function(idx_dt= NULL, evt_dt= NULL, end_dt= NULL, surv_varname= NULL) {
#   if (is.null(idx_dt)) stop("No index date (time zero).")
#   if (is.null(evt_dt)) stop("No event date.")
#   if (is.null(end_dt)) stop("No date of the end of follow-up.")
#
#   idx_dt<- as.Date(as.character(idx_dt), origin= "1970-01-01")
#   evt_dt<- as.Date(as.character(evt_dt), origin= "1970-01-01")
#   end_dt<- as.Date(as.character(end_dt), origin= "1970-01-01")
#
#   evt<- ifelse(is.na(evt_dt), 0, 1)
#   time2evt<- ifelse(is.na(evt_dt), end_dt - idx_dt, evt_dt - idx_dt)
#
#   if (any(time2evt==0)) {
#     time2evt<- replace(time2evt, time2evt==0, 0.5)
#     warning("Event at time zero")
#   }
#
#   if (any(time2evt<0)) {
#     time2evt<- replace(time2evt, time2evt<0, NA)
#     warning("Negative time-to-event!?")
#   }
#
#   out<- data.frame(time2evt= time2evt, evt= evt)
#   if (!is.null(surv_varname)) names(out)<- surv_varname
#
#   out
# }


# cmprisk_var<- function(idx_dt= NULL, evt_dt= NULL, cmp_evt_dt= NULL, end_dt= NULL, out_var_name= NULL) {
#   # provide cmp_evt_dt as a list if there are multiple competing events
#   # idx_dt     - the date of time zero
#   # evt_dt     - the date of the event of interst. For pts without the event, evt_dt= NA
#   # cmp_evt_dt - the date(s) of competing events. For pts with the competing event, cmp_evt_dt= NA.
#   #            - if there are multiple competing events, include them as list(transplant= cmp_evt_dt1, death= cmp_evt_dt2)
#   #            - if there multiple events occur on the same dates, make the event date to which you want to classify 1 or .5 days early. This applies to the event of interest.
#   # end_dt     - the date of the follow-up for the pts with any of the event and competing event.
#
#   if (is.null(idx_dt)) stop("No index date (time zero).")
#   if (is.null(evt_dt)) stop("No event date.")
#   if (is.null(end_dt)) stop("No date of the end of follow-up.")
#
#   if (is.list(cmp_evt_dt)) {
#     # more than 1 competing event
#     n_cmp_evt<- length(cmp_evt_dt)
#     cmp_evt_desc<- names(cmp_evt_dt)
#
#     # create a list containing dates as its component
#     d<- vector("list", 2 + n_cmp_evt)
#     names(d)<- c("censor_dt", "evt_dt", cmp_evt_desc) # order is important
#     d[[ "censor_dt" ]]<- end_dt
#     d[[ "evt_dt" ]]<- evt_dt
#
#     for (i in cmp_evt_desc) {
#       d[[ i ]]<- cmp_evt_dt[[ i ]]
#     }
#     # for (i in 1:n_cmp_evt) {
#     #   d[[ cmp_evt_desc[i] ]]<- cmp_evt_dt[[ cmp_evt_desc[i] ]]
#     # }
#
#     # convert to a data frame
#     d<- as.data.frame(do.call("cbind", d))
#
#   } else {
#     # single competing event
#     d<- data.frame(censor_dt= end_dt, evt_dt, cmp_evt_dt)
#   }
#
#   # set the censored date NA if there is any event (event of interest or any of the competing events)
#   d$censor_dt[apply(d[, -1], 1, function(x) !all(is.na(x)))]<- NA
#   d<- split(d, 1:nrow(d[1])) # per patient
#
#
#   first_evt_dt<- as.Date( vapply(d, function(x) min( as.numeric(x), na.rm= TRUE), FUN.VALUE = numeric(1L)),
#                           origin= "1970-01-01")
#   first_evt<- mapply(function(d, p) match(p, d),
#                      d= d,
#                      p= as.list(first_evt_dt)) # return the first dates matched with the first event date.
#
#   time2evt<- as.numeric(first_evt_dt - idx_dt)
#   time2evt<- replace(time2evt, time2evt==0, 0.5)
#   #time2evt<- mapply(function(x, idx_dt) x - idx_dt, x= first_evt_dt, idx_dt)
#   evt<- first_evt - 1
#
#   out<- data.frame(time2evt= time2evt, evt= evt)
#   if (!is.null(out_var_name)) names(out)<- out_var_name
#   out
# }


cmprisk_var<- function(idx_dt= NULL, evt_dt= NULL, cmp_evt_dt= NULL, end_dt= NULL, out_var_name= NULL) {
  # provide cmp_evt_dt as a list if there are multiple competing events
  # idx_dt     - the date of time zero
  # evt_dt     - the date of the event of interst. For pts without the event, evt_dt= NA
  # cmp_evt_dt - the date(s) of competing events. For pts with the competing event, cmp_evt_dt= NA.
  #            - if there are multiple competing events, include them as list(transplant= cmp_evt_dt1, death= cmp_evt_dt2)
  #            - if there multiple events occur on the same dates, make the event date to which you want to classify 1 or .5 days early. This applies to the event of interest.
  # end_dt     - the date of the follow-up for the pts with any of the event and competing event.

  if (is.null(idx_dt)) stop("No index date (time zero).")
  if (is.null(evt_dt)) stop("No event date.")
  if (is.null(end_dt)) stop("No date of the end of follow-up.")

  if (is.list(cmp_evt_dt)) {
    # more than 1 competing event
    n_cmp_evt<- length(cmp_evt_dt)
    cmp_evt_desc<- names(cmp_evt_dt)

    # create a list containing dates as its component
    d<- vector("list", 2 + n_cmp_evt)
    names(d)<- c("censor_dt", "evt_dt", cmp_evt_desc) # order is important
    d[[ "censor_dt" ]]<- end_dt
    d[[ "evt_dt" ]]<- evt_dt

    for (i in cmp_evt_desc) {
      d[[ i ]]<- cmp_evt_dt[[ i ]]
    }
    # for (i in 1:n_cmp_evt) {
    #   d[[ cmp_evt_desc[i] ]]<- cmp_evt_dt[[ cmp_evt_desc[i] ]]
    # }

    # convert to a data frame
    d<- as.data.frame(do.call("cbind", d))

  } else {
    # single competing event
    d<- data.frame(censor_dt= end_dt, evt_dt, cmp_evt_dt)
  }

  # set the censored date NA if there is any event (event of interest or any of the competing events)
  d$censor_dt[apply(d[, -1], 1, function(x) !all(is.na(x)))]<- NA
  d<- split(d, 1:nrow(d[1])) # per patient


  first_evt_dt<- as.Date( vapply(d, function(x) min( as.numeric(x), na.rm= TRUE), FUN.VALUE = numeric(1L)),
                          origin= "1970-01-01")
  first_evt<- mapply(function(d, p) match(p, d),
                     d= d,
                     p= as.list(first_evt_dt)) # return the first dates matched with the first event date.

  time2evt<- as.numeric(first_evt_dt - idx_dt)
  time2evt<- replace(time2evt, time2evt==0, 0.5)
  #time2evt<- mapply(function(x, idx_dt) x - idx_dt, x= first_evt_dt, idx_dt)
  evt<- first_evt - 1

  out<- data.frame(time2evt= time2evt, evt= evt)
  if (!is.null(out_var_name)) names(out)<- out_var_name
  out
}

#' @title extract_atrisk
#'
#' @details
#' The function creates a dataframe containing the number of at risk patients at time-, overall or by strata
#'
#' @param fit a survfit object
#' @param time.list a numeric vector specifying the time points at which the number of at-risk subjects is calculated.
#' @return A dataframe containing the number of at risk patients at time-, overall or by strata
extract_atrisk<- function(fit, time.list, time.scale= 1) {

  if (any(names(fit)=="strata")){
    strata_lab<- sapply(strsplit(names(fit$strata), "="), function(x) x[2])

    x<- data.frame(time = fit$time/time.scale,
                   n.risk= if (is.matrix(fit$n.risk)) apply(fit$n.risk, 1, sum) else fit$n.risk,
                   strata = factor(unlist(mapply(rep, x = seq_along(fit$strata), each = fit$strata, SIMPLIFY = FALSE)),
                                   seq_along(fit$strata),
                                   labels = strata_lab))

    # at risk process is right-continuous
    # Steve note: have to fix the following line `rule` for data with delayed entries.
    # approxfun(x= time, y= n.risk, method= "constant", rule = 1:1, f= 1)
    atRisk<- lapply(split(x, x$strata),
                    function(x) with(x,
                                     approxfun(x= time, y= n.risk, method= "constant", rule = 2:1, f= 1)
                    )
    )
    # atRisk<- lapply(split(x, x$strata),
    #                 function(x) with(x, stepfun(x= time, y= c(n.risk, 0), f= 1, right= TRUE)))
    # function(x) stepfun(x= x$time[-1], y= x$n.risk, right= FALSE))
    atRiskPts<- rapply(atRisk,
                       function(x) {
                         out<- x(time.list)
                         replace(out, is.na(out), 0)
                       }, how = "unlist")
    atRiskPts<-  matrix(atRiskPts, nrow= length(time.list), byrow= FALSE)
    rownames(atRiskPts)<- time.list
    colnames(atRiskPts)<- strata_lab
    atRiskPts<- as.data.frame(atRiskPts) %>%
      mutate_all(as.integer) %>%
      # rownames_to_column("time") %>%
      mutate(time= time.list) %>%
      dplyr::select(time, everything())
    # atRiskPts<- matrix(atRiskPts, ncol= length(time.list), byrow= TRUE)
    # rownames(atRiskPts)<- names(fit$strata)
    # rownames(atRisk)
    # atRiskPts<- rbind(time= time.list, atRiskPts)
  }
  else {
    x<- data.frame(time = fit$time/time.scale,
                   n.risk=  if (is.matrix(fit$n.risk)) apply(fit$n.risk, 1, sum) else fit$n.risk,
                   strata= factor(1, 1, labels = "Overall"))

    # x<- with(fit, data.frame(time = time/time.scale,
    #                          n.risk=  with(fit, if (is.matrix(n.risk)) apply(n.risk, 1, sum) else n.risk)
    # )
    # )
    # atRisk<- with(x, stepfun(x= time, y= c(n.risk, 0), f= 1, right= TRUE))
    # atRisk<- with(x, approxfun(x= time, y= n.risk, method= "constant", rule = 2:1, f= 1))
    # Steve note: have to fix the following line `rule` for data with delayed entries.
    # approxfun(x= time, y= n.risk, method= "constant", rule = 1:1, f= 1)
    atRisk<- with(x, approxfun(x= time, y= n.risk, method= "constant", rule = 2:1, f= 1))
    atRiskPts<- atRisk(time.list)
    # atRiskPts<- rbind(time.list, atRiskPts)
    # atRiskPts<- rbind(time.list, replace(atRiskPts, is.na(atRiskPts), 0))
    atRiskPts<- data.frame(time= time.list, Overall= as.integer(atRiskPts))
  }
  return(atRiskPts)
}




#' @title admin_censor_surv
#'
#' @details
#' The function creates time-to-event variables with the application of administrative censoring for a binary (survival) process.
#' The newly created variables are named by the same variables names but with a suffix of '_adm' by default. The original
#' variables can be overwritten by specifying overwrite_var= TRUE. Overwriting the original variables is not recommended, but
#' it can be useful in some situations.
#'
#' @param df input data
#' @param evt_time a numeric vector recording the time points at which the event occurs.
#' @param evt an integer vector indicating right censoring (0= censored; 1= event).
#' @param adm_cnr_time a numeric scalar specifying the time point at which administrative censoring is applied.
#' @param overwrite_var a logical scalar (default= FALSE) indiciates if the existing time-to-event variables should be overwritten.
#' @return The input data plus censored time-to-event variables.
#' @example
#' aml %>% admin_censor_surv(evt_time= time, evt= status) # No admin censoring
#' aml %>% admin_censor_surv(evt_time= time, evt= status, adm_cnr_time= 30)
#' aml %>% admin_censor_surv(evt_time= time, evt= status, adm_cnr_time= 30, overwrite_var= TRUE)
admin_censor_surv<- function(df, evt_time, evt, adm_cnr_time= NULL, overwrite_var= FALSE) {
  ######################################################################################
  ## the function creates administrately censored version of event time and indicator ##
  ## for survival (binary) process                                                    ##
  ##   df - input dataframe                                                           ##
  ##   evt_time - continuous time to event                                            ##
  ##   evt - event indicator (1= event; 0= non-event)                                 ##
  ##   adm_cnr_time - time at which admin censoring is applied                        ##
  ######################################################################################

  evt_time<- enquo(evt_time)
  evt     <- enquo(evt)

  if (!is.null(adm_cnr_time)) {

    if (overwrite_var) {
      cnr_evt_time_name<- quo_name(evt_time)
      cnr_evt_name     <- quo_name(evt)
    } else {
      cnr_evt_time_name<- paste0(quo_name(evt_time), "_adm")
      cnr_evt_name     <- paste0(quo_name(evt), "_adm")
    }

    df<- df %>%
      mutate(!!cnr_evt_time_name := replace(!!evt_time, !!evt_time>adm_cnr_time, adm_cnr_time),
             !!cnr_evt_name      := replace(!!evt, !!evt_time> adm_cnr_time & !!evt!=0, 0),
      )
  }

  df
}

#' @title admin_censor_cmprisk
#'
#' @details
#' The function creates time-to-event variables with the application of administrative censoring for a competing risk
#' analysis. The newly created variables are named by the same variables names but with a suffix of '_adm' by default.
#' The original variables can be overwritten by specifying overwrite_var= TRUE. Overwriting the original variables is
#' not recommended, but it can be useful in some situation.
#'
#' @param df input data
#' @param evt_time a numeric vector recording the time points at which the event occurs.
#' @param evt an integer vector indicating right censoring (0= censored; 1= event of interest; other= competing risk(s)).
#' @param adm_cnr_time a numeric vector specifying the time point at which administrative censoring is applied.
#' @param evt_label a numeric vector specifying the time point at which administrative censoring is applied.
#' @param overwrite_var a logical scalar (default= FALSE) indiciates if the existing time-to-event variables should be overwritten.
#' @return The input data plus censored time-to-event variables.
#' @example
#' cmprisk_df<- read.csv2("http://www.stat.unipg.it/luca/misc/bmt.csv")
#' admin_censor_cmprisk(cmprisk_df, ftime, status, evt_label = c("0"= "Event free", "1"= "Event", "2"= "Competing event"), adm_cnr_time= 10)
admin_censor_cmprisk<- function(df, evt_time, evt, adm_cnr_time= NULL, evt_label= NULL, overwrite_var= FALSE) {

  evt_time<- enquo(evt_time)
  evt<- enquo(evt)

  if (is.null(adm_cnr_time)) {

    if (!is.null(evt_label)) {
      df<- df %>%
        mutate(!!quo_name(evt):= factor(!!evt, as.integer(names(evt_label)), labels = evt_label))
    }

  } else {

    if (overwrite_var) {
      cnr_evt_time_name<- quo_name(evt_time)
      cnr_evt_name     <- quo_name(evt)
    } else {
      cnr_evt_time_name<- paste0(quo_name(evt_time), "_adm")
      cnr_evt_name     <- paste0(quo_name(evt), "_adm")
    }

    df<- if (is.null(evt_label)) {
      df %>%
        mutate(!!cnr_evt_time_name := replace(!!evt_time, !!evt_time>adm_cnr_time, adm_cnr_time),
               !!cnr_evt_name      := replace(!!evt, !!evt_time> adm_cnr_time & !!evt!= 0, 0))
    } else {
      df %>%
        mutate(!!cnr_evt_time_name := replace(!!evt_time, !!evt_time>adm_cnr_time, adm_cnr_time),
               !!cnr_evt_name      := factor(replace(!!evt, !!evt_time> adm_cnr_time & !!evt!= 0, 0),
                                             as.integer(names(evt_label)),
                                             labels = evt_label))
    }
  }

  df<- if (overwrite_var | is.null(evt_label)) df else mutate(df,
                                                              !!quo_name(evt):= factor(!!evt, as.integer(names(evt_label)), labels = evt_label))
  df
}

prepare_survfit<- function(surv_obj) {

  prepare_cmprisk<- function(surv_obj) {

    # set up strata
    if (is.null(surv_obj$strata)) {
      nstrat<- 1
      stemp <- rep(1, length(surv_obj$time) * length(surv_obj$states)) # same length as stime
      stemp <- factor(stemp, 1, labels = c("Overall"))
    }
    else {
      nstrat<- length(surv_obj$strata)
      strata_lab<- sapply(strsplit(names(surv_obj$strata), "="), function(x) x[2])
      stemp <- rep(rep(1:nstrat, surv_obj$strata), length(surv_obj$states)) # same length as stime
      stemp <- factor(stemp, 1:nstrat, strata_lab)
    }

    data_frame(strata   = stemp,
               state    = rep(surv_obj$state, each= length(surv_obj$time)),
               time     = rep(surv_obj$time, length(surv_obj$state)),
               prob     = as.numeric(surv_obj$pstate),
               conf_low = as.numeric(surv_obj$lower),
               conf_high= as.numeric(surv_obj$upper))
  }

  prepare_surv<- function(surv_obj) {

    # set up strata
    if (is.null(surv_obj$strata)) {
      nstrat <- 1
      # stemp <- rep(1, length(surv_obj$time)) # same length as stime
      stemp <- factor(rep(1, length(surv_obj$time)), 1, labels = c("Overall"))
    }
    else {
      nstrat <- length(surv_obj$strata)
      strata_lab<- sapply(strsplit(names(surv_obj$strata), "="), function(x) x[2])
      # stemp <- rep(1:nstrat, surv_obj$strata) # same length as stime
      stemp <- factor(rep(1:nstrat, surv_obj$strata), 1:nstrat, strata_lab)
    }

    data_frame(strata   = stemp,
               time     = surv_obj$time,
               prob     = surv_obj$surv,
               conf_low = surv_obj$lower,
               conf_high= surv_obj$upper,
               n_event  = surv_obj$n.event,
               n_censor = surv_obj$n.censor,
               n_risk   = surv_obj$n.risk)
  }

  out<- if (any(class(surv_obj)=="survfitms")) {

    surv_obj %>%
      prepare_cmprisk() %>%
      group_by(strata, state) %>%
      nest() %>%
      mutate(plot_prob_d= map2(state, data,
                               function(state, df) {
                                 df<- df %>%
                                   select_(.dots = c("time", "prob")) %>%
                                   bind_rows(tribble(~time, ~prob,
                                                     0, as.numeric(state=="")))

                                 df %>%
                                   arrange(time, if (state!="") prob else desc(prob))
                               }))
  } else {

    surv_obj %>%
      prepare_surv() %>%
      group_by(strata) %>%
      nest() %>%
      mutate(plot_prob_d= map(data,
                              function(df) {
                                df %>%
                                  select_(.dots = c("time", "prob")) %>%
                                  bind_rows(tribble(~time, ~prob,
                                                    0, 1)) %>%
                                  arrange(time)
                              }))
  }

  out<- out %>%
    mutate(plot_ci_d= map(data,
                          function(df) {
                            nn<- nrow(df)
                            # check http://stackoverflow.com/questions/33874909/how-do-i-add-shading-and-color-to-the-confidence-intervals-in-ggplot-2-generated
                            ys<- rep( 1:nn, each = 2 )[ -2 * nn ]
                            xs<- c( 1, rep( 2:nn, each = 2))

                            df %$%
                              data_frame(time     = time[xs],
                                         conf_low = conf_low[ys],
                                         conf_high= conf_high[ys])
                          }))
  return(out)
}


add_atrisk<- function(p, surv_obj, x_break= NULL) {

  #---- get parameters required for where to include the at-risk table ----#
  atrisk_y_pos<- -0.225 * diff(layer_scales(p)$y$range$range)
  x_break     <- if (is.null(x_break)) {
    layer_scales(p)$x$get_breaks(layer_scales(p)$x$range$range)
  } else {
    x_break[x_break >= min(layer_scales(p)$x$range$range) & x_break<= max(layer_scales(p)$x$range$range)]
  }

  x_break<- if (any(is.na(x_break))) x_break[!is.na(x_break)] else x_break

  risk_tbl<- extract_atrisk(surv_obj, time.list= x_break)
  nstrata <- ncol(risk_tbl)-1
  # as_data_frame(risk_tbl)

  # add 'At-risk' at x= 0 and y= atrisk_y_pos
  out <- p + annotation_custom(
    grob = textGrob(label= format("At-risk N:", width = 20),
                    vjust= 1, hjust = 1,
                    gp = gpar(family="Arial", fontface="bold", cex = 1)),
    ymin = atrisk_y_pos,      # Vertical position of the textGrob
    ymax = atrisk_y_pos,
    xmin = 0,         # Note: The grobs are positioned outside the plot area
    xmax = 0)


  # where there are no strata, ncol(risk_tbl)= 2; the number at-risk is displayed at the same level as 'At-risk N:'
  # otherwise, ncol(risk_tbl)>2, the number of at-risk is displayed below 'At-risk N:'
  if (nstrata==1) {
    # no strata #
    for (i in seq_along(risk_tbl$time))  {
      out<- out + annotation_custom(
        grob = textGrob(label = risk_tbl[i, 2],
                        vjust= 1, hjust = 0.5,
                        gp = gpar(family="Arial",
                                  # fontface="bold",
                                  cex = 1)),
        ymin = atrisk_y_pos,      # Vertical position of the textGrob
        ymax = atrisk_y_pos,
        xmin = risk_tbl$time[i],         # Note: The grobs are positioned outside the plot area
        xmax = risk_tbl$time[i])
    }
  } else if (nstrata>1) {
    # strata #

    # when there are strata, atrisk_y_inc indicates the relative position from the initial at-risk y pos.
    atrisk_y_inc<- -0.075* diff(layer_scales(p)$y$range$range)

    # extract the color code used in the plot for different strata
    strata_col<- unique(layer_data(p)$colour)
    strata_lty<- unique(layer_data(p)$linetype)

    strata_col<- if (length(strata_col)==1 & length(strata_col)< nstrata) rep(strata_col, nstrata) else strata_col
    strata_lty<- if (length(strata_lty)==1 & length(strata_lty)< nstrata) rep(strata_lty, nstrata) else strata_lty

    for (j in 2:ncol(risk_tbl)) {
      tmp_y<- atrisk_y_pos + (j-1)*atrisk_y_inc

      out <- out + annotation_custom(
        grob = textGrob(label = format(paste0(colnames(risk_tbl)[j], ":"),
                                       width = nchar(colnames(risk_tbl)[j])+ (20 - nchar("At-risk N:") + 1)),
                        vjust= 1, hjust = 1,
                        gp = gpar(family= "Arial",
                                  col   = strata_col[j-1],
                                  lty   = strata_lty[j-1],
                                  cex   = 1)),
        ymin = tmp_y,      # Vertical position of the textGrob
        ymax = tmp_y,
        xmin = 0,         # Note: The grobs are positioned outside the plot area
        xmax = 0)

      for (i in seq_along(risk_tbl$time)) {
        tmp_x<- risk_tbl$time[i]

        out <- out + annotation_custom(
          grob = textGrob(label = formatC(risk_tbl[i, j],
                                          digits = 0,
                                          format = "d",
                                          big.mark = ",",
                                          flag = "#"),
                          vjust= 1, hjust = 0.5,
                          gp = gpar(family= "Arial",
                                    col   = strata_col[j-1],
                                    lty   = strata_lty[j-1],
                                    cex   = 1)),
          ymin = tmp_y,      # Vertical position of the textGrob
          ymax = tmp_y,
          xmin = tmp_x,      # Note: The grobs are positioned outside the plot area
          xmax = tmp_x)
      }
    }
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
estimate_km<- function(df, evt_time, evt, group, ...) {

  evt_time<- enquo(evt_time)
  evt     <- enquo(evt)
  group   <- enquo(group)

  out<- if (quo_is_missing(group)) {
    substitute(survfit(Surv(evt_time, evt) ~ 1, data= df, conf.type= "log-log", ...),
               list(evt_time= quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    df      = df))
  } else {
    substitute(survfit(Surv(evt_time, evt) ~ grp, data= df, conf.type= "log-log", ...),
               list(evt_time= quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    grp     = quo_get_expr(group),
                    df      = df))
  }
  out<- eval(out)
  out
}

run_logrank_test<- function(surv_obj) {

  tmp<- surv_obj$call
  tmp[[1]]<- as.name("survdiff")
  tmp$rho<- 0
  tmp$conf.type<- NULL
  test<- eval(tmp, parent.frame())

  pval<- pchisq(test$chisq, df= length(test$n) - 1, lower.tail = FALSE)
  pval
}

show_surv<- function(surv_obj,
                     x_lab= NULL,
                     y_lab= NULL,
                     x_lim= NULL,
                     y_lim= NULL,
                     x_break= NULL,
                     y_break= NULL,
                     color_scheme= c("brewer", "grey", "viridis"),
                     plot_theme= theme_minimal(),

                     add.ci= TRUE,
                     add.atrisk= TRUE,
                     add.legend= FALSE,
                     add.pvalue= TRUE,
                     pvalue.pos= c("topleft", "topright", "bottomleft", "bottomright", "left", "right"),
                     plot_cdf= FALSE) {

  # require(tidyverse)
  # require(scales)
  #---- prepare survfit for plot ----
  surv_mat<- prepare_survfit(surv_obj)

  plot_prob_d<- surv_mat %>%
    # filter(state %in% evt_type) %>%
    dplyr::select(strata, plot_prob_d) %>%
    # mutate(state_label= evt_label(state)) %>%
    unnest()

  add.pvalue<- if (nlevels(plot_prob_d$strata)==1) FALSE else add.pvalue
  add.legend<- if (nlevels(plot_prob_d$strata)==1 | add.atrisk) FALSE else add.legend

  color_scheme<- match.arg(color_scheme)
  fill_fun <- switch(color_scheme,
                     'brewer' = quote(scale_fill_brewer(palette = "Set1")),
                     'grey'   = quote(scale_fill_grey(start= 0, end= 0.65)),
                     'viridis'= quote(scale_fill_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE)))
  color_fun<- switch(color_scheme,
                     'brewer' = quote(scale_color_brewer(palette = "Set1")),
                     'grey'   = quote(scale_color_grey(start= 0, end= 0.65)),
                     'viridis'= quote(scale_color_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE)))

  # x_lab<- if (is.null(x_lab)) "Time" else x_lab
  # y_lab<- if (is.null(y_lab)) "Proportion of subjects" else y_lab
  # x_break<- if (is.null(x_break)) scales::pretty_breaks(6) else x_break
  # y_break<- if (is.null(y_break)) scales::pretty_breaks(6) else y_break

  out<- ggplot() +
    geom_step(data= plot_prob_d,
              aes(x= time, y= prob, group= strata, color= strata),
              size= 1.1, show.legend = add.legend) +
    eval(color_fun) +
    scale_x_continuous(name  = if (is.null(x_lab)) "Time" else x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                       expand= c(0.01, 0.005))

  if (add.ci) {
    plot_ci_d<- surv_mat %>%
      dplyr::select(strata, plot_ci_d) %>%
      unnest()

    out<- out +
      geom_ribbon(data= plot_ci_d,
                  aes(x= time, ymin= conf_low, ymax= conf_high, fill= strata),
                  alpha= .2, show.legend = FALSE) +
      eval(fill_fun)
  }


  out<- if (plot_cdf) {
    # failure function
    failure<- trans_new(name= "failure",
                        transform = function(x) 1-x,
                        inverse = function(y) 1-y,
                        breaks = trans_breaks(trans= function(x) 1-x, inv= function(y) 1-y,  n=6),
                        format = scales::percent_format())

    out + scale_y_continuous(name  = if (is.null(y_lab)) "Proportion of deceased subjects" else y_lab,
                             breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                             expand= c(0.01, 0.005),

                             trans = failure,
                             labels= trans_format(trans= function(x) 1- x, format = percent_format()))
  } else {
    # survival function
    out + scale_y_continuous(name  = if (is.null(y_lab)) "Freedom from death" else y_lab,
                             breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                             expand= c(0.01, 0.005),

                             labels= scales::percent_format())
  }

  if (add.pvalue) {
    pval<- run_logrank_test(surv_obj) %>%
      format_pvalue()
    # pval<- format_pvalue(pval)
    pval<- ifelse(trimws(pval)=="<0.001", "Log-rank p< 0.001", paste0("Log-rank p= ", pval) )

    pvalue.pos<- match.arg(pvalue.pos)
    if (pvalue.pos %in% c("topleft")) {
      pvalue.x<- layer_scales(out)$x$range$range[1]
      pvalue.y<- layer_scales(out)$y$range$range[2]
      pvalue.hjust<- 0
      pvalue.vjust<- 1
    } else if (pvalue.pos %in% c("bottomleft")) {
      pvalue.x<- layer_scales(out)$x$range$range[1]
      pvalue.y<- layer_scales(out)$y$range$range[1]
      pvalue.hjust<- 0
      pvalue.vjust<- 0
    } else if (pvalue.pos %in% c("left")) {
      pvalue.x<- layer_scales(out)$x$range$range[1]
      pvalue.y<- mean(layer_scales(out)$y$range$range)
      pvalue.hjust<- 0
      pvalue.vjust<- 0.5
    } else if (pvalue.pos %in% c("topright")) {
      pvalue.x<- layer_scales(out)$x$range$range[2]
      pvalue.y<- layer_scales(out)$y$range$range[2]
      pvalue.hjust<- 1
      pvalue.vjust<- 1
    } else if (pvalue.pos %in% c("bottomright")) {
      pvalue.x<- layer_scales(out)$x$range$range[2]
      pvalue.y<- layer_scales(out)$y$range$range[1]
      pvalue.hjust<- 1
      pvalue.vjust<- 0
    } else if (pvalue.pos %in% c("right")) {
      pvalue.x<- layer_scales(out)$x$range$range[2]
      pvalue.y<- mean(layer_scales(out)$y$range$range)
      pvalue.hjust<- 1
      pvalue.vjust<- 0.5
    } else {
      pvalue.x<- NULL
      pvalue.y<- NULL
      pvalue.hjust<- NULL
      pvalue.vjust<- NULL
    }

    out<- out +
      annotation_custom(
        grob = textGrob(label= pval,
                        vjust= pvalue.vjust, hjust = pvalue.hjust,
                        gp   = gpar(family  = "Consola",
                                    # fontface="bold.italic",
                                    fontface="italic",
                                    cex   = 1)),
        ymin = pvalue.y,      # Vertical position of the textGrob
        ymax = pvalue.y,
        xmin = pvalue.x,
        xmax = pvalue.x)
  }

  if (add.atrisk) out<- add_atrisk(out, surv_obj = surv_obj, x_break = x_break)

  out<- out +
    # facet_grid(. ~ state, labeller = labeller(state= state_label), drop= TRUE) +
    plot_theme

  # print(out)
  print(out, vp= viewport(width = unit(6.5, "inches"),
                          height = unit(6.5, "inches")))
  return(out)
}
#' @title estimate_cif
#'
#' @details
#' The function analyzes the competing data (df) using Andersen-Johansen method in estimating cumulative incidence
#' function.The function store the input data in the call(), which can be used in
#' run_gray_test().
#'
estimate_cif<- function(df, evt_time, evt, group, ...) {

  evt_time<- enquo(evt_time)
  evt     <- enquo(evt)
  group   <- enquo(group)

  out<- if (quo_is_missing(group)) {
    substitute(survfit(Surv(evt_time, evt, type= "mstate") ~ 1, data= df, ...),
               list(evt_time= quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    df      = df))
  } else {
    substitute(survfit(Surv(evt_time, evt, type= "mstate") ~ grp, data= df, ...),
               list(evt_time= quo_get_expr(evt_time),
                    evt     = quo_get_expr(evt),
                    grp     = quo_get_expr(group),
                    df      = df))
  }
  out<- eval(out)
  out
}

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

show_cif<- function(surv_obj,
                    evt_type = 1,
                    evt_label= identity, # identity function
                    # evt_label= function(x) recode_factor(x,
                    #                                      `1`= "Event",
                    #                                      `2`= "Competing event",
                    #                                      .default= "Event free"),

                    x_lab= NULL,
                    y_lab= NULL,
                    x_break= NULL,
                    y_break= NULL,
                    color_scheme= c("brewer", "grey", "viridis"),
                    plot_theme= theme_minimal(),

                    add.ci= TRUE,
                    add.atrisk= TRUE,
                    add.legend= FALSE,
                    add.pvalue= TRUE,
                    pvalue.pos= c("topleft", "topright", "bottomleft", "bottomright", "left", "right"),
                    plot_cdf= FALSE) {

  #---- prepare survfit for plot ----
  cmprisk_mat<- prepare_survfit(surv_obj)
  plot_prob_d<- cmprisk_mat %>%
    filter(state %in% evt_type) %>%
    dplyr::select(strata, state, plot_prob_d) %>%
    mutate(state_label= evt_label(state)) %>%
    unnest()


  add.pvalue<- if (nlevels(plot_prob_d$strata)==1) FALSE else add.pvalue
  add.legend<- if (nlevels(plot_prob_d$strata)==1 | add.atrisk) FALSE else add.legend

  color_scheme<- match.arg(color_scheme)
  fill_fun <- switch(color_scheme,
                     'brewer' = quote(scale_fill_brewer(palette = "Set1")),
                     'grey'   = quote(scale_fill_grey(start= 0, end= 0.65)),
                     'viridis'= quote(scale_fill_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE)))
  color_fun<- switch(color_scheme,
                     'brewer' = quote(scale_color_brewer(palette = "Set1")),
                     'grey'   = quote(scale_color_grey(start= 0, end= 0.65)),
                     'viridis'= quote(scale_color_viridis(option = "viridis", begin= .2, end= .85, discrete = TRUE)))

  # x_lab<- if (is.null(x_lab)) "Time" else x_lab
  # y_lab<- if (is.null(y_lab)) "Proportion of subjects" else y_lab
  # x_break<- if (is.null(x_break)) scales::pretty_breaks(6) else x_break
  # y_break<- if (is.null(y_break)) scales::pretty_breaks(6) else y_break

  out<- ggplot() +
    geom_step(data= plot_prob_d,
              aes(x= time, y= prob, group= strata, color= strata),
              size= 1.1, show.legend = add.legend) +
    eval(color_fun) +
    scale_x_continuous(name  = if (is.null(x_lab)) "Time" else x_lab,
                       breaks= if (is.null(x_break)) scales::pretty_breaks(6) else x_break,
                       expand= c(0.01, 0.005)) +
    scale_y_continuous(name  = if (is.null(y_lab)) "Proportion of subjects" else y_lab,
                       breaks= if (is.null(y_break)) scales::pretty_breaks(6) else y_break,
                       expand= c(0.01, 0.005),
                       labels= scales::percent_format())

  if (add.ci) {
    plot_ci_d<- cmprisk_mat %>%
      filter(state %in% evt_type) %>%
      dplyr::select(strata, state, plot_ci_d) %>%
      mutate(state_label= evt_label(state)) %>%
      unnest()

    out<- out +
      geom_ribbon(data= plot_ci_d,
                  aes(x= time, ymin= conf_low, ymax= conf_high, fill= strata),
                  alpha= .2, show.legend = FALSE) +
      eval(fill_fun)
  }

  if (add.pvalue) {
    pval<- run_gray_test(surv_obj, evt_type = evt_type) %>%
      format_pvalue()
    pval<- ifelse(trimws(pval)=="<0.001", "Gray's p< 0.001", paste0("Gray's p= ", pval) )

    pvalue.pos<- match.arg(pvalue.pos)
    if (pvalue.pos %in% c("topleft")) {
      pvalue.x<- layer_scales(out)$x$range$range[1]
      pvalue.y<- layer_scales(out)$y$range$range[2]
      pvalue.hjust<- 0
      pvalue.vjust<- 1
    } else if (pvalue.pos %in% c("bottomleft")) {
      pvalue.x<- layer_scales(out)$x$range$range[1]
      pvalue.y<- layer_scales(out)$y$range$range[1]
      pvalue.hjust<- 0
      pvalue.vjust<- 0
    } else if (pvalue.pos %in% c("left")) {
      pvalue.x<- layer_scales(out)$x$range$range[1]
      pvalue.y<- mean(layer_scales(out)$y$range$range)
      pvalue.hjust<- 0
      pvalue.vjust<- 0.5
    } else if (pvalue.pos %in% c("topright")) {
      pvalue.x<- layer_scales(out)$x$range$range[2]
      pvalue.y<- layer_scales(out)$y$range$range[2]
      pvalue.hjust<- 1
      pvalue.vjust<- 1
    } else if (pvalue.pos %in% c("bottomright")) {
      pvalue.x<- layer_scales(out)$x$range$range[2]
      pvalue.y<- layer_scales(out)$y$range$range[1]
      pvalue.hjust<- 1
      pvalue.vjust<- 0
    } else if (pvalue.pos %in% c("right")) {
      pvalue.x<- layer_scales(out)$x$range$range[2]
      pvalue.y<- mean(layer_scales(out)$y$range$range)
      pvalue.hjust<- 1
      pvalue.vjust<- 0.5
    } else {
      pvalue.x<- NULL
      pvalue.y<- NULL
      pvalue.hjust<- NULL
      pvalue.vjust<- NULL
    }

    out<- out +
      annotation_custom(
        grob = textGrob(label= pval,
                        vjust= pvalue.vjust, hjust = pvalue.hjust,
                        gp   = gpar(family  = "Consola",
                                    # fontface="bold.italic",
                                    fontface="italic",
                                    cex   = 1)),
        ymin = pvalue.y,      # Vertical position of the textGrob
        ymax = pvalue.y,
        xmin = pvalue.x,
        xmax = pvalue.x)
  }

  if (add.atrisk) out<- add_atrisk(out, surv_obj = surv_obj, x_break = x_break)

  out<- out +
    # facet_grid(. ~ state, labeller = labeller(state= state_label), drop= TRUE) +
    plot_theme

  # print(out)
  print(out, vp= viewport(width = unit(6.5, "inches"),
                          height = unit(6.5, "inches")))
  return(out)
}




summarize_mi_coxph<- function(cox_mira, exponentiate= TRUE) {

  cox_out<- summary(pool(cox_mira)) %>%
    as.data.frame() %>%
    rownames_to_column("var")
  cox_out<- cox_out[c("var", "est", 'lo 95', 'hi 95', 'Pr(>|t|)')]
  names(cox_out)<- c("var", "est", "conf.low", "conf.high", "pval")
  cox_out<- cox_out %>%
    mutate(est= if (exponentiate) exp(est) else est,
           conf.low= if (exponentiate) exp(conf.low) else conf.low,
           conf.high= if (exponentiate) exp(conf.high) else conf.high,
           stat= paste0( formatC(est,       digits = 3, format= "f", flag= "#"), " [",
                         formatC(conf.low,  digits = 3, format= "f", flag= "#"), ", ",
                         formatC(conf.high, digits = 3, format= "f", flag= "#"), "]"),
           pval= format.pvalue(pval)) %>%
    dplyr::select(var, stat, pval, everything())

  # to calulate the type 3 error
  # Li, Meng, Raghunathan and Rubin. Significance levels from repated p-values with multiply-imputed data. Statistica Sinica (1991)
  x<- model.matrix(cox_mira$analyses[[1]])
  varseq<- attr(x, "assign")
  df<- sapply(split(varseq, varseq), length)
  m <- length(cox_mira$analyses)

  # coef estimate and its vcov for each MI model
  betas<- mitools::MIextract(cox_mira$analyses, fun= coef)
  vars <- mitools::MIextract(cox_mira$analyses, fun= vcov)

  # average betas and vcov cross MI mdls
  mean_betas<- purrr::reduce(betas, .f= `+`)/m
  with_var<- purrr::reduce(vars, .f= `+`)/m # with MI
  # between-MI vcov
  btwn_var<- lapply(betas, function(cc) (cc - mean_betas) %*% t(cc - mean_betas)) %>%
    purrr::reduce(.f= `+`)/(m-1)

  out<- lapply(unique(varseq),
               function(i){
                 df<- sum(varseq==i)
                 # set out the contrast matrix
                 L<- matrix(0, nrow= df, ncol= ncol(x))
                 L[, varseq==i]<- diag(df)

                 cc<- L %*% mean_betas
                 vv<- L %*% with_var %*% t(L) # with-mi vcov for beta
                 v2<- L %*% btwn_var %*% t(L) # btwn-mi vcov for beta

                 # calcualte Wald's test statistics and p-value
                 rm<- (1 + 1/m) * sum(diag(v2 %*% solve(vv))) # eqn (1.18) without dividing by k
                 wald_stat<- as.numeric( t(cc) %*% solve(vv) %*% cc )/(df + rm) # eqn (1.17)
                 # expr (1.19)
                 nu<- df * (m-1)
                 df_denominator<- if (nu> 4) {
                   4 + (nu-4)*(1 + (1-2/nu)/(rm/df))^2
                 } else {
                   0.5*(m-1)*(df+1)*(1+1/(rm/df))^2
                 }

                 pval<- pf(wald_stat, df1= df, df2= df_denominator, lower.tail= FALSE)

                 data.frame(df= round(df, 0),
                            stat= wald_stat,
                            chisq_p= pval)
               })
  names(out)<- attr(cox_mira$analyses[[1]]$terms, "term.labels")[unique(varseq)]

  type3_out<- plyr::ldply(out, .id= "var") %>%
    mutate(pval= format.pvalue(chisq_p)) %>%
    filter(df>1) %>%
    dplyr::select(var, pval)

  bind_rows(cox_out, type3_out) %>% arrange(var)
}
