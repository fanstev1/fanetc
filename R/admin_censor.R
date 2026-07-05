#---- Administrative censoring for survival and competing-risk data ----

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
#' @examples
#' aml %>% admin_censor_surv(evt_time= time, evt= status) # No admin censoring
#' aml %>% admin_censor_surv(evt_time= time, evt= status, adm_cnr_time= 30)
#' aml %>% admin_censor_surv(evt_time= time, evt= status, adm_cnr_time= 30, overwrite_var= TRUE)
#' @export
admin_censor_surv <- function(df, evt_time, evt, adm_cnr_time = NULL, overwrite_var = FALSE) {
  ######################################################################################
  ## the function creates administrately censored version of event time and indicator ##
  ## for survival (binary) process                                                    ##
  ##   df - input dataframe                                                           ##
  ##   evt_time - continuous time to event                                            ##
  ##   evt - event indicator (1= event; 0= non-event)                                 ##
  ##   adm_cnr_time - time at which admin censoring is applied                        ##
  ######################################################################################

  evt_time <- enquo(evt_time)
  evt <- enquo(evt)

  if (!is.null(adm_cnr_time)) {
    if (overwrite_var) {
      cnr_evt_time_name <- lazyeval::as_name(evt_time)
      cnr_evt_name <- lazyeval::as_name(evt)
    } else {
      cnr_evt_time_name <- paste0(lazyeval::as_name(evt_time), "_adm")
      cnr_evt_name <- paste0(lazyeval::as_name(evt), "_adm")
    }

    df <- df %>%
      mutate(
        !!cnr_evt_name := replace(!!evt, !!evt_time > adm_cnr_time & !!evt != 0, 0),
        !!cnr_evt_time_name := replace(!!evt_time, !!evt_time > adm_cnr_time, adm_cnr_time)
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
#' @param evt a factor vector indicating right censoring (0= censored; 1= event of interest; other= competing risk(s)).
#' @param adm_cnr_time a numeric vector specifying the time point at which administrative censoring is applied.
#' @param evt_label a numeric vector specifying the time point at which administrative censoring is applied.
#' @param overwrite_var a logical scalar (default= FALSE) indiciates if the existing time-to-event variables should be overwritten.
#' @return The input data plus censored time-to-event variables.
#' @examples
#' cmprisk_df<- read.csv2("http://www.stat.unipg.it/luca/misc/bmt.csv")
#' admin_censor_cmprisk(cmprisk_df, ftime, status, evt_label = c("0"= "Event free", "1"= "Event", "2"= "Competing event"), adm_cnr_time= 10)
#' @export
admin_censor_cmprisk <- function(df, evt_time, evt, adm_cnr_time = NULL, evt_label = NULL, overwrite_var = FALSE) {
  evt_time <- enquo(evt_time)
  evt <- enquo(evt)

  if (is.null(adm_cnr_time)) {
    stop("No administrative censor time is given.")
  } else {
    if (overwrite_var) {
      cnr_evt_time_name <- lazyeval::as_name(evt_time)
      cnr_evt_name <- lazyeval::as_name(evt)
    } else {
      cnr_evt_time_name <- paste0(lazyeval::as_name(evt_time), "_adm")
      cnr_evt_name <- paste0(lazyeval::as_name(evt), "_adm")
    }

    df <- df %>%
      {
        if (is.null(evt_label)) {
          mutate(
            !!cnr_evt_name := replace(!!evt, !!evt_time > adm_cnr_time & !!evt != "0", "0"),
            !!cnr_evt_time_name := replace(!!evt_time, !!evt_time > adm_cnr_time, adm_cnr_time)
          )
        } else {
          mutate(
            !!cnr_evt_name := factor(replace(!!evt, !!evt_time > adm_cnr_time & !!evt != "0", "0"),
              # as.integer(names(evt_label)),
              names(evt_label),
              labels = evt_label
            ),
            !!cnr_evt_time_name := replace(!!evt_time, !!evt_time > adm_cnr_time, adm_cnr_time)
          )
        }
      }
  }

  df
}

