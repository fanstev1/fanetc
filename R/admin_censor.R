#---- Administrative censoring for survival and competing-risk data ----

# output column names: the originals when overwriting, otherwise "<name>_adm"
.admin_censor_names<- function(evt_time, evt, overwrite_var) {
  suffix<- if (overwrite_var) "" else "_adm"
  c(time= paste0(rlang::as_name(evt_time), suffix),
    evt = paste0(rlang::as_name(evt), suffix))
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
#' @examples
#' library(magrittr)
#' library(survival)
#' aml %>% admin_censor_surv(evt_time= time, evt= status) # No admin censoring
#' aml %>% admin_censor_surv(evt_time= time, evt= status, adm_cnr_time= 30)
#' aml %>% admin_censor_surv(evt_time= time, evt= status, adm_cnr_time= 30, overwrite_var= TRUE)
#' @export
admin_censor_surv <- function(df, evt_time, evt, adm_cnr_time = NULL, overwrite_var = FALSE) {
  evt_time <- enquo(evt_time)
  evt <- enquo(evt)

  if (is.null(adm_cnr_time)) return(df)

  nm<- .admin_censor_names(evt_time, evt, overwrite_var)
  df %>%
    mutate(
      !!nm[["evt"]] := replace(!!evt, !!evt_time > adm_cnr_time & !!evt != 0, 0),
      !!nm[["time"]] := replace(!!evt_time, !!evt_time > adm_cnr_time, adm_cnr_time)
    )
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
#' @param evt_label an optional named vector mapping event codes (its names) to display labels; when given, the censored event variable is returned as a factor with these labels.
#' @param overwrite_var a logical scalar (default= FALSE) indiciates if the existing time-to-event variables should be overwritten.
#' @return The input data plus censored time-to-event variables.
#' @examples
#' \dontrun{
#' # requires internet access to fetch the example dataset
#' cmprisk_df<- read.csv2("http://www.stat.unipg.it/luca/misc/bmt.csv")
#' admin_censor_cmprisk(cmprisk_df, ftime, status, evt_label = c("0"= "Event free", "1"= "Event", "2"= "Competing event"), adm_cnr_time= 10)
#' }
#' @export
admin_censor_cmprisk <- function(df, evt_time, evt, adm_cnr_time = NULL, evt_label = NULL, overwrite_var = FALSE) {
  evt_time <- enquo(evt_time)
  evt <- enquo(evt)

  if (is.null(adm_cnr_time)) stop("No administrative censor time is given.")

  nm<- .admin_censor_names(evt_time, evt, overwrite_var)
  df %>%
    mutate(
      !!nm[["evt"]] := {
        censored<- replace(!!evt, !!evt_time > adm_cnr_time & !!evt != "0", "0")
        if (is.null(evt_label)) censored else factor(censored, names(evt_label), labels = evt_label)
      },
      !!nm[["time"]] := replace(!!evt_time, !!evt_time > adm_cnr_time, adm_cnr_time)
    )
}
