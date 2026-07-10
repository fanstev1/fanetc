#---- Time-to-event variable construction (vectorized) ----

# Warn about non-positive event times, print the offending rows, and apply the
# historical fixes (time 0 -> 0.5, negative time -> NA). `diag_df` supplies the
# context columns shown in the printout.
fix_nonpositive_times <- function(time2evt, diag_df) {
  is_zero <- !is.na(time2evt) & time2evt == 0
  is_neg <- !is.na(time2evt) & time2evt < 0

  if (any(is_zero)) warning("Event at time zero")
  if (any(is_neg)) warning("Negative time-to-event!?")

  if (any(is_zero | is_neg)) {
    bad <- is_zero | is_neg
    print(data.frame(
      diag_df[bad, , drop = FALSE],
      flag_evt_time_zero = is_zero[bad],
      flag_evt_time_neg = is_neg[bad]
    ))
    time2evt[is_zero] <- 0.5
    time2evt[is_neg] <- NA_real_
  }

  time2evt
}

#' @title construct_surv_var
#'
#' @details
#' The function creates time-to-event variables for a binary (survival) process.
#' Subjects with a non-missing event date are events (evt = 1) with time measured
#' from the index date to the event date; all others are censored (evt = 0) at the
#' end of follow-up. Events at time zero are set to 0.5 with a warning; negative
#' times are set to NA with a warning, and the offending rows are printed.
#'
#' @param df input data
#' @param patid the variable indicating subject/patient id
#' @param idx_dt the index date
#' @param evt_dt the date of the event occurrence. Its value should be NA for non-event subjects.
#' @param end_dt the date of the last follow-up
#' @param surv_varname an option of character vector of length 2, the 1st of which is the name of the time variable; the other is the name of the event indicator.
#' @param append if TRUE, the new variables are appended to the input data
#' @return A data frame with patid, evt_time and evt (or the input data plus the new variables when append = TRUE).
#' @examples
#' set.seed(0)
#' nn<- 100
#' test<- data.frame(idx_dt= as.Date("1970-01-01"),
#'                   evt_dt= c(sample((-30:70), nn-1, replace= TRUE), 0) + as.Date("1970-01-01"),
#'                   end_dt= sample((0:99), nn, replace= TRUE) + as.Date("1970-01-01")) %>%
#'   mutate(flag= evt_dt > end_dt,
#'          evt_dt= replace(evt_dt, flag, NA),
#'          end_dt= replace(end_dt, !flag, NA),
#'          patid= 1:n())
#' test %>% construct_surv_var(patid, idx_dt, evt_dt, end_dt)
#' test %>% construct_surv_var(patid, idx_dt, evt_dt, end_dt, surv_varname= c("day_dth", "dth"))
#' @export
construct_surv_var <- function(df, patid, idx_dt, evt_dt, end_dt, surv_varname = NULL, append = FALSE) {

  patid <- rlang::enquo(patid)
  idx_dt <- rlang::enquo(idx_dt)
  evt_dt <- rlang::enquo(evt_dt)
  end_dt <- rlang::enquo(end_dt)

  if (rlang::quo_is_missing(idx_dt)) stop("No index date (time zero).")
  if (rlang::quo_is_missing(evt_dt)) stop("No event date.")
  if (rlang::quo_is_missing(end_dt)) stop("No date of the end of follow-up.")
  if (rlang::quo_is_missing(patid)) stop("Please provide subject id")

  idx_date <- as.Date(dplyr::pull(df, !!idx_dt))
  evt_date <- as.Date(dplyr::pull(df, !!evt_dt))
  end_date <- as.Date(dplyr::pull(df, !!end_dt))

  evt <- as.integer(!is.na(evt_date))
  time2evt <- as.numeric(dplyr::coalesce(evt_date, end_date) - idx_date)

  out <- dplyr::tibble(
    !!rlang::as_name(patid) := dplyr::pull(df, !!patid),
    time2evt = time2evt,
    evt = evt
  )

  out$time2evt <- fix_nonpositive_times(out$time2evt, out)

  varname <- if (is.null(surv_varname)) c("evt_time", "evt") else surv_varname
  names(out)[2:3] <- varname

  if (append) dplyr::bind_cols(df, out[varname]) else out
}

#' @title construct_cmprisk_var
#'
#' @description
#' Creates time-to-event variables for competing risk analysis from date variables.
#' Requires an index date (time zero), event date, competing event date(s), and last follow-up date.
#'
#' @details
#' For each subject, the first event is the earliest of the event of interest, the
#' competing event date(s), and the end of follow-up. Ties are resolved in that
#' priority order (event of interest first, then competing events in the order
#' given, then censoring). The function creates:
#' - `evt_time`: the time from index to the first event
#' - `evt`: a factor indicating the event type (0=censored, 1=event of interest, 2+=competing events)
#'
#' Events at time zero are set to 0.5 with a warning; times before the index date
#' are set to NA with a warning, and the offending rows are printed. Subjects with
#' all dates missing get NA for both variables.
#'
#' @param df Input data frame
#' @param patid Name of patient ID column (unquoted)
#' @param idx_dt Name of index date column (unquoted) - time zero
#' @param evt_dt Name of event date column (unquoted) - event of interest (missing if event doesn't occur)
#' @param end_dt Name of end of follow-up date column (unquoted) (can be missing if event occurred)
#' @param cmprisk_varname Character vector of output variable names: c("time_var_name", "event_var_name")
#'                        If NULL, uses c("evt_time", "evt")
#' @param append Logical. If TRUE, appends to original data; if FALSE, returns only new variables
#' @param ... Additional competing event date columns (unquoted), passed as name=column
#' @param varname Alias of `cmprisk_varname` (name introduced in a later revision; both are accepted)
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
#' construct_cmprisk_var(
#'   test_df,
#'   patid = patid,
#'   idx_dt = idx_dt,
#'   evt_dt = evt_dt,
#'   end_dt = end_dt,
#'   cmp_evt_1 = cmp_evt_dt
#' )
#' }
#'
#' @export
construct_cmprisk_var <- function(df,
                                  patid,
                                  idx_dt,
                                  evt_dt,
                                  end_dt,
                                  cmprisk_varname = NULL,
                                  append = FALSE,
                                  ...,
                                  varname = NULL) {

  patid <- rlang::enquo(patid)
  idx_dt <- rlang::enquo(idx_dt)
  evt_dt <- rlang::enquo(evt_dt)
  end_dt <- rlang::enquo(end_dt)
  cmp_evt_dt <- rlang::enquos(...)

  if (rlang::quo_is_missing(idx_dt)) stop("No index date (time zero).")
  if (rlang::quo_is_missing(evt_dt)) stop("No event date.")
  if (rlang::quo_is_missing(end_dt)) stop("No date of the end of follow-up.")
  if (rlang::quo_is_missing(patid)) stop("Please provide subject id")

  if (is.null(varname)) varname <- cmprisk_varname
  if (is.null(varname)) varname <- c("evt_time", "evt")

  n_cmp_evt <- length(cmp_evt_dt)
  idx_date <- as.Date(dplyr::pull(df, !!idx_dt))

  # Candidate dates as an n x (n_cmp_evt + 2) matrix; column order encodes the
  # tie-break priority: event of interest, competing events, end of follow-up
  cand <- vapply(
    c(list(evt_dt), unname(cmp_evt_dt), list(end_dt)),
    function(q) as.numeric(as.Date(dplyr::pull(df, !!q))),
    numeric(nrow(df))
  )
  cand <- matrix(cand, nrow = nrow(df)) # keep matrix shape for single-row inputs

  first_date <- do.call(pmin, c(asplit(cand, 2), na.rm = TRUE))
  time2evt <- as.numeric(first_date) - as.numeric(idx_date)

  # Column of the earliest date, ties broken by column (priority) order
  cand[is.na(cand)] <- Inf
  first_col <- max.col(-cand, ties.method = "first")
  evt <- c(1L, seq_len(n_cmp_evt) + 1L, 0L)[first_col]
  evt[is.na(time2evt)] <- NA_integer_
  evt <- factor(evt, 0:(n_cmp_evt + 1), labels = 0:(n_cmp_evt + 1))

  out <- dplyr::tibble(
    !!rlang::as_name(patid) := dplyr::pull(df, !!patid),
    time2evt = time2evt,
    evt = evt
  )

  out$time2evt <- fix_nonpositive_times(out$time2evt, out)

  names(out)[2:3] <- varname

  if (append) dplyr::bind_cols(df, out[varname]) else out
}
