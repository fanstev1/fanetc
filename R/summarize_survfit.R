#---- Tabular summaries of survfit objects (KM and CIF) ----

#' @title summarize_km
#'
#' @details
#' The function summarize the fitted KM at the time points specified by a user.
#'
#' @param fit a survfit object
#' @param times a numeric vector of time points at which the estimates are reported (default: pretty(fit$time))
#' @param failure_fun a logical parameter indicating whether the failure probability 1 - S(t) should be reported instead of survival
#' @return a wide tibble with one row per time point and one column per stratum, each cell formatted as "xx.x\% [lower, upper]"
#' @export
summarize_km<- function(fit, times= NULL, failure_fun= FALSE) {

  ss<- summary(fit, times= if (is.null(times)) pretty(fit$time) else times, extend = TRUE)

  if (failure_fun) {
    # failure function: 1 - S(t); the CI bounds swap sides
    ss[c("surv", "lower", "upper")]<- list(1 - ss$surv, 1 - ss$upper, 1 - ss$lower)
  }

  has_strata<- "strata" %in% names(ss)

  dplyr::tibble(
    strata   = if (has_strata) gsub("^.*=", "", ss$strata) else "Overall",
    time     = ss$time,
    surv     = ss$surv,
    conf_low = ss$lower,
    conf_high= ss$upper
  ) %>%
    dplyr::mutate(
      stat = case_when(
        is.na(surv) ~ "---",
        is.na(conf_low) & is.na(conf_high) ~ sprintf("%3.1f%% [---]", surv * 100),
        is.na(conf_low)  ~ sprintf("%3.1f%% [---, %3.1f%%]", surv * 100, conf_high * 100),
        is.na(conf_high) ~ sprintf("%3.1f%% [%3.1f%%, ---]", surv * 100, conf_low * 100),
        TRUE ~ sprintf("%3.1f%% [%3.1f%%, %3.1f%%]", surv * 100, conf_low * 100, conf_high * 100)
      )
    ) %>%
    pivot_wider(id_cols = time, names_from = strata, values_from = stat)
}



#' @title summarize_cif
#'
#' @details
#' The function summarizes the fitted CIF at the time points specified by a user.
#'
#' @param fit a survfitms object
#' @param times a numeric vector of time points at which the estimates are reported (default: pretty(fit$time))
#' @return a wide dataframe with one row per time point and one column per state (and stratum), each cell formatted as "xx.x\% [lower, upper]"
#' @export
summarize_cif<- function(fit, times= NULL) {
  ss <- summary(fit, times = if (is.null(times)) pretty(fit$time) else times, extend = TRUE)
  # relabel survfit's placeholder censor state ("" on some versions) as "0";
  # NB the historical code wrote ss$state, which reached ss$states only via
  # partial matching -- keep the semantics, spell it out
  state_levels <- replace(ss$states, sapply(ss$states, nchar) == 0, "0")
  colnames(ss$pstate) <- colnames(ss$lower) <- colnames(ss$upper) <- state_levels

  has_strata<- "strata" %in% names(fit)
  id_cols<- c(if (has_strata) "strata", "times")

  long<- purrr::map2(
    list(pstate= ss$pstate, conf_low= ss$lower, conf_high= ss$upper),
    c("pstate", "conf_low", "conf_high"),
    function(mat, var) {
      d<- as.data.frame(mat)
      d$times<- ss$time
      if (has_strata) d$strata<- ss$strata
      tidyr::pivot_longer(d, cols = all_of(state_levels),
                          names_to = "states", values_to = var)
    }
  ) %>%
    reduce(full_join, by = c(id_cols, "states")) %>%
    mutate(across(all_of(c("pstate", "conf_low", "conf_high")),
                  function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= ""))) %>%
    mutate(stat= paste0(pstate, " [", conf_low, ", ", conf_high, "]"))

  wide<- tidyr::pivot_wider(long, id_cols = "times",
                            names_from = all_of(c("states", if (has_strata) "strata")),
                            values_from = "stat", names_sep = "_")
  wide<- wide[order(wide$times), ]

  # match the historical wide-format column order:
  # states-major (colnames order), strata varying fastest (survfit level order)
  ordered_cols<- if (has_strata) {
    as.vector(t(outer(state_levels, levels(factor(ss$strata)),
                      function(s, g) paste(s, g, sep = "_"))))
  } else {
    state_levels
  }
  as.data.frame(wide[, c("times", ordered_cols)])
}
