#---- Tabular summaries of survfit objects (KM and CIF) ----

#' @title summarize_km
#'
#' @details
#' The function summarize the fitted KM at the time points specified by a user.
#'
#' @export
summarize_km<- function(fit, times= NULL, failure_fun= FALSE) {

  ss<- summary(fit, times= if (is.null(times)) pretty(fit$time) else times, extend = TRUE)

  if (failure_fun) {
    ff<- 1 - ss$surv
    ll<- 1 - ss$upper
    uu<- 1 - ss$lower

    ss$surv<- ff
    ss$lower<- ll
    ss$upper<- uu
  }

  out <- ss %$%
    {
      if (any(names(ss) == "strata")) {
        tibble(
          strata = strata,
          time = time,
          surv = surv,
          conf_low = lower,
          conf_high = upper
        )
      } else {
        tibble(
          time = time,
          surv = surv,
          conf_low = lower,
          conf_high = upper
        )
      }
    } %>%
    dplyr::mutate(
      stat = case_when(
        is.na(surv) ~ "---",
        !is.na(surv) & is.na(conf_low) & is.na(conf_high) ~ sprintf("%3.1f%% [---]", surv * 100),
        !is.na(surv) & is.na(conf_low) & !is.na(conf_high) ~ sprintf("%3.1f%% [---, %3.1f%%]", surv * 100, conf_high * 100),
        !is.na(surv) & !is.na(conf_low) & is.na(conf_high) ~ sprintf("%3.1f%% [%3.1f%%, ---]", surv * 100, conf_low * 100),
        TRUE ~ sprintf("%3.1f%% [%3.1f%%, %3.1f%%]", surv * 100, conf_low * 100, conf_high * 100)
      )
    ) %>%
    {
      if (any(names(ss) == "strata")) {
        dplyr::mutate(., strata = gsub("^.*=", "", strata))
      } else {
        dplyr::mutate(., strata = "Overall")
      }
    } %>%
    pivot_wider(., id_cols = time, names_from = strata, values_from = stat)

  out
}



#' @title summarize_cif
#'
#' @details
#' The function summarizes the fitted CIF at the time points specified by a user.
#'
#' @export
summarize_cif<- function(fit, times= NULL) {
  ss <- summary(fit, times = if (is.null(times)) pretty(fit$time) else times, extend = TRUE)
  colnames(ss$pstate) <-
    colnames(ss$lower) <-
    colnames(ss$upper) <- replace(ss$state, sapply(ss$states, nchar) == 0, "0")
  # if (is.null(ss$prev)) ss$prev<- ss$pstate

  out<- if (any(names(fit)=="strata")) {

    ss %$%
      map2(.x= c('pstate', 'conf_low', 'conf_high'),
           .y= list(pstate= pstate, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               mutate(strata= strata,
                      times = time) %>%
               melt(id.vars= c('strata', 'times'),
                    value.name = var,
                    variable.name = 'states')
           }) %>%
      reduce(full_join, by = c('strata', 'times', 'states')) %>%
      mutate_at(vars(one_of('pstate', 'conf_low', 'conf_high')),
                function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      mutate(stat= paste0(pstate, " [", conf_low, ", ", conf_high, "]")) %>%
      dcast(times ~ states + strata, value.var = 'stat')

  } else {

    ss %$%
      map2(.x= c('pstate', 'conf_low', 'conf_high'),
           .y= list(pstate= pstate, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               mutate(times = time) %>%
               melt(id.vars= c('times'),
                    value.name = var,
                    variable.name = 'states')
           }) %>%
      reduce(full_join, by = c('times', 'states')) %>%
      mutate_at(vars(one_of('pstate', 'conf_low', 'conf_high')),
                function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      mutate(stat= paste0(pstate, " [", conf_low, ", ", conf_high, "]")) %>%
      dcast(times ~ states, value.var = 'stat')
  }
  out
}
