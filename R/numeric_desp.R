

#---- summarize numeric variables ----
#' @title numeric_desp
#'
#' @details
#' An internal function that report mean, standard deviation, median and interquartile range by group.
#'
#' @param df Dataframe
#' @return a dataframe consisting of columns of character variables.
#' @export
numeric_desp<- function(df, group) {
  group<- rlang::enquo(group)
  df<- df %>%
    ungroup()

  if (rlang::quo_is_missing(group)) {

    sum_stat<- df %>%
      summarise_if(is.numeric, funs(mean_sd, med_iqr)) %>%
      rownames_to_column() %>%
      melt(id.vars= "rowname", value.name = "stat") %>%
      mutate(type= ifelse(grepl("mean_sd$", variable), "meansd", "mediqr"),
             variable= gsub("(_mean_sd|_med_iqr)$", "", variable)) %>%
      dplyr::select(variable, type, stat)

    n_var<- df %>%
      summarise_if(is.numeric, funs(n_avail)) %>%
      rownames_to_column() %>%
      melt(id.vars= "rowname", value.name = "n") %>%
      dplyr::select(-rowname)

  } else {

    df<- df %>%
      group_by(!!group)
      # group_by(rlang::UQ(group))

    test_fun<- if (n_groups(df)==2) two_sample_test else if (n_groups(df)>2) k_sample_test

    sum_stat<- df %>%
      summarise_if(is.numeric, funs(mean_sd, med_iqr)) %>%
      melt(id.vars= quo_name(group), factorsAsStrings= TRUE) %>%
      dcast(as.formula(paste("variable", quo_name(group), sep= " ~ "))) %>%
      mutate(type= ifelse(grepl("mean_sd$", variable), "meansd", "mediqr"),
             variable= gsub("(_mean_sd|_med_iqr)$", "", variable)) %>%
      # adding the p-values
      left_join(test_fun(df, rlang::UQ(group)), by= c("variable", "type"))

    n_var<- df %>%
      summarise_if(is.numeric, funs(n_avail)) %>%
      melt(id.vars= quo_name(group), factorsAsStrings= TRUE) %>%
      dcast(as.formula(paste("variable", quo_name(group), sep= " ~ ")))

  }

  left_join(n_var, sum_stat, by= "variable", suffix= c("_n", "_stat")) %>%
    dplyr::select(variable, type, everything()) %>%
    arrange(variable, type)
}



#' @title n_avail
#'
#' @details
#' An internal function that format the number of non-missing observations in a variable
#'
#' @param x Numeric variable
#' @return a character reporting the number of non-missing observations
#' @export
n_avail<- function(x) formatC( sum( !is.na(x) ), digits= 0, format= "d", big.mark = ",")

#' @title mean_sd
#'
#' @details An internal function that format mean and standard deviation of a continuous variable
#'
#' @param x Numeric variable
#' @return a character reporting the mean plus/minus standard deviation
#' @export
mean_sd<- function(x) {
  n_dec<- decimalplaces(x)

  fun<- c(mean, sd)
  out<- sapply(fun,
               function(f) {
                 res<- try(f(x, na.rm= TRUE), silent = TRUE)
                 res<- if (class(res)== "try-error") NA else res
                 return(res)
               })
  out<- formatC( out, digits= n_dec, format= "f", big.mark = ",", flag= "#")
  out<- paste0(out, collapse = " \u00B1 ") # plusminus sign

  out<- c(stat= out)
  out
}



#' @title med_iqr
#'
#' @details An internal function that format median and interquartile range of a continuous variable
#'
#' @param x Numeric variable
#' @return a character reporting the median (Q1 - Q3)
#' @export
med_iqr<- function(x) {
  q1<- function(x, ...) quantile(x, probs = .25, ...)
  q3<- function(x, ...) quantile(x, probs = .75, ...)

  n_dec<- decimalplaces(x)
  fun<- c(median, q1, q3)
  out<- sapply(fun,
               function(f) {
                 res<- try(f(x, na.rm= TRUE), silent = TRUE)
                 res<- if (class(res)== "try-error") NA else res
                 return(res)
               })
  out<- formatC( out, digits= n_dec, format= "f", big.mark = ",", flag= "#")
  out<- paste0(out[1], " (",
               paste0(out[-1], collapse= " \u2013 "), ")") # long dash

  out<- c(stat= out)
  out
}



#---- test ----
#' @title two_sample_test
#'
#' @details
#' An internal function that tests equality of location parameter using two sample t-tests with unequal
#' variance when mean is reported and nonparametric rank-sum tests otherwise in comparing of two groups.
#'
#' @param x Numeric variable
#' @param grp Factor variable
#' @return a 2-tuple vectors reporting p-values
#' @export
two_sample_test<- function(df, group) {

  group<- enquo(group)

  df %>%
    ungroup() %>%
    # group_by(rlang::UQ(group)) %>%
    group_by(!!group) %>%
    select_if(is.numeric) %>%
    melt(id.vars= quo_name(group), factorsAsStrings= TRUE, na.rm= TRUE) %>%
    group_by(variable) %>%
    nest() %>%
    mutate(ttest= map(data,
                      function(df) {
                        fml<- as.formula(paste0("value ~ factor(", quo_name(group), ")"))
                        # fml<- as.formula(paste("value", quo_name(group), sep= " ~ "))
                        res<- try(t.test(fml, data= df, var.equal = FALSE),
                                  silent = FALSE)
                        if (class(res)=="try-error") NA else res$p.value
                      }),
           wilcox= map(data,
                       function(df) {
                         # if (!is.factor(df[quo_name(group)])) df[quo_name(group)]<- factor(df[quo_name(group)])
                         fml<- as.formula(paste0("value ~ factor(", quo_name(group), ")"))
                         # fml<- as.formula(paste("value", quo_name(group), sep= " ~ "))
                         res<- try(wilcox.test(fml, data= df),
                                   silent = FALSE)
                         if (class(res)=="try-error") NA else res$p.value
                       })) %>%
    dplyr::select(-data) %>%
    unnest() %>%
    melt(id.vars= "variable", variable.name= "type", value.name = "pval") %>%
    mutate(type= ifelse(grepl("^ttest", type), "meansd", "mediqr"),
           pval= format_pvalue(pval)
    )
  #   variable   type   pval
  # 1   income meansd   0.52
  # 2   weight meansd <0.001
  # 3   income mediqr   0.47
  # 4   weight mediqr <0.001
}



#' @title k_sample_test
#'
#' @details An internal function that tests equality of location parameter using one way ANOVA with unequal
#' variance when mean is reported and nonparametric Kruskal-Wallis tests otherwise in comparing >2 groups.
#'
#' @param x Numeric variable
#' @param grp Factor variable
#' @return a 2-tuple vectors reporting p-values
#' @export
k_sample_test<- function(df, group) {

  group<- enquo(group)

  df %>%
    ungroup() %>%
    group_by(!!group) %>%
    select_if(is.numeric) %>%
    melt(id.vars= quo_name(group), factorsAsStrings= TRUE, na.rm= TRUE) %>%
    group_by(variable) %>%
    nest() %>%
    mutate(oneway= map(data,
                       function(df) {
                         fml<- as.formula(paste0("value ~ factor(", quo_name(group), ")"))
                         res<- try(oneway.test(fml, data= df, var.equal = FALSE),
                                   silent = FALSE)
                         if (class(res)=="try-error") NA else res$p.value
                       }),
           kruskal= map(data,
                        function(df) {
                          # if (!is.factor(df[quo_name(group)])) df[quo_name(group)]<- as_factor(df[quo_name(group)])
                          fml<- as.formula(paste0("value ~ factor(", quo_name(group), ")"))
                          # fml<- as.formula(paste("value", quo_name(group), sep= " ~ "))
                          res<- try(kruskal.test(fml, data= df),
                                    silent = FALSE)
                          if (class(res)=="try-error") NA else res$p.value
                        })) %>%
    dplyr::select(-data) %>%
    unnest() %>%
    melt(id.vars= "variable", variable.name= "type", value.name = "pval") %>%
    mutate(type= ifelse(grepl("^oneway", type), "meansd", "mediqr"),
           pval= format_pvalue(pval)
    )
}
