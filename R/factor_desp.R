#---- summarize factor variables ----
#' @title factor_desp
#'
#' @details An internal function that calculate frequencies and proportion of levels in factor/categorical variables.
#' If there are more than 1 group, then fisher exact tests are applied to assess the between-group differences.
#'
#' @param df Dataframe
#' @return a dataframe consisting of columns of character variables indicating the frequency and proportion of logical variables.
factor_desp<- function(df, group) {

  group<- rlang::enquo(group)

  # 1 - select variables of factor class
  df<- if (rlang::quo_is_missing(group)) {
    df %>%
      ungroup() %>%
      select_if(is.factor)
  } else {
    df %>%
      group_by(!!group) %>%
      select_if(is.factor)
  }

  # 2 - create table object for ALL selected factor variables (not sure if it is a good idea but ...)
  # here we are going to drop unused levels (drop.unused.levels = TRUE)
  table_obj<- xtabs( ~ ., data= df, addNA= FALSE, drop.unused.levels = TRUE)

  if (rlang::quo_is_missing(group)) {
    factor_dist(table_obj = table_obj,
                pct_digits = if (nrow(df)< 200) 0 else 1)
  } else {
    factor_dist(table_obj = table_obj,
                col_var = rlang::UQ(group),
                pct_digits = if (nrow(df)< 200) 0 else 1)
  }

}

#' @title factor_dist
#'
#' @details
#' An internal function that takes a table object (i.e. a cross table for all factor variables in the data) and outputs
#' marginal table. If there are more than 1 group, then fisher exact tests are applied to assess the between-group differences.
#'
#' @param df Dataframe
#' @return a dataframe consisting of columns of character variables indicating the frequency and proportion of logical variables.
factor_dist<- function(table_obj, col_var, pct_digits= 1, removeNA= TRUE) {
  ## This function takes a table object (i.e. a cross table for all factor variables in the data) and outputs
  ## marginal table.
  ##   Row variables are the variables of factor class in input data
  ##   At most one column variable is allowed.

  col_var<- rlang::enquo(col_var)
  fct_var_name<- names(dimnames(table_obj))

  # no column variable
  if (rlang::quo_is_missing(col_var)) {

    out<- lapply(fct_var_name,
                 function(x) {
                   # for each row variable do the following:

                   # get the dimension index for each of the fct_var_name
                   m_idx<- grep(x, fct_var_name)

                   freq <- margin.table(table_obj, margin= m_idx)
                   # if (removeNA) {
                   #   freq <- freq[!is.na(names(freq))]
                   # }
                   pct  <- prop.table(freq)

                   freq<- freq %>%
                     addmargins() %>%
                     as.data.frame(row.names= x,
                                   responseName = "freq",
                                   stringsAsFactors = FALSE) %>%
                     rownames_to_column("level") %>%
                     mutate(n   = ifelse(level=="Sum", freq, NA),
                            freq= ifelse(level=="Sum", NA, freq),
                            n   = formatC(n, format= "d", big.mark = ","),
                            freq= formatC(freq, format= "d", big.mark = ",")
                     ) %>%
                     dplyr::select(level, n, freq)

                   pct<- pct %>%
                     addmargins() %>%
                     as.data.frame(row.names= x,
                                   responseName = "pct",
                                   stringsAsFactors = FALSE) %>%
                     rownames_to_column("level") %>%
                     mutate(pct= formatC(pct*100, digits= pct_digits, format= "f")) %>%
                     dplyr::select(level, pct)

                   full_join(freq, pct, by = c("level")) %>%
                     mutate(stat= paste0(freq, " (", pct, "%)"),
                            stat= ifelse(level=="Sum", NA_character_, stat),
                            level= ifelse(level=="Sum", ".", level)) %>%
                     dplyr::select(level, n, stat) %>%
                     arrange(level)

                 }) %>%
      setNames(fct_var_name)

  } else {
    # column is the grouping variable
    col_idx <- grep(quo_name(col_var), fct_var_name)
    row_vars<- grep(quo_name(col_var), fct_var_name, invert = TRUE, value= TRUE)

    out<- lapply(row_vars,
                 function(x) {

                   # get the dimension index for each of the fct_var_name
                   m_idx<- grep( paste0("^", x, "$"), fct_var_name)

                   freq <- margin.table(table_obj, margin= c(m_idx, col_idx))
                   # if (removeNA) {
                   #   freq <- freq[!is.na(rownames(freq)), !is.na(colnames(freq))]
                   # }
                   pct  <- prop.table(freq, margin = 2)

                   # fisher exact test
                   test<- try(fisher.test(freq, hybrid = TRUE, conf.int = FALSE), silent = TRUE)
                   test<- if (class(test)=="try-error") NA else test$p.value

                   # total
                   total<- margin.table(freq, 2) %>%
                     as.data.frame(responseName = "n", stringsAsFactors = FALSE) %>%
                     mutate(n= formatC(n, format= "d", big.mark = ",")) %>%
                     dcast(as.formula( paste0(". ~ ", quo_name(col_var))), value.var = "n") %>%
                     bind_cols(pval= format_pvalue(test))


                   freq<- freq %>%
                     as.data.frame(responseName = "freq", stringsAsFactors = FALSE) %>%
                     mutate(freq= formatC(freq, format= "d", big.mark = ","))

                   pct<- pct %>%
                     as.data.frame(responseName = "pct", stringsAsFactors = FALSE) %>%
                     mutate(pct= formatC(pct*100, digits= pct_digits, format= "f"))

                   out<- full_join(freq, pct, by= c(x, quo_name(col_var))) %>%
                     mutate(stat= paste0(freq, " (", pct, "%)")) %>%
                     # bind_rows(total) %>%
                     dplyr::select(-freq, -pct) %>%
                     dcast(as.formula(paste(x, quo_name(col_var), sep= " ~ ")), value.var = "stat")

                   names(out)[1]<- names(total)[1]<- "level"
                   out<- full_join(total, out, by= "level", suffix= c("_n", "_stat"))
                   out
                 }) %>%
      setNames(row_vars)
  }
  out %>%
    bind_rows(.id= "variable") %>%
    mutate(level= ifelse(level==".", NA_character_, level))
}


