#---- this is the main function ----
#' @title table_one
#'
#' @details
#' Main function that users interact. \code{table_one} calculate the selected summary statistics for continuous, logical,
#' and factor variables per statitstical guidelines of the Annals of medicine. If a group variable is provided, then
#' it will also assess the between-group difference.The input data frame should only consists of numeric, logical
#' and factor variables. Factor variables with
#' only two levels should be converted to logical variables. Date and datetime variables should be removed.
#'
#' @param df Dataframe consisting of numeric, logical, and factor variables with or without a grouping variable
#' @param group Name of the grouping variable.
#' @return The function returns a dataframe, rows of which are summary statistics depending on the variable types.
#' @examples
#' set.seed(0)
#' df<- data_frame(sex   = factor(c(rep("F", 90), rep("M", 900))),
#'                 grade = factor(sample(c("A", "B", "C"), 990, replace= TRUE), c("A", "B", "C", "D")),
#'                 income=  100 * (rnorm(990) + 5),
#'                 dm= rbernoulli(990, p= .5),
#'                 af= rbernoulli(990, p= .95)) %>%
#'   mutate(weight= if_else( sex=="F" & income>500, 3, 1),
#'          income= ifelse(income<456, NA, income),
#'          sex   = ifelse(runif(990)<.2, NA, sex),
#'          sex   = factor(sex, 1:2, labels = c("Female", "Male")),
#'          grade = ifelse(runif(990)<.25, NA, grade),
#'          grade   = factor(grade, 1:4, labels = c("A", "B", "C", "D")))
#'  datadic<- data.frame(var_name= c("sex", "grade", "income", "dm", "af"),
#'                       var_desp= c("Sex", "Grade", "Household income",
#'                                   "Presence of diabetes mellitus", "African American"))
#'
#' table_one(df, sex)
#' table_one(df, sex, datadic= datadic)
table_one<- function(df, group, datadic= NULL, var_name= "var_name", var_desp= "var_desp") {
  op<- options(warn = -1)
  on.exit(options(op))

  group<- rlang::enquo(group)

  if (rlang::quo_is_missing(group)) {
    df<- df %>%
      select_if(Negate(is.character)) %>%
      select_if(Negate(is.Date)) %>%
      mutate_if(is.factor, droplevels)

    group_var_idx<- NULL
  } else {
    df<- df %>%
      ungroup() %>%
      select_if(Negate(is.character)) %>%
      select_if(Negate(is.Date)) %>%
      mutate_if(is.factor, droplevels) %>%
      filter(!is.na(!!group)) %>%
      group_by(!!group)

    group_var_idx<- grep(group_vars(df), names(df))
  }

  num_out_lst<- if (any(sapply(if (is.null(group_var_idx)) df else df[-group_var_idx], class)=="numeric")) {
    numeric_desp(df, !!group) %>%
      rownames_to_column("row_id") %>%
      mutate(row_id= paste(variable, type, sep= "_")) %>%
      split(., .$variable)
  } else NULL

  fct_out_lst<- if (any(sapply(if (is.null(group_var_idx)) df else df[-group_var_idx], class)=="factor")) {
    factor_desp(df, !!group) %>%
      rownames_to_column("row_id") %>%
      rename(type= level) %>%
      mutate(row_id= ifelse(type!= ".", paste(variable, type, sep= "_"), variable)) %>%
      split(., .$variable)
  } else NULL

  logic_out_lst<- if (any(sapply(if (is.null(group_var_idx)) df else df[-group_var_idx], class)=="logical")) {
    logical_desp(df, !!group) %>%
      rownames_to_column("row_id") %>%
      mutate(row_id= paste0(variable, "TRUE")) %>%
      split(., .$variable)
  } else NULL


  out_lst<- num_out_lst %>%
    append(fct_out_lst) %>%
    append(logic_out_lst)

  if (is.null(datadic)) {
    out<- out_lst[names(df)] %>%
      bind_rows() %>%
      dplyr::select(row_id, variable, type,
                    ends_with("n"), ends_with("stat"),
                    everything())
  } else {
    out<- out_lst[names(df)] %>%
      bind_rows() %>%
      left_join(select_(datadic, .dots= c(var_name, var_desp)),
                by= c("variable"= quo_name(var_name))) %>%
      dplyr::select(row_id, variable, type,
                    ends_with("n"), ends_with("stat"),
                    everything())
  }

  out
}


