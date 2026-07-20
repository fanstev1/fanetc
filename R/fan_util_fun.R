#---- utility functions ----

#' @title decimalplaces
#'
#' @details
#' An internal function that determines the number of digits in the summary of a continuous variable.
#'
#' @param x a continous variable
#' @param max_dec the maximum number of decimal places to return (default 4)
#' @return the most frequent number of digits in the variable
#' @export
decimalplaces <- function(x, max_dec= 4L) {
  frac<- round(x[!is.na(x)] %% 1, 10)
  frac<- frac[frac != 0]
  if (length(frac) == 0) return(0L)

  # decimal digits of each fractional part, trailing zeros stripped; values
  # whose character form has no "." (scientific notation) are ignored
  txt<- gsub("0+$", "", as.character(frac))
  parts<- strsplit(txt, ".", fixed = TRUE)
  n_dec<- nchar(vapply(parts[lengths(parts) == 2L], `[`, "", 2L))
  if (length(n_dec) == 0) return(0L)

  counts<- table(n_dec)
  modes<- as.integer(names(counts)[counts == max(counts)])
  pmin.int(as.integer(max_dec), max(modes))
}

# format_pvalue() now lives in R/desp_table_gtsummary.R

#' @title updateWorksheet
#'
#' @details
#' An internal function that adds a new worksheet or updates (remove and add) the existing worksheet in wb object.
#'
#' @param wb a \code{wb} object
#' @param sheetName a name of the sheet to be updated
#' @param x a dataframe to be write in the \code{wb} object
#' @param ... additional arguments (currently ignored)
#' @return a \code{wb} object
#' @export
updateWorksheet<- function(wb, sheetName, x, ...) {
  sheet_exists<- sheetName %in% names(wb)

  if (sheet_exists) {
    sheetOrder<- openxlsx::worksheetOrder(wb)
    names(sheetOrder)<- names(wb)
    openxlsx::removeWorksheet(wb, sheetName)
  }

  openxlsx::addWorksheet(wb, sheetName)
  openxlsx::writeData(wb, sheetName, x)

  if (sheet_exists) openxlsx::worksheetOrder(wb)<- sheetOrder[names(wb)]
  wb
}


#' @title summarize_coxph
#'
#' @details
#' The function summarizes the fitted cox model with the type 3 error based on Wald's statistics.
#'
#' @param mdl a fitted \code{coxph} or \code{coxph.penal} object
#' @param exponentiate a logical parameter indicating whether hazard ratios should be reported instead of log-hazard ratios (default TRUE)
#' @param maxlabel the maximum length of the term labels, passed to \code{summary.coxph()}
#' @param alpha the significance level; estimates are reported with (1-alpha) normal-approximation confidence intervals
#' @return a dataframe with columns term, stat ("estimate [lower, upper]") and pval; terms with more than 1 df get an additional row carrying the type 3 Wald p-value
#' @export
summarize_coxph<- function(mdl, exponentiate= TRUE, maxlabel= 100, alpha= 0.05) {

  if (!inherits(mdl, c("coxph", "coxph.penal"))) stop("Not a coxph or coxph.penal object.")

  out<- summary(mdl, maxlabel= maxlabel)$coefficient %>%
    as.data.frame() %>%
    rownames_to_column("term")

  if (inherits(mdl, "coxph.penal")) {
    out<- rename(out, se= 'se(coef)')
  } else if (identical(class(mdl), "coxph")) {
    out<- rename(out, se= 'se(coef)', p= 'Pr(>|z|)')
  }

  out<- out %>%
    mutate(conf_low = coef - qnorm(1-alpha/2) * se,
           conf_high= coef + qnorm(1-alpha/2) * se,
           coef     = if (exponentiate) exp(coef) else coef,
           conf_low = if (exponentiate) exp(conf_low) else conf_low,
           conf_high= if (exponentiate) exp(conf_high) else conf_high,
           stat= ifelse(is.na(coef), NA_character_,
                        paste0(formatC(coef, format= "f", digits= 3, flag= "#"), " [",
                               formatC(conf_low, format= "f", digits= 3, flag= "#"), ", ",
                               formatC(conf_high, format= "f", digits= 3, flag= "#"), "]")),
           pval= format_pvalue(p)) %>%
    dplyr::select(all_of(c("term", "stat", "pval")))

  type3_coxph<- function(mdl, beta_var= vcov(mdl)) {
    x<- model.matrix(mdl)
    varseq <- attr(x, "assign")
    out<- lapply(unique(varseq),
                 function(i){
                   df<- sum(varseq==i)
                   # set out the contrast matrix
                   L<- matrix(0, nrow= df, ncol= ncol(x))
                   L[, varseq==i]<- diag(df)

                   #
                   vv<- L %*% beta_var %*% t(L)
                   cc<- L %*% coef(mdl)

                   # calculate Wald's test statistics and p-value
                   wald_stat<- as.numeric( t(cc) %*% solve(vv) %*% cc )
                   pval<- pchisq(wald_stat,
                                 df= if (inherits(mdl, "coxph.penal") && !is.na(mdl$df[i])) mdl$df[i] else df,
                                 lower.tail = FALSE)

                   data.frame(df= round(df, 0), stat= wald_stat, chisq_p= pval)
                 })
    out<- do.call(rbind, out)
    var_label<- grep("(strata|cluster|tt|ridge|pspline|frailty)\\(.*\\)", attr(mdl$terms, "term.labels"), value = T, invert = T)
    out<- cbind(variable= var_label, out, stringsAsFactors= FALSE)
    out
  }

  type3_out<- type3_coxph(mdl)

  out<- type3_out %>%
    filter(df> 1) %>%
    mutate(pval= format_pvalue(chisq_p)) %>%
    dplyr::select(variable, pval) %>%
    rename(term= variable) %>%
    bind_rows(out) %>%
    arrange(term) %>%
    dplyr::select(term, stat, pval)

  out
}

# mice and mitools are Suggests, not Imports: error informatively if the
# multiple-imputation helpers are called without them installed. Note that
# getfit()/pool() live in mice and MIextract()/MIcombine() in mitools -- the
# old require(mitools) calls never actually provided getfit()/pool().
check_mi_packages<- function() {
  for (pkg in c("mice", "mitools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for the multiple-imputation helpers; please install it.",
           call. = FALSE)
    }
  }
}

#' @title calculate_type3_mi
#'
#' @details
#' The function calculates the type 3 p-values based on Wald's statistics for a model
#' fitted to multiply-imputed data, following Li, Meng, Raghunathan and Rubin (1991),
#' "Significance levels from repeated p-values with multiply-imputed data",
#' Statistica Sinica.
#'
#' @param mira_obj a \code{mira} object: a list of models fitted to each imputed dataset, e.g. from \code{with(mice::mice(...), glm(...))}
#' @param vcov_fun an optional function extracting the variance-covariance matrix from each fitted model (e.g. \code{sandwich::vcovHC}); the default NULL uses \code{stats::vcov}
#' @return a list with one dataframe per model term (columns var, rid, df, stat, chisq_p); each element carries its design-matrix columns in the "col_in_X" attribute
#' @export
calculate_type3_mi<- function(mira_obj, vcov_fun= NULL) {
  check_mi_packages()

  # to calulate the type 3 error
  # Li, Meng, Raghunathan and Rubin. Significance levels from repeated p-values with multiply-imputed data. Statistica Sinica (1991)
  first_fit<- mice::getfit(mira_obj, 1L)
  x<- model.matrix(first_fit)
  varseq<- attr(x, "assign")
  m <- length(mira_obj$analyses)

  # coef estimate and its vcov for each MI model
  betas<- mitools::MIextract(mira_obj$analyses, fun= coef)
  vars <- mitools::MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)

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

                 out<- data.frame(var= if (i==0) '(Intercept)' else attr(first_fit$terms, "term.labels")[i],
                                  rid = i,
                                  df= round(df, 0),
                                  stat= wald_stat,
                                  chisq_p= pval)
                 attr(out, "col_in_X")<- data.frame(term= colnames(x)[i==varseq],
                                                    rid= i,
                                                    stringsAsFactors = FALSE)
                 out
               })
  out
}

# shared by summarize_mi_glm()/summarize_mi_coxph(): design-matrix term index
# carried in the "col_in_X" attributes, and the final type3-row append/ordering
.mi_term_index<- function(type3_list) {
  dplyr::bind_rows(lapply(type3_list, function(x) attr(x, "col_in_X")))
}
.bind_type3_rows<- function(main, type3) {
  main %>%
    dplyr::bind_rows(type3) %>%
    dplyr::arrange(rid, var) %>%
    dplyr::select(var, stat, pval, dplyr::everything())
}


#' @title summarize_mi_glm
#'
#' @details
#' The function summarizes a generalized linear model fitted to multiply-imputed data:
#' the per-imputation coefficients are pooled by Rubin's rules via
#' \code{mitools::MIcombine()}, and type 3 p-values are taken from
#' \code{calculate_type3_mi()}.
#'
#' @param mira_obj a \code{mira} object: a list of GLMs fitted to each imputed dataset, e.g. from \code{with(mice::mice(...), glm(...))}
#' @param exponentiate a logical parameter indicating whether the pooled estimates and confidence limits should be exponentiated (default FALSE)
#' @param alpha the significance level; estimates are reported with (1-alpha) normal-approximation confidence intervals
#' @param vcov_fun an optional function extracting the variance-covariance matrix from each fitted model (e.g. \code{sandwich::vcovHC}); the default NULL uses \code{stats::vcov}
#' @return a dataframe with one row per coefficient (columns var, stat "estimate [lower, upper]", pval, est, se, conf_low, conf_high, rid); terms with more than 1 df get an additional row carrying the type 3 Wald p-value
#' @export
summarize_mi_glm<- function(mira_obj, exponentiate= FALSE, alpha= .05, vcov_fun= NULL) {

  out<- calculate_type3_mi(mira_obj, vcov_fun= vcov_fun)

  # the order of the variables in the output
  out_tmp<- .mi_term_index(out)

  type3_out<- out %>%
    bind_rows() %>%
    mutate(pval= format_pvalue(chisq_p))

  glm_out<- mitools::MIcombine(mitools::MIextract(mira_obj$analyses, fun= coef),
                      mitools::MIextract(mira_obj$analyses, fun= if (is.null(vcov_fun)) vcov else vcov_fun)) %$%
    data.frame(est= coefficients,
               se = sqrt(diag(variance))) %>%
    rownames_to_column(var= "term") %>%
    right_join(out_tmp, by= "term") %>%
    mutate(conf_low = est - qnorm(1-alpha/2) * se,
           conf_high= est + qnorm(1-alpha/2) * se,
           est      = if (exponentiate) exp(est) else est,
           conf_low = if (exponentiate) exp(conf_low) else conf_low,
           conf_high= if (exponentiate) exp(conf_high) else conf_high,
           stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
           # match each coefficient row to its type-3 variable: strip glm's
           # "TRUE" suffix on logical dummies, then prefix-match (charmatch)
           # against the type-3 variable names
           pval= type3_out$pval[charmatch(gsub("TRUE$", "", term), type3_out$var)]) %>%
    select(term, stat, pval, rid, everything())  %>%
    rename(var= term)

  type3_out<- type3_out %>%
    filter(df>1) %>%
    dplyr::select(var, pval, rid)

  .bind_type3_rows(glm_out, type3_out)
}

#' @title summarize_mi_coxph
#'
#' @details
#' The function summarizes a Cox model fitted to multiply-imputed data: the
#' per-imputation coefficients are pooled by Rubin's rules via \code{mice::pool()},
#' with the complete-data degrees of freedom set to the number of events minus the
#' number of coefficients, and type 3 p-values are taken from
#' \code{calculate_type3_mi()}.
#'
#' @param cox_mira a \code{mira} object: a list of \code{coxph} models fitted to each imputed dataset
#' @param exponentiate a logical parameter indicating whether hazard ratios should be reported instead of log-hazard ratios (default TRUE)
#' @param alpha the significance level; estimates are reported with (1-alpha) confidence intervals
#' @return a dataframe with one row per coefficient (columns var, stat "estimate [lower, upper]", pval, est, conf_low, conf_high, rid); terms with more than 1 df get an additional row carrying the type 3 Wald p-value
#' @export
summarize_mi_coxph<- function(cox_mira, exponentiate= TRUE, alpha= .05) {

  out<- calculate_type3_mi(cox_mira)

  # the order of the variables in the output
  out_tmp<- .mi_term_index(out)

  type3_out<- out %>%
    bind_rows() %>%
    mutate(pval= format_pvalue(chisq_p)) %>%
    filter(df>1) %>%
    dplyr::select(var, pval, rid)

  cox_out<- cox_mira %>%
    # see https://github.com/amices/mice/issues/246#
    mice::pool(dfcom = mice::getfit(., 1L)$nevent - length(coef(mice::getfit(., 1L)))) %>%
    summary(conf.int = TRUE,
            conf.level = 1-alpha,
            exponentiate= exponentiate) %>%
    rename(est      = estimate,
           pval     = p.value,
           conf_low = `2.5 %`,
           conf_high= `97.5 %`) %>%
    mutate(var= as.character(term),
           stat = sprintf("%4.3f [%4.3f, %4.3f]", est, conf_low, conf_high),
           pval= format_pvalue(pval)) %>%
    dplyr::select(var, stat, pval, est, conf_low, conf_high) %>%
    full_join(out_tmp, by= c("var" = "term"))

  .bind_type3_rows(cox_out, type3_out)
}

#' @title generate_mi_glm_termplot_df
#'
#' @details
#' The function builds \code{stats::termplot()}-style plot data for a generalized
#' linear model fitted to multiply-imputed data. The per-imputation coefficients and
#' variance-covariance matrices are pooled by Rubin's rules via
#' \code{mitools::MIcombine()}, and each term's partial effect is returned with
#' pointwise 95\% confidence limits.
#'
#' @param mira_obj a \code{mira} object: a list of GLMs fitted to each imputed dataset
#' @param terms an integer vector selecting the model terms to include (default: all terms)
#' @param center_at an optional list (one element per selected term) giving the covariate value at which each term's effect is anchored at 0: a factor level for factor terms, the smallest observed x at or above the given value for numeric terms (logical terms are anchored at FALSE); the default NULL keeps termplot's own centering
#' @param vcov_fun an optional function extracting the variance-covariance matrix from each fitted model (e.g. \code{sandwich::vcovHC}); the default NULL uses \code{stats::vcov}
#' @param ... additional arguments (currently ignored)
#' @return a named list with one dataframe per term (columns x, y, se, conf_low, conf_high)
#' @export
generate_mi_glm_termplot_df<- function(mira_obj,
                                       terms= NULL,
                                       center_at= NULL,
                                       vcov_fun= NULL, ...) {
  check_mi_packages()
  dummy_mdl<- mice::getfit(mira_obj, 1L)
  tt<- stats::terms(dummy_mdl)
  terms<- if (is.null(terms)) 1:length(labels(tt)) else terms
  cn<- attr(tt, "term.labels")[terms]
  varseq<- attr(mm_orig<- model.matrix(dummy_mdl), "assign")

  carrier.name <- function(term) {
    if (length(term) > 1L)
      carrier.name(term[[2L]])
    else as.character(term)
  }

  betas <- mitools::MIextract(mira_obj$analyses,
                     fun = coef)

  vars  <- mitools::MIextract(mira_obj$analyses,
                     fun = if (is.null(vcov_fun)) vcov else vcov_fun)

  mi_res<- mitools::MIcombine(betas, vars)

  dummy_mdl$coefficients<- mi_res$coefficients

  plot_d<- termplot(dummy_mdl, terms= terms, plot= FALSE)

  plot_d<- mapply(function(df, cc, var, term_id) {

    mm<- matrix(apply(mm_orig, 2, mean),
                nrow = nrow(df),
                ncol = length(varseq),
                byrow = T)
    colnames(mm)<- colnames(mm_orig)
    rownames(mm)<- df$x

    # calculate fitted value
    df<- if (!is.null(cc)) {
      which_x<- if (is.factor(df$x)) {
        which(df$x==cc)
      } else if (is.logical(df$x)) {
        which(!df$x)
      } else {
        min(which(df$x>=cc))
      }
      mutate(df, y = y - y[which_x])
    } else df

    # now calculate the standard error
    tmp<- df
    # termplot() names its per-term data frames c("x", "y"); rename "x" to the
    # term's carrier variable so model.matrix() finds it below
    names(tmp)<- gsub("x", sapply(str2expression(var), carrier.name), names(tmp))
    mm[, term_id == varseq]<- (model.matrix(as.formula(paste("~ ", var)), data= tmp)[,-1])
    df$se<- sqrt(diag(mm %*% mi_res$variance %*% t(mm)))

    df %>%
      mutate(conf_low= y - qnorm(0.975) * se,
             conf_high= y + qnorm(0.975) * se)
  },
  df= plot_d,
  cc= if (is.null(center_at)) vector("list", length(terms)) else center_at,
  var= cn,
  term_id= terms,
  SIMPLIFY = FALSE)

  plot_d
}
