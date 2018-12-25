


summarize_mi_coxph<- function(cox_mira, exponentiate= TRUE) {
  # require(mice)
  # require(tidyverse)

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
