#' @keywords internal
"_PACKAGE"

## Package-level imports, replacing the old Depends list: these packages were
## previously attached by library(fanetc), which is what made the unqualified
## calls in the older files resolve. dplyr, ggplot2, grid, and gtsummary are
## used unqualified throughout, so they are imported wholesale (the identical
## re-exports they share produce harmless "replacing previous import" install
## messages); the rest are imported selectively or fully qualified in the code.
## mice/mitools stay out of the namespace on purpose: they are Suggests, and
## the MI helpers reach them via ::-qualified calls behind check_mi_packages().
#' @import dplyr
#' @import ggplot2
#' @import grid
#' @import gtsummary
#' @import survival
#' @importFrom cmprsk cuminc
#' @importFrom reshape2 melt dcast
#' @importFrom viridis scale_color_viridis scale_colour_viridis scale_fill_viridis
#' @importFrom forcats fct_drop
#' @importFrom magrittr %>% %$%
#' @importFrom rlang := quo_get_expr
#' @importFrom tidyr unnest pivot_wider
#' @importFrom purrr map2 reduce
#' @importFrom tibble rownames_to_column
#' @importFrom stats as.formula coef model.matrix pchisq pf qnorm quantile relevel sd setNames termplot vcov
NULL

## Column names created/consumed inside dplyr/tidyr/magrittr pipelines (plus
## the magrittr dot and the list-element names extracted via %$%). Declaring
## them silences R CMD check's "no visible binding for global variable" NOTE
## without touching the pipelines themselves.
utils::globalVariables(c(
  ".", "2.5 %", "97.5 %", "chisq_p", "coefficients", "conf_high", "conf_low",
  "data", "df", "est", "estimate", "lower", "p", "p.value", "prob", "pstate",
  "pval", "rid", "se", "state", "state_label", "state_strata", "surv", "term",
  "time", "upper", "var", "variable", "variance", "y"
))

## broom and cardx are never called directly, but gtsummary::add_p() needs
## them at runtime, so they must stay in Imports (a Suggests package is not
## guaranteed to be installed). Referencing their namespaces here is what
## keeps "checking dependencies in R code" from flagging them as unused.
ignore_unused_imports<- function() {
  broom::tidy
  cardx::ard_stats_t_test
}
