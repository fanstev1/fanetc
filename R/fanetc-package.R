#' @keywords internal
"_PACKAGE"

## Package-level imports, replacing the old Depends list: these packages were
## previously attached by library(fanetc), which is what made the unqualified
## calls in the older files resolve. dplyr, ggplot2, grid, and gtsummary are
## used unqualified throughout, so they are imported wholesale (the identical
## re-exports they share produce harmless "replacing previous import" install
## messages); the rest are imported selectively or fully qualified in the code.
## mice/mitools/sandwich stay out of the namespace on purpose: the MI helpers
## have always resolved them through require() and the user's search path.
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
NULL
