# Shared package attachments for the testthat suite (converted from dev-tests/).
# These packages are all already declared as Imports in DESCRIPTION; the various
# dev-tests/*.R scripts each attached a subset of them directly (via library())
# because their setup code calls dplyr/tidyr/ggplot2 verbs unqualified, not just
# through fanetc's own (already-imported) internals. Centralizing here avoids
# repeating the same suppressPackageStartupMessages() block in every test file.
#
# rlang is attached LAST on purpose (mirrors dev-tests/test_construct_equiv.R's
# own comment: "rlang last so its as_name() wins"): lazyeval also defines an
# as_name(), and tests/testthat/fixtures/ref_construct.R (the frozen pre-refactor
# reference implementation) calls as_name() unqualified -- it needs rlang's
# version (which resolves a quosure to a string) to be the one found on the
# search path, not lazyeval's.
suppressPackageStartupMessages({
  library(lazyeval)
  library(dplyr)
  library(forcats)
  library(tidyselect)
  library(gtsummary)
  library(tidyr)
  library(survival)
  library(purrr)
  library(magrittr)
  library(ggplot2)
  library(grid)
  library(scales)
  library(viridis)
  library(cmprsk)
  library(rlang)
})
