# Shared package attachments for the testthat suite (converted from dev-tests/).
# These packages are all already declared as Imports in DESCRIPTION; the various
# dev-tests/*.R scripts each attached a subset of them directly (via library())
# because their setup code calls dplyr/tidyr/ggplot2 verbs unqualified, not just
# through fanetc's own (already-imported) internals. Centralizing here avoids
# repeating the same suppressPackageStartupMessages() block in every test file.
suppressPackageStartupMessages({
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
