suppressPackageStartupMessages({
  library(survival); library(dplyr); library(tidyr); library(magrittr); library(ggplot2); library(grid)
})
if (basename(getwd()) == "dev-tests") setwd("..")
source("R/event_time_desp.R")

ok <- TRUE
check <- function(label, cond) {
  cat(if (isTRUE(cond)) "PASS:" else {ok <<- FALSE; "FAIL:"}, label, "\n")
}

fit <- survfit(Surv(time, status) ~ sex, data = lung)
breaks <- c(0, 250, 500, 750)
# reference counts from survival itself (number at risk at each requested time)
ref <- summary(fit, times = breaks, extend = TRUE)
ref_wide <- tapply(ref$n.risk, list(ref$time, ref$strata), identity)

r <- extract_atrisk(fit, time.list = breaks)

check("stratified: plain data.frame (not tibble/grouped)", identical(class(r), "data.frame"))
check("stratified: wide with time first, one column per stratum",
      identical(names(r), c("time", "1", "2")))
check("stratified: counts match summary(fit)$n.risk",
      isTRUE(all.equal(unname(as.matrix(r[, -1])), unname(ref_wide), check.attributes = FALSE)))
check("stratified: counts are integers", all(vapply(r[-1], is.integer, TRUE)))

fit1 <- survfit(Surv(time, status) ~ 1, data = lung)
ref1 <- summary(fit1, times = breaks, extend = TRUE)
r1 <- extract_atrisk(fit1, time.list = breaks)
check("no strata: plain data.frame with time, Overall", identical(names(r1), c("time", "Overall")))
check("no strata: counts match summary(fit)$n.risk", isTRUE(all.equal(r1$Overall, as.integer(ref1$n.risk))))

# default time.list still works
r_def <- extract_atrisk(fit)
check("default time.list: wide shape", identical(names(r_def), c("time", "1", "2")))

## ---- add_atrisk renders the right numbers ----
# base plot mimics show_surv/show_cif: no scale limits (they use coord_cartesian or none)
p <- ggplot(data.frame(time = c(0, 1000), prob = c(1, 0)), aes(time, prob)) +
  geom_step()

# text labels regardless of implementation (annotation_custom grob or text layer data)
get_labels <- function(p) {
  trimws(unlist(lapply(p$layers, function(l) {
    g <- l$geom_params$grob
    if (inherits(g, "text")) return(as.character(g$label))
    if (is.data.frame(l$data) && !is.null(l$data$label)) return(as.character(l$data$label))
    NULL
  })))
}
panel_ranges <- function(p) {
  pp <- ggplot_build(p)$layout$panel_params[[1]]
  list(x = pp$x.range, y = pp$y.range)
}

out <- add_atrisk(p, fit, x_break = breaks)
labs <- get_labels(out)
expected_counts <- as.character(c(ref_wide))          # 138 62 20 7 90 53 21 3
check("add_atrisk: header + 2 strata rows + 8 cells",
      sum(labs == "At-risk N:") == 1 && all(c("1:", "2:") %in% labs))
check("add_atrisk: all 8 stratified counts rendered",
      all(expected_counts %in% labs) &&
        sum(labs %in% expected_counts) == length(expected_counts))
check("add_atrisk: panel ranges unchanged (no ylim set by caller)",
      isTRUE(all.equal(panel_ranges(p), panel_ranges(out))))

out1 <- add_atrisk(p, fit1, x_break = breaks)
labs1 <- get_labels(out1)
check("add_atrisk no strata: 4 overall counts rendered",
      all(as.character(ref1$n.risk) %in% labs1))
check("add_atrisk no strata: panel ranges unchanged",
      isTRUE(all.equal(panel_ranges(p), panel_ranges(out1))))

# caller-supplied coord limits (as in show_surv) must survive add_atrisk
p_lim <- p + coord_cartesian(ylim = c(0, 1), clip = "on")
out_lim <- add_atrisk(p_lim, fit, x_break = breaks)
check("add_atrisk: existing coord ylim preserved",
      isTRUE(all.equal(panel_ranges(p_lim)$y, panel_ranges(out_lim)$y)))

cat(if (ok) "\nALL AT-RISK CHECKS PASS\n" else "\nAT-RISK FAILURES PRESENT\n")
