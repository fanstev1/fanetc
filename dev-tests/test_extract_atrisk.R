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
p <- ggplot(data.frame(time = c(0, 1000), prob = c(1, 0)), aes(time, prob)) +
  geom_step() +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 1))
out <- add_atrisk(p, fit, x_break = breaks)

labs <- unlist(lapply(out$layers, function(l) {
  g <- l$geom_params$grob
  if (inherits(g, "text")) as.character(g$label) else NULL
}))
labs <- trimws(labs)
expected_counts <- as.character(c(ref_wide))          # 138 62 20 7 90 53 21 3
check("add_atrisk: header + 2 strata rows + 8 cells",
      sum(labs == "At-risk N:") == 1 && all(c("1:", "2:") %in% labs))
check("add_atrisk: all 8 stratified counts rendered",
      all(expected_counts %in% labs) &&
        sum(labs %in% expected_counts) == length(expected_counts))

out1 <- add_atrisk(p, fit1, x_break = breaks)
labs1 <- trimws(unlist(lapply(out1$layers, function(l) {
  g <- l$geom_params$grob
  if (inherits(g, "text")) as.character(g$label) else NULL
})))
check("add_atrisk no strata: 4 overall counts rendered",
      all(as.character(ref1$n.risk) %in% labs1))

cat(if (ok) "\nALL AT-RISK CHECKS PASS\n" else "\nAT-RISK FAILURES PRESENT\n")
