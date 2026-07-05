library(fanetc)
set.seed(0)
test_df <- data.frame(
  patid = 1:100,
  idx_dt = as.Date("2020-01-01"),
  evt_dt = c(NA, sample(0:200, 99)) + as.Date("2020-01-01"),
  cmp_evt_dt = c(NA, sample(50:250, 99)) + as.Date("2020-01-01"),
  end_dt = sample(100:300, 100) + as.Date("2020-01-01")
) %>%
  mutate(
    evt_dt = case_when(
      is.na(evt_dt) ~ NA_Date_,
      evt_dt < idx_dt ~ NA_Date_, # Set to NA if event date is before index date
      evt_dt > end_dt ~ NA_Date_, # Set to end date if event date is after end date
      TRUE ~ evt_dt
    ),
    cmp_evt_dt = case_when(
      is.na(cmp_evt_dt) ~ NA_Date_,
      cmp_evt_dt < idx_dt ~ NA_Date_, # Set to NA if event date is before index date
      cmp_evt_dt > end_dt ~ NA_Date_, # Set to end date if event date is after end date
      TRUE ~ cmp_evt_dt
    ),
    end_dt = case_when(
      !is.na(evt_dt) | !is.na(cmp_evt_dt) ~ NA_Date_,
      TRUE ~ end_dt
    ),
    grp = rbernoulli(n(), 0.5),
    grp = factor(grp, c(F, T), labels = c("Control", "Treatment")),
  )
test_df$evt_dt[c(12, 45, 78)] <- test_df$cmp_evt_dt[c(12, 45, 78)] <- test_df$end_dt[c(12, 45, 78)] <- NA

# Basic usage
 xx <- construct_cmprisk_var(
   test_df,
   patid = patid,
   idx_dt = idx_dt,
   evt_dt = evt_dt,
   end_dt = end_dt,
   append = TRUE,
   cmp_evt_1 = cmp_evt_dt
 )
 xx <- filter(xx, if_all(c(evt_time, evt), ~ !is.na(.)))
 xx <- xx %>%
   mutate(
     dth = as.numeric(evt != "0")
   )

xx


times<- c(6, 12, 24)
failure_fun <- TRUE
fit<- estimate_km(
   df = xx,
   group = grp,
   evt_time = evt_time,
   evt = dth
 )
surv_mat<- prepare_survfit(fit)
risk_tbl <- extract_atrisk(fit, time.list = times)
plot_cdf <- FALSE
add_legend <- TRUE





out + if (!is.null(y_lim)) coord_cartesian(ylim = y_lim, clip = "on") else coord_cartesian(clip = "on")




xx1 <- ggplot(data = risk_tbl, aes(x = time, y = strata, label = value, color = strata)) +
  geom_text(show.legend = FALSE, hjust = 0.5, vjust = 0) +
  scale_x_continuous(
    name = NULL,
    breaks = scales::pretty_breaks(6),
    expand = c(0.01, 0),
    labels = function(x) scales::number(x, accuracy = 1)
  ) +
  scale_y_discrete(name = NULL, breaks = c(-0.1, -0.11)) +
  ggtitle("Number at risk") +
  theme(
    # title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    panel.border = element_blank(),
    legend.position = "none"
  )

out/xx1
out + annotation_custom(
  grob = ggplotGrob(xx1),
  xmin = -6, xmax = 206,
  ymin = -0.1, ymax = -0.25
) +
  theme(plot.margin = margin(t = 5, r = 5, b = 200, l = 5))

plot_grid(out, xx1, ncol = 1, heights = c(0.8, 0.2))

xx2<- align_plots(out, xx1, align = "v", axis = "l")
ggdraw(xx[[2]])





fit <- estimate_cif(
   df = xx,
   group = grp,
   evt_time = evt_time,
   evt = evt
 )
# undebug(extract_atrisk) 
# debug(extract_atrisk)
extract_atrisk(fit)
xx1<- prepare_survfit(fit)

estimate_cif(
   df = xx,
   evt_time = evt_time,
   evt = evt
 ) %>% #tbl_survfit(times = 12, label_header = "**{time} Months**")
 {
    tbl_stack(
      list(
        Overall = . %>% tbl_survfit(times = 6, label_header = "**{time} Months**"),
        Control = . %>% tbl_survfit(times = 12, label_header = "**{time} Months**"),
        Treatment = . %>% tbl_survfit(times = 24, label_header = "**{time} Months**")
      )
    )
 }
  
lapply(c(6, 12, 24), function(t) {
   estimate_cif(
     df = xx,
     evt_time = evt_time,
     evt = evt
   ) %>%
     tbl_survfit(times = t, label_header = "**Months post-transplant**")
  }
  ) %>% 
  tbl_stack(group_header = c("6 Months", "12 Months", "24 Months"))

 mdl <- estimate_cif(
   df = xx,
   evt_time = evt_time,
   evt = evt
 )
 show_cif_revised(mdl, color_scheme = "brewer")


 tt <- atrisk_table %>% mutate(prob = -0.055)
 out +
   geom_text(
     data = tt, aes(x = time, y = prob, label = Overall),
     vjust = 1
   ) +
   coord_cartesian(ylim = c(0, 1), clip = "off")



    ss %$%
      map2(.x= c('surv', 'conf_low', 'conf_high'),
           .y= list(surv= surv, lower= lower, upper= upper),
           .f= function(var, mat, ...) {
             mat %>%
               as.data.frame() %>%
               mutate(strata= strata,
                      times = time) %>%
               melt(id.vars= c('strata', 'times'),
                    value.name = var) %>%
               dplyr::select(-variable)
           }) %>%
      reduce(full_join, by = c('strata', 'times')) %>%
      mutate_at(vars(one_of('surv', 'conf_low', 'conf_high')),
                function(x) paste(formatC(round(x, 3)*100, format= "f", digits= 1, flag= "#"), "%", sep= "")) %>%
      mutate(stat= paste0(surv, " [", conf_low, ", ", conf_high, "]")) %>%
      dcast(times ~ strata, value.var = 'stat')