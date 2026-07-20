#---- Internal helpers shared by show_surv() and show_cif() ----

# npc coordinates and justification for the p-value annotation
pvalue_npc_position <- function(pvalue_pos, tiny_nudge = 0.01) {
  switch(pvalue_pos,
    topleft     = list(x = 0 + tiny_nudge, y = 1 - tiny_nudge, hjust = 0,   vjust = 1),
    bottomleft  = list(x = 0 + tiny_nudge, y = 0 + tiny_nudge, hjust = 0,   vjust = 0),
    topright    = list(x = 1 - tiny_nudge, y = 1 - tiny_nudge, hjust = 1,   vjust = 1),
    bottomright = list(x = 1 - tiny_nudge, y = 0 + tiny_nudge, hjust = 1,   vjust = 0),
    left        = list(x = 0 + tiny_nudge, y = 0.5,            hjust = 0,   vjust = 0.5),
    right       = list(x = 1 - tiny_nudge, y = 0.5,            hjust = 1,   vjust = 0.5),
    top         = list(x = 0.5,            y = 1 - tiny_nudge, hjust = 0.5, vjust = 1),
    bottom      = list(x = 0.5,            y = 0 + tiny_nudge, hjust = 0.5, vjust = 0)
  )
}

# add the italic Inconsolata p-value textGrob at the requested position
annotate_pvalue <- function(p, label, pvalue_pos, plot_theme) {
  pos <- pvalue_npc_position(pvalue_pos)
  p + annotation_custom(
    grob = textGrob(
      label = label,
      x = pos$x, y = pos$y, hjust = pos$hjust, vjust = pos$vjust,
      gp = gpar(
        family = "Inconsolata",
        fontface = "italic",
        fontsize = if (is.null(plot_theme$text$size)) 11 else plot_theme$text$size
      )
    )
  )
}

# colour/fill scale pair for a color scheme. show_surv() uses grey_end = 0.75 with no
# guide argument; show_cif() uses grey_end = 0.65 and passes guide_legend(title = "")
# positionally, as its original inline code did. The "manual" scheme takes its
# arguments verbatim from color_list and (as always) ignores the extra guide.
event_time_color_scales <- function(color_scheme, color_list, grey_end = 0.75, blank_guide_title = FALSE) {
  extra <- if (blank_guide_title) list(guide_legend(title = "")) else list()
  fns <- switch(color_scheme,
    "brewer"  = list(colour = scale_color_brewer, fill = scale_fill_brewer,
                     args = c(list(palette = "Set1"), extra)),
    "grey"    = list(colour = scale_color_grey, fill = scale_fill_grey,
                     args = c(list(start = 0, end = grey_end), extra)),
    "viridis" = list(colour = viridis::scale_color_viridis, fill = viridis::scale_fill_viridis,
                     args = c(list(option = "viridis", begin = .2, end = .85, discrete = TRUE), extra)),
    "manual"  = list(colour = scale_color_manual, fill = scale_fill_manual,
                     args = color_list)
  )
  list(colour = do.call(fns$colour, fns$args), fill = do.call(fns$fill, fns$args))
}
