#
#  Color palettes 
#
#   Two colorblind palettes
  cbPal5 <- c("#009E73","#E69F00",  "#0072B2", "#CC79A7", "#999999")
  cbPal8 <- c("#66c2a5","#fc8d62",  "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3" )
#
#
#   (Minor) tweaks to ggplot themes 
#  
# A theme for ggplotting ordinations 
#
theme_ord <- function (base_size = 12, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="bold"),
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(2,10,2,2), "mm"), 
          strip.background = element_rect(fill = "lightgreen", colour = "grey50", size = 0.2))
}
#
# No gridlines or axes for general graphics like conceptual models
#
theme_empty <- function (base_size = 12, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.line = element_line(colour = "black"),
          axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="bold"),
          panel.border = element_rect(fill = NA, colour = NA), 
          panel.grid.major = element_line(colour = "white"), 
          panel.grid.minor = element_line(colour = "white"), 
          strip.background = element_rect(fill = "#A6CEE3", 
                                          colour = "grey50", 
                                          size = 0.2), 
          panel.background = element_rect(fill = "white", 
                                          colour = "white"), 
          plot.background = element_rect(fill = "white", 
                                         colour = "white"), 
          legend.background= element_rect(fill = "white", 
                                          colour = "white"), 
          legend.key = element_rect(fill = "white", 
                                    colour = "white"), 
          plot.caption = element_text(size = 9, hjust = 1, vjust=-5,
                                      color = "grey40", face = "italic"))
}
#
# A theme for plotting maps with ggplot 
#
theme_map <- function (base_size = 12, legend = FALSE) 
{
  if (legend) {
    return(theme_grey(base_size = base_size, base_family = base_family),
           theme(legend.text = element_text(size = rel(0.8)), 
                 legend.title = element_text(size = rel(0.8)),
                 legend.key = element_rect(colour = "white"), 
                 panel.background = element_rect(fill = "white", 
                                                 colour = NA)))
  }
  else {
    return(theme(line = element_blank(), 
                 rect = element_blank(), 
                 axis.title = element_blank(), 
                 axis.text = element_blank(),
                 axis.ticks.length = unit(0, "cm"), 
                 panel.spacing = unit(0, "lines"), 
                 plot.margin = unit(c(0, 0, -0.5, -0.5), "lines")) )
  }
}
#
# Beamer themes 
#
# Theme Beamer 1 for transparent ggplots on slides
#
theme_bm1 <- function (base_size = 12, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="bold"),
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "#A6CEE3", colour = "grey50", size = 0.2), 
          panel.background = element_rect(fill = "transparent", 
                                          colour = NA), 
          plot.background = element_rect(fill = "transparent", 
                                         colour = NA), 
          legend.background= element_rect(fill = "transparent", 
                                          colour = NA), 
          legend.key = element_rect(fill = "transparent", 
                                    colour =NA))
}
#
# Another beamer theme 
#
theme_bw2 <- function (base_size = 12, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="bold"),
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "lightgreen", colour = "grey50", size = 0.2))
}
