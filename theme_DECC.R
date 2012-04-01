## Nice default formats:
## Graph border is 1pt

DECC.linewidth <- (1/72.27)*25.4 # 1pt linewidth
DECC.grey <- "grey60"            # Grey for axes
DECC.darkgrey <- "black"         # Grey for axis labels

DECC.colours <- list(cyan   = "#009EE3",
                     orange = "#C75200",
                     red    = "#AA0721",
                     green  = "#797900",
                     purple = "#742F89",
                     blue   = "#0066A1")

theme_DECC <- function(base_size = 10, base_family = "Helvetica") {
  structure(list(
                 axis.line =          theme_blank(),
                 axis.text.x =        theme_text(family = base_family, size = base_size, colour = DECC.grey, vjust = 1),
                 axis.text.y =        theme_text(family = base_family, size = base_size, colour = DECC.grey, hjust = 1),
                 axis.ticks =         theme_segment(size = DECC.linewidth, colour = DECC.grey),
                 axis.title.x =       theme_text(family = base_family, size = base_size, colour = DECC.darkgrey, vjust = 0),
                 axis.title.y =       theme_text(family = base_family, size = base_size, colour = DECC.darkgrey, angle = 90, hjust = 0.5, vjust = 0.3),
                 axis.ticks.length =  unit(1.5, "mm"),
                 axis.ticks.margin =  unit(1.5, "mm"),
                 
                 panel.background =   theme_blank(),
                 panel.border =       theme_rect(size = 2*DECC.linewidth, colour = DECC.grey), # The linewidth appears to be half of what is asked for.
                 panel.grid.major =   theme_blank(),
                 panel.grid.minor =   theme_blank(),
                 panel.margin =       unit(0.25, "lines"), 
                 
                 strip.background =   theme_rect(fill = "grey80", colour = NA),
                 strip.text.x =       theme_text(family = base_family, size = base_size * 0.8),
                 strip.text.y =       theme_text(family = base_family, size = base_size * 0.8, angle = -90),
                 
                 plot.background =    theme_rect(colour = NA, fill = NA),
                 plot.title =         theme_text(family = base_family, size = base_size * 1.2),
                 plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
                 
                 legend.background =  theme_rect(colour="white"),
                 legend.margin =      unit(0.2, "cm"),
                 legend.key =         theme_rect(fill = "grey95", colour = "white"),
                 legend.key.size =    unit(1.2, "lines"),
                 legend.key.height =  NULL,
                 legend.key.width =   NULL,
                 legend.text =        theme_text(family = base_family, size = base_size * 0.8),
                 legend.text.align =  NULL,
                 legend.title =       theme_text(family = base_family, size = base_size * 0.8, face = "bold", hjust = 0),
                 legend.title.align = NULL,
                 legend.position =    "right",
                 legend.direction =   NULL,
                 legend.justification = "center",
                 legend.box =         NULL   
                 ), class = "options")
}



