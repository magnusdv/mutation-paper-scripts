library(tidyverse)
library(ggh4x)
source("https://gist.githubusercontent.com/r2evans/6057f7995c117bb787495dc14a228d5d/raw/coord_cartesian_panels.R")


# Plot densities of logLRs
plotDensities = function(logLRs, cut = 0, adj = 1.5, xlim = NULL, ylim = NULL, 
                         file = NULL) {
  
  plotdat = calcD(logLRs)
  
  # Trim tails
  if(cut > 0)
    plotdat = plotdat |>
      group_by(Exclusions, Model, Transform) |>
      filter(abs(D - mean(D)) <= cut * sd(D)) |>
      ungroup()
  
  
  g = ggplot(plotdat, aes(x = D, fill = Transform)) +
    theme_bw(base_size = 12) +
    geom_density(alpha = 0.5, adjust = adj, linewidth = 0.2) +
    scale_x_continuous(expand = c(0.02, 0.02), n.breaks = 4) +
    scale_y_continuous(expand = c(0, 0.02)) +
    #coord_cartesian(xlim = xlim, ylim = ylim) +
    scale_fill_manual(values = c(`MH*` = "yellow", `BA*` = 2, PM = 3, PR = 4, DA = 5)) +
    facet_grid2(Model ~ Exclusions, independent = "y", scales = "free") +
    labs(x = expression(log[2]~LRR[M*","*R]), y = NULL) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(axis.text.x  = element_text(size = 8),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  
  g
}


# Alternative version of plotDensities that uses ggh4x for facetting
plotDensities2 = function(logLRs, cut = 0, adj = 1.5, file = NULL, panel_limits = NULL) {
  
  plotdat = calcD(logLRs)
  
  # Trim tails
  if(cut > 0) {
    plotdat = plotdat |>
      group_by(Exclusions, Model, Transform) |>
      filter(abs(D - mean(D)) <= cut * sd(D)) |>
      ungroup()
  }
  
  g = ggplot(plotdat, aes(x = D, fill = Transform)) +
    theme_bw(base_size = 12) +
    geom_density(alpha = 0.5, adjust = adj, linewidth = 0.2) +
    scale_x_continuous(expand = c(0.02, 0.02), n.breaks = 4) +
    scale_y_continuous(expand = c(0, 0.02)) +
    #coord_cartesian(xlim = xlim, ylim = ylim) +
    scale_fill_manual(values = c(`MH*` = "yellow", `BA*` = 2, PM = 3, PR = 4, DA = 5)) +
    facet_grid(Model ~ Exclusions, scales = "free") +
    labs(x = expression(log[2]~LRR[M*","*R]), y = NULL) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(axis.text.x  = element_text(size = 8),
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  
  if(!is.null(panel_limits)) {
    g = g + coord_cartesian_panels(panel_limits = panel_limits)
  }
  
  g
}
