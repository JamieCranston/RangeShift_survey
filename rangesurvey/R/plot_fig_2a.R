#' plot_fig_2a
#'
#' @param data brms marginal effects objects
#'
#' @return figure 2(a) - a ggplot of the marginal effects of familiarity
#' @import ggplot2
#' @export
#'
plot_fig_2a <- function(data) {
  effects_of_familiarity_plots <- ggplot(data) +
    geom_pointrange(aes(
      x = .data$cats__,
      ymin = .data$lower__,
      ymax = .data$upper__,
      y = .data$estimate__,
      colour = .data$cats__
    )) +
    geom_errorbar(aes(
      x = .data$cats__,
      ymin = .data$lower__,
      ymax = .data$upper__,
      y = .data$estimate__,
      colour = .data$cats__
    )) +
    facet_grid(
      ~ Match + Seen,
      labeller = function(labels) {
        list(
          c(
            "Unseen species\nin unrecorded\ntaxon group (n=346)",
            "Unseen species\nin recorded\ntaxon group (n=389)",
            "Seen species \nin unrecorded\ntaxon group (n=93)",
            "Seen species\nin recorded\ntaxon group (n=338)"
          )
        )
      }
    ) +
    theme(
      axis.text = element_text(size = 8),
      plot.tag.position = c(0.04, 0.98),
      plot.tag = element_text(colour = "black", face = "bold"),
      axis.title.x = element_text(vjust = -4),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_text(size = 6.25, face = "bold"),
      axis.title.y = element_text(size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.margin = margin(0.1, 0, 0.1, 0, "cm"),
      legend.title = element_blank(),
      legend.spacing = unit(0, "mm"),
      legend.key.width = unit(5.0, "mm"),
      legend.key.height = unit(5.0, "mm"),
      panel.spacing = unit(1, "mm"),
      panel.grid.major.x = element_blank(),
      plot.margin = unit(c(1, 1, 1, 0), "mm")
    ) +
    ylab("Probability of attitude") +
    xlab("Attitude") +
    labs(colour = "Attitude", tag = "(a)") +
    scale_colour_manual(values = c("#C24641", "#736F6E", "#6495ED"))

  return(effects_of_familiarity_plots)
}
