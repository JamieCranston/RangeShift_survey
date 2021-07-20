#' plot_fig_2b
#'
#' @param model species attitudes model
#' @param config config
#' @return figure 2(b) ggplot
#' @export
#'
plot_fig_2b <- function(model,
                        config) {
  
  species_rasters <- create_species_rasters(config)

  data <- predict_species_marginal_effects(data = model$data,
                                          model = model)
  species_effect_plot <-
    ggplot(
      data,
      aes(
        x = stats::reorder(.data$LName, .data$P.Y...Positive.),
        y = .data$P.Y...Positive.,
        fill = .data$Group,
        Colour = .data$Group
      )
    ) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = .data$LName, hjust = 0),
      fontface = 3,
      nudge_y = 0.01
    ) +
    annotation_raster(
      species_rasters$wasp,
      xmin = 0.6,
      xmax = 1.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$bee,
      xmin = 6.6,
      xmax = 7.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$dragonfly,
      xmin = 9.6,
      xmax = 10.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$zygoptera,
      xmin = 7.6,
      xmax = 8.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$shieldbug,
      xmin = 2.6,
      xmax = 3.5,
      ymin = 0.92,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$shieldbug,
      xmin = 1.6,
      xmax = 2.5,
      ymin = 0.92,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$moth,
      xmin = 4.6,
      xmax = 5.5,
      ymin = 0.935,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$moth,
      xmin = 3.6,
      xmax = 4.5,
      ymin = 0.935,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$LittleEgret,
      xmin = 8.6,
      xmax = 9.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$LittleEgret,
      xmin = 5.6,
      xmax = 6.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$Spoonbill,
      xmin = 15.5,
      xmax = 16.4,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$LittleHeron,
      xmin = 14.65,
      xmax = 15.35,
      ymin = 0.93,
      ymax = 0.96
    ) +
    annotation_raster(
      species_rasters$FakeStilt,
      xmin = 13.6,
      xmax = 14.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$LittleEgret,
      xmin = 12.6,
      xmax = 13.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$LittleEgret,
      xmin = 11.6,
      xmax = 12.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      species_rasters$FakeIbis,
      xmin = 10.6,
      xmax = 11.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    coord_flip(ylim = c(0.0, 1), expand = F) +
    ylab("Probability of positive attitude") +
    labs(tag = "(b)") +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      # plot.tag.position = c(0.04,0.98),
      # plot.tag.position = c(0, 0.98),
      plot.tag = element_text(colour = "black", face = "bold"),
      axis.text.x = element_text(hjust = 0),
      # panel.background = element_rect(fill = "white"),
      # panel.grid = element_blank(),
      # panel.grid.major.x =  element_line(colour = "light grey"),
      legend.position = "bottom",
      legend.box.spacing = unit(0, "mm"),
      legend.title = element_blank(),
      legend.margin = margin(0.1, 0, 0.1, 0, "cm"),
      legend.spacing = unit(0, "mm"),
      legend.key.width = unit(4.8, "mm"),
      legend.key.height = unit(4.8, "mm"),
      plot.margin = unit(c(1, 1, 1, 1), "mm"),
      panel.grid.major.y = element_blank()
    ) +
    scale_fill_manual(
      values = c(
        "Bird" = "#332288",
        "Bee / Wasp" = "#44AA99",
        "Moth" = "#117733",
        "Dragonfly" = "#b300b3",
        "Shieldbug" = "#ceb53a"
      )
    )

  return(species_effect_plot)
}
