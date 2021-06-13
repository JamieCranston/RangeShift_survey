plot_fig_2b <- function(data) {
  
  
  species_rasters <- create_species_rasters(config)
  
  species_effect_plot <-
    ggplot(Species,
           aes(
             x = reorder(LName, P.Y...Positive.),
             y = P.Y...Positive.,
             fill = Group,
             Colour = Group
           )) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = LName, hjust = 0),
              fontface = 3,
              nudge_y = 0.01) +
    annotation_raster(
      Wasp,
      xmin = 0.6,
      xmax = 1.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Bee,
      xmin = 6.6,
      xmax = 7.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Dragonfly,
      xmin = 9.6,
      xmax = 10.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Zygoptera,
      xmin = 7.6,
      xmax = 8.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Shieldbug,
      xmin = 2.6,
      xmax = 3.5,
      ymin = 0.92,
      ymax = 0.975
    ) +
    annotation_raster(
      Shieldbug,
      xmin = 1.6,
      xmax = 2.5,
      ymin = 0.92,
      ymax = 0.975
    ) +
    annotation_raster(
      Moth,
      xmin = 4.6,
      xmax = 5.5,
      ymin = 0.935,
      ymax = 0.975
    ) +
    annotation_raster(
      Moth,
      xmin = 3.6,
      xmax = 4.5,
      ymin = 0.935,
      ymax = 0.975
    ) +
    annotation_raster(
      Egarz,
      xmin = 8.6,
      xmax = 9.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Egarz,
      xmin = 5.6,
      xmax = 6.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Spoonbill,
      xmin = 15.5,
      xmax = 16.4,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Imin,
      xmin = 14.65,
      xmax = 15.35,
      ymin = 0.93,
      ymax = 0.96
    ) +
    annotation_raster(
      Stilt,
      xmin = 13.6,
      xmax = 14.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Egarz,
      xmin = 12.6,
      xmax = 13.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Egarz,
      xmin = 11.6,
      xmax = 12.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    annotation_raster(
      Gibis,
      xmin = 10.6,
      xmax = 11.5,
      ymin = 0.925,
      ymax = 0.975
    ) +
    coord_flip(ylim = c(0.0, 1), expand = F)  +
    ylab("Probability of positive attitude") +
    labs(tag = '(b)') +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      #plot.tag.position = c(0.04,0.98),
      #plot.tag.position = c(0, 0.98),
      plot.tag = element_text(colour = "black", face = "bold"),
      axis.text.x = element_text(hjust = 0),
      #panel.background = element_rect(fill = "white"),
      #panel.grid = element_blank(),
      #panel.grid.major.x =  element_line(colour = "light grey"),
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

#' Title
#'
#' @param config 
#'
#' @return
#' @export
#'
#' @examples
create_species_rasters <- function(config){

  #Chosen colours for figure 2 b
  hex_colours <-  c("#44AA99", "#332288", "#b300b3","#117733","#ceb53a")
  rgb_colours <- lapply(hex_colours , col2rgb)
  
  names(rgb_colours) <- c("hymenoptera_#44AA99",
                          "bird_colour_#332288",
                          "odonata_colour_#b300b3",
                          "lepidoptera_colour_#117733",
                          "hemiptera_colour_#ceb53a")
    
  images <- config$species_pics
  
  recoloured_images_hymenoptera <- lapply(images$hymenoptera, FUN = function(X){
    recolour_species_pics(pic = png::readPNG(X),
                          colour = rgb_colours$`hymenoptera_#44AA99`)
    })
  names(recoloured_images_hymenoptera) <- c("wasp", "bee")
  
  recoloured_images_birds <- lapply(images$birds, FUN = function(X){
    recolour_species_pics(pic = png::readPNG(X),
                          colour = rgb_colours$`bird_colour_#332288`)
  })
  names(recoloured_images_birds) <- c("LittleEgret",
                                      "LittleHeron",
                                      "PurpleHeron",
                                      "FakeStilt",
                                      "FakeIbis",
                                      "Spoonbill",
                                      "CattleEgret")
  
  recoloured_images_odonata <- lapply(images$odonata, FUN = function(X){
    recolour_species_pics(pic = png::readPNG(X),
                          colour = rgb_colours$`odonata_colour_#b300b3`)
  })
  names(recoloured_images_odonata) <- c("dragonfly","zygoptera")
  
  recoloured_images_lepidoptera<- lapply(images$lepidoptera, FUN = function(X){
    recolour_species_pics(pic = png::readPNG(X),
                          colour = rgb_colours$`lepidoptera_colour_#117733`)
  })
  recoloured_images_hemiptera <- lapply(images$hemiptera, FUN = function(X){
    recolour_species_pics(pic = png::readPNG(X),
                          colour = rgb_colours$`hemiptera_colour_#ceb53a`)
  })
  
  species_rasters <- c(recoloured_images_hymenoptera,
                       recoloured_images_birds,
                       recoloured_images_odonata,
                       "moth" = recoloured_images_lepidoptera,
                       "shieldbug" = recoloured_images_hemiptera)
  
  return(species_rasters)
}

predict_marginal_seen_and_match <- function(data = species_responses_to_model,
                                            model = species_attitude_model){
  
newdata <- expand.grid(Species = unique(ABC$Species),
                       id = "Mean",
                       Seen = NA,
                       WildlifeSector = NA,
                       Involvement = NA,
                       ClimateEq = NA,
                       Education = NA,
                       Gender = NA,
                       ScaleAge = 0,
                       ScaleYR = 0,
                       Match = NA)

#Generate predictions for the probability of each response at each level of the random factor.
Species <- predict(model,
                   newdata = newdata,
                   re_formula = NULL,
                   probs = c(0.025, 0.975),
                   allow_new_levels = T)
#Add Species names to rows
row.names(Species) <- unique(ABC$Species)

#Order by Probability of response being positive and coerce to dataframe
Species <- data.frame(Species[order(Species[,"P(Y = Positive)"], decreasing = T),])

#Add Latin name to dataframe and remove rownames.
Species$LName <- gsub(pattern = '^(.{1})(.*)$', replacement = '\\1. \\2', x = row.names(Species))
row.names(Species) <- NULL

#Add in information on taxonomic group (if time convert to lookup table for safety)
Species$Group <- c(rep("Bird",6),
                   "Dragonfly",
                   "Bird",
                   "Dragonfly",
                   "Bee / Wasp",
                   "Bird",
                   rep("Moth",2),
                   rep("Shieldbug",2),
                   "Bee / Wasp")

}

