#' create_species_rasters
#'
#' @param config config file (for paths to raster images)
#'
#' @return list of species rasters
#' @export
#'
create_species_rasters <- function(config) {
  
  # Chosen colours for figure 2 b
  hex_colours <- c("#44AA99",
                   "#332288",
                   "#b300b3",
                   "#117733",
                   "#ceb53a")
  
  rgb_colours <- lapply(hex_colours,
                        grDevices::col2rgb)
  
  names(rgb_colours) <- c(
    "hymenoptera_#44AA99",
    "bird_colour_#332288",
    "odonata_colour_#b300b3",
    "lepidoptera_colour_#117733",
    "hemiptera_colour_#ceb53a"
  )
  
  images <- config$species_pics
  
  recoloured_images_hymenoptera <- lapply(images$hymenoptera, FUN = function(X) {
    recolour_species_pics(
      pic = png::readPNG(X),
      colour = rgb_colours$`hymenoptera_#44AA99`
    )
  })
  names(recoloured_images_hymenoptera) <- c("wasp", "bee")
  
  recoloured_images_birds <- lapply(images$birds, FUN = function(X) {
    recolour_species_pics(
      pic = png::readPNG(X),
      colour = rgb_colours$`bird_colour_#332288`
    )
  })
  names(recoloured_images_birds) <- c(
    "LittleEgret",
    "LittleHeron",
    "PurpleHeron",
    "FakeStilt",
    "FakeIbis",
    "Spoonbill",
    "CattleEgret"
  )
  
  recoloured_images_odonata <- lapply(images$odonata, FUN = function(X) {
    recolour_species_pics(
      pic = png::readPNG(X),
      colour = rgb_colours$`odonata_colour_#b300b3`
    )
  })
  names(recoloured_images_odonata) <- c("dragonfly", "zygoptera")
  
  recoloured_images_lepidoptera <- lapply(images$lepidoptera, FUN = function(X) {
    recolour_species_pics(
      pic = png::readPNG(X),
      colour = rgb_colours$`lepidoptera_colour_#117733`
    )
  })
  recoloured_images_hemiptera <- lapply(images$hemiptera, FUN = function(X) {
    recolour_species_pics(
      pic = png::readPNG(X),
      colour = rgb_colours$`hemiptera_colour_#ceb53a`
    )
  })
  
  species_rasters <- c(recoloured_images_hymenoptera,
                       recoloured_images_birds,
                       recoloured_images_odonata,
                       "moth" = recoloured_images_lepidoptera,
                       "shieldbug" = recoloured_images_hemiptera
  )
  
  return(species_rasters)
}