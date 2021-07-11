#' plot_supp_fig_5
#'
#' @param data cleaned respondent data table
#' @param config config file for paths to spatial polygons
#'
#' @return Map of respondent locations
#' @export
#'

plot_supp_fig_5 <- function(data, config) {
  dir <- config$geography_dirs$postcode_areas$dir
  file <- config$geography_dirs$postcode_areas$file

  postcode_areas_shapefile <- rgdal::readOGR(
    dsn = dir,
    layer = file
  )

  results <- data %>%
    dplyr::mutate(postcode_area = sapply(.data$postcode,
      FUN = function(X) {
        first_two_chars <- substr(X, 1, 2)
        pc_area <- gsub(x = first_two_chars, pattern = "[[:digit:]]+", replacement = "")
        return(pc_area)
      }
    )) %>%
    dplyr::group_by(.data$postcode_area) %>%
    dplyr::count() %>%
    stats::na.omit()

  postcode_areas_data <- sp::merge(postcode_areas_shapefile,
    results,
    by.x = "pc_area",
    by.y = "postcode_area"
  )

  my.palette <- c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")

  # I can add more tones to this palette :
  coul <- grDevices::colorRampPalette(my.palette)(max(results$n))

  l2 <- list(
    "SpatialPolygonsRescale",
    sp::layout.north.arrow(),
    offset = c(-80000, 7820000),
    scale = 350000
  )
  l3 <- list(
    "SpatialPolygonsRescale",
    sp::layout.scale.bar(),
    offset = c(-290000, 6450000),
    scale = 400000,
    fill = c("transparent", "black")
  )
  l4 <- list(
    "sp.text",
    c(-290000, 6500000),
    "0"
  )
  l5 <- list(
    "sp.text",
    c(110000, 6500000),
    "400km"
  )


  scale.parameter <- 0.85 # scaling paramter. less than 1 is zooming in, more than 1 zooming out.
  xshift <- -34000 # Shift to right in map units.
  yshift <- -175000 # Shift to left in map units.

  postcode_areas_data@bbox[1, 2] <- 350000
  original.bbox <- postcode_areas_data@bbox # Pass bbox of your Spatial* Object.

  # Just copy-paste the following
  edges <- original.bbox

  edges[1, ] <-
    (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1, ]) + xshift
  edges[2, ] <-
    (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2, ]) + yshift

  survey_responses_map <- sp::spplot(
    obj = postcode_areas_data,
    zcol = "n",
    # main = paste0(getOrdinalNumber(mday(as.Date(max(rawresults$day)-1, origin = "2019-01-01")))," ", months(as.Date((max(rawresults$day))-1, origin = "2019-01-01")), " Survey Responses"),
    # col = "transparent",
    col.regions = coul,
    # at = labelat,
    # labels = labeltext,
    # cuts = 12,
    at = seq(from = 0.5, to = 17.5, by = 1),
    colorkey = list(
      at = seq(from = 0.5, to = 17.5, by = 1),
      labels = list(
        at = seq(
          from = 1, to = 17, by = 1
        ),
        labels = as.character(c(1:17))
      ),
      col = (coul)
    ),
    par.settings =
      list(panel.background = list(col = "lightblue1")),
    sp.layout = list(
      l2,
      l3,
      l4,
      l5,
      list(
        "sp.polygons",
        postcode_areas_data,
        first = TRUE,
        fill = "lightgoldenrod1"
      )
    ),
    xlim = edges[1, ],
    ylim = edges[2, ]
  )

  return(survey_responses_map)
}
