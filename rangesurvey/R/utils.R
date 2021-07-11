
#' @title load_config
#' @param config path to config
#' @return
#' @export

load_config <- function(config = "config.yaml") {
  config <- yaml::read_yaml(config)

  return(config)
}

#' @title getOrdinalNumber
#'
#' @param num a number (day in month)
#'
#' @return character vector of day in string format with ordinal suffix
#' @export

getOrdinalNumber <- function(num) {
  result <- ""
  if (!(num %% 100 %in% c(11, 12, 13))) {
    result <- switch(as.character(num %% 10),
      "1" = {
        paste0(num, "st")
      },
      "2" = {
        paste0(num, "nd")
      },
      "3" = {
        paste0(num, "rd")
      },
      paste0(num, "th")
    )
  } else {
    result <- paste0(num, "th")
  }
  return(result)
}


#' @title  scale_numerics
#'
#' @param data respondent character table
#'
#' @return respondent character table with years recording and age mean center scaled
#' @export
#'
scale_numerics <- function(data) {
  data <- data %>%
    dplyr::mutate(
      scaled_age = as.numeric(scale(.data$age)),
      scaled_years_recording = as.numeric(scale(.data$years_recording))
    )
  return(data)
}

#' recolour_species_pics
#'
#' @param pic jpeg object in r (array)
#' @param colour colour in rgb format (i.e. vector length 3)
#'
#' @return
#' @export
#'
recolour_species_pics <- function(pic, colour) {
  pic[, , 1] <- colour[1, ] / 255
  pic[, , 2] <- colour[2, ] / 255
  pic[, , 3] <- colour[3, ] / 255
  return(pic)
}

#' load_theme
#' @param custom ggplot2 theme
#' @return theme for ggplots
#' @export
load_theme <- function(custom) {
  if (missing(custom)) {
    theme <- ggplot2::theme_classic() +
      ggplot2::theme()
    return(theme)
  }
  if (!"theme" %in% class(custom)) {
    stop("custom must be a ggplot theme")
  }
  custom_theme <- custom
  return(custom_theme)
}
