
#' @title load_config
#'
#' @return
#' @export

load_config <- function(){
  
  config <- yaml::read_yaml("config.yaml")
  
  return(config)
}

#' @title getOrdinalNumber
#'
#' @param num 
#'
#' @return
#' @export

getOrdinalNumber <- function(num) {
  result <- ""
  if (!(num %% 100 %in% c(11, 12, 13))) {
    result <- switch(as.character(num %% 10), 
                     "1" = {paste0(num, "st")}, 
                     "2" = {paste0(num, "nd")},
                     "3" = {paste0(num, "rd")},
                     paste0(num,"th"))
  } else {
    result <- paste0(num, "th")
  }
  return(result)
}


#' @title  scale_numerics
#'
#' @param data 
#'
#' @return
#' @export
#'
scale_numerics <- function(data){
  
  data <- data %>% 
    dplyr::mutate(scaled_age = as.numeric(scale(age)),
                  scaled_years_recording = as.numeric(scale(years_recording)))
  return(data)
}

#' recolour_species_pics
#'
#' @param pic 
#' @param colour 
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
