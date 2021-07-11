#' get_species_responses
#'
#' @param data whole survey data
#'
#' @return species responses data
#' @export
#'
get_species_responses <- function(data) {
  data <- data %>%
    dplyr::select(
      -c(
        dplyr::contains("GRecorderInfo"),
        dplyr::contains("Gawareness"),
        dplyr::contains("ertEq"),
        "ClimateEq",
        dplyr::contains("VoN"),
        dplyr::contains("associations"),
        dplyr::contains("GDemographics"),
        dplyr::contains("Array"),
        dplyr::contains("Dual"),
        "FinalClimateCheck",
        "GFreeFromFinal"
      )
    ) %>%
    dplyr::mutate(id = as.factor(.data$id))


  return(data)
}
