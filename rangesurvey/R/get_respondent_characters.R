#' get_respondent_characters
#'
#' @param data whole survey data
#'
#' @return respondent character table
#' @export
#'
get_respondent_characters <- function(data) {
  data %>%
    dplyr::select(
      "id",
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
    ) %>%
    dplyr::mutate(id = as.factor(.data$id))
}
