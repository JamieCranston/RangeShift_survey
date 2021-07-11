#' format_respondents_postcode
#'
#' @param data dataframe (respondent character table)
#'
#' @return dataframe (respondent character table with postcodes renamed and formatted.
#' @export
#'
format_respondents_postcode <- function(data) {
  data <- data %>%
    dplyr::rename(postcode = .data$GDemographics04) %>%
    dplyr::mutate(postcode = factor(toupper(.data$postcode)))

  return(data)
}
