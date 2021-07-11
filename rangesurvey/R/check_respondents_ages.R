#' check_respondents_ages
#'
#' @param data respondent character table
#'
#' @return
#' @export

check_respondents_ages <- function(data) {
  data <- data %>%
    dplyr::mutate(age = dplyr::case_when(
      .data$GDemographics01 > 120 ~ 2019 - .data$GDemographics01,
      .data$GDemographics01 < 120 ~ .data$GDemographics01,
      T ~ NA_real_
    )) %>%
    dplyr::select(-.data$GDemographics01)

  return(data)
}
