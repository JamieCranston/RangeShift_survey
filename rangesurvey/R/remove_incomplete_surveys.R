#' remove_incomplete_surveys
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with incomplete responses removed
#' @export
#'
remove_incomplete_surveys <- function(data) {

  # remove those who didn't reach the last page of compulsory questions on the survey
  # remove those who left age and gender blank as likely skippers
  data <- dplyr::filter(
    data,
    .data$lastpage >= 21,
    !is.na(.data$GDemographics01) & !is.na(.data$GDemographics02)
  )
}
