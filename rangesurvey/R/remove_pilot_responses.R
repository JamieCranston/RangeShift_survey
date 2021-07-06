#' remove_pilot_responses
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with responses to the pilot removed
#' @export
#'
remove_pilot_responses <- function(data) {
  data <- dplyr::filter(data, .data$id > 34)
}