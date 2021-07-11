#' remove_response_times
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with the question response times removed
#' @export
#'
remove_response_times <- function(data) {
  data <- dplyr::select(
    data,
    -dplyr::contains("Time")
  )
  return(data)
}
