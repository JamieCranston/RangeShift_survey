#' check_and_score_awareness
#'
#' @param data respondent data
#' @param config config file needed for links to validation sheets
#' @return respondent data with a column added for respondent's awareness of
#'   range-shifting species
#' @export
check_and_score_awareness <- function(data, config) {
  checked_and_scored_awareness <- check_awareness(data, config) %>%
    dplyr::mutate(awareness_numeric = score_awareness(
      data = .,
      numeric_out = TRUE
    ) %>%
      dplyr::pull(.data$awareness_score))
  return(checked_and_scored_awareness)
}
