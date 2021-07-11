#' check_and_score_awareness
#'
#' @param data
#' @param config
#' @return
#' @export
check_and_score_awareness <- function(data, config) {
  checked_and_scored_awareness <- check_awareness(data, config) %>%
    dplyr::mutate(awareness_numeric = score_awareness(
      data = .,
      numeric_out = TRUE
    ) %>%
      dplyr::pull(awareness_score))
  return(checked_and_scored_awareness)
}
