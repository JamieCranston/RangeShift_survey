#' check_n_scale_age_n_years_recording
#'
#' @param data
#'
#' @return
#' @export
check_n_scale_age_n_years_recording <- function(data) {
  scaled_data <- data %>%
    check_respondents_ages() %>%
    scale_numerics()
  return(scaled_data)
}
