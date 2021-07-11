#' clean_survey_data
#' @description clean_survey_data applies 5 functions consecutively: remove_pilot_responses, remove_incomplete_surveys, remove_meta_info,remove_response_times, remove_instructions.
#' @param data raw survey data
#' @seealso  remove_pilot_responses, remove_incomplete_survey, remove_meta_info, remove_response_times, remove_instructions
#' @return cleaned survey data
#' @export
clean_survey_data <- function(data) {
  clean_data <- data %>%
    remove_pilot_responses() %>%
    remove_incomplete_surveys() %>%
    remove_meta_info() %>%
    remove_response_times() %>%
    remove_instructions()
  return(clean_data)
}
