#' impute_from_responses_other
#'
#' @param data respondent data
#' @param config config file needed for links to validation sheets
#' @param quiet #TODO
#' @param view_imputations #TODO
#'
#' @return respondent data with a columns added for questions where imputations
#'   to data from open text fields were required.
#' @export
impute_from_responses_other <- function(data, config,
                                        quiet = TRUE,
                                        view_imputations = FALSE) {
  data_with_imputations <- data %>%
    check_respondents_reported_gender() %>%
    check_education(., config) %>%
    check_recorder_role(., config) %>%
    check_groups_recorded(., config) %>%
    check_and_score_awareness(., config)

  if (view_imputations == FALSE) {
    return(data_with_imputations)
  } else {
    return()
  }
}
