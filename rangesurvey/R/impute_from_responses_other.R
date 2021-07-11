#' impute_from_responses_other
#'
#' @param data
#' @param config
#' @param quiet
#' @param view_imputations
#'
#' @return
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
