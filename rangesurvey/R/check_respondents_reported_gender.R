#' check_respondents_reported_gender
#'
#' @param data respondent character table
#'
#' @return dataframe with imputed gender added (not added in practice as no responses required imputing)
#' @export
#'
check_respondents_reported_gender <- function(data) {
  if (all(is.na(.data$`GDemographics02[other]`)) == TRUE) {
    data <- data %>%
      dplyr::mutate(gender = as.factor(.data$GDemographics02)) %>%
      dplyr::select(-dplyr::all_of(c(
        "GDemographics02",
        "GDemographics02[other]"
      ))) %>%
      dplyr::filter(.data$gender != "Prefer not to Say") %>%
      dplyr::mutate(gender = forcats::fct_drop(f = .data$gender, only = "Prefer not to Say"))
    print("No Respondents answered other when asked their gender")
    return(data)
  }

  return(data)
}

