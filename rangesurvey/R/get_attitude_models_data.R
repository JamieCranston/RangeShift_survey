#' get_attitude_models_data
#'
#' @param data survey_responses_to_model
#'
#' @return management_responses_to_model
#' @export
get_attitude_models_data <- function(data,
                                     model = c(
                                       "species",
                                       "management"
                                     )) {
  model <- match.arg(model)

  if (model == "management") {
    management_responses_to_model <- data %>%
      likert_7_to_3() %>%
      dplyr::select(
        id,
        species,
        attitude_to_species,
        Remove,
        Mitigate,
        Accept,
        Adapt,
        Support,
        seen,
        match
      )
    return(management_responses_to_model)
  } else if (model == "species") {
    species_responses_to_model <- data %>%
      likert_7_to_3() %>%
      dplyr::select(
        id,
        species,
        attitude_to_species,
        seen,
        match
      )
    return(species_responses_to_model)
  }
}
