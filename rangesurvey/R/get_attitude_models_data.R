#' get_attitude_models_data
#'
#' @param data survey_responses_to_model
#' @param model either species or management depending on which data is needed
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
        .data$id,
        .data$species,
        .data$attitude_to_species,
        .data$Remove,
        .data$Mitigate,
        .data$Accept,
        .data$Adapt,
        .data$Support,
        .data$seen,
        .data$match
      )
    return(management_responses_to_model)
  } else if (model == "species") {
    species_responses_to_model <- data %>%
      likert_7_to_3() %>%
      dplyr::select(
        .data$id,
        .data$species,
        .data$attitude_to_species,
        .data$seen,
        .data$match
      )
    return(species_responses_to_model)
  }
}
