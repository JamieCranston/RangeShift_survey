#' make_tidy_datasets
#'
#' @param data clean survey data
#' @param frame either respondents or species
#'
#' @return either the starting point for a tidy dataframe containing respondents traits, or species responses.
#' @export
make_tidy_datasets <- function(data,
                               frame = c(
                                 "respondents",
                                 "species"
                               )) {
  frame <- match.arg(frame)

  if (frame == "respondents") {
    respondent_character_table <- data %>%
      get_respondent_characters() %>%
      rename_respondent_frame() %>%
      strings_as_factors()
    return(respondent_character_table)
  } else if (frame == "species") {
    respondent_character_table <- data %>%
      get_respondent_characters() %>%
      rename_respondent_frame() %>%
      strings_as_factors()

    survey_species_responses <- data %>%
      get_species_responses() %>%
      clean_species_frame(., respondent_frame = respondent_character_table)
    return(survey_species_responses)
  }
}
