
#' clean_species_frame
#'
#' @param data species_frame
#'
#' @return species_frame_cleaned
#' @export
clean_species_frame <- function(data) {
  species_frame_cleaned <- data %>%
    pivot_species_long() %>%
    check_species_shown_by_id(
      speciesdata = .,
      respondentdata = respondent_table_clean
    ) %>%
    is_species_in_recorded_group(
      speciesdata = .,
      respondentdata = respondent_table_clean
    )
  return(species_frame_cleaned)
}
