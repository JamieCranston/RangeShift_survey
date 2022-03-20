#' check_groups_recorded
#'
#' @param data respondent character table
#' @param config config file (for paths to validated groups recorded csv)
#'
#' @return respondent data with a column added for the groups the respondent's reported recording as listed in text field of the other option on the groups recorded question.
#'   range-shifting species
#' @export
check_groups_recorded <- function(data, config) {
  #species_group_val <- readr::read_csv(config$validation_dirs$species_group_val, col_types = readr::cols(id = "c"))

  print("please see our imputation for respondent answers about other groups they recorded")
  print(species_group_val)

  other_validated <- data %>%
    dplyr::left_join(
      x = .,
      y = species_group_val %>%
        dplyr::select(-.data$OtherGroups),
      by = "id"
    ) %>%
    dplyr::select(-.data$OtherGroups) %>%
    dplyr::mutate(.data$imputed_group)

  return(other_validated)
}
