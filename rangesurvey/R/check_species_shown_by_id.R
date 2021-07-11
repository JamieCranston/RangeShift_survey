#' check_species_shown_by_id
#'
#' @param speciesdata  cleaned species responses data
#' @param respondentdata  cleaned  respondent data
#'
#' @return
#' @export
#'
check_species_shown_by_id <- function(speciesdata,
                                      respondentdata) {
  species_shown <- respondentdata %>%
    dplyr::select(
      .data$id,
      dplyr::contains("verte")
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::contains("verte"),
      values_to = "species"
    ) %>%
    dplyr::select(-.data$name) %>%
    dplyr::left_join(.data$., speciesdata)

  return(species_shown)
}
