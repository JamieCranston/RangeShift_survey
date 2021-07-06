#' rename_species_of_interest
#'
#' @param data  respondent character table
#'
#' @return dataframe (respondent character table with species of interest columns renamed)
#' @export
rename_species_of_interest <- function(data) {
  data <- dplyr::rename(data,
                        "RiskSpecies" = .data$`Dual[SQ001_SQ001]`,
                        "ConserveSpecies" = .data$`Dual[SQ003_SQ001]`
  )
  return(data)
}