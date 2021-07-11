#' check_wildlife_sector
#'
#' @param data dataframe (respondent character table)
#'
#' @return dataframe (respondent character with wildlife sector column renamed
#' @export
#'
rename_wildlife_sector <- function(data) {
  data <- data %>%
    dplyr::rename(wildlife_sector = .data$`GDemographics05[SQ002]`)
  return(data)
}
