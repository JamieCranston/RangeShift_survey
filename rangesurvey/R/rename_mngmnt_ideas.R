#' rename_mngmnt_ideas
#'
#' @param data  dataframe (respondent character table)
#'
#' @return dataframe (respondent character table with management ideas renamed)
#' @export
#'
rename_mngmnt_ideas <- function(data) {
  data <- dplyr::rename(data,
    "Monitoring_Do" = .data$`Array[SQ002_SQ001]`,
    "Monitoring_DontDo" = .data$`Array[SQ002_SQ002]`,
    "Protecting_Do" = .data$`Array[SQ003_SQ001]`,
    "Protecting_DontDo" = .data$`Array[SQ003_SQ002]`,
    "Minimising_Do" = .data$`Array[SQ004_SQ001]`,
    "Minimising_DontDo" = .data$`Array[SQ004_SQ002]`
  )
  return(data)
}
