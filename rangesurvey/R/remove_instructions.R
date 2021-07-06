#' remove_instructions
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with the question intstructions removed
#' @export
#'
remove_instructions <- function(data) {
  data <- dplyr::select(
    data,
    -dplyr::contains("C1"),
    -dplyr::contains("C0"),
    -"Gcolinfo",
    -"GColonistClimateQues",
    -"SurveyInstructions1",
    -"GDemographics05[SQ001]"
  )
  return(data)
}