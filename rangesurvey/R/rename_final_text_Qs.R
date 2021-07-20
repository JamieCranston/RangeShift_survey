#' rename_final_text_Qs
#'
#' @param data respondent_table_clean
#'
#' @return respondent data with a two final open text question columns renamed.
#' @export
#'
rename_final_text_Qs <- function(data) {
  data <- data %>%
    dplyr::rename(
      "implications_of_climate_change" = "FinalClimateCheck",
      "final_open_response_q" = "GFreeFromFinal"
    )

  return(data)
}
