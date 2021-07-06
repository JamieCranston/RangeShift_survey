#' strings_as_factors
#'
#' @param data respondent character table
#'
#' @return dataframe of responses with columns converted from character to factor class
#' @export
#'
strings_as_factors <- function(data) {
  data <- data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          "Birds",
          "Mammals",
          "Hymenoptera",
          "Coleoptera",
          "Lepidoptera",
          "Diptera",
          "Odonata",
          "Hemiptera",
          "NoSpGroups",
          "Informal",
          "Recorder",
          "Verifier",
          "Organiser",
          "NoRole"
        )),
        ~ factor(.x, levels = c("No", "Yes"))
      )
    )

  data <- data %>%
    dplyr::mutate(climate_treatment = factor(.data$ClimateEq,
                                             levels = c(0, 1),
                                             labels = c("Control",
                                                        "Climate Change Prompt"))) %>%
    dplyr::select(-.data$ClimateEq)

  return(data)
}
