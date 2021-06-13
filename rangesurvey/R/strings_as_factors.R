#' strings_as_factors
#'
#' @param data 
#'
#' @return
#' @export
#'
strings_as_factors <- function(data) {
  data <- data %>%
    dplyr::mutate(
      dplyr::across(all_of(c(
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
        "NoRole")),
        ~ factor(.x,levels = c("No","Yes"))
      )
    )
  
  data <- data %>% 
    dplyr::mutate(climate_treatment = factor(ClimateEq, levels = c(0, 1), labels = c("Control", "Climate Change Prompt"))) %>% 
    dplyr::select(-ClimateEq)
  
  return(data)
  
}