#' check_education
#'
#' @param data respondent character table
#' @param config config file (for paths to validated education csv)
#'
#' @return dataframe with imputed education added to respondent character table
#' @export
#'
check_education <- function(data, config) {
  data <- data %>%
    dplyr::mutate(education = dplyr::case_when(
      .data$`GDemographics03[SQ001]` == "Yes" ~ "Postgrad",
      .data$`GDemographics03[SQ002]` == "Yes" ~ "Undergrad",
      .data$`GDemographics03[SQ003]` == "Yes" ~ "A-levels",
      .data$`GDemographics03[SQ004]` == "Yes" ~ "GCSEs",
      T ~ NA_character_
    )) %>%
    dplyr::select(
      -.data$`GDemographics03[SQ001]`,
      -.data$`GDemographics03[SQ002]`,
      -.data$`GDemographics03[SQ003]`,
      -.data$`GDemographics03[SQ004]`
    )

  #education_val <- readr::read_csv(config$validation_dirs$education_val, col_types = readr::cols(id = "c"))

  print("please see our imputations of respondent education from the Education (other) responses")
  print(education_val)

  other_validated <- data %>%
    dplyr::left_join(x = ., y = education_val, by = "id") %>%
    dplyr::select(-dplyr::contains("GDemographics03"))


  other_validated <- other_validated %>%
    dplyr::mutate(
      education = ordered(.data$education, levels = c("GCSEs", "A-levels", "Undergrad", "Postgrad")),
      imputed_education = ordered(.data$imputed_education, levels = c("GCSEs", "A-levels", "Undergrad", "Postgrad"))
    )

  return(other_validated)
}
