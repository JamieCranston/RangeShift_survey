#' fill_imputed_species_group
#'
#' @param data dataframe with respondent ID and imputed group
#'
#' @return dataframe with imputed species group info merged into group recorded columns
#' @export
fill_imputed_species_group <- function(data) {
  impute_sep <- data %>%
    dplyr::select(.data$id,
                  .data$imputed_group) %>%
    tidyr::separate(.data$.,
                    .data$imputed_group,
                    sep = ";",
                    into = c(
                      "imputed_1",
                      "imputed_2"
                    )
    )
  
  impute_sep <- impute_sep %>%
    dplyr::filter(!dplyr::across(
      dplyr::all_of(dplyr::contains("imputed")),
      ~ is.na(.x)
    ))
  imputed_merged_in <- dplyr::left_join(
    data,
    impute_sep
  ) %>%
    dplyr::mutate(
      Plants = dplyr::case_when(
        imputed_1 == "Plants" |
          imputed_2 == "Plants" |
          imputed_1 == "All" |
          imputed_2 == "All" ~ "Yes",
        TRUE ~ "No"
      ),
      Other = dplyr::case_when(
        imputed_1 == "Other" |
          imputed_2 == "Other" |
          imputed_1 == "All" |
          imputed_2 == "All" ~ "Yes",
        T ~ "No"
      ),
      Odonata = dplyr::case_when(
        imputed_1 == "Odonata" |
          imputed_2 == "Odonata" |
          imputed_1 == "All" |
          imputed_2 == "All" |
          Odonata == "Yes" ~ "Yes",
        T ~ "No"
      ),
      Coleoptera = dplyr::case_when(
        imputed_1 == "Coleoptera" |
          imputed_2 == "Coleoptera" |
          imputed_1 == "All" |
          imputed_2 == "All" |
          Coleoptera == "Yes" ~ "Yes",
        T ~ "No"
      ),
      Lepidoptera = dplyr::case_when(
        imputed_1 == "Lepidoptera" |
          imputed_2 == "Lepidoptera" |
          imputed_1 == "All" |
          imputed_2 == "All" |
          Lepidoptera == "Yes" ~ "Yes",
        TRUE ~ "No"
      ),
      Hemiptera = dplyr::case_when(
        imputed_1 == "Hemiptera" |
          imputed_2 == "Hemiptera" |
          imputed_1 == "All" |
          imputed_2 == "All" |
          Hemiptera == "Yes" ~ "Yes",
        TRUE ~ "No"
      ),
      Diptera = dplyr::case_when(
        imputed_1 == "All" |
          imputed_2 == "All" |
          Diptera == "Yes" ~ "Yes",
        T ~ "No"
      ),
      Hymenoptera = dplyr::case_when(
        imputed_1 == "All" |
          imputed_2 == "All" |
          Hymenoptera == "Yes" ~ "Yes",
        T ~ "No"
      ),
      Mammals = dplyr::case_when(
        imputed_1 == "All" |
          imputed_2 == "All" |
          Mammals == "Yes" ~ "Yes",
        T ~ "No"
      ),
      Birds = dplyr::case_when(
        imputed_1 == "All" |
          imputed_2 == "All" |
          Birds == "Yes" ~ "Yes",
        T ~ "No"
      )
    ) %>%
    dplyr::select(
      -.data$imputed_1,
      -.data$imputed_2
    )
  
  return(imputed_merged_in)
}