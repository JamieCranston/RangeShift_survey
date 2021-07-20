
#' pivot_species_long
#'
#' @param data survey_species_responses
#'
#' @return dataframe (nrow =  unique(respondent) x length(species) (16))
#' @export
#'
pivot_species_long <- function(data) {
  vert_names <- c(
    "Egarzetta",
    "Pleucorodia",
    "Aalba",
    "Bibis",
    "Hhimantopus",
    "Pfalcinellus",
    "Iminutus",
    "Apurpurea"
  )
  invert_names <- c(
    "Eviridulum",
    "Eornata",
    "Dsaxonica",
    "Aaffinis",
    "Rnebulosa",
    "Pkadenii",
    "Calgae",
    "Bhypnorum"
  )

  species_stack <- lapply(c(
    vert_names,
    invert_names
  ),
  FUN = function(X) {
    species_data <- data %>%
      dplyr::select(
        "id",
        dplyr::contains(X)
      ) %>%
      dplyr::mutate(species = X)
    names(species_data) <- c(
      "id",
      "01[SQ001]",
      "01[SQ002]",
      "02[SQ001]",
      "T01",
      "03[SQ001]",
      "03[SQ002]",
      "03[SQ003]",
      "03[SQ004]",
      "03[SQ005]",
      "T02",
      "species"
    )

    return(species_data)
  }
  )

  data <- purrr::reduce(
    species_stack,
    dplyr::bind_rows
  )
  data <- data %>%
    dplyr::mutate("seen" = dplyr::coalesce(
      .data$`01[SQ001]`,
      .data$`01[SQ002]`
    )) %>%
    dplyr::select(
      -"01[SQ001]",
      -"01[SQ002]"
    )
  data <- data %>%
    dplyr::rename(
      "attitude_to_species" = .data$`02[SQ001]`,
      "attitude_to_species_text" = .data$T01,
      "attitude_to_management_text" = .data$T02,
      "Remove" = .data$`03[SQ001]`,
      "Mitigate" = .data$`03[SQ002]`,
      "Accept" = .data$`03[SQ003]`,
      "Adapt" = .data$`03[SQ004]`,
      "Support" = .data$`03[SQ005]`
    )

  data$species <- as.factor(data$species)
  data$seen <- as.factor(data$seen)
  data$id <- as.factor(data$id)
  return(data)
}
