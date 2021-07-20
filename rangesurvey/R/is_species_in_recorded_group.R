#' @title is_species_in_recorded_group
#'
#' @param speciesdata cleaned species responses data
#' @param respondentdata cleaned respondent data
#'
#' @return respondent data with a column added for match which is positive when
#'   a respondent records the species group of the species they were shown in
#'   the survey question and negative where not.
#' @export
#'
is_species_in_recorded_group <- function(speciesdata,
                                         respondentdata) {
  vertnames <- c(
    "Egarzetta",
    "Pleucorodia",
    "Aalba",
    "Bibis",
    "Hhimantopus",
    "Pfalcinellus",
    "Iminutus",
    "Apurpurea"
  )

  odonames <- c(
    "Eviridulum",
    "Aaffinis"
  )

  lepinames <- c(
    "Calgae",
    "Eornata"
  )
  hymennames <- c(
    "Dsaxonica",
    "Bhypnorum"
  )

  hemipnames <- c(
    "Rnebulosa",
    "Pkadenii"
  )

  match_dat <- respondentdata %>%
    dplyr::select(
      .data$id,
      .data$Hymenoptera,
      .data$Lepidoptera,
      .data$Hemiptera,
      .data$Odonata,
      .data$Birds,
      dplyr::contains("vert")
    ) %>%
    tidyr::pivot_longer(dplyr::contains("vert"),
      values_to = "species"
    ) %>%
    dplyr::mutate(match = as.factor(dplyr::case_when(
      species %in% vertnames & Birds == "Yes" |
        species %in% hemipnames & Hemiptera == "Yes" |
        species %in% hymennames & Hymenoptera == "Yes" |
        species %in% lepinames & Lepidoptera == "Yes" |
        species %in% odonames & Odonata == "Yes"
      ~ "Yes",
      species %in% vertnames & Birds == "No" |
        species %in% hemipnames & Hemiptera == "No" |
        species %in% hymennames & Hymenoptera == "No" |
        species %in% lepinames & Lepidoptera == "No" |
        species %in% odonames & Odonata == "No"
      ~ "No",
      TRUE ~ NA_character_
    ))) %>%
    dplyr::mutate(taxa = as.factor(dplyr::case_when(
      species %in% vertnames ~ "vertebrate",
      !species %in% vertnames ~ "invertebrate"
    ))) %>%
    dplyr::select(
      -.data$name,
      -.data$Hymenoptera,
      -.data$Lepidoptera,
      -.data$Hemiptera,
      -.data$Odonata,
      -.data$Birds
    )

  speciesdata <- dplyr::left_join(
    speciesdata,
    match_dat
  )

  return(speciesdata)
}
