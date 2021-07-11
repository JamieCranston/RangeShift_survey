#' rename_presented_species
#'
#' @param data dataframe (respondent character table)
#'
#' @return dataframe (respondent character table with species shown to respondent listed)
#' @export
#'
rename_presented_species <- function(data) {
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

  data <- data %>%
    dplyr::mutate(
      vertebrate_1 = cut(
        .data$VertEq1,
        breaks = 8,
        labels = vert_names
      ),
      vertebrate_2 = cut(
        .data$VertEq2,
        breaks = 8,
        labels = vert_names
      ),
      invertebrate_1 = cut(
        .data$InvertEq1,
        breaks = 8,
        labels = invert_names
      ),
      invertebrate_2 = cut(
        .data$InvertEq2,
        breaks = 8,
        labels = invert_names
      )
    ) %>%
    dplyr::select(-dplyr::all_of(dplyr::contains("ertEq")))

  return(data)
}
