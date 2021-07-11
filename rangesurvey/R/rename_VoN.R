#' rename_VoN
#'
#' @param data dataframe (respondent character table)
#'
#' @return dataframe (respondent character table with Visions of Nature columnes renamed)
#' @export
#'
rename_VoN <- function(data) {
  data <- data %>%
    dplyr::rename(c(
      "participant_1" = "GVoN01[SQ001]",
      "partner_1" = "GVoN01[SQ002]",
      "partner_2" = "GVoN01[SQ003]",
      "master_1" = "GVoN01[SQ004]",
      "partner_3" = "GVoN01[SQ005]",
      "steward_1" = "GVoN01[SQ006]",
      "participant_2"  = "GVoN01[SQ007]",
      "master_2" = "GVoN01[SQ008]",
      "steward_2" = "GVoN01[SQ009]",
      "participant3" = "GVoN01[SQ010]",
      "steward_3" = "GVoN01[SQ011]",
      "master_3" = "GVoN01[SQ012]"
    ))

  von_numeric <- dplyr::mutate(
    data,
    dplyr::across(
      .cols = dplyr::contains(c("steward", "participant", "partner", "master")),
      function(X) {
        as.numeric(factor(X,
          ordered = TRUE,
          levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
        ))
      }
    )
  )

  von_perceptions <- von_numeric %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Steward = apply(dplyr::select(., dplyr::starts_with("steward")), 1, mean, na.rm = T),
      Partner = apply(dplyr::select(., dplyr::starts_with("partner")), 1, mean, na.rm = T),
      Participant = apply(dplyr::select(., dplyr::starts_with("participant")), 1, mean, na.rm = T),
      Master = apply(dplyr::select(., dplyr::starts_with("master")), 1, mean, na.rm = T)
    )

  von_perceptions <- von_perceptions %>%
    dplyr::mutate(Dominant = dplyr::case_when(
      .data$Steward > .data$Partner & .data$Steward > .data$Participant & .data$Steward > .data$Master ~ "Steward",
      .data$Partner > .data$Steward & .data$Partner > .data$Participant & .data$Partner > .data$Master ~ "Partner",
      .data$Master > .data$Steward &
        .data$Master > .data$Participant &
        .data$Master > .data$Partner ~ "Master",
      .data$Participant > .data$Steward & .data$Participant > .data$Master & .data$Participant > .data$Partner ~ "Participant",
      TRUE ~ "Tie"
    )) %>%
    dplyr::select(
      .data$id,
      .data$Steward,
      .data$Partner,
      .data$Participant,
      .data$Master,
      .data$Dominant
    ) %>%
    dplyr::left_join(data, .data$., by = "id")




  return(von_perceptions)
}
