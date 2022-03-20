#' check_recorder_role
#'
#' @param data respondent character table
#' @param config config file (for paths to validated recorder roles csv)
#'
#' @return respondent data with a column added for involvement using imputed recorder role from the other response option (text field).
#' @export
#'
check_recorder_role <- function(data, config) {
  data <- data %>%
    dplyr::mutate(involvement = dplyr::case_when(NoRole == "Yes" ~ NA_character_)) %>%
    dplyr::mutate(involvement = dplyr::case_when(
      .data$Organiser == "Yes" ~ "organiser",
      .data$Verifier == "Yes" ~ "verifier",
      .data$Recorder == "Yes" ~ "recorder_formal",
      .data$Informal == "Yes" ~ "recorder_informal",
      T ~ NA_character_
    ))

  #recorder_val <- readr::read_csv(config$validation_dirs$recorder_val, col_types = readr::cols(
  #  id = "c",
  #  imputed_involvement = "c"
  #))

  print("please see our assessment of respondent answers to whether they could name an arriving range-shifting species")
  print(recorder_val)

  other_validated <- data %>%
    dplyr::left_join(x = ., y = recorder_val %>%
      dplyr::select(
        -.data$YearsRecording,
        -.data$OtherRole,
        -.data$NoRole,
        -.data$Organiser,
        -.data$Verifier,
        -.data$Recorder,
        -.data$Informal
      ), by = "id")



  other_validated <- other_validated %>%
    dplyr::mutate(
      involvement = ordered(.data$involvement, levels = c("recorder_informal", "recorder_formal", "verifier", "organiser")),
      imputed_involvement = ordered(.data$imputed_involvement, levels = c("recorder_informal", "recorder_formal", "verifier", "organiser"))
    ) %>%
    dplyr::select(-dplyr::all_of(c(
      "NoRole",
      "Organiser",
      "Verifier",
      "Recorder",
      "Informal",
      "OtherRole"
    )))
  return(other_validated)
}
