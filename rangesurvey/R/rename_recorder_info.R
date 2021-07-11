#' rename_recorder_info
#'
#' @param data respondent character table
#'
#' @return dataframe (respondent character table with recorder info columns renames)
#' @export
#'
rename_recorder_info <- function(data) {
  data <- dplyr::rename(data,
    "Birds" = "GRecorderInfo02[SQ002]",
    "Mammals" = "GRecorderInfo02[SQ009]",
    "Hymenoptera" = "GRecorderInfo02[SQ008]",
    "Coleoptera" = "GRecorderInfo02[SQ003]",
    "Lepidoptera" = "GRecorderInfo02[SQ005]",
    "Diptera" = "GRecorderInfo02[SQ004]",
    "Odonata" = "GRecorderInfo02[SQ006]",
    "Hemiptera" = "GRecorderInfo02[SQ007]",
    "NoSpGroups" = "GRecorderInfo02[spanstylefontsize14p]",
    "OtherGroups" = "GRecorderInfo02[other]",
    "years_recording" = "GRecorderInfo04",
    "Informal" = "GRecorderInfo03[SQ006]",
    "Recorder" = "GRecorderInfo03[SQ001]",
    "Verifier" = "GRecorderInfo03[SQ002]",
    "Organiser" = "GRecorderInfo03[SQ003]",
    "NoRole" = "GRecorderInfo03[SQ005]",
    "OtherRole" = "GRecorderInfo03[other]"
  )

  return(data)
}
