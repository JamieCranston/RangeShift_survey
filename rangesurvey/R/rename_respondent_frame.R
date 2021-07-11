#' rename_respondent_frame
#'
#' @param data respondent frame
#'
#' @return renamed respondent frame
#' @export
rename_respondent_frame <- function(data) {
  renamed_respondent_frame <- data %>%
    rename_recorder_info() %>%
    rename_mngmnt_ideas() %>%
    rename_presented_species() %>%
    rename_species_of_interest() %>%
    rename_VoN() %>%
    rename_final_text_Qs() %>%
    merge_word_associations()
  return(renamed_respondent_frame)
}
