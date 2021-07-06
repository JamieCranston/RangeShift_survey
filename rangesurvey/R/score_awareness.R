#' score_awareness 
#'
#' @param data respondent character table
#' @param numeric_out whether function ouput should be numeric or character (Boolean)
#' @return respondent character table with awareness_score addedd either as character or numeric
#' @export
#'
score_awareness <- function(data = respondent_table_clean,
                            numeric_out = FALSE) {
  data_awareness_scored <- data %>%
    dplyr::select(
      .data$id,
      .data$awareness,
      .data$Category,
      .data$SpeciesIdentified,
      .data$Taxa,
      .data$SpeciesNamed,
    ) %>%
    dplyr::mutate(awareness_score = dplyr::case_when(
      awareness == "No, I haven't heard of this phenomenon." ~ "Don't know/\nDidn't answer",
      awareness == "Yes, I am familiar with this phenomenon but couldn't name a specific example." & is.na(Category) ~ "Didn't\nEvidence",
      awareness == "Yes, I am familiar with this phenomenon but couldn't name a specific example." & Category == "Colonist" ~ "Correctly\nNamed", # Error in survey
      awareness == "Yes, I am familiar with this phenomenon & I can name ONE species which has established in the UK." & Category == "Colonist" ~ "Correctly\nNamed",
      awareness == "Yes, I am familiar with this phenomenon & I can name ONE species which has established in the UK." & Category != "Colonist" ~ "Incorrectly\nNamed",
      is.na(awareness) & Category == "Colonist" ~ "Correctly\nNamed",
      is.na(awareness) & Category != "Colonist" ~ "Incorrectly\nNamed",
      is.na(awareness) & is.na(Category) != "Colonist" ~ "Don't know/\nDidn't answer",
      awareness == "Yes, I am familiar with this phenomenon & I can name ONE species which has established in the UK." & is.na(Category) ~ "Didn't\nEvidence",
      TRUE ~ "Look at these"
    )) %>%
    dplyr::mutate(Taxa = dplyr::case_when(
      awareness_score == "Didn't\nEvidence" ~ NA_character_,
      TRUE ~ Taxa
    )) %>%
    dplyr::select(
      .data$id,
      .data$Taxa,
      .data$awareness_score
    )
  if(numeric_out == TRUE){
    data_awareness_scored <- data_awareness_scored %>%
      dplyr::mutate(awareness_score = dplyr::case_when(awareness_score == "Correctly\nNamed" ~ 2,
                                                       awareness_score == "Incorrectly\nNamed" ~ 1,
                                                       awareness_score == "Didn't\nEvidence" ~ 1,
                                                       awareness_score == "Don't know/\nDidn't answer" ~ 0,
                                                      TRUE ~ NA_real_ 
                                                       )
                    )
  }
  
  return(data_awareness_scored)
}
