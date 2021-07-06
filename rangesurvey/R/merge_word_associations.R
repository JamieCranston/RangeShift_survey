#' merge_word_associations
#'
#' @param data respondent character table
#'
#' @return dataframe (respondent character table with word association questions coalesced)
#' @export
#'
merge_word_associations <- function(data) {
  data <- dplyr::mutate(data,
                        Associations = dplyr::coalesce(
                          .data$CCassociations,
                          .data$InfoAssociations
                        )
  ) %>%
    dplyr::select(-dplyr::all_of(c(
      "CCassociations",
      "InfoAssociations"
    )))
  
  return(data)
}