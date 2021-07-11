# TODO
#' reorder_table_columns
#'
#' @param data respondent_table_clean
#'
#' @return respondent_table_clean with reordered columns
#' @export
#'
reorder_table_columns <- function(data) {
  data <- dplyr::relocate()

  return(data)
}
