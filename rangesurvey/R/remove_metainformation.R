#' remove_metainformation
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with the metainformation removed
#' @export
#'
remove_meta_info <- function(data) {
  data <- dplyr::select(
    data,
    -dplyr::all_of(c(
      "submitdate",
      "lastpage",
      "startlanguage",
      "seed",
      "startdate",
      "datestamp"
    ))
  )
  
  return(data)
}