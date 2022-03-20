#' ingest_survey_data
#'
#' @param config config file with paths to survey data
#'
#' @return raw survey data
#' @export

ingest_survey_data <- function(config) {
  survey_data_file_path <- paste0(
    config$data_dir,
    config$data_file
  )
  survey_data <- readr::read_csv(survey_data_file_path)
  return(survey_data)
}
