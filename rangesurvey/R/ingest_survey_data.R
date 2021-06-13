#' ingest_survey_data
#'
#' @param config 
#'
#' @return
#' @export

ingest_survey_data <- function(config){
  
  survey_data_file_path <- paste0(config$data_dir, "FinalSurveyResults.csv")
  survey_data <- readr::read_csv(survey_data_file_path)
  return(survey_data)
  
  }