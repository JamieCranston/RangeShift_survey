

#' remove_response_times
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with the question response times removed
#' @export
#'
remove_response_times <- function(data){
  
  data  <- dplyr::select(data, -contains("Time"))
  return(data)

  }

#' remove_instructions 
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with the question intstructions removed
#' @export
#'
remove_instructions <- function(data){
  
  data <- dplyr::select(data,
                        -contains("C1"),
                        -contains("C0"),
                        -"Gcolinfo",
                        -"GColonistClimateQues",
                        -"SurveyInstructions1",
                        -"GDemographics05[SQ001]")
  return(data)
}

#' remove_metainformation
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with the metainformation removed
#' @export
#'
remove_meta_info <- function(data){
  
  data <- dplyr::select(data,
                        -all_of(c(
    "submitdate",
    "lastpage",
    "startlanguage",
    "seed",
    "startdate",
    "datestamp")
  ))
  
  return(data)
}

#' remove_pilot_responses
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with responses to the pilot removed
#' @export
#'
remove_pilot_responses <- function(data){
  
  data<- dplyr::filter(data, id > 34)
  
}

#' remove_incomplete_surveys
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with incomplete responses removed
#' @export
#'
remove_incomplete_surveys <- function(data){
  
  #remove those who didn't reach the last page of compulsory questions on the survey
  #remove those who left age and gender blank as likely skippers
  data <- dplyr::filter(data,
                       lastpage >= 21,
                       !is.na(GDemographics01)& !is.na(GDemographics02))
  
}


