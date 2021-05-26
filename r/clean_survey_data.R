

#' remove_response_times
#'
#' @param data a data.frame containing the raw survey responses
#'
#' @return data.frame with the question response times removed
#' @export
#'
#' @examples
remove_response_times <- function(data){
  
  data  <- dplyr::select(data, -contains("Time"))
  return(data)

  }

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

remove_pilot_responses <- function(data){
  
  data<- dplyr::filter(data, id > 34)
  
}

remove_incomplete_surveys <- function(data){
  
  #remove those who didn't reach the last page of compulsory questions on the survey
  #remove those who left age and gender blank as likely skippers
  data <- dplyr::filter(data,
                       lastpage >= 21,
                       !is.na(GDemographics01)& !is.na(GDemographics02))
  
}

check_education <- function(data, config) {
  
  data <- data %>% 
    dplyr::mutate(education = dplyr::case_when(`GDemographics03[SQ001]` == "Yes"~ "Postgrad",
                                        `GDemographics03[SQ002]` == "Yes"~ "Undergrad",
                                        `GDemographics03[SQ003]` == "Yes"~ "A-Levels",
                                        `GDemographics03[SQ004]` == "Yes"~ "GCSEs",
                                        T ~ NA_character_ 
                                        )) %>% 
    dplyr::select(-`GDemographics03[SQ001]`,
                  -`GDemographics03[SQ002]`,
                  -`GDemographics03[SQ003]`,
                  -`GDemographics03[SQ004]`)
  
  education_val <- readr::read_csv(config$validation_dirs$education_val)
  
  print("please see our imputations of respondent education from the Education (other) responses")
  print(education_val)
  
  other_validated <- data %>% 
    dplyr::left_join(., education_val, by = "id")%>% 
    dplyr::select(-contains("GDemographics03"))
  
  
  
    return(other_validated)
}
