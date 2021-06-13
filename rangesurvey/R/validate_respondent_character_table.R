#' check_respondents_reported_gender
#'
#' @param data 
#'
#' @return
#' @export
#'
check_respondents_reported_gender <- function(data){
  
  if (all(is.na(data$`GDemographics02[other]`)) == TRUE) {
    
    data <- data %>% 
      dplyr::mutate(gender = as.factor(GDemographics02)) %>% 
             dplyr::select(-all_of(c("GDemographics02",
                           "GDemographics02[other]"))) %>% 
      dplyr::filter(gender != "Prefer not to Say") %>% 
      dplyr::mutate(gender = forcats::fct_drop(f = gender, only = "Prefer not to Say"))
    
    print("No Respondents answered other when asked their gender")
    return(data)
  }
  
return(data)
  }

#' check_respondents_ages
#'
#' @param data 
#'
#' @return
#' @export

check_respondents_ages <- function(data){
  data <- data %>% 
     dplyr::mutate(age = dplyr::case_when(GDemographics01 > 120 ~  2019 - GDemographics01,
                            GDemographics01 < 120 ~ GDemographics01,
                            T ~ NA_real_)
                   ) %>%
    dplyr::select(-GDemographics01)
                     
  return(data)
}


#' check_awareness
#'
#' @param data 
#' @param config 
#'
#' @return
#' @export
#'
check_awareness <- function(data, config){
  
  awareness_val <- readr::read_csv(config$validation_dirs$awareness_val, col_types =  readr::cols(id = "c"))
  
  print("please see our assessment of respondent answers to whether\n
        they could name an arriving range-shifting species")
  print(awareness_val)
  
  other_validated <- data %>% 
    dplyr::left_join(., awareness_val, by = "id") %>% 
    dplyr::select(- `GAwareness01[comment]`)%>%
    dplyr::rename("awareness" = "GAwareness01")
  
  

  
  return(other_validated)
  
} 

#' check_recorder_role
#'
#' @param data 
#' @param config 
#'
#' @return
#' @export
#'
check_recorder_role <- function(data, config){
  
  
  data <- data %>% 
    dplyr::mutate(involvement = dplyr::case_when(NoRole == "Yes" ~ NA_character_)) %>% 
    dplyr::mutate(involvement = dplyr::case_when(Organiser  == "Yes"~ "organiser",
                                               Verifier == "Yes"~ "verifier",
                                               Recorder == "Yes"~ "recorder_formal",
                                               Informal == "Yes"~ "recorder_informal",
                                               T ~ NA_character_ )) 
  
  recorder_val <- readr::read_csv(config$validation_dirs$recorder_val, col_types =  readr::cols(id = "c",
                                                                                         imputed_involvement = "c"
                                                                                                                                                                                 )
  )
  
  print("please see our assessment of respondent answers to whether they could name an arriving range-shifting species")
  print(recorder_val)
  
  other_validated <- data %>% 
    dplyr::left_join(., recorder_val %>% 
                       dplyr::select(-YearsRecording,
                                     -OtherRole,
                                     -NoRole,
                                     -Organiser,
                                     -Verifier,
                                     -Recorder,
                                     -Informal), by = "id")
  
  
  
   other_validated <- other_validated %>% 
     dplyr::mutate(involvement = ordered(involvement, levels = c("recorder_informal", "recorder_formal", "verifier", "organiser")),
                   imputed_involvement = ordered(imputed_involvement, levels = c("recorder_informal", "recorder_formal", "verifier", "organiser"))
     )%>% 
    dplyr::select(-all_of(c("NoRole",
                  "Organiser",
                  "Verifier",
                  "Recorder",
                  "Informal",
                  "OtherRole")))
   return(other_validated)
  
}


#' check_groups_recorded 
#'
#' @param data 
#' @param config 
#'
#' @return
#' @export
#'
check_groups_recorded <- function(data, config){
  
  species_group_val <- readr::read_csv(config$validation_dirs$species_group_val, col_types =  readr::cols(id = "c"))
  
  print("please see our imputation for respondent answers about other groups they recorded")
  print(species_group_val)
  
  other_validated <- data %>% 
    dplyr::left_join(.,
                     species_group_val %>% 
                       dplyr::select(-OtherGroups),
                     by = "id") %>% 
    dplyr::select(-OtherGroups) %>% 
    dplyr::mutate(imputed_group)
  
  return(other_validated)
  
  
}

#' check_education
#'
#' @param data 
#' @param config 
#'
#' @return
#' @export
#'
check_education <- function(data, config) {
  
  data <- data %>% 
    dplyr::mutate(education = dplyr::case_when(`GDemographics03[SQ001]` == "Yes"~ "Postgrad",
                                               `GDemographics03[SQ002]` == "Yes"~ "Undergrad",
                                               `GDemographics03[SQ003]` == "Yes"~ "A-levels",
                                               `GDemographics03[SQ004]` == "Yes"~ "GCSEs",
                                               T ~ NA_character_ 
    )) %>% 
    dplyr::select(-`GDemographics03[SQ001]`,
                  -`GDemographics03[SQ002]`,
                  -`GDemographics03[SQ003]`,
                  -`GDemographics03[SQ004]`)
  
  education_val <- readr::read_csv(config$validation_dirs$education_val, col_types =  readr::cols(id = "c"))
  
  print("please see our imputations of respondent education from the Education (other) responses")
  print(education_val)
  
  other_validated <- data %>% 
    dplyr::left_join(., education_val, by = "id")%>% 
    dplyr::select(-contains("GDemographics03"))
  
  
  other_validated <- other_validated %>% 
    dplyr::mutate(education = ordered(education, levels = c("GCSEs", "A-levels", "Undergrad", "Postgrad")),
                  imputed_education = ordered(imputed_education, levels = c("GCSEs", "A-levels", "Undergrad", "Postgrad"))
    )
  
  return(other_validated)
}