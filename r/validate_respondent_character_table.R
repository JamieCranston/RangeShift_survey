check_respondents_reported_gender <- function(data){
  
  if (all(is.na(data$`GDemographics02[other]`)) == TRUE) {
    
    data <- data %>% 
      dplyr::mutate(Gender = as.factor(GDemographics02)) %>% 
             dplyr::select(-all_of(c("GDemographics02",
                           "GDemographics02[other]")))
    
    print("No Respondents answered other when asked their gender")
    return(data)
  }
  
return(data)
  }

check_respondents_ages <- function(data){
  data <- data %>% 
     dplyr::mutate(Age = dplyr::case_when(GDemographics01 > 120 ~  2019 - GDemographics01,
                            GDemographics01 < 120 ~ GDemographics01,
                            T ~ NA_real_)
                   ) %>%
    dplyr::select(-GDemographics01)
                     
  return(data)
}


check_awareness <- function(data, config){
  
  awareness_val <- readr::read_csv(config$validation_dirs$awareness_val)
  
  print("please see our assessment of respondent answers to whether\n
        they could name an arriving range-shifting species")
  print(awareness_val)
  
  other_validated <- data %>% 
    dplyr::left_join(., awareness_val, by = "id") %>% 
    dplyr::select(- `GAwareness01[comment]`)%>%
    dplyr::rename("awareness" = "GAwareness01")
  
  

  
  return(other_validated)
  
} 

check_recorder_role <- function(data, config){
  
  
  data <- data %>% 
    dplyr::mutate(involvement = dplyr::case_when(NoRole == "Yes" ~ NA_character_)) %>% 
    dplyr::mutate(involvement = dplyr::case_when(Organiser  == "Yes"~ "organiser",
                                               Verifier == "Yes"~ "verifier",
                                               Recorder == "Yes"~ "recorder_formal",
                                               Informal == "Yes"~ "recorder_informal",
                                               T ~ NA_character_ )) 
  
  recorder_val <- readr::read_csv(config$validation_dirs$recorder_val)
  
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


check_groups_recorded <- function(data, config){
  
  species_group_val <- readr::read_csv(config$validation_dirs$species_group_val)
  
  print("please see our imputation for respondent answers about other groups they recorded")
  print(species_group_val)
  
  other_validated <- data %>% 
    dplyr::left_join(.,  species_group_val %>% 
                       dplyr::select(-OtherGroups), by = "id") %>% 
    dplyr::select(-OtherGroups)
  
  return(other_validated)
  
  
}