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
