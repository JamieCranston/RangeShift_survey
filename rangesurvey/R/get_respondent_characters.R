#' get_respondent_characters
#'
#' @param data 
#'
#' @return
#' @export
#'
get_respondent_characters <- function(data){
  
  data %>% 
    dplyr::select("id",
                    dplyr::contains("GRecorderInfo"),
                    dplyr::contains("Gawareness"),
                    dplyr::contains("ertEq"),
                    "ClimateEq",
                  dplyr::contains("VoN"),
                  dplyr::contains("associations"),
                  dplyr::contains("GDemographics"),
                  dplyr::contains("Array"),
                  dplyr::contains("Dual"),
                 "FinalClimateCheck",
                 "GFreeFromFinal"
    ) %>% 
    dplyr::mutate(id = as.factor(id))
  
}

#' get_species_responses
#'
#' @param data 
#'
#' @return
#' @export
#'
get_species_responses <- function(data) {
  
 data <- data %>% 
    dplyr::select(
      -c(dplyr::contains("GRecorderInfo"),
                  dplyr::contains("Gawareness"),
                  dplyr::contains("ertEq"),
                  "ClimateEq",
                  dplyr::contains("VoN"),
                  dplyr::contains("associations"),
                  dplyr::contains("GDemographics"),
                  dplyr::contains("Array"),
                  dplyr::contains("Dual"),
                  "FinalClimateCheck",
                  "GFreeFromFinal"
    )
    )%>% 
   dplyr::mutate(id = as.factor(id))
 
  
  return(data)
    

  
}

