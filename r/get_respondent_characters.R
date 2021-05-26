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
    )
  
}

get_species_responses <- function(data) {
  
  data %>% 
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
    )
    

  
}

