#' @title is_species_in__recorded_group
#'
#' @param speciesdata 
#' @param respondentdata 
#'
#' @return
#' @export
#'
is_species_in__recorded_group <- function(speciesdata = .,
                                             respondentdata = respondent_character_table_clean){
  
  
  vertnames <- c("Egarzetta",
                  "Pleucorodia",
                  "Aalba",
                  "Bibis",
                  "Hhimantopus",
                  "Pfalcinellus",
                  "Iminutus"  ,
                  "Apurpurea"
  )
  
  odonames <- c(
    "Eviridulum",
    "Aaffinis"
  )
  
  lepinames <- c(
    "Calgae",
   "Eornata")
  hymennames <- c(
    "Dsaxonica",
     "Bhypnorum"
  )
  
  hemipnames <- c(
    "Rnebulosa",
    "Pkadenii"
  )
  
  match_dat <-  respondentdata  %>% 
    dplyr::select(id,
                  Hymenoptera,
                  Lepidoptera,
                  Hemiptera,
                  Odonata,
                  Birds,
                  dplyr::contains("vert")) %>% 
    tidyr::pivot_longer(contains("vert"), values_to = "species") %>% 
    dplyr::mutate(match = as.factor(dplyr::case_when(species %in% vertnames & Birds == "Yes" |
                                           species %in% hemipnames & Hemiptera == "Yes"|
                                           species %in% hymennames & Hymenoptera == "Yes"|
                                           species %in% lepinames & Lepidoptera == "Yes"|
                                           species %in% odonames & Odonata == "Yes"
                                             ~ "Yes",
                                           species %in% vertnames & Birds == "No" |
                                             species %in% hemipnames & Hemiptera == "No"|
                                             species %in% hymennames & Hymenoptera == "No"|
                                             species %in% lepinames & Lepidoptera == "No"|
                                             species %in% odonames & Odonata == "No"
                                             ~ "No",
                                           TRUE ~ NA_character_))
                  ) %>% 
    dplyr::mutate(taxa = as.factor(dplyr::case_when(species %in% vertnames ~ "vertebrate",
                                                    !species %in% vertnames ~ "invertebrate" ))
                  ) %>% 
    dplyr::select(-name,
                  -Hymenoptera,
                  -Lepidoptera,
                  -Hemiptera,
                  -Odonata,
                  -Birds)
  
  speciesdata <- dplyr::left_join(speciesdata,
                                  match_dat)
  
  return(speciesdata)
}
