#' rename_recorder_info 
#'
#' @param data 
#'
#' @return
#' @export
#'
rename_recorder_info <- function(data){

data <- dplyr::rename(data,
                    "Birds" = "GRecorderInfo02[SQ002]",
                    "Mammals" = "GRecorderInfo02[SQ009]",
                    "Hymenoptera" = "GRecorderInfo02[SQ008]",
                    "Coleoptera" = "GRecorderInfo02[SQ003]",
                    "Lepidoptera" = "GRecorderInfo02[SQ005]",
                    "Diptera" = "GRecorderInfo02[SQ004]",
                    "Odonata" = "GRecorderInfo02[SQ006]",
                    "Hemiptera" = "GRecorderInfo02[SQ007]",
                    "NoSpGroups" = "GRecorderInfo02[spanstylefontsize14p]",
                    "OtherGroups" = "GRecorderInfo02[other]",
                    "years_recording" = "GRecorderInfo04",
                    "Informal" = "GRecorderInfo03[SQ006]",
                    "Recorder" = "GRecorderInfo03[SQ001]",
                    "Verifier" = "GRecorderInfo03[SQ002]",
                    "Organiser" = "GRecorderInfo03[SQ003]",
                    "NoRole" = "GRecorderInfo03[SQ005]",
                    "OtherRole" = "GRecorderInfo03[other]"
)

return(data)
}

#' merge_word_associations
#'
#' @param data 
#'
#' @return
#' @export
#'
merge_word_associations <- function(data){
  
  data <- dplyr::mutate(data,
                        Associations = dplyr::coalesce(CCassociations,
                                                     InfoAssociations)) %>% 
    dplyr::select(-all_of(c("CCassociations",
                          "InfoAssociations"))
                  )

  return(data)
  
}

#' rename_species_of_interest 
#'
#' @param data 
#'
#' @return
#' @export
rename_species_of_interest <- function(data){
 data <- dplyr::rename(data,
                       "RiskSpecies" = `Dual[SQ001_SQ001]`,
                       "ConserveSpecies" = `Dual[SQ003_SQ001]`)
 return(data)
}

  
  
#' rename_mngmnt_ideas
#'
#' @param data 
#'
#' @return
#' @export
#'
rename_mngmnt_ideas <- function(data){
data <- dplyr::rename(data,
                      "Monitoring_Do" =     `Array[SQ002_SQ001]`,
                       "Monitoring_DontDo" = `Array[SQ002_SQ002]`,
                           "Protecting_Do" = `Array[SQ003_SQ001]`,
                       "Protecting_DontDo" = `Array[SQ003_SQ002]`,
                           "Minimising_Do" = `Array[SQ004_SQ001]`,
                       "Minimising_DontDo" = `Array[SQ004_SQ002]`
) 
return(data)
}

#' rename_VoN
#'
#' @param data 
#'
#' @return
#' @export
#'
rename_VoN <- function(data){
  
  data <- data %>% 
    dplyr::rename(c(
      "participant_1" = "GVoN01[SQ001]",
      "partner_1" = "GVoN01[SQ002]",
      "partner_2" = "GVoN01[SQ003]",
      "master_1" = "GVoN01[SQ004]",
      "partner_3" = "GVoN01[SQ005]",
      "steward_1" = "GVoN01[SQ006]",
      "participant_2"  = "GVoN01[SQ007]",
      "master_2" = "GVoN01[SQ008]",
      "steward_2" = "GVoN01[SQ009]",
      "participant3" = "GVoN01[SQ010]",
      "steward_3" = "GVoN01[SQ011]",
      "master_3" = "GVoN01[SQ012]"))
  
  von_numeric <- dplyr::mutate(data,
                               across(.cols = contains(c("steward","participant","partner","master")),
                              function(X) {
    as.numeric(factor(X,
                      ordered = TRUE,
                      levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    ))
  
   } ))
  
  von_perceptions <-  von_numeric %>%
    dplyr::ungroup()%>%
    dplyr::mutate(Steward = apply(dplyr::select(., starts_with("steward")), 1, mean, na.rm =T),
           Partner = apply(dplyr::select(., starts_with("partner")), 1, mean, na.rm =T),
           Participant = apply(dplyr::select(., starts_with("participant")), 1, mean, na.rm =T),
           Master = apply(dplyr::select(., starts_with("master")), 1, mean , na.rm =T)
    )
  
  von_perceptions <- von_perceptions  %>%
    dplyr::mutate(Dominant = dplyr::case_when(Steward > Partner & Steward > Participant & Steward > Master ~ "Steward",
                                Partner > Steward & Partner > Participant & Partner > Master ~ "Partner",
                                Master > Steward & Master > Participant & Master > Partner ~ "Master",
                                Participant > Steward & Participant > Master & Participant > Partner ~ "Participant",
                                TRUE ~ "Tie")) %>% 
    dplyr::select(id, Steward, Partner,Participant, Master,Dominant) %>% 
    dplyr::left_join(data,., by = "id")
  
  

  
  return(von_perceptions )
  
}

#' check_respondents_postcode 
#'
#' @param data 
#'
#' @return
#' @export
#'
check_respondents_postcode <- function(data){
  
  data <- data %>% 
    dplyr::rename(postcode = GDemographics04) %>% 
    dplyr::mutate(postcode = factor(toupper(postcode)))
  
  return(data)
}

#' check_wildlife_sector
#'
#' @param data 
#'
#' @return
#' @export
#'
check_wildlife_sector<- function(data){
  
  data <- data %>% 
   dplyr::rename(wildlife_sector = `GDemographics05[SQ002]`)
  return(data)
}
  
#' rename_presented_species
#'
#' @param data 
#'
#' @return
#' @export
#'
rename_presented_species <- function(data){
 
  vert_names <- c(
    "Egarzetta",
    "Pleucorodia",
    "Aalba",
    "Bibis",
    "Hhimantopus",
    "Pfalcinellus",
    "Iminutus"  ,
    "Apurpurea"
  )
  invert_names <- c(
    "Eviridulum",
    "Eornata",
    "Dsaxonica",
    "Aaffinis",
    "Rnebulosa",
    "Pkadenii",
    "Calgae",
    "Bhypnorum"
  )
  
  data <- data %>%
    dplyr::mutate(vertebrate_1 = cut(
      VertEq1,
      breaks = 8,
      labels = vert_names),
      vertebrate_2 = cut(
        VertEq2,
        breaks = 8,
        labels = vert_names),
      invertebrate_1 = cut(
        InvertEq1,
        breaks = 8,
        labels = invert_names),
      invertebrate_2 = cut(
        InvertEq2,
        breaks = 8,
        labels = invert_names)
    ) %>% 
    dplyr::select(-all_of(contains("ertEq")))
  
  return(data) 
}

#' rename_final_text_Qs
#'
#' @param data 
#'
#' @return
#' @export
#'
rename_final_text_Qs <- function(data){
  
  data <- data %>% 
    dplyr::rename("implications_of_climate_change" ="FinalClimateCheck",
                  "final_open_response_q" = "GFreeFromFinal")
  
  return(data)
}

#TODO
#' reorder_table_columns
#'
#' @param data 
#'
#' @return
#' @export
#'
reorder_table_columns <- function(data){
  
  data <- dplyr::relocate()

return(data)
  
}