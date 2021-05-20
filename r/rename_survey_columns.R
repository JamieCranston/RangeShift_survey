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
                    "YearsRecording" = "GRecorderInfo04",
                    "Informal" = "GRecorderInfo03[SQ006]",
                    "Recorder" = "GRecorderInfo03[SQ001]",
                    "Verifier" = "GRecorderInfo03[SQ002]",
                    "Organiser" = "GRecorderInfo03[SQ003]",
                    "NoRole" = "GRecorderInfo03[SQ005]",
                    "OtherRole" = "GRecorderInfo03[other]"
)

return(data)
}

merge_word_associations <- function(data){
  
  data <- dplyr::mutate(data,
                        Associations = dplyr::coalesce(CCassociations,
                                                     InfoAssociations)) %>% 
    dplyr::select(-all_of(c("CCassociations",
                          "InfoAssociations"))
                  )

  return(data)
  
}

rename_species_of_interest <- function(data){
 data <- dplyr::rename(data,
                       "RiskSpecies" = `Dual[SQ001_SQ001]`,
                       "ConserveSpecies" = `Dual[SQ003_SQ001]`)
 return(data)
}

  
  
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
