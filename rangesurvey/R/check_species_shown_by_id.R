#' check_species_shown_by_id 
#'
#' @param speciesdata 
#' @param respondentdata 
#'
#' @return
#' @export
#'
check_species_shown_by_id <- function(speciesdata, respondentdata) {
  
  species_shown <- respondentdata %>%
    dplyr::select(id, contains("verte")) %>%
    tidyr::pivot_longer(cols = contains("verte"), values_to = "species") %>% 
    dplyr::select(-name) %>% 
    dplyr::left_join(., speciesdata)
  
  
  return(species_shown)
}