#' model_species_attitude
#'
#' @param species_data 
#' @param respondent_data 
#'
#' @return
#' @import brms
#' @export

model_species_attitudes <- function(species_data = species_responses_to_model,
                                    respondent_data = respondent_character_table_clean) {
  
  model_input_data <- dplyr::left_join(species_data, respondent_data) %>% 
    dplyr::select(., attitude_to_species,
                  species,
                  id,
                  match,
                  seen,
                  involvement,
                  wildlife_sector,
                  gender,
                  scaled_age,
                  scaled_years_recording,
                  education,
                  climate_treatment
                  )

  
  model_input_data <- na.omit(model_input_data)
  
  # Run Model
  Full_Model <- brms::brm(attitude_to_species ~ (1 | species) + (1 | id) + match + wildlife_sector + involvement + seen + climate_treatment+education + gender + scaled_age + scaled_years_recording,
                    iter = 4000,
                    seed = 1045428673,
                    warmup = 2000,
                    prior = set_prior(horseshoe(df = 1, par_ratio = 0.33)),
                    thin = 2,
                    data = model_input_data,
                    family = categorical(link = "logit"),
                    control = list(adapt_delta = 0.95,
                                   max_treedepth = 10),
                    cores = getOption(
                      "mc.cores",
                      4
                    ))
  
  return(Full_Model)
}