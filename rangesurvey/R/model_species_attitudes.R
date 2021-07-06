#' model_species_attitude
#'
#' @param species_data cleaned species responses data
#' @param respondent_data cleaned respondent data
#'
#' @return brms model object of predicted attitudes to management
#' @import brms
#' @export

model_species_attitudes <- function(species_data = species_responses_to_model,
                                    respondent_data = respondent_character_table_clean) {
  model_input_data <- dplyr::left_join(species_data,
                                       respondent_data) %>%
    dplyr::select(
      .data$.,
      .data$attitude_to_species,
      .data$species,
      .data$id,
      .data$match,
      .data$seen,
      .data$involvement,
      .data$wildlife_sector,
      .data$gender,
      .data$scaled_age,
      .data$scaled_years_recording,
      .data$education,
      .data$climate_treatment
    )


  model_input_data <- stats::na.omit(model_input_data)

  # Run Model
  Full_Model <- brms::brm(attitude_to_species ~ (1 | species) + (1 | id) + match + wildlife_sector + involvement + seen + climate_treatment + education + gender + scaled_age + scaled_years_recording,
    iter = 4000,
    seed = 1045428673,
    warmup = 2000,
    prior = brms::set_prior(horseshoe(df = 1, par_ratio = 0.33)),
    thin = 2,
    data = model_input_data,
    family = brms::categorical(link = "logit"),
    control = list(
      adapt_delta = 0.95,
      max_treedepth = 10
    ),
    cores = getOption(
      "mc.cores",
      4
    )
  )

  return(Full_Model)
}
