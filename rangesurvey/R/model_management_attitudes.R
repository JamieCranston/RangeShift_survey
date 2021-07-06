#' model_management_attitudes
#'
#' @param species_data cleaned species responses data
#' @param respondent_data cleaned respondent data
#'
#' @return brms model object of predicted attitudes to management
#' @importFrom brms brm set_prior categorical
#' @export
#'
model_management_attitudes <- function(species_data = management_responses_to_model,
                                       respondent_data = respondent_table_clean) {
  model_input_data <- dplyr::left_join(
    species_data,
    respondent_data
  ) %>%
    dplyr::select(
      .data$.,
      .data$attitude_to_species,
      .data$Remove,
      .data$Mitigate,
      .data$Accept,
      .data$Adapt,
      .data$Support,
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

  # 1147
  model_input_data <- stats::na.omit(model_input_data)

  model_input_data <- model_input_data %>%
    dplyr::mutate(dplyr::across(.cols = c(
      "Remove",
      "Mitigate",
      "Accept",
      "Adapt",
      "Support"
    ), .fns = ~ factor(.x,
      levels = levels(.x)[c(2, 1, 3)]
    )))


  Full_Model_Condensed <- brms::brm(
    mvbind(
      Accept,
      Support,
      Adapt,
      Mitigate,
      Remove
    ) ~ (1 | species) +
      (1 | id) +
      match +
      wildlife_sector +
      involvement +
      seen +
      climate_treatment +
      education +
      gender +
      scaled_age +
      scaled_years_recording +
      awareness_numeric +
      attitude_to_species,
    iter = 4000,
    warmup = 2000,
    prior = brms::set_prior(horseshoe(df = 1, par_ratio = 0.2)),
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

  return(Full_Model_Condensed)
}
