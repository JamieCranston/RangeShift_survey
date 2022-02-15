#' predict_species_marginal_effects
#'
#' @param data  input data for species attitude brms model object
#' @param model species attitude brms model object
#'
#' @return marginal predictions for species
#' @export
predict_species_marginal_effects <- function(data,
                                             model) {
  newdata <- expand.grid(
    species = unique(data$species),
    id = "Mean",
    seen = NA,
    wildlife_sector = NA,
    involvement = NA,
    climate_treatment = NA,
    education = NA,
    gender = NA,
    scaled_age = 0,
    scaled_years_recording = 0,
    match = NA
  )

  # Generate predictions for the probability of each response at each level of the random factor.
  set.seed(333)
  Species <- stats::predict(model,
    newdata = newdata,
    re_formula = NULL,
    probs = c(0.025, 0.975),
    allow_new_levels = TRUE
  )
  # Add Species names to rows
  row.names(Species) <- unique(data$species)

  # Order by Probability of response being positive and coerce to dataframe
  Species <- data.frame(Species[order(Species[, "P(Y = Positive)"],
                                      decreasing = TRUE), ])

  # Add Latin name to dataframe and remove rownames.
  Species$LName <- gsub(pattern = "^(.{1})(.*)$", replacement = "\\1. \\2", x = row.names(Species))
  row.names(Species) <- NULL

  # Add in information on taxonomic group (if time convert to lookup table for safety)
  Species <- Species %>%
    dplyr::mutate(Group = dplyr::case_when(LName ==  "A. affinis" | LName ==  "E. viridulum" ~ "Dragonfly",
                                    LName ==  "P. kadenii" | LName ==  "C. algae" ~ "Moth",
                                    LName ==  "B. hypnorum" | LName ==  "D. saxonica" ~ "Bee / Wasp",
                                    LName ==  "R. nebulosa" | LName ==  "E. ornata" ~ "Shieldbug",
                                    TRUE ~ "Bird" ))

  return(Species)
}
