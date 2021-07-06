#' predict_marginal_seen_and_match
#'
#' @param data  input data for species attitude brms model object
#' @param model species attitude brms model object 
#'
#' @return marginal predictions for seen and match
#' @export
predict_marginal_seen_and_match <- function(data = species_responses_to_model,
                                            model = species_attitude_model) {
  newdata <- expand.grid(
    Species = unique(data$Species),
    id = "Mean",
    Seen = NA,
    WildlifeSector = NA,
    Involvement = NA,
    ClimateEq = NA,
    Education = NA,
    Gender = NA,
    ScaleAge = 0,
    ScaleYR = 0,
    Match = NA
  )
  
  # Generate predictions for the probability of each response at each level of the random factor.
  Species <- stats::predict(model,
                     newdata = newdata,
                     re_formula = NULL,
                     probs = c(0.025, 0.975),
                     allow_new_levels = T
  )
  # Add Species names to rows
  row.names(Species) <- unique(data$Species)
  
  # Order by Probability of response being positive and coerce to dataframe
  Species <- data.frame(Species[order(Species[, "P(Y = Positive)"], decreasing = T), ])
  
  # Add Latin name to dataframe and remove rownames.
  Species$LName <- gsub(pattern = "^(.{1})(.*)$", replacement = "\\1. \\2", x = row.names(Species))
  row.names(Species) <- NULL
  
  # Add in information on taxonomic group (if time convert to lookup table for safety)
  Species$Group <- c(
    rep("Bird", 6),
    "Dragonfly",
    "Bird",
    "Dragonfly",
    "Bee / Wasp",
    "Bird",
    rep("Moth", 2),
    rep("Shieldbug", 2),
    "Bee / Wasp"
  )
}
