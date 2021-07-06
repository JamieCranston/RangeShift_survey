
#' likert_7_to_3
#'
#' @param data species or management attitude responses
#'
#' @return dataframe with collapsed likert attitude responses from a scale of 7 to a scale of 3
#' @export
#'
likert_7_to_3 <- function(data) {
  data <- data %>%
    dplyr::mutate(
      Accept = forcats::fct_collapse(as.factor(.data$Accept),
        Anti = c("Strongly Negative", "Quite Negative", "A Bit Negative"),
        Neutral = "Neutral",
        Pro = c("A Bit Positive", "Quite Positive", "Strongly Positive")
      ),
      Adapt = forcats::fct_collapse(as.factor(.data$Adapt),
        Anti = c("Strongly Negative", "Quite Negative", "A Bit Negative"),
        Neutral = "Neutral",
        Pro = c("A Bit Positive", "Quite Positive", "Strongly Positive")
      ),
      Support = forcats::fct_collapse(as.factor(.data$Support),
        Anti = c("Strongly Negative", "Quite Negative", "A Bit Negative"),
        Neutral = "Neutral",
        Pro = c("A Bit Positive", "Quite Positive", "Strongly Positive")
      ),
      Remove = forcats::fct_collapse(as.factor(.data$Remove),
        Anti = c("Strongly Negative", "Quite Negative", "A Bit Negative"),
        Neutral = "Neutral",
        Pro = c("A Bit Positive", "Quite Positive", "Strongly Positive")
      ),
      Mitigate = forcats::fct_collapse(as.factor(.data$Mitigate),
        Anti = c("Strongly Negative", "Quite Negative", "A Bit Negative"),
        Neutral = "Neutral",
        Pro = c("A Bit Positive", "Quite Positive", "Strongly Positive")
      ),
      attitude_to_species = forcats::fct_collapse(as.factor(.data$attitude_to_species),
        Negative = c("Strongly Negative", "Quite Negative", "A Bit Negative"),
        Neutral = "Neutral",
        Positive = c("A Bit Positive", "Quite Positive", "Strongly Positive")
      )
    )
  return(data)
}
