#' check_awareness
#'
#' @param data respondent character table
#' @param config config file (for paths to validated range-shifters csv)
#'
#' @return dataframe with imputed awareness added
#' @export
#'
check_awareness <- function(data, config) {
  awareness_val <- readr::read_csv(config$validation_dirs$awareness_val,
                                   col_types = readr::cols(id = "c"))
  
  print("please see our assessment of respondent answers to whether\n
        they could name an arriving range-shifting species")
  print(awareness_val)
  
  other_validated <- data %>%
    dplyr::left_join(.data$., .data$awareness_val, by = "id") %>%
    dplyr::select(-.data$`GAwareness01[comment]`) %>%
    dplyr::rename("awareness" = "GAwareness01")

  return(other_validated)
}
