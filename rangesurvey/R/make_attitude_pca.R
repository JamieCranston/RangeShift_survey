#' make_attitude_pca
#'
#' @param data respondent attitudes to management to model
#'
#' @return pca object
#' @export
#'
make_attitude_pca <- function(data) {
  data.mca <- data %>%
    dplyr::select(
      -.data$id,
      -.data$species,
      -.data$attitude_to_species,
      -.data$seen,
      -.data$match
    ) %>%
    stats::na.omit() %>%
    FactoMineR::MCA(
      X = .data$.,
      graph = TRUE,
      ncp = Inf
    )

  data.hcpc <- FactoMineR::HCPC(data.mca,
    nb.clust = 4
  )

  return(data.hcpc)
}
