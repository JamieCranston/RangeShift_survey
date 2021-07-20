#' make_supp_fig_10c
#'
#' @param pca_output pca of management attitudes
#'
#' @return lineplot of attitudes to different management options at the respondent level (facetted by cluster)
#' @export
#'
make_supp_fig_10c <- function(pca_output) {
  linedata <- pca_output$data.clust %>%
    dplyr::mutate(id = row.names(pca_output$data.clust)) %>%
    tidyr::pivot_longer(
      cols = c(
        "Remove",
        "Mitigate",
        "Accept",
        "Adapt",
        "Support"
      ),
      values_to = "Q"
    ) %>%
    dplyr::mutate(Q = gsub(
      x = .data$Q,
      pattern = ".*_",
      replacement = ""
    )) %>%
    dplyr::mutate(name = factor(.data$name,
      levels = c(
        "Support",
        "Adapt",
        "Accept",
        "Mitigate",
        "Remove"
      )
    ))


  labels <- c(
    "1" = "Cluster 1: Support Colonists",
    "2" = "Cluster 2: Wary of Colonists",
    "3" = "Cluster 3: Non-Intervention",
    "4" = "Cluster 4: Neutral"
  )

  lineplot <- ggplot(
    data = linedata,
    aes(
      x = .data$name,
      y = .data$Q,
      group = .data$id
    )
  ) +
    geom_line(position = position_jitter(0.1, 0.1)) +
    facet_wrap(~clust,
      scales = "free_y",
      labeller = labeller(clust = labels)
    )

  return(lineplot)
}
