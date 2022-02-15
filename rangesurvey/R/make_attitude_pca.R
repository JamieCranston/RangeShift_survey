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
      X = .,
      graph = TRUE,
      ncp = Inf
    )
return(data.mca)
}

#' Title
#'
#' @param mca 
#'
#' @return
#' @export
#'
make_attitude_pca_plots <- function(mca){
  biplot <- factoextra::fviz_mca(mca,choice = "var.cat", label = "var")
  
  screeplot <- factoextra::fviz_screeplot(mca)

  return(list(biplot = biplot,
              screeplot= screeplot)
  )
}

#' make_attitude_clusters
#'
#' @param mca 
#'
#' @return
#' @export
#'
#' @examples
make_attitude_clusters <- function(mca){
  data.hcpc <- FactoMineR::HCPC(mca,
                                nb.clust = 4
  )
  return(data.hcpc)
}
  
#' Title
#'
#' @param hcpc 
#'
#' @return
#' @export

make_attitude_cluster_plots <- function(hcpc){
  attitude_cluster_plots <- list(
  clust_plot = factoextra::fviz_cluster(hcpc),
  dend_plot = factoextra::fviz_dend(hcpc)
)
  return(attitude_cluster_plots)
}  
  

