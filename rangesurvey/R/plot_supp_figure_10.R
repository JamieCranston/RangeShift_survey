#' plot_supp_fig_10
#'
#' @param data management_responses_to_model
#'
#' @return supplementary figure 10 (outputs of clustering method)
#' @import patchwork
#' @export
#'
plot_supp_fig_10 <- function(data) {
  data.mca <- make_attitude_pca(data)
  data.hcpc <- make_attitude_clusters(data.mca)
  
  attitude_pca_plots <- make_attitude_pca_plots(data.mca)
  attitude_cluster_plots <- make_attitude_cluster_plots(data.hcpc)
  
  supp_fig_10c <- make_supp_fig_10c(data.hcpc)

  design <- c(
    area(1, 1, 2, 2),
    area(1, 3, 1, 3),
    area(2, 3, 2, 3),
    area(1, 4, 2, 5),
    area(3, 1, 5, 5)
  )
  
  design <- c(
    area(1, 1, 2, 3),
    area(1, 4, 2, 4),
    area(1, 5, 2, 5),
    area(3, 4, 4, 5),
    area(3, 1, 4, 3)
  )
  plot(design)
  
  supp_fig_10 <- attitude_cluster_plots$clust_plot+
    attitude_cluster_plots$dend_plot +
    attitude_pca_plots$screeplot +
    attitude_pca_plots$biplot +
    supp_fig_10c + 
    plot_layout(design = design)+
    plot_annotation(tag_levels = 'a',tag_prefix = "(",tag_suffix = ")")
  
  
  return(supp_fig_10)
}
