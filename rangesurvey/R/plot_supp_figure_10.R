#' plot_supp_figure_10
#'
#' @param data management_responses_to_model
#'
#' @return supplementary figure 10 (outputs of clustering method)
#' @export
#'
plot_supp_figure_10 <- function(data) {
  data.hcpc <- make_attitude_pca(data)
  supp_fig_10c <- make_supp_fig_10c(data.hcpc)

  supp_fig_10 <- supp_fig_10c
  return(supp_fig_10)
}
