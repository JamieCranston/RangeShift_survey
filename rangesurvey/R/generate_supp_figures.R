#' generate_supp_figures
#'
#' @param respondent_frame 
#' @param species_frame 
#' @param management_model
#' @param management_responses_to_model
#' @param config 
#'
#' @return a list containing all supplementary figures
#' @export
generate_supp_figures <- function(respondent_frame,
                                  species_frame,
                                  management_model,
                                  management_responses_to_model,
                                  config) {
  supp_figures <- list( 
  supp_fig_1 = plot_supp_fig_1(data = respondent_frame,
                               config = config),
  supp_fig_2 = plot_supp_fig_2(data = respondent_frame,
                   config = config),
  supp_fig_3 = plot_supp_fig_3(data = respondent_frame),
  supp_fig_4 = plot_supp_fig_4(data = respondent_frame),
  supp_fig_5 = plot_supp_fig_5(data = respondent_frame,
                   config =  config),
  supp_fig_6 = plot_supp_fig_6(speciesdata = species_frame),
  supp_fig_7 = plot_supp_fig_7(data = respondent_frame),
  supp_fig_8 = plot_supp_fig_8(data = management_model),
  supp_fig_9 = plot_supp_fig_9(model = management_model),
  #TODO turn supp fig 10 into something which doesn't look hodge-podge
  #http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization
  supp_fig_10 = plot_supp_fig_10(data = management_responses_to_model)
  )
  
  return(supp_figures)
}
