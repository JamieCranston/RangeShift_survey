#' render_supp_figures_doc
#'
#' @param respondent_frame 
#' @param species_frame 
#' @param management_model 
#' @param management_responses_to_model
#' @param config 
#' @param html_file_dir_out 
#'
#' @return writes a word doc containing the supplementary figures
#' @export
render_supp_figures_doc <- function(respondent_frame,
                                    species_frame,
                                    management_model,
                                    management_responses_to_model,
                                    config,
                                    html_file_dir_out = "./doc"){
  
  figures <- generate_supp_figures(respondent_frame,
                                   species_frame,
                                   management_model = management_model,
                                   management_responses_to_model = management_responses_to_model,
                                   config = config)
  
  rmarkdown::render(input = "rmarkdown/supp_figures.Rmd",
                    output_dir = html_file_dir_out,
                    output_file =  "supp_figures.docx",
                    output_format = "all",
                    quiet = FALSE,
                    params = list(figures = figures)
  ) 
}