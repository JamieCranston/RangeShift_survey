
#' plot_supp_fig_6
#'
#' @param speciesdata cleaned attitude to species responses
#'
#' @return supplementary figure 6
#' @importFrom HH likert likertColor
#' @export
plot_supp_fig_6 <- function(speciesdata = survey_responses_to_model) {
  data <- speciesdata %>%
    dplyr::select(.data$species, .data$attitude_to_species) %>%
   stats::na.omit() %>%
    dplyr::group_by(.data$., .data$species, .data$attitude_to_species) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(.data$species,
                       .data$attitude_to_species,
      values_from = .data$n,
      values_fill  = 0
    ) %>%
    dplyr::mutate(species = sapply(.data$species, stringr::str_replace, pattern = "(^.)", replacement = "\\1. ")) %>%
    dplyr::relocate("species", "Strongly Negative", "Quite Negative", "A Bit Negative", "Neutral", "A Bit Positive", "Quite Positive", "Strongly Positive") %>%
    data.frame()

  row.names(data) <- data$species

  species_attitude_plot <- HH::likert(data,
    xlab = "Percentage", # scales = list(y = list(labels = rev(percentage2$Group))),
    main = "Attitude across Species", xlim = c(-100, 100), as.percent = TRUE,
    positive.order = TRUE,
    rightAxisLabels = NULL,
    key = list(
      # space = "right",
      corner = c(0.05, 0.95),
      rect = list(
        col = rev(HH::likertColor(n = 7)),
        size = 1.5,
        border = "white"
      ),
      columns = 1,
      text = list(rev(names(data)[2:8])),
      title = "Attitude",
      cex = 1,
      cex.title = 1.2
      # border ="black"
      # background = " light grey"
    ),
    reference.line.col = "black",
    rightAxis = F,
    xlim = c(-100, 100),
    par.settings.in = list(
      axis.text = list(
        cex = 1,
        lineheight = 0.8 # ,
        # font = 3
      ),
      axis.components.left.tck = 1.5,
      # increase the size of the x axis tick labels
      par.xlab.text = list(cex = 1.2), # increase the size of the x axis label
      # layout.widths=list(ylab.left=1, left.padding=0)
      layout.heights = list(
        bottom.padding = 0,
        top.padding = 0,
        axis.bottom = 0.75,
        axis.top = 0.5
      )
    )
  )

  species_attitude_plot$y.scales$font <- 3
  return(species_attitude_plot)
}
