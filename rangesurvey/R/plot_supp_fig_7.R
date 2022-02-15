#' plot_supp_fig_7
#'
#' @param data cleaned respondent data
#'
#' @return supplementary figure 7
#' @import ggplot2
#' @export
plot_supp_fig_7 <- function(data) {
  data <- fill_imputed_species_group(data)

  data$Other <- ifelse(data$Diptera == "Yes", "Yes", data$Other)

  data <- data %>%
    dplyr::select(
      c(
        "Birds",
        "Mammals",
        "Plants",
        "Hymenoptera",
        "Coleoptera",
        "Lepidoptera",
        "Odonata",
        "Other",
        "Hemiptera",
        "NoSpGroups"
      )
    ) %>%
    tidyr::pivot_longer(1:10) %>%
    dplyr::group_by(.data$name, .data$value) %>%
    dplyr::count() %>%
    dplyr::filter(.data$value == "Yes") %>%
    dplyr::mutate(n = round(.data$n / 311 * 100, 2))

  plot <- data %>%
    ggplot() +
    geom_bar(stat = "identity", aes(
      x = stats::reorder(.data$name, .data$n),
      y = .data$n,
      fill = .data$name
    )) +
    geom_label(aes(
      x = stats::reorder(.data$name, .data$n),
      y = .data$n + 5,
      label = .data$n
    )) +
    ylab("% of Respondents recording each group") +
    xlab("Taxon Group") +
    guides(fill = "none",
           colour = "none") +
    scale_fill_manual(
      values = c(
        "Birds" = "#88CCEE",
        "Coleoptera" = "#44AA99",
        "Hemiptera" = "#117733",
        "Hymenoptera" = "#332288",
        "Lepidoptera" = "#DDCC77",
        "Mammals" = "#999933",
        "Odonata" = "#CC6677",
        "Other" = "#AA4499",
        "Plants" = "#882255",
        "NoSpGroups" = "gray26"
      )
    ) +
    coord_flip() +
    theme(
      plot.margin = unit(c(1, 0, 1, 0), "mm"),
      axis.text = element_text(
        colour = "black",
        lineheight = 0.8,
        size = 10
      ),
      legend.text = element_text(colour = "black", size = 10),
      legend.title = element_text(colour = "black", size = 12),
      axis.title = element_text(colour = "black", size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(colour = "black", size = 0.75),
      panel.grid.minor.x = element_line(colour = "black", size = 0.5),
      panel.grid.major.y = element_blank(),
      axis.line.x = element_line(colour = "black", size = 1),
      axis.line.y = element_line(colour = "black", size = 1)
    ) +
    scale_y_continuous(
      position = "left",
      expand = expansion(add = c(0, 5)),
      limits = c(0, 100)
    )
  return(plot)
}
