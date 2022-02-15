#' plot_supp_fig_3
#'
#' @param data survey data (respondent cleaned)
#'
#' @return supplementary figure 3
#' @export
plot_supp_fig_3 <- function(data) {
  data_awareness_scored <- data %>%
    score_awareness()

  data_awareness_scored <- data_awareness_scored %>%
    dplyr::group_by(.data$awareness_score, .data$Taxa) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(Taxa = dplyr::case_when(
      stringr::str_detect(string = .data$Taxa, pattern = "Other") ~ "Other",
      !is.na(.data$Taxa) ~ .data$Taxa,
      is.na(.data$Taxa) ~ "Not Applicable"
    ))

  data_awareness_scored$Taxa <- factor(data_awareness_scored$Taxa, levels = c(
    "Bird",
    "Mammal",
    "Coleoptera",
    "Hemiptera",
    "Hymenoptera",
    "Lepidoptera",
    "Odonata",
    "Plant",
    "Other",
    "Not Applicable"
  ))

  data_awareness_scored$awareness_score <- factor(data_awareness_scored$awareness_score, levels = rev(c(
    "Correctly\nNamed",
    "Incorrectly\nNamed",
    "Didn't\nEvidence",
    "Don't know/\nDidn't answer"
  )))

  awareness_plot <- ggplot(data_awareness_scored, aes(
    x = .data$awareness_score,
    y = .data$count,
    fill = .data$Taxa
  )) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Number of Respondents") +
    scale_x_discrete(name = element_blank()) +
    scale_y_continuous(position = "right",
                       expand = expand_scale(add = c(0, 30))) + #todo update deprecated function
    scale_fill_manual(
      values = c(
        "Bird" = "#88CCEE",
        "Coleoptera" = "#44AA99",
        "Hemiptera" = "#117733",
        "Hymenoptera" = "#332288",
        "Lepidoptera" = "#DDCC77",
        "Mammal" = "#999933",
        "Odonata" = "#CC6677",
        "Other" = "#AA4499",
        "Plant" = "#882255",
        "Not Applicable" = "gray26"
      ),
      name = "Taxa"
    ) +
    theme(
      plot.margin = unit(c(1, 0, 1, 0), "mm"),
      axis.text = element_text(colour = "black", lineheight = 0.8, size = 10),
      legend.text = element_text(colour = "black", size = 10),
      legend.title = element_text(colour = "black", size = 12),
      axis.title = element_text(colour = "black", size = 12),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(colour = "black", size = 0.75),
      panel.grid.minor.x = element_line(colour = "black", size = 0.5),
      panel.grid.major.y = element_blank(),
      axis.line.x = element_line(colour = "black", size = 1),
      axis.line.y = element_line(colour = "black", size = 1)
    )

  return(awareness_plot)
}
