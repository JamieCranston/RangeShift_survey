#' plot_fig_3
#'
#' @param speciesdata attitudes to species management dataset
#'
#' @return
#' @importFrom HH likert likertColor
#' @export
#'
#'
plot_fig_3 <- function(speciesdata) {
  ManagementResponses <- speciesdata %>%
    dplyr::select(.data$Remove, .data$Mitigate, .data$Accept, .data$Adapt, .data$Support) %>%
    stats::na.omit() %>%
    tidyr::pivot_longer(
      cols = c(.data$Remove, .data$Mitigate, .data$Accept, .data$Adapt, .data$Support),
      names_to = "MO"
    ) %>%
    dplyr::group_by(.data$MO, .data$value) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = .data$value, values_from = .data$Count) %>%
    # dplyr::rename(
    #   "Very Negative" = "1",
    #   "Quite Negative" = "2",
    #   "A Bit Negative" = "3",
    #   "Neutral" = "4",
    #   "A Bit Positive" = "5",
    #   "Quite Positive" = "6",
    #   "Very Positive" = "7"
    # ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Type = "Management Option")

  ManagementResponses <- ManagementResponses[c(5, 2, 1, 3, 4), ] %>%
    dplyr::relocate("Strongly Negative",
                    "Quite Negative",
                    "A Bit Negative",
                    "Neutral",
    )
    

  fig_3 <- HH::likert(
    MO ~ . | Type,
    as.percent = TRUE,
    ReferenceZero = 4,
    between = list(y = 0),
    data = ManagementResponses,
    strip = FALSE,
    strip.left = lattice::strip.custom(bg = "gray97"),
    positive.order = FALSE,
    main = "How do you feel about the following management\noptions for species establishing in the UK?",
    layout = c(1, 1),
    scales = list(y = list(relation = "free")),
    # auto.key = list(
    #   cex = 1.1,
    #   columns = 1,
    #   title = "Attitude",
    #   space = "right"
    # ),
    key = list(
      space = "bottom",
      between.columns = 0.2,
      between = 0.5,
      # corner= c(0.1,1),
      height = 0.3,
      adj = 0,
      rect = list(
        col = rev(HH::likertColor(n = 7)),
        size = 1.2,
        border = "white"
      ),
      columns = 7,
      text = list(rev(names(
        ManagementResponses
      )[2:8])),
      title = "Attitude",
      cex = 0.9,
      cex.title = 1.2
      # background = " light grey"
    ),
    # key = list(
    #   # space = "right",
    #   corner = c(0.95, 0.05),
    #   rect = list(
    #     col = rev(likertColor(n = 7)),
    #     size = 1.5,
    #     border = "white"
    #   ),
    #   columns = 1,
    #   text = list(rev(names(ManagementResponses)[2:8])),
    #   title = "Attitude",
    #   cex = 1,
    #   cex.title = 1.2
    #   # border ="black"
    #   # background = " light grey"
    # ),
    ylab = NULL,
    reference.line.col = "black",
    rightAxis = FALSE,
    xlim = c(-100, 100),
    par.settings.in = list(
      axis.text = list(
        cex = 1,
        lineheight = 0.8
      ),
      # increase the size of the x axis tick labels
      par.xlab.text = list(cex = 1.2),
      # increase the size of the x axis label
      # layout.widths=list(ylab.left=1, left.padding=0)
      layout.heights = list(
        bottom.padding = 0,
        top.padding = 0,
        axis.bottom = 0.75,
        axis.top = 0.5
      )
    ),
    par.strip.text = list(cex = 1.1),
    # increase the size of the strip labels
    # h.resizePanels = c(1, 2, 2.3, 2, 2)
  )
  return(fig_3)
}
