#' Title
#'
#' @param data brms model object of the management multivariate model
#'
#' @return supplementary figure 8 (ggplot)
#' @import ggplot2
#' @import cubelyr
#' @export
plot_supp_fig_8 <- function(data) {

  predicted_management_attitudes_by_id <- brms::posterior_predict(data,
    type = "bars",
    nsamples = NULL
  )

  predicted_management_attitudes <- apply(predicted_management_attitudes_by_id,
    MARGIN = c(1, 3),
    FUN = function(X) prop.table(table(X))
  )

  predicted_management_attitudes_quantiles <- apply(predicted_management_attitudes,
    MARGIN = c(1, 3),
    FUN = function(X) stats::quantile(X, probs = c(0.025, 0.5, 0.975))
  )

  names(dimnames(predicted_management_attitudes_quantiles)) <- c(
    "proportion",
    "attitude",
    "management_option"
  )

  predicted_management_attitudes_quantiles <- predicted_management_attitudes_quantiles %>%
    cubelyr::as.tbl_cube() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(attitude = factor(.data$attitude,
      levels = c(2, 1, 3),
      labels = c("Anti", "Neutral", "Pro")
    )) %>%
    tidyr::pivot_wider(.,
      names_from = "proportion",
      values_from = .data$`.`
    )

  predicted_management_attitudes_quantiles <- predicted_management_attitudes_quantiles %>%
    dplyr::mutate(management_option = forcats::fct_recode(as.factor(.data$management_option),
      "Support\nRange-Shifters" = "Support",
      "Adapt to\nRange-Shifters" = "Adapt",
      "Non-\nIntervention" = "Accept",
      "Mitigate\nRange-Shifters'\nImpacts" = "Mitigate",
      "Remove\nRange-Shifters" = "Remove"
    )) %>%
    dplyr::mutate(management_option = factor(.data$management_option,
      levels = c(
        "Support\nRange-Shifters",
        "Adapt to\nRange-Shifters",
        "Non-\nIntervention",
        "Mitigate\nRange-Shifters'\nImpacts",
        "Remove\nRange-Shifters"
      )
    ))


  plotted_attitudes_on_management <- ggplot(predicted_management_attitudes_quantiles) +
    geom_point(aes(color = .data$attitude,
                   x = .data$attitude,
                   y = .data$`50%`)) +
    geom_errorbar(aes(color = .data$attitude,
                      x = .data$attitude,
                      ymin = .data$`2.5%`,
                      y = .data$`50%`,
                      ymax = .data$`97.5%`)) +
    facet_grid(~management_option,
      scales = "free_x"
    ) +
    ylab("Probability of Attitude") +
    ylim(c(0, 1)) +
    scale_colour_manual(
      values = c("#C24641", "#736F6E", "#6495ED"),
      guide = guide_legend(
        title.position = "top"
      )
    ) +
    theme(
      axis.text = element_text(size = 8),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_text(size = 8.5, face = "bold"),
      axis.title.y = element_text(size = 9),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.margin = margin(0.1, 0, 0.1, 0, "cm"),
      legend.spacing = unit(0, "mm"),
      legend.key.width = unit(5.0, "mm"),
      legend.key.height = unit(5.0, "mm"),
      panel.spacing = unit(1, "mm"),
      panel.grid.major.x = element_blank(),
      plot.margin = unit(c(1, 1, 1, 0), "mm")
    )

  return(plotted_attitudes_on_management)
}

