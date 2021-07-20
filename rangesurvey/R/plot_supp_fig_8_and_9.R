#' Title
#'
#' @param data brms model object of the management multivariate model
#'
#' @return supplementary figure 8 (ggplot)
#' @import ggplot2
#' @import cubelyr
#' @export
plot_supp_fig_8 <- function(data) {
  data <- readRDS("D:/Academic_Work/PhD Thesis/Chapter_2/R_Analysis/Management_Condensed.Rds")

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
    #as_tibble.tbl_cube()
    tidyr::pivot_wider(.data$.,
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
    geom_point(aes(color = .data$attitude, x = .data$attitude, y = .data$`50%`)) +
    geom_errorbar(aes(color = .data$attitude, x = .data$attitude, ymin = .data$`2.5%`, y = .data$`50%`, ymax = .data$`97.5%`)) +
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


#' @title plot_supp_fig_9
#'
#' @param model Management attitudes brms model
#'
#' @return supplementary figure 9
#' @import ggplot2
#' @export 
plot_supp_fig_9 <- function(model) {
  
  model <- readRDS("testmodel.Rds")

  model_input_data <- model$data
  
  CE <- brms::conditional_effects(
    x = model,
    categorical = TRUE,
    effects = c("attitude_to_species"),
    conditions = data.frame(attitude_to_species = c(
      "Negative",
      "Neutral",
      "Positive"
    ))
  )

  # Extract data to plot in ggplot
  PredictionData <- list(
    plot(CE,
      plot = FALSE,
      ask = FALSE
    )[[1]]$data,
    plot(CE,
      plot = FALSE,
      ask = FALSE
    )[[2]]$data,
    plot(CE,
      plot = FALSE,
      ask = FALSE
    )[[3]]$data,
    plot(CE,
      plot = FALSE,
      ask = FALSE
    )[[4]]$data,
    plot(CE,
      plot = FALSE,
      ask = FALSE
    )[[5]]$data
  )

  names(PredictionData) <- names(CE)

  PredictionData <- do.call("rbind", PredictionData)

  PredictionData$MO <- gsub(row.names(PredictionData),
    replacement = "",
    pattern = "_Attitude:cats__|[0-9]"
  )

  PredictionData$attitude_to_species <- factor(PredictionData$attitude_to_species,
    levels = levels(PredictionData$attitude_to_species)[c(1, 3, 2)]
  )

  PredictionData$MO <- factor(PredictionData$MO,
    levels = levels(as.factor(PredictionData$MO))[c(5, 2, 1, 3, 4)],
    ordered = TRUE
  )

  PredictionData$MO <- forcats::fct_relabel(PredictionData$MO, ~ gsub(x = ., replacement = "", pattern = "\\..*"))

  Ns <-  model_input_data %>%
    tidyr::pivot_longer(
      cols = c(.data$Accept, .data$Adapt, .data$Mitigate, .data$Remove, .data$Support),
      names_to = "MO"
    ) %>%
    dplyr::group_by(.data$MO, .data$attitude_to_species, .data$value) %>%
    dplyr::count()

  PD <- PredictionData %>%
    dplyr::select(
      .data$attitude_to_species,
      .data$MO,
      .data$cats__,
      .data$lower__,
      .data$upper__,
      .data$estimate__
    )

  PD <- dplyr::left_join(PD,
    Ns,
    by = c(
      "cats__" = "value",
      "attitude_to_species" = "attitude_to_species",
      "MO" = "MO"
    )
  )

  PD$MO <- forcats::fct_recode(as.factor(PD$MO),
    "Support\nRange-Shifters" = "Support",
    "Adapt to\nRange-Shifters" = "Adapt",
    "Non-\nIntervention" = "Accept",
    "Mitigate\nRange-Shifters'\nImpacts" = "Mitigate",
    "Remove\nRange-Shifters" = "Remove"
  )

  PD$MO <- factor(PD$MO, levels = c(
    "Support\nRange-Shifters",
    "Adapt to\nRange-Shifters",
    "Non-\nIntervention",
    "Mitigate\nRange-Shifters'\nImpacts",
    "Remove\nRange-Shifters"
  ))

  PD <- PD %>%
    dplyr::mutate(Freq = dplyr::case_when(
      n < 10 ~ "1-9",
      n > 9 & n < 50 ~ "10-49",
      n > 49 & n < 150 ~ "50-149",
      n > 149 & n < 250 ~ "150-249",
      T ~ "250-750"
    ))

  PD$Freq <- as.factor(PD$Freq)
  PD$Freq <- factor(PD$Freq,
    levels = c(
      "1-9",
      "10-49",
      "50-149",
      "150-249",
      "250-750"
    )
  )
  PD$cats__ <- forcats::fct_relevel(PD$cats__, ... = c("Anti", "Neutral", "Pro"))

  MOModel <- ggplot(PD) +
    geom_pointrange(aes(x = .data$cats__, ymin = .data$lower__, ymax = .data$upper__, y = .data$estimate__, colour = .data$cats__, lty = .data$Freq, shape = .data$Freq)) +
    geom_point(aes(x = .data$cats__, y = .data$estimate__, colour = .data$cats__, shape = .data$Freq), size = 0.4) +
    geom_errorbar(aes(x = .data$cats__, ymin = .data$lower__, ymax = .data$upper__, y = .data$estimate__, colour = .data$cats__, lty = .data$Freq)) +
    facet_grid(MO ~ attitude_to_species) +
    theme(
      axis.text = element_text(size = 8),
      axis.title.x = element_text(vjust = -4),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_text(size = 8.5, face = "bold"),
      axis.title.y = element_text(size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.margin = margin(0.1, 0, 0.1, 0, "cm"),
      legend.title = element_blank(),
      legend.spacing = unit(0, "mm"),
      legend.key.width = unit(5.0, "mm"),
      legend.key.height = unit(5.0, "mm"),
      panel.spacing = unit(1, "mm"),
      panel.grid.major.x = element_blank(),
      plot.margin = unit(c(1, 1, 1, 0), "mm")
    ) +
    ylab("Probability of attitude") +
    xlab("Attitude") +
    labs(colour = "Attitude") +
    ylim(c(0, 1)) +
    scale_colour_manual(values = c("Anti" = "#C24641", "Neutral" = "#736F6E", "Pro" = "#6495ED")) +
    scale_shape_manual(values = c("1-9" = 21, "10-49" = 16, "50-149" = 17, "150-249" = 18, "250-750" = 15)) +
    scale_linetype_manual(values = c("1-9" = 3, "10-49" = 2, "50-149" = 5, "150-249" = 5, "250-750" = 1))

  return(MOModel)
}
