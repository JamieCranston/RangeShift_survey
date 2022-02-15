
#' plot_fig_1
#'
#' @param speciesdata survey_species_responses
#'
#' @return figure 1 (likert lattice plot)
#' @importFrom HH likert likertColor
#' @export
#'
#'
plot_fig_1 <- function(speciesdata) {
  list_vars <- list(
    c("attitude_to_species"),
    c("attitude_to_species", "seen"),
    c("attitude_to_species", "match")
  )

  att <- speciesdata %>%
    dplyr::select(.data$attitude_to_species) %>%
    dplyr::group_by(.data$attitude_to_species) %>%
    stats::na.omit()

  att_seen <- speciesdata %>%
    dplyr::select(.data$attitude_to_species, .data$seen) %>%
    dplyr::group_by(.data$attitude_to_species, .data$seen) %>%
    stats::na.omit()

  att_match <- speciesdata %>%
    dplyr::select(.data$attitude_to_species, .data$match) %>%
    dplyr::group_by(.data$attitude_to_species, .data$match) %>%
    stats::na.omit()

  attitudes_list <- lapply(list(att, att_seen, att_match), function(X) {
    data <- X %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      tidyr::pivot_wider(
        .,
        names_from = .data$attitude_to_species,
        values_from = .data$Count,
        values_fill = list(Count = 0)
      )
    return(data)
  })
  
  names(attitudes_list) <- c("All_Responses", "Seen", "Match")
  
  combined_data <- dplyr::bind_rows(attitudes_list[c("Seen", "Match")]) %>%
    tidyr::pivot_longer(
      cols = c("seen", "match"),
      names_to = "type",
      values_to = "attribute",
      values_drop_na = TRUE
    ) %>%
    dplyr::bind_rows(cbind(attitudes_list[["All_Responses"]], type = "all responses", attribute = "")) %>%
    dplyr::relocate(
      "Strongly Negative",
      "Quite Negative",
      "A Bit Negative",
      "Neutral",
    )
  
  combined_data <- combined_data %>%
    dplyr::mutate(
      type = dplyr::case_when(
        type == "all responses" ~ "",
        TRUE ~ stringr::str_to_sentence(type)
      ),
      attribute = dplyr::case_when(
        type == "" ~ "All Survey\nResponses",
        type == "Match" & attribute == "Yes" ~ "Group\nrecorded",
        type == "Match" & attribute == "No" ~ "Group not\nrecorded",
        TRUE ~ attribute
      )) %>%
        dplyr::arrange(type)
  
 names(combined_data) <- c("Strongly\nNegative",
                           "Quite\nNegative",
                           "A Bit\nNegative",
                           "Neutral",
                           "A Bit\nPositive",
                           "Quite\nPositive",
                           "Strongly\nPositive",
                           "type",
                           "attribute") 

  fig_1 <- HH::likert(
    attribute ~ . | type,
    as.percent = TRUE,
    ReferenceZero = 4,
    between = list(y = 0),
    data = combined_data,
    strip = FALSE,
    strip.left = lattice::strip.custom(bg = "gray97"),
    positive.order = T,
    main = "How do you feel about this species establishing in the UK?",
    layout = c(1, 3),
    scales = list(y = list(relation = "free")),
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
      text = list(rev(names(combined_data)[1:7])),
      title = "Attitude",
      cex = 0.9,
      cex.title = 1.2
    ),
    ylab = NULL,
    reference.line.col = "black",
    rightAxis = F,
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
    h.resizePanels = c(1, 2, 2)
  )
  

  return(fig_1)
}



#' plot_fig_1
#'
#' @param speciesdata survey_species_responses
#'
#' @return figure 1 (likert lattice plot)
#' @importFrom HH likert likertColor
#' @export
#'
#'
plot_fig_1_test <- function(speciesdata) {
  list_vars <- list(
    c("attitude_to_species"),
    c("attitude_to_species", "seen"),
    c("attitude_to_species", "match")
  )
  
  att <- speciesdata %>%
    dplyr::select(.data$attitude_to_species) %>%
    dplyr::group_by(.data$attitude_to_species) %>%
    stats::na.omit()
  
  att_seen <- speciesdata %>%
    dplyr::select(.data$attitude_to_species, .data$seen) %>%
    dplyr::group_by(.data$attitude_to_species, .data$seen) %>%
    stats::na.omit()
  
  att_match <- speciesdata %>%
    dplyr::select(.data$attitude_to_species, .data$match) %>%
    dplyr::group_by(.data$attitude_to_species, .data$match) %>%
    stats::na.omit()
  
  attitudes_list <- lapply(list(att, att_seen, att_match), function(X) {
    data <- X %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      tidyr::pivot_wider(
        .,
        names_from = .data$attitude_to_species,
        values_from = .data$Count,
        values_fill = list(Count = 0)
      )
    return(data)
  })
  
  names(attitudes_list) <- c("All_Responses", "Seen", "Match")
  
  combined_data <- dplyr::bind_rows(attitudes_list[c("Seen", "Match")]) %>%
    tidyr::pivot_longer(
      cols = c("seen", "match"),
      names_to = "type",
      values_to = "attribute",
      values_drop_na = TRUE
    ) %>%
    dplyr::bind_rows(cbind(attitudes_list[["All_Responses"]], type = "all responses", attribute = "")) %>%
    dplyr::relocate(
      "Strongly Negative",
      "Quite Negative",
      "A Bit Negative",
      "Neutral",
    )
  
  combined_data <- combined_data %>%
    dplyr::mutate(
      type = dplyr::case_when(
        type == "all responses" ~ "",
        TRUE ~ stringr::str_to_sentence(type)
      ),
      attribute = dplyr::case_when(
        type == "" ~ "All/nSurvey Responses",
        type == "Match" & attribute == "Yes" ~ "Group\nrecorded",
        type == "Match" & attribute == "No" ~ "Group not\nrecorded",
        TRUE ~ attribute
      )) %>%
    dplyr::arrange(type)
  
  
  
  fig_1 <- HH::likert(
    attribute ~ . | type,
    as.percent = TRUE,
    ReferenceZero = 4,
    between = list(y = 0),
    data = combined_data,
    strip = FALSE,
    strip.left = lattice::strip.custom(bg = "gray97"),
    positive.order = T,
    main = "How do you feel about this species establishing in the UK?",
    layout = c(1, 3),
    scales = list(y = list(relation = "free")),
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
      text = list(rev(names(combined_data)[1:7])),
      title = "Attitude",
      cex = 0.9,
      cex.title = 1.2
    ),
    ylab = NULL,
    reference.line.col = "black",
    rightAxis = F,
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
    h.resizePanels = c(1, 2, 2)#,
    #panel = my_func
  )
  
  
  return(fig_1)
}

 
my_func <-  function(...){
  panel.likert(...)
  DF <- data.frame(correctX = rep(c(-50, 0, 75), each = 5),
             abs = rep(c(-50, 0, 75), each = 5),
             perc = c(c(4.1, 2.9, 5, 3.2, 5.9),
                      c(35.6, 15.6, 47.2, 28.2, 47.9),
                      c(60.3, 81.5, 47.9, 68.7, 46.2)),
             y =  rep(5:1, times = 3))
  panel.text(x = DF$correctX,
             y = DF$y,
             label = paste0(DF$perc,'%'), cex=0.7)
}
