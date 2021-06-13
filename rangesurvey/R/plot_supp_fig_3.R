#' plot_supp_fig_3
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
plot_supp_fig_3 <- function(data) {
  
  
  data_awareness_scored <- data %>% 
    score_awareness()
  
  data_awareness_scored <- data_awareness_scored %>%
  dplyr::group_by(awareness_score, Taxa) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(Taxa = dplyr::case_when(
    stringr::str_detect(string = Taxa, pattern = "Other") ~ "Other",
    !is.na(Taxa) ~ Taxa,
    is.na(Taxa) ~ "Not Applicable"
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

  data_awareness_scored$awareness_score <- factor(data_awareness_scored$awareness_score, levels = rev(c("Correctly\nNamed",
                                                                           "Incorrectly\nNamed",
                                                                           "Didn't\nEvidence",
                                                                           "Don't know/\nDidn't answer")))

awareness_plot <- ggplot(data_awareness_scored, aes(
  x = awareness_score,
  y = count,
  fill = Taxa
)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Number of Respondents") +
  scale_x_discrete(name = element_blank())+
  scale_y_continuous(position = "right",expand = expand_scale(add = c(0,30)))+
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
  )+
  theme(plot.margin = unit(c(1,0,1,0), "mm"),
        axis.text = element_text(colour = "black", lineheight = 0.8, size = 10),
        legend.text = element_text(colour = "black", size = 10),
        legend.title = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour ="black",size = 0.75),
        panel.grid.minor.x = element_line(colour ="black",size = 0.5),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_line(colour ="black", size = 1),
        axis.line.y = element_line(colour ="black", size = 1)
  )

return(awareness_plot)
}  

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
score_awareness <- function(data = respondent_table_clean){
  
  data_awareness_scored <- data %>% 
    dplyr::select(id,
                  awareness,
                  Category,
                  SpeciesIdentified,
                  Taxa,
                  SpeciesNamed,
                  ) %>% 
    dplyr::mutate(awareness_score = dplyr::case_when(awareness == "No, I haven't heard of this phenomenon." ~ "Don't know/\nDidn't answer",
                                                     awareness == "Yes, I am familiar with this phenomenon but couldn't name a specific example." & is.na(Category)  ~ "Didn't\nEvidence",
                                                     awareness == "Yes, I am familiar with this phenomenon but couldn't name a specific example." & Category == "Colonist"  ~ "Correctly\nNamed", #Error in survey
                                                     awareness == "Yes, I am familiar with this phenomenon & I can name ONE species which has established in the UK." & Category == "Colonist"  ~ "Correctly\nNamed",
                                                     awareness == "Yes, I am familiar with this phenomenon & I can name ONE species which has established in the UK." & Category != "Colonist" ~ "Incorrectly\nNamed",
                                                     is.na(awareness) &  Category == "Colonist"  ~ "Correctly\nNamed",
                                                     is.na(awareness) &  Category != "Colonist"  ~ "Incorrectly\nNamed",
                                                     is.na(awareness) &  is.na(Category) != "Colonist"  ~ "Don't know/\nDidn't answer",
                                                     awareness == "Yes, I am familiar with this phenomenon & I can name ONE species which has established in the UK." & is.na(Category) ~ "Didn't\nEvidence",
                                                     T ~ "Look at these")) %>% 
    dplyr::mutate(Taxa = dplyr::case_when(awareness_score == "Didn't\nEvidence" ~ NA_character_,
                                          T ~ Taxa)) %>% 
    dplyr::select(id,
                  Taxa,
                  awareness_score)
  
  return(data_awareness_scored)
}
