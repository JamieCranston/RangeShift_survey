#' Title
#'
#' @param data 
#'
#' @return
#' @import ggplot2
#' @export
#'
plot_supp_fig_7 <- function(data = respondent_table_clean) {
  
  data <- fill_imputed_species_group(data) 
  
  data$Other<- ifelse(data$Diptera == "Yes","Yes", data$Other)
  
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
    dplyr::group_by(name, value) %>%
    dplyr::count() %>%
    dplyr::filter(value == "Yes") %>%
    dplyr::mutate(n = round(n / 311 * 100, 2))
  
  plot <- data %>% 
    ggplot() +
    geom_bar(stat = "identity", aes(
      x = reorder(name, n),
      y = n,
      fill = name
    )) +
    geom_label(aes(
      x = reorder(name, n),
      y = n + 5,
      label = n
    )) +
    ylab("% of Respondents recording each group") +
    xlab("Taxon Group") +
    guides(fill = FALSE, colour = FALSE) +
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


#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
fill_imputed_species_group <- function(data) {
  
impute_sep <- data %>% 
    dplyr::select(id, imputed_group) %>% 
    tidyr::separate(.,
                    imputed_group,
                    sep = ";",
                    into  = c("imputed_1",
                              "imputed_2")
                    )

impute_sep <- impute_sep %>% 
  dplyr::filter(!dplyr::across(all_of(contains("imputed")),
                   ~ is.na(.x))
                )
imputed_merged_in <- dplyr::left_join(data,
                                      impute_sep) %>%
  dplyr::mutate(Plants = dplyr::case_when(imputed_1 == "Plants" |
                                            imputed_2 == "Plants"|
                                            imputed_1 == "All" |
                                            imputed_2 == "All" ~ "Yes",
                                             T ~ "No"),
                Other =  dplyr::case_when(imputed_1 == "Other" |
                                            imputed_2 == "Other"|
                                            imputed_1 == "All" |
                                            imputed_2 == "All" ~ "Yes",
                                         T ~ "No"),
                Odonata =  dplyr::case_when(imputed_1 == "Odonata" |
                                              imputed_2 == "Odonata"|
                                              imputed_1 == "All" |
                                              imputed_2 == "All"|
                                              Odonata == "Yes" ~ "Yes",
                                          T ~ "No"),
                Coleoptera =  dplyr::case_when(imputed_1 == "Coleoptera" |
                                                 imputed_2 == "Coleoptera"|
                                                 imputed_1 == "All" |
                                                 imputed_2 == "All"|
                                                 Coleoptera == "Yes" ~ "Yes",
                                            T ~ "No"),
                Lepidoptera =  dplyr::case_when(imputed_1 == "Lepidoptera" |
                                                  imputed_2 == "Lepidoptera"|
                                                  imputed_1 == "All" |
                                                  imputed_2 == "All"|
                                                  Lepidoptera == "Yes" ~ "Yes",
                                               T ~ "No"),
                Hemiptera =  dplyr::case_when(imputed_1 == "Hemiptera" |
                                                  imputed_2 == "Hemiptera"|
                                                imputed_1 == "All" |
                                                imputed_2 == "All"|
                                                Hemiptera== "Yes" ~ "Yes",
                                                T ~ "No"),
                Diptera =  dplyr::case_when(imputed_1 == "All" |
                                                imputed_2 == "All"|
                                              Diptera == "Yes" ~ "Yes",
                                              T ~ "No"),
                Hymenoptera =  dplyr::case_when(imputed_1 == "All" |
                                              imputed_2 == "All"|
                                                Hymenoptera == "Yes" ~ "Yes",
                                            T ~ "No"),
                Mammals =  dplyr::case_when(imputed_1 == "All" |
                                                  imputed_2 == "All"|
                                              Mammals == "Yes" ~ "Yes",
                                                T ~ "No"),
                Birds =  dplyr::case_when(imputed_1 == "All" |
                                              imputed_2 == "All"|
                                            Birds == "Yes" ~ "Yes",
                                            T ~ "No")
  ) %>% 
  dplyr::select(-imputed_1,
                -imputed_2)

return(imputed_merged_in)
}
 