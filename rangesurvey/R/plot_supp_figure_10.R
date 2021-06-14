plot_supp_figure_10 <- function(){
  
  
  supp_fig_10c <- make_supp_fig_10c()
  
  return(supp_figure_10)
}
  
make_attitude_pca <- function(data = survey_responses_to_model  ) {
  
  data <- data %>%
    dplyr::select(Remove, Mitigate, Accept, Adapt, Support, id)%>%
    dplyr::mutate_all(.,as.factor) %>% 
    dplyr::mutate(Remove = fct_collapse(Remove,"Positive" = c("5", "6", "7"),
                                 "Neutral" = c("4"),
                                 "Negative" = c("1", "2", "3")),
           Mitigate = fct_collapse(Mitigate,"Positive" = c("5", "6", "7"),
                                   "Neutral" = c("4"),
                                   "Negative" = c("1", "2", "3")),
           Accept = fct_collapse(Accept,"Positive" = c("5", "6", "7"),
                                 "Neutral" = c("4"),
                                 "Negative" = c("1", "2", "3")),
           Adapt = fct_collapse(Adapt,"Positive" = c("5", "6", "7"),
                                "Neutral" = c("4"),
                                "Negative" = c("1", "2", "3")),
           Support = fct_collapse(Support,"Positive" = c("5", "6", "7"),
                                  "Neutral" = c("4"),
                                  "Negative" = c("1", "2", "3")),
    ) %>% 
    na.omit()
  

  data.mca <- FactoMineR::MCA(dataNArmAll[,-6],
                              graph = T,
                              ncp = Inf)
  
  data.hcpc <- FactoMineR::HCPC(data.mca,
                                nb.clust = 4)
  
  return(data.hcpc)
}

make_supp_fig_10c <- function(data){

linedata <- data.hcpc$data.clust%>%
  dplyr::mutate(id = row.names(data.hcpc$data.clust))%>%
  dplyr::pivot_longer(cols = c("Remove", "Mitigate", "Accept", "Adapt", "Support"),
               values_to = "Q"
  )%>%
  dplyr::mutate(Q = gsub(x = Q, pattern = ".*_", replacement = "")) %>%
  dplyr::mutate(name = factor(name, levels = c("Support", "Adapt", "Accept", "Mitigate", "Remove"))
  )


labels <- c("1" = "Cluster 1: Support Colonists",
            "2" = "Cluster 2: Wary of Colonists", 
            "3" = "Cluster 3: Non-Intervention",
            "4" = "Cluster 4: Neutral")

lineplot <- ggplot(data = linedata,
       aes(x = name, y = Q, group = id))+
  geom_line(position = position_jitter(0.1, 0.1))+
  facet_wrap( ~ clust,
             scales = "free_y",
             labeller = labeller(clust = labels))

return(lineplot)
}
