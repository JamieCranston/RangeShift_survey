#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
plot_supp_figure_10 <- function(data = management_responses_to_model){
  
  data.hcpc <- make_attitude_pca(data)
  supp_fig_10c <- make_supp_fig_10c(data = data.hcpc)
  
  supp_fig_10 <-supp_fig_10c
  return(supp_fig_10)
}
  
#' make_attitude_pca
#'
#' @param data 
#'
#' @return
#' @export
#'

make_attitude_pca <- function(data = management_responses_to_model) {
  
  data.mca <- data %>% 
    dplyr::select( -id, -species, - attitude_to_species, -seen, - match) %>% 
    na.omit() %>% 
    FactoMineR::MCA(X = ., 
                    graph = TRUE,
                    ncp = Inf)
  
  data.hcpc <- FactoMineR::HCPC(data.mca,
                                nb.clust = 4)
  
  return(data.hcpc)
}

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
make_supp_fig_10c <- function(data= data.hcpc){

linedata <- data$data.clust%>%
  dplyr::mutate(id = row.names(data$data.clust))%>%
  tidyr::pivot_longer(cols = c("Remove", "Mitigate", "Accept", "Adapt", "Support"),
               values_to = "Q") %>%
  dplyr::mutate(Q = gsub(x = Q, pattern = ".*_", replacement = "")) %>%
  dplyr::mutate(name = factor(name, levels = c("Support", "Adapt", "Accept", "Mitigate", "Remove"))
  )


labels <- c("1" = "Cluster 1: Support Colonists",
            "2" = "Cluster 2: Wary of Colonists", 
            "3" = "Cluster 3: Non-Intervention",
            "4" = "Cluster 4: Neutral")

lineplot <- ggplot(data = linedata,
       aes(x = name,
           y = Q,
           group = id))+
  geom_line(position = position_jitter(0.1, 0.1))+
  facet_wrap( ~ clust,
             scales = "free_y",
             labeller = labeller(clust = labels))

return(lineplot)
}

