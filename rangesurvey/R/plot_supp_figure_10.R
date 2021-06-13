plot_supp_figure_10 <- function(data) {
  
}

setwd("D:/Work/Chapter_2/")
library(FactoMineR)
library(tidyverse)

data <- readxl::read_xlsx("D:/Work/Chapter_2/IdBySpecies07052020.xlsx")

data <- data %>%
  select(Remove, Mitigate, Accept, Adapt, Support, Q_ID)%>%
  mutate_all(.,as.factor)

dataNA <- filter(data, is.na(Remove) & is.na(Mitigate) & is.na(Accept) & is.na(Adapt) & is.na(Support))
dataNArm <- filter(data, !(is.na(Remove) & is.na(Mitigate) & is.na(Accept) & is.na(Adapt) & is.na(Support)))

dataNArmAll <- na.omit(data)



data.mca = MCA(dataNArmAll[,-6], graph=T, ncp = Inf)

summary(data.mca)

data.hcpc <- HCPC(data.mca)

data.hcpc$call$t

data.hcpc$data.clust%>%View()

data.hcpc$desc.axes

data.hcpc$desc.ind


# PCA approach ------------------------------------------------------------

dataNArmAll <-  mutate_at(dataNArmAll, vars(!matches("Q_ID")), as.numeric)

data.pca =PCA(dataNArmAll[,-6], graph=T, ncp = Inf)

data.hcpc <- HCPC(data.pca)

data.hcpc$data.clust%>%View()
data.hcpc$data.clust%>%group_by(clust)%>%group_map(summary)


# Aggregate to Pos Neg Neutral --------------------------------------------
data <- readxl::read_xlsx("D:/Work/Chapter_2/IdBySpecies07052020.xlsx")


data <- data %>%
  select(Remove, Mitigate, Accept, Adapt, Support, Q_ID)%>%
  mutate_all(.,as.factor)%>%
  mutate(Remove = fct_collapse(Remove,"Positive" = c("5", "6", "7"),
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
  )

dataNArm <- filter(data, !(is.na(Remove) & is.na(Mitigate) & is.na(Accept) & is.na(Adapt) & is.na(Support)))
dataNArmAll <- na.omit(data)

data.mca = MCA(dataNArmAll[,-6], graph=T, ncp = Inf)

data.hcpc <- HCPC(data.mca, nb.clust = 4)

# inputdata<-data.frame(dataNArmAll%>%
#                        mutate(ID = as.factor(gsub(Q_ID, pattern = "_.*", replacement = ""))))
# 
# data.mca2 = MCA(inputdata[,-6], graph=T, ncp = Inf)
# 
# data.hcpc <- HCPC(data.mca2)


data.hcpc$data.clust%>%View()

lapply(1:length(unique(data.hcpc$data.clust$clust)), 
       FUN = function(x){
         summary(data.hcpc$data.clust[which(data.hcpc$data.clust$clust==x),])})

Clusters4 <- ggplot(data = data.hcpc$data.clust%>%
                      pivot_longer(cols = c("Remove", "Mitigate", "Accept", "Adapt", "Support"),
                                   values_to = "Q"
                      )%>%
                      mutate(Q = gsub(x = Q, pattern = ".*_", replacement = ""))%>%
                      group_by(clust,name, Q)%>%
                      count()%>%
                      ungroup()%>%
                      mutate(name = factor(name, levels = c("Support", "Adapt", "Accept", "Mitigate", "Remove"))
                      )%>%
                      complete(.,
                               name,
                               Q,
                               clust),
                    aes(x= name, y = n, fill = Q))+
  geom_bar(stat = "identity",
           position = "dodge",
           colour ="black")+
  facet_wrap(~clust, scales = "free_y")

linedata <- data.hcpc$data.clust%>%
  mutate(id = row.names(data.hcpc$data.clust))%>%
  pivot_longer(cols = c("Remove", "Mitigate", "Accept", "Adapt", "Support"),
               values_to = "Q"
  )%>%
  mutate(Q = gsub(x = Q, pattern = ".*_", replacement = ""))%>%
  mutate(name = factor(name, levels = c("Support", "Adapt", "Accept", "Mitigate", "Remove"))
  )


labels <- c("1" = "Cluster 1: Support Colonists", "2" = "Cluster 2: Wary of Colonists", "3" = "Cluster 3: Non-Intervention", "4" = "Cluster 4: Neutral")
ggplot(data = linedata,
       aes(x= name, y = Q, group = id))+
  geom_line(position = position_jitter(0.1,0.1))+
  facet_wrap(~clust, scales = "free_y", labeller=labeller(clust = labels))



Clusters4_2 <- ggplot(data = subset(data.hcpc$data.clust%>%
                                      pivot_longer(cols = c("Remove", "Mitigate", "Accept", "Adapt", "Support"),
                                                   values_to = "Q"
                                      )%>%
                                      mutate(Q = gsub(x = Q, pattern = ".*_", replacement = ""))%>%
                                      group_by(clust,name, Q)%>%
                                      count()%>%
                                      ungroup()%>%
                                      mutate(name = factor(name, levels = c("Support", "Adapt", "Accept", "Mitigate", "Remove"))
                                      )%>%
                                      complete(.,
                                               name,
                                               Q,
                                               clust), clust == 4),
                      aes(x= name, y = n, fill = Q))+
  geom_bar(stat = "identity",
           position = "dodge",
           colour ="black")

#Multiple Cuts
for(clusters in c(4,5,6,9,12)){
  data.hcpc <- HCPC(data.mca, nb.clust = clusters, graph = F)
  
  Ids <- data.hcpc$call$X%>%
    mutate(Id =  row.names(data.hcpc$call$X))
  
  dataNArmAll[,as.character(clusters)] <- Ids%>%
    arrange(as.numeric(Id))%>%
    pull(clust)
}
dataNArmAll->copy

dataNArmAll$`5` <- dataNArmAll%>%
  group_by(`4`)%>%
  summarise(levels = n_distinct(`5`))




dataNaSome <- dataNArm[which(!dataNArm$Q_ID%in%dataNArmAll$Q_ID),]

#write.csv(dataNArmAll, "20200514_6_Clusterassigned.csv")

data <- readxl::read_xlsx("D:/Work/Chapter_2/IdBySpecies07052020.xlsx")
ClusteredDataSet <- left_join(data, dataNArmAll, by = c("Q_ID"="Q_ID"))%>%
  select(-ends_with(".y"))

xlsx::write.xlsx(data.frame(ClusteredDataSet), "ClusteredManagementResponses.xlsx", row.names = F)

