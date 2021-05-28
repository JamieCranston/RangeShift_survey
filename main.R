source("r/utils.R")
source("r/ingest_survey_data.R")
source("r/clean_survey_data.R")
source("r/rename_survey_columns.R")
source("r/get_respondent_characters.R")
source("r/validate_respondent_character_table.R")
source("r/pivot_survey_long.R")

library(magrittr)

config <- load_config()

# Load in raw survey data
survey_data_raw <- ingest_survey_data(config)

# Remove pilot survey responses and incomplete surveys 
survey_data_complete_responses <- survey_data_raw %>% 
  remove_pilot_responses() %>% 
  remove_incomplete_surveys()

# Remove columns showing metainformation, question response times and survey instructions
survey_data_clean <- survey_data_complete_responses %>%
  remove_meta_info() %>% 
  remove_response_times()%>%
  remove_instructions()

# Split raw survey data to begin building two tidy datasets: one for respondent info, one for species responses.
respondent_character_table <- survey_data_clean %>%
  get_respondent_characters()

survey_species_responses  <- survey_data_clean %>% 
  get_species_responses()

# begin cleaning respondent_character table
respondent_character_table_clean <- respondent_character_table %>% 
  rename_recorder_info() %>% 
  rename_mngmnt_ideas() %>% 
  rename_presented_species() %>%
  rename_species_of_interest() %>% 
  rename_VoN() %>% 
  rename_final_text_Qs() %>% 
  merge_word_associations() 

respondent_character_table_clean <- respondent_character_table_clean %>% 
  check_respondents_ages() %>% 
  check_respondents_reported_gender() %>% 
  check_respondents_postcode() %>% 
  check_wildlife_sector() %>% 
  check_education(., config) %>% 
  strings_as_factors() %>% 
  check_awareness(., config) %>%
  check_recorder_role(., config) %>%
  check_groups_recorded(., config) %>% 
  reorder_table_columns()


# begin clean species responses  
survey_species_responses  <- survey_species_responses %>%
         pivot_species_long() %>%
         check_species_shown_by_id(speciesdata = ., respondentdata = respondent_character_table_clean) #TODO write 



SpeciesRecorded <- Responses[which(Responses$OtherGroups != 0), 1:11]
#SpeciesRecorded%>%select(OtherGroups,id)%>%group_by(id)%>%slice(1)%>%write_csv("Othergroups.csv")
SpeciesRecordedLookup <- read_csv("Othergroups.csv",col_types = cols("c","f","c"))

Responses$OtherGroups<- left_join(Responses,SpeciesRecordedLookup)%>%select(FinalClass) 

Responses$Plants <- ifelse(sapply(Responses$OtherGroups,FUN = function(X){grepl(x=X,pattern= "Plant|All")})[,1],"Yes","No")  

Responses<-Responses%>%mutate(Odonata = case_when(OtherGroups == "Odonata"|OtherGroups == "All"~ "Yes",
                                                  T ~ as.character(Odonata)),
                              Hymenoptera = case_when(OtherGroups == "Hymenoptera"|OtherGroups == "All"~ "Yes",
                                                      T ~  as.character(Hymenoptera)),
                              Coleoptera = case_when(OtherGroups == "Coleoptera"|OtherGroups == "All"~ "Yes",
                                                     T ~ as.character(Coleoptera)),
                              Lepidoptera = case_when(OtherGroups == "Lepidoptera"|OtherGroups == "All"~ "Yes",
                                                      T ~  as.character(Lepidoptera)),
                              Hemiptera = case_when(OtherGroups == "Hemiptera"|OtherGroups == "All"~ "Yes",
                                                    T ~  as.character(Hemiptera)),
                              Mammals = case_when(OtherGroups == "Mammals"|OtherGroups == "All"~ "Yes",
                                                  T ~  as.character(Mammals)),
                              Birds = case_when(OtherGroups == "Birds"|OtherGroups == "All"~ "Yes",
                                                T ~  as.character(Birds)))%>%
  mutate(Other = case_when(OtherGroups== "All"|OtherGroups== "Other;Plants"|OtherGroups== "Other"|Diptera== "Yes"~"Yes"))%>%
  select(-OtherGroups, -Diptera)


GR <-Responses%>%
  dplyr::select(c("Birds", "Mammals","Plants", "Hymenoptera", "Coleoptera", "Lepidoptera", "Odonata", "Other", "Hemiptera", "NoSpGroups"))%>%pivot_longer(1:10)%>%
  group_by(name,value)%>%
  count()%>%
  filter(value=="Yes")%>%
  mutate(n=round(n/4/315*100,2))%>%
  ggplot()+
  geom_bar(stat="identity", aes(x= reorder(name,n), y = n, fill = name))+
  geom_label(aes(x= reorder(name,n), y = n+5,label = n))+
  ylab("% of Respondents recording each group")+
  xlab("Taxon Group")+
  guides(fill=FALSE, colour=FALSE)+
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
      "NoSpGroups" = "gray26")
  )+
  coord_flip()+
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
  )+
  scale_y_continuous(position = "left",expand = expansion(add = c(0,5)),limits = c(0,100))

ggsave(GR, filename = "Figures/SuppGroupsRecorded.tif", device = "tiff", width = 180, units = "mm", height = 80, dpi = 600)
