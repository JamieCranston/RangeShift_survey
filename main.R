source("r/utils.R")
source("r/ingest_survey_data.R")
source("r/clean_survey_data.R")
source("r/rename_survey_columns.R")

library(magrittr)

config <- load_config()


survey_data_raw <- ingest_survey_data(config)

survey_data_complete_responses <- survey_data_raw %>% 
  remove_pilot_responses() %>% 
  remove_incomplete_surveys()

survey_data_clean <- survey_data_complete_responses %>%
  remove_meta_info() %>% 
  remove_response_times()%>%
  remove_instructions()

respondent_character_table <- survey_data_clean %>%
  get_respondent_characters()

#TODO VON Script
respondent_character_table_clean <- respondent_character_table %>% 
  rename_recorder_info() %>% 
  rename_mngmnt_ideas() %>% 
  rename_species_of_interest() %>% 
  merge_word_associations()

respondent_character_table_clean %>%
  check_gender_other_q() %>% 
  check_education_other_q() %>% 
  check_



validated_respondent_character_table <-  respondent_character_table_clean %>%
  c
  validate_awareness() %>%
  validate_recorder_role() %>% 
  validate_

  

#refactored up to line 135 of 01


Checklist <- dplyr::select(Responses, id, `GDemographics03[other]`) %>%
  group_by(id) %>%
  dplyr::slice(1) %>%
  filter(`GDemographics03[other]` != 0)

# write.csv(file = paste0("ValidationFiles/",Sys.Date(),"EducationValidation.csv"),
# x = Responses[which(Responses$id%in%Checklist$id),]%>%group_by(id)%>%slice(1),
# row.names = FALSE)

EVal <- read.csv(file = "ValidationFiles/Validated/2019-06-03EducationValidated.csv")

Responses$Education <- as.factor(ifelse(Responses$`GDemographics03[SQ004]` == "Yes", "GCSEs",
                                        ifelse(Responses$`GDemographics03[SQ003]` == "Yes", "A-levels",
                                               ifelse(Responses$`GDemographics03[SQ002]` == "Yes", "Undergrad", "Postgrad")
                                        )
))

for (i in unique(EVal$id)) {
  Responses[which(Responses$id == i), "Education"] <- EVal[which(EVal$id == i), "X"]
}


Responses$Education <- ordered(Responses$Education, levels = c("GCSEs", "A-levels", "Undergrad", "Postgrad"))

Responses <- dplyr::select(Responses, -c(
  "GDemographics03[SQ004]",
  "GDemographics03[SQ003]",
  "GDemographics03[SQ002]",
  "GDemographics03[other]",
  "GDemographics03[SQ001]"
))

# Check GenderOther Column
all(is.na(Responses$`GDemographics02[other]`))
if (all(is.na(Responses$`GDemographics02[other]`))) {
  Responses$`GDemographics02[other]` <- NULL
}
# True

Responses <- rename(Responses,
                    "Age" = "GDemographics01",
                    "Gender" = "GDemographics02",
                    "Postcode" = "GDemographics04",
                    "WildlifeSector" = "GDemographics05[SQ002]"
)

# Check Categories
Responses$Age <- ifelse(Responses$Age > 120, 2019 - Responses$Age, Responses$Age)

Responses$Gender <- as.factor(Responses$Gender)
Responses$WildlifeSector <- as.factor(Responses$WildlifeSector)

Responses$id <- as.factor(Responses$id)

Responses[, 2:10] <- lapply(2:10, FUN = function(X) {
  factor(Responses[, X], levels = c("No", "Yes"))
})

Responses[, 14:17] <- lapply(14:17, FUN = function(X) {
  factor(Responses[, X], levels = c("No", "Yes"))
})

Responses$ClimateEq <- as.factor(ifelse(Responses$ClimateEq == 0, "Control", "Climate Change Prompt"))
