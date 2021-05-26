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
  merge_word_associations() 

respondent_character_table_clean <- respondent_character_table_clean %>% 
  check_respondents_ages() %>% 
  check_respondents_reported_gender() %>% 
  check_respondents_postcode() %>% 
  check_wildlife_sector() %>% 
  check_education(., config) %>% 
  strings_as_factors()
  check_awareness(., config) %>%  #todo
  check_recorder_role(., config) %>% #todo
  check_groups_recorded_groups(., config) #todo

validated_respondent_character_table <- respondent_character_table_clean %>%
  validate_awareness() %>%
  validate_recorder_role()

# begin clean species responses  
survey_species_responses  <- survey_species_responses %>%
         pivot_species_long() %>%
         check_species_shown_by_id() #TODO write this 

# save outputs

Responses$Education <- ordered(Responses$Education, levels = c("GCSEs", "A-levels", "Undergrad", "Postgrad"))


strings_as_factors <- function(data) {
  data <- data %>%
    dplyr::mutate(
      dplyr::across(all_of(c(
        "Birds",
        "Mammals",
        "Hymenoptera",
        "Coleoptera",
        "Lepidoptera",
        "Diptera",
        "Odonata",
        "Hemiptera",
        "NoSpGroups",
        "Informal",
        "Recorder",
        "Verifier",
        "Organiser", 
        "NoRole")),
        ~ factor(.x,levels = c("No","Yes"))
            )
    )
  
  data <- data %>% 
    dplyr::mutate(climate_treatment = factor(ClimateEq, levels = "Control", "Climate Change Prompt"))
  
  return(data)

  }



Responses$ClimateEq <- as.factor(ifelse(Responses$ClimateEq == 0, "Control", "Climate Change Prompt"))
