
source("r/utils.R")
source("r/ingest_survey_data.R")
source("r/clean_survey_data.R")
source("r/rename_survey_columns.R")
config <- load_config()

library(magrittr)

survey_data_raw <- ingest_survey_data(config)

survey_data_complete_responses <- survey_data_raw %>% 
  remove_pilot_responses() %>% 
  remove_incomplete_surveys()

survey_data_clean <- survey_data_complete_responses %>%
  remove_meta_info() %>% 
  remove_response_times()%>%
  remove_instructions()

survey_data_clean %>%
  rename_recorder_info() %>% 
  merge_word_associations()

#refactored up to line 135