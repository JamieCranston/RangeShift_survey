
library(rangesurvey)
library(magrittr)

config <- load_config()


# data cleaning -----------------------------------------------------------

# Load in raw survey data
survey_data_raw <- ingest_survey_data(config)

# Remove pilot survey responses and incomplete surveys
survey_data_complete_responses <- survey_data_raw %>%
  remove_pilot_responses() %>%
  remove_incomplete_surveys()

# Remove columns showing metainformation, question response times and survey
# instructions
survey_data_clean <- survey_data_complete_responses %>%
  remove_meta_info() %>%
  remove_response_times() %>%
  remove_instructions()

# Split raw survey data to begin building two tidy datasets: one for respondent
# info, one for species responses.
respondent_character_table <- survey_data_clean %>%
  get_respondent_characters()

survey_species_responses  <- survey_data_clean %>%
  get_species_responses()

# begin cleaning respondent_character table
respondent_table_clean <- respondent_character_table %>%
  rename_recorder_info() %>%
  rename_mngmnt_ideas() %>%
  rename_presented_species() %>%
  rename_species_of_interest() %>%
  rename_VoN() %>%
  rename_final_text_Qs() %>%
  merge_word_associations()

respondent_table_clean <-
  respondent_table_clean %>%
  check_respondents_ages() %>%
  check_respondents_reported_gender() %>%
  check_respondents_postcode() %>%
  check_wildlife_sector() %>%
  check_education(., config) %>%
  strings_as_factors() %>%
  check_awareness(., config) %>%
  check_recorder_role(., config) %>%
  check_groups_recorded(., config) %>%
  scale_numerics()

#reorder_table_columns()


# begin clean species responses
survey_responses_to_model  <- survey_species_responses %>%
  pivot_species_long() %>%
  check_species_shown_by_id(speciesdata = .,
                            respondentdata = respondent_table_clean) %>%
  is_species_in__recorded_group(speciesdata = .,
                                respondentdata = respondent_table_clean) 

species_responses_to_model <- survey_responses_to_model%>%
  likert_7_to_3() %>%
  dplyr::select(id, species, attitude_to_species, seen, match)

management_responses_to_model <-  survey_responses_to_model %>%
  likert_7_to_3() %>%
  dplyr::select(id,
                species,
                attitude_to_species,
                Remove,
                Mitigate,
                Accept,
                Adapt,
                Support,
                seen,
                match)

# visualise model input data ----------------------------------------------

plot_fig_1(survey_responses_to_model)
plot_fig_3(survey_responses_to_model)

plot_supp_fig_1(data = respondent_table_clean,
                config = config)
plot_supp_fig_2(data = respondent_table_clean,
                config = config)
plot_supp_fig_3(data = respondent_table_clean)
plot_supp_fig_4(data = respondent_table_clean)
plot_supp_fig_5(data = respondent_table_clean,
                config =  config)
plot_supp_fig_6(survey_responses_to_model)
plot_supp_fig_7(data = respondent_table_clean)

# build models for respondents' attitudes to species and management -------

species_attitude_model <-
  model_species_attitudes(species_data = species_responses_to_model,
                          respondent_data = respondent_table_clean)

management_attitudes_model <-
  model_management_attitudes(species_data = management_responses_to_model,
                             respondent_data = respondent_table_clean)

# visualise model outputs -------------------------------------------------

plot_fig_2a()
plot_fig_2b()

plot_supp_fig_8()
plot_supp_fig_9()
plot_supp_fig_10()

# plot table insets -------------------------------------------------------
plot_table_2_figures_insets()
plot_supp_table_2_figures_insets()
plot_supp_table_3_figures_insets()

save_output_figures()