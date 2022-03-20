
library(rangesurvey)
library(magrittr)
library(ggplot2)
library(patchwork)

ggplot2::theme_set(load_theme())

config <- load_config(config = "config.yaml")

# Load in raw survey data
survey_data_raw <- ingest_survey_data(config)

# Clean survey data, remove:
# - pilot survey responses
# - incomplete surveys
# - columns showing metainformation
# - question response times
# - survey instructions

survey_data_clean <- clean_survey_data(data = survey_data_raw)

# Split raw survey data to begin building two tidy datasets: one for respondent
# info...
respondent_frame <- make_tidy_datasets(survey_data_clean,
                                                 frame = "respondents")

# Make imputations / validations for respondent free text material
respondent_table_clean <-
  respondent_frame  %>%
  check_n_scale_age_n_years_recording() %>% 
  impute_from_responses_other(., config = config) # %>% 
#reorder_table_columns()

#... one for species responses.
species_frame <- make_tidy_datasets(survey_data_clean,
                                    frame = "species")

# select relevant columns for the 2 models & collapse likert scale from 7 -> 3
species_responses_to_model <- get_attitude_models_data(species_frame,
                                                       model = "species")

management_responses_to_model <- get_attitude_models_data(species_frame,
                                                          model = "management")
# visualise model input data ----------------------------------------------

# number of responses by species
species_responses_to_model %>%
  dplyr::count(species)

fig_1 <- plot_fig_1(species_frame)

fig_3 <- plot_fig_3(species_frame)

# build models for respondents' attitudes to species and management -------

species_attitude_model <-
  model_species_attitudes(species_responses_to_model,
                          respondent_data = respondent_table_clean)

management_attitudes_model <-
  model_management_attitudes(management_responses_to_model,
                             respondent_data = respondent_table_clean)

# visualise model outputs -------------------------------------------------

fig_2a <- plot_fig_2a(model = species_attitude_model)+
  theme(axis.text = element_text(size = 9),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        strip.text.x = element_text(size = 10))

fig_2b <- plot_fig_2b(model = species_attitude_model, 
            config = config)+
  xlab("")+
  theme(legend.text = element_text(size = 11),
        axis.text = element_text(size = 9),
        axis.title.y = element_text(size = 11),
        plot.tag.position = c(0.05, 0.98)
        )

fig_2 <- fig_2a + fig_2b
