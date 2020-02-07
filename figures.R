library(tidyverse)
library(osfr)
library(here)
library(lubridate)

##read in data
osf_retrieve_file("https://osf.io/9gdfc/") %>%
  osf_download(overwrite = T)

character_data <- read_csv(here::here('hhmi_character_response.csv')) %>%
                  select(-c('Progress', 'RecordedDate', 'DistributionChannel', 'Finished', 'UserLanguage', 'ResponseId', 'country', 'num_articles', 'career_level', 'career_level_8_TEXT'), 
                         -starts_with('Duration')) %>%
                  mutate(StartDate = mdy_hm(StartDate),
                         EndDate = mdy_hm(EndDate)) %>%
                  mutate(publication_time = fct_relevel(publication_time, "Much too short", "Slightly too short", 'Just the right amount of time', 'Slightly too long', 'Much too long'),
                         favors_established =  fct_relevel(favors_established, "Does not at all favor established scientists", "Slightly favors established scientists", "Moderately favors established scientists", "Very much favors established scientists", "Extremely favors established scientists"),
                         influence_hiring = fct_relevel(influence_hiring, "Much less than it should", "Slightly less than it should", "Just the right amount", "Slightly more than it should", "Much more than it should"),
                         influence_funding = fct_relevel(influence_funding,  "Much less than it should", "Slightly less than it should", "Just the right amount", "Slightly more than it should", "Much more than it should"),
                         influential_tenure = fct_relevel(influential_tenure, "Not at all influential", "Slightly influential", "Moderately influential", "Very influential", "Extremely influential"),
                         public_OA = fct_relevel(public_OA, "Never", 'Rarely', "Sometimes", "Very often", "Always"),
                         paywall_problems = fct_relevel(paywall_problems, "Never", 'Rarely', "Sometimes", "Very often", "Always"),
                         your_library = fct_relevel(your_library, "No more likely", "Slightly more likely", "Moderately more likely", "Much more likely", "Extremely more likely"),
                         other_libraries = fct_relevel(other_libraries, "No more likely", "Slightly more likely", "Moderately more likely", "Much more likely", "Extremely more likely"),
                         subscription_vs_OA = fct_relevel(subscription_vs_OA, "Much worse", "Slightly worse", "Neither worse nor better", "Slightly better", "Much better"),
                         funder_mandate = fct_relevel(funder_mandate, "Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat favor", "Strongly favor"),
                         communicate_findings = fct_relevel(communicate_findings, "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"),
                         learn_research = fct_relevel(learn_research, "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"),
                         post_preprint = fct_relevel(post_preprint, "Never", 'Rarely', "Sometimes", "Very often", "Always")) %>%
                  mutate(funder_names = case_when(funder == 'hhmitrainee' ~ 'Investigator Trainees',
                                            funder == 'hhmi' ~ 'Investigators',
                                            funder == 'janeliatrainee' ~ 'Janelia Trainees',
                                            funder == 'janelia' ~ 'Janelia Group Leader',
                                            funder == 'hannagreyfellow' ~ 'Hanna Gray Fellows'),
                         funder_names  = fct_relevel(funder_names , 'Investigators', 'Investigator Trainees', 'Janelia Group Leader', 'Janelia Trainees', 'Hanna Gray Fellows'),
                         level = case_when(grepl('trainee', funder) | funder == 'hannagreyfellow' ~ 'Trainee',
                                           TRUE ~ 'PI'),
                         level = as.factor(level))


##