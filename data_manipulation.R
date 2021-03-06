library(osfr)
library(tidyverse)
library(here)
library(lubridate)


#read in data
osf_retrieve_file("https://osf.io/c7aj9/") %>%
  osf_download(overwrite = T)

osf_retrieve_file("https://osf.io/9gdfc/") %>%
  osf_download(overwrite = T)

numeric_data <- read_csv(here::here('hhmi_numeric_answers.csv')) %>%
                      select(-c('EndDate', 'Status', 'Progress', 'RecordedDate', 
                                'DistributionChannel', 'Finished', 'UserLanguage', 'ResponseId', 
                                'country', 'num_articles', 'discipline'), 
                             -starts_with('Duration'), -starts_with('gender'), -starts_with('career'), -starts_with('open_comment')) %>%
                        mutate(StartDate = mdy_hm(StartDate),
                               level = case_when(funder == 'hhmi' | funder == 'janelia' ~ 'PI',
                                       funder == 'hhmitrainee' | funder == 'janeliatrainee' | funder == 'hannagreyfellow' ~ 'Trainee')) 

character_data <- read_csv(here::here('hhmi_character_response.csv')) %>%
                      select(-c('EndDate', 'Status', 'Progress', 'RecordedDate', 
                                'DistributionChannel', 'Finished', 'UserLanguage', 'ResponseId', 
                                'country', 'num_articles', 'discipline'), 
                             -starts_with('Duration'), -starts_with('gender'), -starts_with('career'), -starts_with('open_comment')) %>%
                      mutate(StartDate = mdy_hm(StartDate),
                             level = case_when(funder == 'hhmi' | funder == 'janelia' ~ 'PI',
                                               funder == 'hhmitrainee' | funder == 'janeliatrainee' | funder == 'hannagreyfellow' ~ 'Trainee')) 

## seperately format unipolar & bipolar data
numeric_data_bipolar <- numeric_data %>%
                            select(StartDate, funder, level, publication_time, influence_hiring, influence_funding, subscription_vs_OA, funder_mandate, preprint_mandate, open_licenses, restrictive_license, id_peerreviewers, open_ided_peerreview, open_peerreview) %>%
                            mutate_at(vars(-c(funder, level, StartDate)), factor, levels=-2:2, ordered=TRUE) %>%
                            mutate(funder = as.factor(funder)) %>%
                            pivot_longer(cols = publication_time:open_peerreview, names_to = 'variable_name', values_to = 'response') %>%
                            filter(!is.na(response))


numeric_data_unipolar <- numeric_data %>%
                                select(-c(publication_time, influence_hiring, influence_funding, subscription_vs_OA, funder_mandate, proportion_OA_1, preprint_mandate, open_licenses, restrictive_license, id_peerreviewers, open_ided_peerreview, open_peerreview)) %>%
                                mutate_at(vars(-c(funder, level, StartDate)), factor, levels=1:5, ordered=TRUE) %>%
                                mutate(funder = as.factor(funder)) %>%
                                pivot_longer(cols = favors_established:OA_mandate, names_to = 'variable_name', values_to = 'response') %>%
                                filter(!is.na(response))

## calculate percentages
bipolar_percentage <- left_join(numeric_data_bipolar %>%
                                    group_by(variable_name, funder, response) %>%
                                    tally() %>%
                                    mutate(perc = round(100*n/sum(n),2)) %>%
                                    select(-n) %>%
                                    ungroup() %>%
                                    pivot_wider(names_from = 'funder', values_from = 'perc') %>%
                                    replace(., is.na(.), 0) %>%
                                    arrange(variable_name, response),
                                numeric_data_bipolar %>%
                                  filter(funder != 'hannagreyfellow' | (funder == 'hannagreyfellow' & StartDate < '2019-11-08')) %>%
                                  group_by(variable_name, level, response) %>%
                                  tally() %>%
                                  mutate(perc = round(100*n/sum(n),2)) %>%
                                  select(-n) %>%
                                  ungroup() %>%
                                  pivot_wider(names_from = 'level', values_from = 'perc') %>%
                                  replace(., is.na(.), 0) %>%
                                  arrange(variable_name, response),
                                by = c('variable_name', 'response'))

unipolar_percentage <- left_join(numeric_data_unipolar %>%
                                    group_by(variable_name, funder, response) %>%
                                    tally() %>%
                                    mutate(perc = round(100*n/sum(n),2)) %>%
                                    select(-n) %>%
                                    ungroup() %>%
                                    pivot_wider(names_from = 'funder', values_from = 'perc') %>%
                                    replace(., is.na(.), 0) %>%
                                    arrange(variable_name, response),
                                 numeric_data_unipolar %>%
                                   filter(funder != 'hannagreyfellow' | (funder == 'hannagreyfellow' & StartDate < '2019-11-08')) %>%
                                   group_by(variable_name, level, response) %>%
                                   tally() %>%
                                   mutate(perc = round(100*n/sum(n),2)) %>%
                                   select(-n) %>%
                                   ungroup() %>%
                                   pivot_wider(names_from = 'level', values_from = 'perc') %>%
                                   replace(., is.na(.), 0) %>%
                                   arrange(variable_name, response),
                                 by = c('variable_name', 'response'))

# create and format final csv
rbind(bipolar_percentage, unipolar_percentage) %>%
    select(variable_name, response, hhmi, janelia, hhmitrainee, janeliatrainee, hannagreyfellow, PI, Trainee) %>%
    rename(Investigator = hhmi, `Janelia Group Leader` = janelia, `Investigator Trainee` = hhmitrainee, `Janelia Trainee` = janeliatrainee, `Hanna Gray Fellows` = hannagreyfellow) %>%
    write_csv('aggregate_percent_data.csv')

# create and output subset of raw data that underlies graphs
numeric_data %>%
  write_csv('numeric_graph_data.csv')

character_data %>%
  write_csv('character_graph_data.csv')
