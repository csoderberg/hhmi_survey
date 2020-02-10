library(tidyverse)
library(osfr)
library(here)
library(lubridate)
library(likert)

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
                         funder_names  = fct_relevel(funder_names , 'Investigators', 'Janelia Group Leader', 'Investigator Trainees', 'Janelia Trainees', 'Hanna Gray Fellows'),
                         level = case_when(grepl('trainee', funder) | funder == 'hannagreyfellow' ~ 'Trainee',
                                           TRUE ~ 'PI'),
                         level = as.factor(level))

abs_formatter <- function(x) {
  return(abs(x))
}

## example graph with only top percentages 
my.labels <- c("Hanna Gray Fellows\n(early career grantees)",
               "Janelia Trainees",
               "Investigator Trainees",
               "Janelia Group Leaders", 
               "Investigators")

plot <- likert(as.data.frame(character_data$funder_mandate), grouping = fct_rev(character_data$funder_names)) %>%
    plot(group.order = levels(character_data$funder_names), 
         plot.percent.neutral = F, plot.percent.low= F,
         colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'),
         text.size = 22.57778, ## 32 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme
         panel.arrange = 'NULL',
         axes = FALSE) +
    scale_x_discrete(labels= my.labels) +
    scale_y_continuous(limits = c(-100, 125), breaks = c(-100, -50, 0, 50, 100), labels = c(100, 50, 0, 50, 100)) +
    theme(axis.text = element_text(family = 'Helvetica', size = 28),
          axis.title.x = element_text(family = 'Helvetica', size = 28, hjust = .445, vjust = .075),
          axis.line = element_line(),
          panel.background = element_rect(fill = "white", colour = "white"),
          legend.text = element_text(family = 'Helvetica', size = 28,  margin = margin(r = 100, unit = "pt")),
          legend.title = element_blank(),
          plot.margin = margin(t = 5.5, l = 5.5, r = 10, b = 10, "pt"))


ggsave('test_plot.jpeg', plot, dpi = 600, height = 8, width = 24, units = 'in')




## example graph stacked bar with percentages 
character_data %>%
  select(funder_names, funder_mandate) %>%
  filter(funder_names != 'Hanna Gray Fellows') %>%
  filter(!is.na(funder_mandate)) %>%
  group_by(funder_names, funder_mandate) %>%
  tally() %>%
  mutate(perc = round(100*n/sum(n),0),
         percentage = paste0(perc, '%')) %>%
  ggplot(aes(fill = fct_rev(funder_mandate), x = funder_names, y = perc)) +
  geom_col(stat = 'identity', position = 'fill', width = .5) +
  geom_text(aes(x = funder_names ,label = percentage), size = 4.938889, position=position_fill(vjust=0.5)) + ## 14 * 0.352777778 since this geom_text size is in mm, not pt like microsoft and theme
  scale_y_continuous(labels=scales::percent, expand = c(0, 0)) +
  scale_fill_manual(values=c('#058d96', '#00a450','#8ac341','#AAAAAA','#838286')) +
  theme(axis.text = element_text(family = 'Helvetica', size = 14),
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = 'bottom',
        legend.text = element_text(family = 'Helvetica', size = 14,  margin = margin(r = 75, unit = "pt")),
        legend.title = element_blank(),
        plot.margin = margin(t = 10, l = 5.5, r = 5.5, b = 10, "pt")) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))
