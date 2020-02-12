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
                         level = case_when(grepl('trainee', funder) | funder == 'hannagreyfellow' ~ 'Trainees',
                                           TRUE ~ 'PIs'),
                         level = as.factor(level))


# function to create likert data frame - code taken from likert package to allow for specific plotting later
likert_perc <- function(data, grouping){
  nlevels <- length(levels(data))
  lowrange <- 1 : ceiling(nlevels / 2 - nlevels %% 2)
  highrange <- ceiling(nlevels / 2 + 1 ) : nlevels
  
  results <- data.frame(
    Group = rep(unique(grouping), each=nlevels),
    Response = rep(1:nlevels, length(unique(grouping)))
  )
  
  for(i in 1:ncol(items)) {
    t <- as.data.frame(table(grouping, items[,i]))
    t <- reshape2::dcast(t, Var2 ~ grouping, value.var='Freq', add.missing=TRUE)
    t <- cbind(Response=t[,1], 
               apply(t[,2:ncol(t)], 2, FUN=function(x) { x / sum(x) * 100 } )
    )
    t <- reshape2::melt(t)
    results <- merge(results, t, 
                     by.x=c('Group','Response'), by.y=c('Var2','Var1'), 
                     all.x=TRUE)
    names(results)[ncol(results)] <- paste0('Col', i)
  }
  
  names(results)[3:ncol(results)] <- names(items)
  
  results$Response <- factor(results$Response, levels=1:nlevels, 
                             labels=levels(items[,i]))
  results <- reshape2::melt(results, id=c('Group', 'Response'))
  results <- reshape2::dcast(results, Group + variable ~ Response)
  results <- as.data.frame(results)
  names(results)[2] <- 'Item'
  
  for(i in 3:ncol(results)) {
    narows <- which(is.na(results[,i]))
    if(length(narows) > 0) {
      results[narows, i] <- 0
    }
  }
  
  r <- list(results=results, items=items, grouping=grouping, nlevels=nlevels, levels=levels(items[,1]))
  return(r)
}





## example graph with only top percentages 
my.labels <- c("Hanna Gray\nFellows",
               "Janelia\nTrainees",
               "Investigator\nTrainees",
               "Janelia\nGroup Leaders", 
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

## large graph
png(file = 'testplot.png', width = 1340, height = 1004, res = 72)
likert(as.data.frame(character_data$funder_mandate), grouping = fct_rev(character_data$funder_names)) %>%
  plot(group.order = levels(character_data$funder_names), 
       plot.percent.neutral = F, plot.percent.low= F,
       colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'),
       text.size = 16.93333, ## 48 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme
       panel.arrange = 'NULL',
       axes = FALSE) +
  scale_x_discrete(labels= my.labels) +
  scale_y_continuous(limits = c(-100, 125), breaks = c(-100, -50, 0, 50, 100), labels = c(100, 50, 0, 50, 100)) +
  theme(axis.text.y = element_text(family = 'Helvetica', size = 22, hjust = .5),
        axis.text.x = element_text(family = 'Helvetica', size = 22, margin = margin(t = 4, unit = 'pt')),
        axis.title.x = element_text(family = 'Helvetica', size = 22, hjust = .445, vjust = .075),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(6, 'pt'),
        axis.line = element_line(),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.text = element_text(family = 'Helvetica', size = 22,  margin = margin(r = 60, unit = "pt")),
        legend.title = element_blank(),
        plot.margin = margin(t = 517, l = 30, r = 30, b = 10, "pt"))
dev.off()

## small graph
character_data$funder_mandate <- as.factor(str_wrap(character_data$funder_mandate, 10))

png(file = 'testplot_small.png', width = 670, height = 503, res = 72)
likert(as.data.frame(character_data$funder_mandate), grouping = fct_rev(character_data$funder_names)) %>%
  plot(group.order = levels(character_data$funder_names), 
       plot.percent.neutral = F, plot.percent.low= F,
       colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'),
       text.size = 9.877778, ## 28 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme
       panel.arrange = 'NULL',
       axes = FALSE) +
  scale_x_discrete(labels= my.labels) +
  scale_y_continuous(limits = c(-100, 125), breaks = c(-100, -50, 0, 50, 100), labels = c(100, 50, 0, 50, 100)) +
  theme(axis.text.y = element_text(family = 'Helvetica', size = 12, hjust = .5),
        axis.text.x = element_text(family = 'Helvetica', size = 12, margin = margin(t = 2, unit = 'pt')),
        axis.title.x = element_text(family = 'Helvetica', size = 12, hjust = .445, vjust = .075),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(3, 'pt'),
        axis.line = element_line(),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.text = element_text(family = 'Helvetica', size = 12, margin = margin(r = 20, unit = "pt")),
        legend.title = element_blank(),
        plot.margin = margin(t = 200, l = 30, r = 30, b = 10, "pt"))
dev.off()


## large 2 bars
png(file = 'testplot_2bar.png', width = 1340, height = 1004, res = 72)
likert(as.data.frame(character_data$funder_mandate), grouping = fct_rev(character_data$level)) %>%
  plot(group.order = levels(character_data$level), 
       plot.percent.neutral = F, plot.percent.low= F,
       colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'),
       text.size = 16.93333, ## 48 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme
       panel.arrange = 'NULL',
       axes = FALSE) +
  scale_y_continuous(limits = c(-100, 125), breaks = c(-100, -50, 0, 50, 100), labels = c(100, 50, 0, 50, 100)) +
  theme(axis.text.y = element_text(family = 'Helvetica', size = 22, hjust = .5),
        axis.text.x = element_text(family = 'Helvetica', size = 22, margin = margin(t = 4, unit = 'pt')),
        axis.title.x = element_text(family = 'Helvetica', size = 22, hjust = .445, vjust = .075),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(6, 'pt'),
        axis.line = element_line(),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.text = element_text(family = 'Helvetica', size = 22,  margin = margin(r = 60, unit = "pt")),
        legend.title = element_blank(),
        plot.margin = margin(t = 756, l = 92, r = 30, b = 10, "pt"))
dev.off()

## small 2 bar graph
png(file = 'testplot_small_2bar.png', width = 670, height = 503, res = 72)
likert(as.data.frame(character_data$funder_mandate), grouping = fct_rev(character_data$level)) %>%
  plot(group.order = levels(character_data$level), 
       plot.percent.neutral = F, plot.percent.low= F,
       colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'),
       text.size = 10.58333, ## 30 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme
       panel.arrange = 'NULL',
       axes = FALSE) +
  scale_y_continuous(limits = c(-100, 125), breaks = c(-100, -50, 0, 50, 100), labels = c(100, 50, 0, 50, 100)) +
  theme(axis.text.y = element_text(family = 'Helvetica', size = 14),
        axis.text.x = element_text(family = 'Helvetica', size = 14, margin = margin(t = 2, unit = 'pt')),
        axis.title.x = element_text(family = 'Helvetica', size = 14, hjust = .445, vjust = .075),
        axis.ticks.x = element_line(),
        axis.ticks.length.x = unit(3, 'pt'),
        axis.line = element_line(),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.text = element_text(family = 'Helvetica', size = 14,  margin = margin(r = 20, unit = "pt")),
        legend.title = element_blank(),
        plot.margin = margin(t = 280, l = 30, r = 30, b = 10, "pt"))
dev.off()

