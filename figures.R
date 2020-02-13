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
                         concerned_mandate_1 = fct_relevel(concerned_mandate_1, "Not at all concerned", "Slightly concerned", "Moderately concerned", "Very concerned", "Extremely concerned"),
                         concerned_mandate_2 = fct_relevel(concerned_mandate_2, "Not at all concerned", "Slightly concerned", "Moderately concerned", "Very concerned", "Extremely concerned"),
                         concerned_mandate_3 = fct_relevel(concerned_mandate_3, "Not at all concerned", "Slightly concerned", "Moderately concerned", "Very concerned", "Extremely concerned"),
                         concerned_mandate_4 = fct_relevel(concerned_mandate_4, "Not at all concerned", "Slightly concerned", "Moderately concerned", "Very concerned", "Extremely concerned"),
                         concerned_mandate_5 = fct_relevel(concerned_mandate_5, "Not at all concerned", "Slightly concerned", "Moderately concerned", "Very concerned", "Extremely concerned"),
                         funder_activities_1 = fct_relevel(funder_activities_1, "Not at all influential", "Slightly influential", "Moderately influential", "Very influential", "Extremely influential"),
                         funder_activities_2 = fct_relevel(funder_activities_2, "Not at all influential", "Slightly influential", "Moderately influential", "Very influential", "Extremely influential"),
                         funder_activities_3 = fct_relevel(funder_activities_3, "Not at all influential", "Slightly influential", "Moderately influential", "Very influential", "Extremely influential"),
                         communicate_findings = fct_relevel(communicate_findings, "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"),
                         learn_research = fct_relevel(learn_research, "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"),
                         post_preprint = fct_relevel(post_preprint, "Never", 'Rarely', "Sometimes", "Very often", "Always"),
                         preprint_mandate = fct_relevel(preprint_mandate, "Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat favor", "Strongly favor"),
                         open_licenses = fct_relevel(open_licenses, "Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat favor", "Strongly favor"),
                         restrictive_license = fct_relevel(restrictive_license, "Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat favor", "Strongly favor"),
                         id_peerreviewers = fct_relevel(id_peerreviewers, "Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat favor", "Strongly favor"),
                         open_peerreview = fct_relevel(open_peerreview, "Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat favor", "Strongly favor"),
                         open_ided_peerreview = fct_relevel(open_ided_peerreview, "Strongly oppose", "Somewhat oppose", "Neutral", "Somewhat favor", "Strongly favor"),
                         familiarity_1 = fct_relevel(familiarity_1, "Not at all familiar", "Slightly familiar", "Moderately familiar", "Very familiar", "Extremely familiar"),
                         familiarity_2 = fct_relevel(familiarity_2, "Not at all familiar", "Slightly familiar", "Moderately familiar", "Very familiar", "Extremely familiar"),
                         familiarity_3 = fct_relevel(familiarity_3, "Not at all familiar", "Slightly familiar", "Moderately familiar", "Very familiar", "Extremely familiar"),
                         familiarity_4 = fct_relevel(familiarity_4, "Not at all familiar", "Slightly familiar", "Moderately familiar", "Very familiar", "Extremely familiar"),
                         importance_1 = fct_relevel(importance_1, "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"),
                         importance_2 = fct_relevel(importance_2, "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"),
                         importance_3 = fct_relevel(importance_3, "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"),
                         importance_4 = fct_relevel(importance_4, "Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"),
                         often_behavior_1 = fct_relevel(often_behavior_1, "Never", 'Rarely', "Sometimes", "Very often", "Always"),
                         often_behavior_2 = fct_relevel(often_behavior_2, "Never", 'Rarely', "Sometimes", "Very often", "Always"),
                         often_behavior_3 = fct_relevel(often_behavior_3, "Never", 'Rarely', "Sometimes", "Very often", "Always"),
                         often_behavior_4 = fct_relevel(often_behavior_4, "Never", 'Rarely', "Sometimes", "Very often", "Always")) %>%
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

# set up function to create basic plot (adapted from likert package)
likert_bar_plot <- function(l, group.order, center = (l$nlevels-1)/2 + 1, colors, geom_textsize, theme_textsize, 
                            nlegend_char, ngroup_char, xaxis_margin, xaxis_ticks, legend_margin, 
                            plot_margin_top, plot_left_margin, plot_margin_bottom,
                            bar_width) {
  ymin <- -100
  ymax <- 100
  ybuffer <- 5
  
  lowrange <- 1 : floor(center - 0.5)
  highrange <- ceiling(center + 0.5) : l$nlevels
  cols <- colors
  
  p <- NULL
  
  results <- l$results
  results <- reshape2::melt(results, id=c('Group', 'Item'))
  levels(results$variable) <- str_wrap(levels(results$variable),nlegend_char)
  levels(results$Group) <- str_wrap(levels(results$Group),ngroup_char)
  
  top_perc <- results %>%
    select(-Item) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(above_midline = round(rowSums(.[5:6]),0)) %>%
    select(Group, above_midline)
  
  rows <- which(results$variable %in% levels(results$variable)[1:2])
  results[rows,'value'] <- -1 * results[rows,'value']
  rows.mid <- which(results$variable %in% levels(results$variable)[3])
  tmp <- results[rows.mid,]
  tmp$value <- tmp$value / 2 * -1
  results[rows.mid,'value'] <- results[rows.mid,'value'] / 2
  results <- rbind(results, tmp)
  
  results.low <- results[results$value < 0,]
  results.high <- results[results$value > 0,]
  
  results.high$variable <- factor(as.character(results.high$variable),
                                  levels = rev(levels(results.high$variable)))
  
  p <- ggplot(results, aes(y=value, x=fct_rev(Group), group=variable)) + 
    geom_hline(yintercept=0) +
    geom_bar(data=results.low[nrow(results.low):1,], aes(fill=variable), stat='identity', width = bar_width) + 
    geom_bar(data=results.high, aes(fill=variable), stat='identity', width = bar_width)
  
  names(cols) <- levels(results$variable)
  plot <- p + 
        scale_fill_manual(guide = 'legend', breaks=names(cols), values=cols, drop=FALSE) +
        geom_text(data=top_perc, aes(x=Group, y=100, 
                             label=paste0(above_midline, '%'), 
                             group=Group), size=geom_textsize, hjust=-.2, color='black') +
    coord_flip() +
    ylab('Percent') +
    scale_y_continuous(limits = c(-100, 125), breaks = c(-100, -50, 0, 50, 100), labels = c(100, 50, 0, 50, 100)) +
    theme(axis.text.y = element_text(family = 'Helvetica', size = theme_textsize, hjust = .5),
          axis.text.x = element_text(family = 'Helvetica', size = theme_textsize, margin = margin(t = xaxis_margin, unit = 'pt')),
          axis.title.y = element_blank(),
          axis.title.x = element_text(family = 'Helvetica', size = theme_textsize, hjust = .445, vjust = .075),
          axis.ticks.x = element_line(),
          axis.ticks.length.x = unit(xaxis_ticks, 'pt'),
          axis.line = element_line(),
          panel.background = element_rect(fill = "white", colour = "white"),
          legend.position = 'bottom',
          legend.text = element_text(family = 'Helvetica', size = theme_textsize,  margin = margin(r = legend_margin, unit = "pt")),
          legend.title = element_blank(),
          plot.margin = margin(t = plot_margin_top, l = plot_left_margin, r = 30, b = plot_margin_bottom, "pt"))
  
  return(plot)
}

# function for large & small 5 bar graphs
graph_5bar <- function(variable){
  likert_data <- likert_perc(character_data[[variable]], grouping = character_data$funder_names)
  
  large_5bar_plot <- likert_bar_plot(likert_data, 
                                     group.order = levels(character_data$funder_names), 
                                     center = (l$nlevels-1)/2 + 1, 
                                     colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'), 
                                     geom_textsize = 16.93333, #48 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme 
                                     theme_textsize = 22, 
                                     nlegend_char = 10, ngroup_char = 12, 
                                     xaxis_margin = 4, xaxis_ticks = 6, 
                                     legend_margin = 60, plot_margin_top = 100, plot_left_margin = 30, plot_margin_bottom = 100,
                                     bar_width = .5)
  
  small_5bar_plot <- likert_bar_plot(likert_data, 
                                     group.order = levels(character_data$funder_names), 
                                     center = (l$nlevels-1)/2 + 1, 
                                     colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'), 
                                     geom_textsize = 10.58333, #30 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme 
                                     theme_textsize = 14, 
                                     nlegend_char = 10, ngroup_char = 12, 
                                     xaxis_margin = 2, xaxis_ticks = 3, 
                                     legend_margin = 20, plot_margin_top = 50, plot_left_margin = 30, plot_margin_bottom = 50, 
                                     bar_width = .5)

  file_name_large <- paste0(variable, 'large_5bar.png')
  file_name_small <- paste0(variable, 'small_5bar.png')
  
  png(file = file_name_large, width = 1340, height = 1004, res = 72)
  print(large_5bar_plot)
  dev.off()
  
  png(file = file_name_small, width = 670, height = 503, res = 72)
  print(small_5bar_plot)
  dev.off()
}

# function for large & small 2bar graphs
graph_2bar <- function(variable) {
  likert_data <- likert_perc(character_data[[variable]], grouping = character_data$level)
  large_2bar_plot <- likert_bar_plot(likert_data, 
                               group.order = levels(character_data$level), 
                               center = (l$nlevels-1)/2 + 1, 
                               colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'), 
                               geom_textsize = 16.93333, #48 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme 
                               theme_textsize = 22, 
                               nlegend_char = 10, ngroup_char = 12, 
                               xaxis_margin = 4, xaxis_ticks = 6, 
                               legend_margin = 60, plot_margin_top = 501, plot_left_margin = 81, plot_margin_bottom = 100, 
                               bar_width = .5)
  
  file_name_large <- paste0(variable, 'large_2bar.png')
  file_name_small <- paste0(variable, 'small_2bar.png')
  
  small_2bar_plot <- likert_bar_plot(likert_data, 
                                          group.order = levels(character_data$level), 
                                          center = (l$nlevels-1)/2 + 1, 
                                          colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'), 
                                          geom_textsize = 10.58333, #30 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme 
                                          theme_textsize = 14, 
                                          nlegend_char = 10, ngroup_char = 12, 
                                          xaxis_margin = 2, xaxis_ticks = 3, 
                                          legend_margin = 20, plot_margin_top = 235, plot_left_margin = 62, plot_margin_bottom = 50,
                                          bar_width = .5)
  
  png(file = file_name_large, width = 1340, height = 1004, res = 72)
  print(large_2bar_plot)
  dev.off()
  
  png(file = file_name_small, width = 670, height = 503, res = 72)
  print(small_2bar_plot)
  dev.off()
}


# calls to create graphs broken out by 5 funder categories
variables_5bars <- names(character_data)[4:44]
map(variables_5bars, graph_5bar)

# calls to create graphs broken out by 2 career levels
variables_2bars <- c('funder_mandate', 'id_peerreviewers', 'open_peerreview', 'open_ided_peerreview')
map(variables_2bars, graph_2bar)


## large 5-bar graph with new function

likert_data <- likert_perc(character_data$funder_mandate, grouping = character_data$funder_names)
test_plot <- likert_bar_plot(likert_data, 
                group.order = levels(character_data$funder_names), 
                center = (l$nlevels-1)/2 + 1, 
                colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'), 
                geom_textsize = 16.93333, #48 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme 
                theme_textsize = 22, 
                nlegend_char = 10, ngroup_char = 12, 
                xaxis_margin = 4, xaxis_ticks = 6, 
                legend_margin = 60, plot_margin_top = 5.5, plot_left_margin = 30, 
                bar_width = .5)

png(file = 'testplot.png', width = 1340, height = 1004, res = 72)
test_plot
dev.off()


## small 5-bar graph with new function
test_plot_small <- likert_bar_plot(likert_data, 
                             group.order = levels(character_data$funder_names), 
                             center = (l$nlevels-1)/2 + 1, 
                             colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'), 
                             geom_textsize = 10.58333, #30 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme 
                             theme_textsize = 14, 
                             nlegend_char = 10, ngroup_char = 12, 
                             xaxis_margin = 2, xaxis_ticks = 3, 
                             legend_margin = 20, plot_margin_top = 5.5, plot_left_margin = 30,
                             bar_width = .5)

png(file = 'testplot_small.png', width = 670, height = 503, res = 72)
test_plot_small
dev.off()


## large 2 bar graph example
likert_data <- likert_perc(character_data$funder_mandate, grouping = character_data$level)
test_plot <- likert_bar_plot(likert_data, 
                             group.order = levels(character_data$level), 
                             center = (l$nlevels-1)/2 + 1, 
                             colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'), 
                             geom_textsize = 16.93333, #48 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme 
                             theme_textsize = 22, 
                             nlegend_char = 10, ngroup_char = 12, 
                             xaxis_margin = 4, xaxis_ticks = 6, 
                             legend_margin = 60, plot_margin_top = 506, plot_left_margin = 81,
                             bar_width = .5)

png(file = 'testplot_2bar.png', width = 1340, height = 1004, res = 72)
test_plot
dev.off()


## small 2 bar graph example
test_plot_2bar_small <- likert_bar_plot(likert_data, 
                             group.order = levels(character_data$level), 
                             center = (l$nlevels-1)/2 + 1, 
                             colors = c('#838286', '#AAAAAA', '#8ac341','#00a450', '#058d96'), 
                             geom_textsize = 10.58333, #30 * 0.352777778 since this text.size is in mm, not pt like microsoft and theme 
                             theme_textsize = 14, 
                             nlegend_char = 10, ngroup_char = 12, 
                             xaxis_margin = 2, xaxis_ticks = 3, 
                             legend_margin = 20, plot_margin_top = 240, plot_left_margin = 62,
                             bar_width = .5)

png(file = 'testplot_2bar_small.png', width = 670, height = 503, res = 72)
test_plot_2bar_small
dev.off()








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

