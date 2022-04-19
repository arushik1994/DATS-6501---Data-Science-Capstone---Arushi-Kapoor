#Loading the required libraries
library(dplyr)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(scales)
library(grid)
library(reshape2)
library(tidyverse)
library(grDevices)
library(plotly)
library(patchwork)
library(waffle)


#Setting the scientific notation
options(scipen = 999,
        warn = -1)

#Reading the data
df <- readRDS("Master.rds")
df <- df%>%filter(Gender %in% c('Man','Woman'))

#Exploratory Data Analysis
#Percentage of Respondents
resp <- df %>% 
  group_by(Gender) %>% 
  summarise(count = n()) %>% 
  mutate(pc = (count/sum(count))*100) %>% 
  ggplot() + aes(fill = Gender, values = pc) +
  geom_waffle(n_rows = 10, size = 0.5, colour = "#ffffff",  flip = FALSE) +
  scale_fill_manual(values = c("cornflowerblue","darkseagreen")) + ggtitle(label="Total Number of Respondents, 64505: Men (52525) vs. Women (11980)",
                                                                          subtitle = "Based on 2019 - 2021 survey data") +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() + theme(legend.position = 'top', legend.justification='center', 
                                 plot.title = element_text(face = "bold", size = 18, color = "Black", hjust = 0.5),
                                 plot.subtitle = element_text(size = 16, color = "Black", hjust = 0.5))
resp


#Setting tidyBarLeg
tidyBarLeg <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "Black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        axis.text.x = element_text(size = 12),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.ticks.y = element_blank(),
        legend.position = 'top', 
        legend.justification='left',
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
#Gender Distribution by Age 
g_age <- df %>% 
  group_by(Gender, Age) %>% 
  summarise(count = n(), .groups= 'drop') %>% 
  group_by(Gender) %>%
  mutate(pc = (count/sum(count))*100) %>% 
  ungroup() %>% 
  filter(Gender %in% c('Man','Woman')) %>% 
  ggplot() + 
  aes(x = Age, 
      y = pc,
      group = Gender,
      fill = Gender) + 
  geom_bar(stat = 'identity',
           position = 'dodge', color="black") + 
  geom_label(aes(label = paste0(round(pc, 1), '%')), position = position_dodge(width = 0.9), vjust=-0.25, fill = "white", color = "black") +
  scale_fill_manual(values = c("cornflowerblue", "darkseagreen")) +
  ggtitle(label = "Distribution of Gender across Age Groups", subtitle = "Based on 2019 - 2021 survey data") + tidyBarLeg
g_age



#Gender Distribution by Education 
g_educ <- df %>% 
  group_by(Gender, Education) %>% 
  summarise(count = n(),  .groups= 'drop') %>% 
  group_by(Gender) %>%
  mutate(pc = (count/sum(count))*100) %>% 
  ungroup() %>% 
  filter(Gender %in% c('Man','Woman')) %>% 
  ggplot() + 
  aes(x = reorder(Education, desc(pc)), 
      y = pc,
      group = Gender,
      fill = Gender) + 
  geom_bar(stat = 'identity',
           position = 'dodge', color="black") + 
  geom_label(aes(label = paste0(round(pc, 1), '%')), position = position_dodge(width = 0.9), vjust=-0.25, fill = "white", color = "black") +
  scale_fill_manual(values = c("cornflowerblue", "darkseagreen")) + 
  ggtitle(label = "Distribution of Gender across Education Levels", subtitle = "Based on 2019 - 2021 survey data") + tidyBarLeg
g_educ


#Gender Distribution by Job Titles
#Filtering the top job titles 
topJobs <- df %>% distinct(JobTitle, SurveyYear) %>% 
  group_by(JobTitle) %>%
  summarise(count = n(),.groups= 'drop') %>%
  filter(count >= 2)
#Setting tidyBarLegJob
tidyBarLegJob <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "Black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5, hjust=1),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.ticks.y = element_blank(),
        legend.position = 'top', 
        legend.justification='left',
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))
g_job <- df %>%   
  group_by(Gender, JobTitle) %>% 
  summarise(count = n(),  .groups= 'drop') %>% 
  group_by(Gender) %>%
  mutate(pc = (count/sum(count))*100) %>% 
  ungroup() %>% 
  filter(Gender %in% c('Man','Woman')) %>% #& JobTitle %in% topJobs$JobTitle) %>%#
  ggplot() + 
  aes(x = reorder(JobTitle,desc(pc)), 
      y = pc,
      group = Gender,
      fill = Gender) + 
  geom_bar(stat = 'identity',
           position = 'dodge', color='black') + 
  scale_fill_manual(values = c("cornflowerblue", "darkseagreen")) +
  geom_label(aes(label = paste0(round(pc, 1), '%')), position = position_dodge(width = 0.95), vjust=-0.15, fill = "white", color = "black") +
  ggtitle(label = "Distribution of Gender across Job Titles", subtitle = "Based on 2019 - 2021 survey data") + tidyBarLegJob
g_job


#Gender Distribution by Years of Coding Experience
g_code <- df %>% 
  group_by(Gender, CodingExp) %>% 
  summarise(count = n(),  .groups= 'drop') %>% 
  group_by(Gender) %>%
  mutate(pc = (count/sum(count))*100) %>% 
  ungroup() %>% 
  filter(Gender %in% c('Man','Woman')) %>% 
  ggplot() + 
  aes(x = reorder(CodingExp, desc(pc)), 
      y = pc,
      group = Gender,
      fill = Gender) + 
  geom_bar(stat = 'identity',
           position = 'dodge', color="black") + 
  scale_fill_manual(values = c("cornflowerblue", "darkseagreen")) +
  geom_label(aes(label = paste0(round(pc, 1), '%')), position = position_dodge(width = 0.9), vjust=-0.25, fill = "white", color = "black") +
  ggtitle(label = "Distribution of Gender across Years of Coding Experience", subtitle = "Based on 2019 - 2021 survey data") + tidyBarLeg
g_code


#Gender Distribution by Countries
#Filtering the top countries 
topCountries <- df %>% 
  group_by(Country) %>%
  summarise(count = n(),
            .groups= 'drop') %>%
  mutate(pc = count/sum(count)*100) %>%
  arrange(desc(pc)) %>%
  slice(1:10)

g_nat <- df %>%
  group_by(Gender, Country) %>% 
  summarise(count = n(),
            .groups = 'drop') %>% 
  group_by(Gender) %>%
  mutate(pc = count/sum(count)*100) %>% 
  ungroup() %>% 
  filter(Gender %in% c('Man','Woman') & Country %in% topCountries$Country) %>% 
  ggplot() + 
  aes(x = reorder(Country,desc(pc)), 
      y = pc,
      fill = Gender,
      group = Gender) + 
  geom_bar(stat = 'identity',
           position = 'dodge', color="black") + 
  scale_fill_manual(values = c("cornflowerblue", "darkseagreen")) +
  geom_label(aes(label = paste0(round(pc, 1), '%')), position = position_dodge(width = 0.9), vjust=-0.25, fill = "white", color = "black") +
  ggtitle(label = "Distribution of Gender across Countries of Residence: Top 10", subtitle = "Based on 2019 - 2021 survey data") + 
  labs(caption="Note: Other just indicates a location that is not among the listed countries, as indicated by the survey. Other does
       not imply or suggest a collection of the countries excluding those shown on the graph.") + tidyBarLeg
g_nat

#Gender Distribution by Compensation Groups
g_compg <- df %>%
  #filter(CompGroup != 'Unknown') %>%#
  group_by(Gender, CompGroup) %>% 
  summarise(count = n(),
            .groups = 'drop') %>% 
  group_by(Gender) %>%
  mutate(pc = count/sum(count)*100) %>% 
  ungroup() %>% 
  filter(Gender %in% c('Man','Woman')) %>% 
  ggplot() + 
  aes(x = reorder(CompGroup, desc(pc)), 
      y = pc,
      fill = Gender,
      group = Gender) + 
  geom_bar(stat = 'identity',
           position = 'dodge', color="black") + 
  scale_fill_manual(values = c("cornflowerblue", "darkseagreen")) +
  geom_label(aes(label = paste0(round(pc, 1), '%')), position = position_dodge(width = 0.9), vjust=-0.25, fill = "white", color = "black") +
  ggtitle(label = "Distribution of Gender across Compensation Groups", subtitle = "Based on 2019 - 2021 survey data") + 
  labs(caption="Note: The x-axis variable was created during data pre-processing. The unit is in thousands USD.
       For instance, if a respondent declares compensation as $7500-9999, then the corresponding CompGroup is 0-10.") + tidyBarLeg
g_compg



############################################################################################################
#Dissecting the Pay Gap 
#Average Compensation across the years
#Calculating average compensation of men and women
avg_comp <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender) %>% 
  summarise(avg = mean(mid),
            count_n = n(),
            .groups = 'drop')
#Setting tidyLine
tidyLine <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "Black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'top',
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
#Plotting the graph
g_comp <- avg_comp %>%
  ggplot() + 
  aes(x = SurveyYear,
      y = avg,
      group = Gender,
      color = Gender) + 
  geom_line(size = 1.5) +
  scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
  scale_x_continuous(breaks = 2019:2021) + 
  geom_text(aes(label = paste0(round(avg,-3)/1000,'K')),
            vjust = -0.5,
            size = 8) +
  ylim(0,max(avg_comp$avg)*1.2) + 
  ggtitle(label = 'Average Compensation in USD: Men vs. Women', subtitle = "Based on 2019 - 2021 survey data") + tidyLine
g_comp


#Examining the percent difference
diff_pc <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Man', 'Woman')) %>% 
  group_by(SurveyYear,
           Gender) %>% 
  summarise(avg = mean(mid),
            .groups = 'drop') %>% 
  arrange(SurveyYear,
          desc(Gender)) %>%
  group_by(SurveyYear) %>%
  mutate(diff_pc = (avg-lead(avg))/lead(avg)*100)%>%
  #mutate(diff_pc = ((lag(avg)-avg)/avg)*100) %>%
  select(SurveyYear,
         diff_pc) %>%
  filter(!is.na(diff_pc))

diff_pc
#Setting tidyBar
tidyBar <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "Black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')
#Plotting the graph
g_comp_pc <- diff_pc %>% 
  ggplot() + 
  aes(x = SurveyYear,
      y = diff_pc) + 
  geom_bar(stat = 'identity', width=0.60, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 8) +
  ggtitle(label = "Decrease in Women's Average Compensation", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar
g_comp_pc


#Pay Gap by Age 
#Setting tidyBar
tidyBar <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "Black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')
#Plotting the bar graph
diff_pc_age <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender,
           Age) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          Age)%>%
  filter(SurveyYear == '2021')%>%
  group_by(Age)%>%
  mutate(diff_pc_age = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_age)%>%
  ggplot() + 
  aes(reorder(Age, diff_pc_age),
      y = diff_pc_age) + 
  geom_bar(stat = 'identity', width=0.60, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_age, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by Age Group; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_age
#Plotting the yearly pay gap for each age group
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "Black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in unique(df$Age) %>% sort()){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             Age == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  
  p1 <- diff_nom %>%
    ggplot() + 
    aes(x = SurveyYear,
        y = avg,
        group = Gender,
        color = Gender) + 
    geom_line(size = 0.8) +
    scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
    scale_x_continuous(breaks = 2019:2021) + 
    geom_hline(yintercept = 0,
               colour = 'grey') + 
    geom_text(aes(label = paste0(round(avg,-3)/1000,
                                 'K')),
              vjust = -0.5,
              size = 3) +
    ylim(0,max(diff_nom$avg)*1.2) + 
    ggtitle(label = " ",
            subtitle = i)
  
  
  plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
}

g3 <- arrangeGrob(grobs = plotList, ncol = 4)
grid.arrange(g3)
diff_pc_age / grid.arrange(g3)

#Pay Gap by Education
#Plotting the bar graph
diff_pc_educ <- filter(df, Education != "Not Provided")
diff_pc_educ2 <- diff_pc_educ %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender,
           Education) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          Education)%>%
  filter(SurveyYear == '2021')%>%
  group_by(Education)%>%
  mutate(diff_pc_educ2 = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_educ2)%>%
  ggplot() + 
  aes(reorder(Education, diff_pc_educ2),
      y = diff_pc_educ2) + 
  geom_bar(stat = 'identity', width=0.60, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_educ2, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by Education Level; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_educ2
#Plotting the yearly pay gap for each education level
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in unique(diff_pc_educ$Education) %>% sort()){
  
  
  diff_nom <- diff_pc_educ %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             Education == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  
  p1 <- diff_nom %>%
    ggplot() + 
    aes(x = SurveyYear,
        y = avg,
        group = Gender,
        color = Gender) + 
    geom_line(size = 0.8) +
    scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
    scale_x_continuous(breaks = 2019:2021) + 
    geom_hline(yintercept = 0,
               colour = 'grey') + 
    geom_text(aes(label = paste0(round(avg,-3)/1000,
                                 'K')),
              vjust = -0.5,
              size = 3) +
    ylim(0,max(diff_nom$avg)*1.2) + 
    ggtitle(label = '',
            subtitle = i)
  
  
  plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
}

g4 <- arrangeGrob(grobs = plotList, ncol = 4)
grid.arrange(g4)
diff_pc_educ2 / grid.arrange(g4)


#Pay Gap by Job Title
#Plotting the bar graph
diff_pc_jt <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man') & JobTitle %in% topJobs$JobTitle) %>%
  group_by(SurveyYear,
           Gender,
           JobTitle) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          JobTitle)%>%
  filter(SurveyYear == '2021')%>%
  group_by(JobTitle)%>%
  mutate(diff_pc_jt = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_jt)%>%
  ggplot() + 
  aes(reorder(JobTitle, diff_pc_jt),
      y = diff_pc_jt) + 
  geom_bar(stat = 'identity', width=0.60, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_jt, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by Job Title; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_jt
#Plotting the yearly pay gap for each job title
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "Black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in topJobs$JobTitle[topJobs$JobTitle != 'Student']){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             JobTitle == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  if (nrow(diff_nom) > 0)  {
  
  p1 <- diff_nom %>%
    ggplot() + 
    aes(x = SurveyYear,
        y = avg,
        group = Gender,
        color = Gender) + 
    geom_line(size = 0.8) +
    scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
    scale_x_continuous(breaks = 2019:2021) + 
    geom_hline(yintercept = 0,
               colour = 'grey') + 
    geom_text(aes(label = paste0(round(avg,-3)/1000,
                                 'K')),
              vjust = -0.5,
              size = 3) +
    ylim(0,max(diff_nom$avg)*1.2) + 
    ggtitle(label = '',
            subtitle = i)
  
  
  plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
  }
  
  else {NULL}
  
}

g5 <- arrangeGrob(grobs = plotList, ncol = 4)
grid.arrange(g5)
diff_pc_jt / grid.arrange(g5)


#Pay Gap by Country of Residence
#Plotting the bar graph
diff_pc_na <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man') & 
           Country %in% topCountries$Country) %>% 
  group_by(SurveyYear,
           Gender,
           Country) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          Country)%>%
  filter(SurveyYear == '2021')%>%
  group_by(Country)%>%
  mutate(diff_pc_na = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_na)%>%
  ggplot() + 
  aes(reorder(Country, diff_pc_na),
      y = diff_pc_na) + 
  geom_bar(stat = 'identity', width=0.60, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_na, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by Countries of Residence: Top 10; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_na
#Plotting the yearly pay gap for each country of residence
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 18, color = "Black"),
        plot.subtitle = element_text(size = 16, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in topCountries$Country){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             Country == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  if (nrow(diff_nom) > 0)  {
    
    p1 <- diff_nom %>%
      ggplot() + 
      aes(x = SurveyYear,
          y = avg,
          group = Gender,
          color = Gender) + 
      geom_line(size = 0.8) +
      scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
      scale_x_continuous(breaks = 2019:2021) + 
      geom_hline(yintercept = 0,
                 colour = 'grey') + 
      geom_text(aes(label = paste0(round(avg,-3)/1000,
                                   'K')),
                vjust = -0.5,
                size = 3) +
      ylim(0,max(diff_nom$avg)*1.2) + 
      ggtitle(label = '',
              subtitle = i)
    
    
    plotList[[length(plotList) + 1]] = p1 + tidyLine2
    
  }
  
  else {NULL}
  
}

g7 <- arrangeGrob(grobs = plotList, ncol = 4)
grid.arrange(g7)
diff_pc_na / grid.arrange(g7)

#Pay Gap by Python Programming Language
#Setting tidyBar
tidyBar <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 12, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')
#Plotting the bar graph
diff_pc_py <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender,
           ProgLang_Python) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
         ProgLang_Python)%>%
  filter(SurveyYear == '2021')%>%
  group_by(ProgLang_Python)%>%
  mutate(diff_pc_py = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_py)%>%
  ggplot() + 
  aes(reorder(ProgLang_Python, diff_pc_py),
      y = diff_pc_py) + 
  geom_bar(stat = 'identity', width=0.40, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_py, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by Python; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_py
#Plotting the yearly pay gap for those who know/don't know Python
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 14, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in unique(df$ProgLang_Python) %>% sort()){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             ProgLang_Python == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
    
    p1 <- diff_nom %>%
      ggplot() + 
      aes(x = SurveyYear,
          y = avg,
          group = Gender,
          color = Gender) + 
      geom_line(size = 0.8) +
      scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
      scale_x_continuous(breaks = 2019:2021) + 
      geom_hline(yintercept = 0,
                 colour = 'grey') + 
      geom_text(aes(label = paste0(round(avg,-3)/1000,
                                   'K')),
                vjust = -0.5,
                size = 3) +
      ylim(0,max(diff_nom$avg)*1.2) + 
      ggtitle(label = '',
              subtitle = i)
    
    
    plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
}

g6 <- arrangeGrob(grobs = plotList, ncol = 2)
grid.arrange(g6)
diff_pc_py / grid.arrange(g6)

#Pay Gap by R Programming Language
#Setting tidyBar
tidyBar <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 12, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')
#Plotting the bar graph
diff_pc_r <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender,
           ProgLang_R) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          ProgLang_R)%>%
  filter(SurveyYear == '2021')%>%
  group_by(ProgLang_R)%>%
  mutate(diff_pc_r = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_r)%>%
  ggplot() + 
  aes(reorder(ProgLang_R, diff_pc_r),
      y = diff_pc_r) + 
  geom_bar(stat = 'identity', width=0.40, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_r, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by R; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_r
#Plotting the yearly pay gap for those who know/don't know R
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 14, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in unique(df$ProgLang_R) %>% sort()){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             ProgLang_R == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  p1 <- diff_nom %>%
    ggplot() + 
    aes(x = SurveyYear,
        y = avg,
        group = Gender,
        color = Gender) + 
    geom_line(size = 0.8) +
    scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
    scale_x_continuous(breaks = 2019:2021) + 
    geom_hline(yintercept = 0,
               colour = 'grey') + 
    geom_text(aes(label = paste0(round(avg,-3)/1000,
                                 'K')),
              vjust = -0.5,
              size = 3) +
    ylim(0,max(diff_nom$avg)*1.2) + 
    ggtitle(label = '',
            subtitle = i)
  
  
  plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
}

g8 <- arrangeGrob(grobs = plotList, ncol = 2)
grid.arrange(g8)
diff_pc_r / grid.arrange(g8)

#Pay Gap by SQL Programming Language
#Setting tidyBar
tidyBar <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 12, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')
#Plotting the bar graph
diff_pc_sql <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender,
           ProgLang_SQL) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          ProgLang_SQL)%>%
  filter(SurveyYear == '2021')%>%
  group_by(ProgLang_SQL)%>%
  mutate(diff_pc_sql = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_sql)%>%
  ggplot() + 
  aes(reorder(ProgLang_SQL, diff_pc_sql),
      y = diff_pc_sql) + 
  geom_bar(stat = 'identity', width=0.40, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_sql, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by SQL; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_sql
#Plotting the yearly pay gap for those who know/don't know SQL
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 14, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in unique(df$ProgLang_SQL) %>% sort()){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             ProgLang_SQL == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  p1 <- diff_nom %>%
    ggplot() + 
    aes(x = SurveyYear,
        y = avg,
        group = Gender,
        color = Gender) + 
    geom_line(size = 0.8) +
    scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
    scale_x_continuous(breaks = 2019:2021) + 
    geom_hline(yintercept = 0,
               colour = 'grey') + 
    geom_text(aes(label = paste0(round(avg,-3)/1000,
                                 'K')),
              vjust = -0.5,
              size = 3) +
    ylim(0,max(diff_nom$avg)*1.2) + 
    ggtitle(label = '',
            subtitle = i)
  
  
  plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
}

g9 <- arrangeGrob(grobs = plotList, ncol = 2)
grid.arrange(g9)
diff_pc_sql / grid.arrange(g9)

#Pay Gap by Java Programming Language
#Setting tidyBar
tidyBar <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 12, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')
#Plotting the bar graph
diff_pc_java <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender,
           ProgLang_Java) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          ProgLang_Java)%>%
  filter(SurveyYear == '2021')%>%
  group_by(ProgLang_Java)%>%
  mutate(diff_pc_java = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_java)%>%
  ggplot() + 
  aes(reorder(ProgLang_Java, diff_pc_java),
      y = diff_pc_java) + 
  geom_bar(stat = 'identity', width=0.40, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_java, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by Java; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_java
#Plotting the yearly pay gap for those who know/don't know Java
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 14, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in unique(df$ProgLang_Java) %>% sort()){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             ProgLang_Java == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  p1 <- diff_nom %>%
    ggplot() + 
    aes(x = SurveyYear,
        y = avg,
        group = Gender,
        color = Gender) + 
    geom_line(size = 0.8) +
    scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
    scale_x_continuous(breaks = 2019:2021) + 
    geom_hline(yintercept = 0,
               colour = 'grey') + 
    geom_text(aes(label = paste0(round(avg,-3)/1000,
                                 'K')),
              vjust = -0.5,
              size = 3) +
    ylim(0,max(diff_nom$avg)*1.2) + 
    ggtitle(label = '',
            subtitle = i)
  
  
  plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
}

g10 <- arrangeGrob(grobs = plotList, ncol = 2)
grid.arrange(g10)
diff_pc_java / grid.arrange(g10)

#Pay Gap by C Programming Language
#Setting tidyBar
tidyBar <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 12, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')
#Plotting the bar graph
diff_pc_c <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender,
           ProgLang_C) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          ProgLang_C)%>%
  filter(SurveyYear == '2021')%>%
  group_by(ProgLang_C)%>%
  mutate(diff_pc_c = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_c)%>%
  ggplot() + 
  aes(reorder(ProgLang_C, diff_pc_c),
      y = diff_pc_c) + 
  geom_bar(stat = 'identity', width=0.40, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_c, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by C; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_c
#Plotting the yearly pay gap for those who know/don't know C
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold',size = 14, color = "Black"),
        plot.subtitle = element_text(size = 14, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in unique(df$ProgLang_C) %>% sort()){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             ProgLang_C == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  p1 <- diff_nom %>%
    ggplot() + 
    aes(x = SurveyYear,
        y = avg,
        group = Gender,
        color = Gender) + 
    geom_line(size = 0.8) +
    scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
    scale_x_continuous(breaks = 2019:2021) + 
    geom_hline(yintercept = 0,
               colour = 'grey') + 
    geom_text(aes(label = paste0(round(avg,-3)/1000,
                                 'K')),
              vjust = -0.5,
              size = 3) +
    ylim(0,max(diff_nom$avg)*1.2) + 
    ggtitle(label = '',
            subtitle = i)
  
  
  plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
}

g11 <- arrangeGrob(grobs = plotList, ncol = 2)
grid.arrange(g11)
diff_pc_c / grid.arrange(g11)

#Pay Gap by C++ Programming Language
#Setting tidyBar
tidyBar <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold', size = 14, color = "Black"),
        plot.subtitle = element_text(size = 12, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none')
#Plotting the bar graph
diff_pc_cpl <- df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man')) %>% 
  group_by(SurveyYear,
           Gender,
           ProgLang_C_Pl) %>% 
  summarise(avg = mean(mid),
            .groups= 'drop') %>% 
  ungroup() %>%
  arrange(SurveyYear,
          ProgLang_C_Pl)%>%
  filter(SurveyYear == '2021')%>%
  group_by(ProgLang_C_Pl)%>%
  mutate(diff_pc_cpl = ((avg-lag(avg))/lag(avg))*100)%>%
  filter(Gender == 'Woman')%>%
  arrange(diff_pc_cpl)%>%
  ggplot() + 
  aes(reorder(ProgLang_C_Pl, diff_pc_cpl),
      y = diff_pc_cpl) + 
  geom_bar(stat = 'identity', width=0.40, fill="darkseagreen", color="black") +
  geom_text(aes(label = paste0(round(diff_pc_cpl, 1), '%')), position = position_dodge(width = 0.9), vjust=-1.5, size = 4) +
  ggtitle(label = "Decrease in Women's Average Compensation by C++; Bar Graph: 2021, Line Graph: 2019 - 2021", subtitle = "Based on 2019 - 2021 survey data; Compared to Men's Average Compensation")+tidyBar

diff_pc_cpl
#Plotting the yearly pay gap for those who know/don't know C++
tidyLine2 <-  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold',size = 14, color = "Black"),
        plot.subtitle = element_text(size = 14, color = "Black"),
        plot.margin=unit(c(0.75, 0.75, 0.75, 0.75),"cm"),
        axis.text.x = element_text(size = 15),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank())
plotList = list()
for (i in unique(df$ProgLang_C_Pl) %>% sort()){
  
  
  diff_nom <- df %>% 
    filter(mid != 0 & 
             !is.na(mid) & 
             Gender %in% c('Woman','Man') & 
             ProgLang_C_Pl == i) %>% 
    group_by(SurveyYear,Gender) %>% 
    summarise(avg = mean(mid),
              count_n = n(),
              .groups= 'drop')
  
  p1 <- diff_nom %>%
    ggplot() + 
    aes(x = SurveyYear,
        y = avg,
        group = Gender,
        color = Gender) + 
    geom_line(size = 0.8) +
    scale_color_manual(values = c("cornflowerblue", "darkseagreen")) +
    scale_x_continuous(breaks = 2019:2021) + 
    geom_hline(yintercept = 0,
               colour = 'grey') + 
    geom_text(aes(label = paste0(round(avg,-3)/1000,
                                 'K')),
              vjust = -0.5,
              size = 3) +
    ylim(0,max(diff_nom$avg)*1.2) + 
    ggtitle(label = '',
            subtitle = i)
  
  
  plotList[[length(plotList) + 1]] = p1 + tidyLine2
  
}

g12 <- arrangeGrob(grobs = plotList, ncol = 2)
grid.arrange(g12)
diff_pc_cpl / grid.arrange(g12)


#Examining Severity
pg_jt_s = df %>% 
  filter(mid != 0 & 
           !is.na(mid) & 
           Gender %in% c('Woman','Man') & JobTitle %in% topJobs$JobTitle &
           SurveyYear == 2021) %>% 
  group_by(JobTitle,Gender) %>% 
  summarise(count = n(),
            avg = mean(mid),
            .groups = 'drop') %>%
  arrange(JobTitle,desc(Gender)) %>% 
  group_by(JobTitle) %>% 
  mutate(diff_pc = ((avg)-lead(avg))/(lead(avg))*100,
         diff_count = count/(count + lead(count))*100) %>%
  mutate(dir = ifelse(diff_pc < 0,'neg','pos')) %>%
  filter(!is.na(dir) ) %>%
  ggplot() +
  aes(x = diff_count,y = diff_pc) + 
  geom_point(size = 5) + 
  geom_smooth(method = "lm",formula = 'y ~ x', fill="red") + 
  xlab('% of Respondents in the Role who are Women') +
  ylab('Pay Gap') + 
  geom_text(aes(label=JobTitle),hjust=.3, vjust=-.5,size = 4) +
  ggtitle(label = 'A Representation: Women and Pay Gaps by Job Titles: 2021', subtitle = "Based on 2021 survey data")+ 
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold',size = 18, color = "Black"),
        plot.subtitle = element_text(size = 14, color = "Black"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.position = 'none',
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

