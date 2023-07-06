library(tidyverse)
library(ggplot2)

# load helpers
source('R/parseTaskData.R')
source('R/data.R')
source('R/statistics.R')

# download gonogo data from OSF: RUN ONCE

getOSFdata(2020, "fall", "gonogo")
getOSFdata(2020, "fall", "visualsearch")
getOSFdata(2020, "fall", "nback")
getOSFdata(2020, "fall", "trailmaking")

# download questionnaire files and quality files: RUN ONCE
getOSFdata(2020, "fall", "questionnaires")
getOSFdata(2020, "fall", "quality")

# try to get out a transformed df

gng_df <- getGroupPerformance(2020, "fall", "gonogo")
vs_df <- getGroupPerformance(2020, "fall", "visualsearch")
nback_df <- getGroupPerformance(2020, "fall", "nback")
tm_df <- getGroupPerformance(2020, "fall", "trailmaking")

#### create data based on Assel's ####

source('R/MilaR/CleaningQ1.R')

# calculate use frequency
# 7: daily
# 6: a few times a week
# 5: once a week
# 4: a few times a month
# 3: around once a month
# 2: once or twice within these past 3 months
# 1: not in the past 3 months
# 0: [empty / never]

q1$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  q1$cannabis_freqnum[which(q1$cannabis_use_frequency == freqlist[idx])] <- idx
}

# calculate cannabis groups

q1 <- q1 %>% mutate(cannabis_group = cannabis_freqnum,
                    cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                            cannabis_freqnum > 5 ~ "Frequent users",
                                            TRUE ~ "Infrequent users"))

#### merging with tasks ####

gng_df <- gng_df[which(gng_df$passedscreening == TRUE),]
vs_df <- vs_df[which(vs_df$passedscreening == TRUE),]
df   <- merge(q1, gng_df,   by.x='id', by.y='participant', all=FALSE)
df   <- merge(df, vs_df,   by.x='id', by.y='participant', all=FALSE)

# write that into data for now
write.csv(df, 'data/2020/mila_full_df.csv', row.names=FALSE)

#### calculate mean by groups ####

df %>%
  group_by(cannabis_group) %>%
  summarise_at(vars(dprime), list(name = mean))

df %>%
  group_by(cannabis_group) %>%
  summarise_at(vars(RT_6_absent, RT_12_absent, RT_18_absent,
                    RT_6_present, RT_12_present, RT_18_present), list(name = mean))


df %>%
  summarise_at(vars(dprime), list(name = mean))

# Box plot by group
ggplot(df, aes(x = cannabis_group, y = dprime, fill= cannabis_group)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  labs(title = "NoGo", x = "")+ 
  scale_x_discrete(labels=c("Frequent User\n (n=47)", "Infrequent User\n (n=94)", "Non-User\n (n=174)"))+ 
  theme(legend.position = "none")  

# line segment graph
df %>%
  select(RT_6_absent:RT_18_present, cannabis_group)  %>%
  pivot_longer(RT_6_absent:RT_18_present, names_to = "Group", values_to = "RT") %>%
  mutate(set_size = Group,
         set_size = case_when(Group == "RT_6_absent" ~ 6,
                              Group == "RT_12_absent" ~ 12,
                              Group == "RT_18_absent" ~ 18,
                              Group == "RT_6_present" ~ 6,
                              Group == "RT_12_present" ~ 12,
                              Group == "RT_18_present" ~ 18)) %>%
  mutate(Present = Group,
         Present = case_when(Group == "RT_6_absent" ~ "absent",
                             Group == "RT_12_absent" ~ "absent",
                             Group == "RT_18_absent" ~ "absent",
                             Group == "RT_6_present" ~ "present",
                             Group == "RT_12_present" ~ "present",
                             Group == "RT_18_present" ~ "present"))  %>%
  group_by(cannabis_group, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(cannabis_group, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count)) %>%
  ggplot(aes(x=factor(set_size), y=mean, 
             group=interaction(Present, cannabis_group),
             shape = Present,
             color=cannabis_group)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.1) +
  geom_line(aes(linetype = Present)) +
  geom_point(size=3) +
  scale_y_continuous(limits = c(0, 6), name = "Reaction Time (s)") +
  xlab("Set Size")

