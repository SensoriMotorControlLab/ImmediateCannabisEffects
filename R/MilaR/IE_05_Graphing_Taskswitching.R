library(ggplot2)
library(survey)
library(flextable)
library(dplyr)

library(rlang)

library(srvyr)

library(gtsummary)

library(stargazer)
library(tibble)

library(jtools)
library(broom.mixed)

library(ggpubr)
#library(ggstatsplot)

library(BayesFactor)

library(ggrepel)

library(reshape2)

library(performance)
library(tidyr)

#### taskswitching ####

## filter those that didn't pass the screening (n = 111)
#taskswitching <- taskswitching %>%
#  filter(passedscreening == TRUE)

# add new category for high users
taskswitching <- taskswitching %>% 
  mutate(users = cannabis_group,
         users = case_when(
           group == "experimental" ~ "High users",
           cannabis_group == "Non-users" ~ "Non-users",
           cannabis_group == "Infrequent users" ~ "Infrequent users",
           cannabis_group == "Frequent users" ~ "Frequent users"
         ))

# fill used by down
#taskswitching <- taskswitching %>% 
#  group_by(id) %>% 
#  fill(sex, physically_activity, stressed, video_games, sleep_last, 
#       concussion, music, year_of_birth, cannabis_group,
#       cannabis_freqnum, .direction = "downup")

taskswitching  %>%
  group_by(users) %>%
  summarise_at(vars(singleblock_1_RT, singleblock_2_RT, switch_RT,
                    nonswitch_RT, congruent_RT, nonCongruent_RT), list(name = mean), na.rm = TRUE)

# Reorder the 'users' factor variable according to the vector above
taskswitching$users <- factor(taskswitching$users, levels = user_order)

#### BF calculations ####

bf_df_all <- data.frame(group = character(),
                    group1 = character(),
                    group2 = character(),
                    p.adj = numeric(),
                    stringsAsFactors = FALSE)


# loop over the variables and fill the matrix
for (k in c("singleblock_1_RT", "switch_RT", "congruent_RT")) {
  
  bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(taskswitching$users), levels(taskswitching$users)))
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j | i > j) {
        bf_matrix[i,j] <- "-"
      } else {
        test <- ttestBF(x = subset(taskswitching, users == levels(taskswitching$users)[i])[[k]],
                        y = subset(taskswitching, users == levels(taskswitching$users)[j])[[k]])@numerator$`Alt., r=0.707`@analysis$bf
        bf_matrix[i,j] <- round(exp(test), 2)
      }
    }
  }
  
  # Transform bf_matrix to long format
  bf_df <- melt(bf_matrix)
  
  # Rename columns
  names(bf_df) <- c("group1", "group2", "p.adj")
  
  # Remove rows with "-" values
  bf_df <- bf_df[bf_df$p.adj != "-",]
  
  # add identifier
  bf_df$group <- k
  
  # keep only data for high users
  bf_df <- bf_df[(bf_df$group1 == "High users") | (bf_df$group2 == "High users"), ]
  
  # append bf_df to bf_df_all_new
  bf_df_all <- rbind(bf_df_all, bf_df)
}

bf_df_all <- bf_df_all %>% mutate(multiplier = group,
                                  multiplier = case_when(group == "singleblock_1_RT" ~ 1,
                                                         group == "congruent_RT" ~ 2,
                                                         TRUE ~ 3))
bf_df_all <- bf_df_all %>% mutate(group_2 = group,
                                  group_2 = case_when(group == "singleblock_1_RT" ~ 1.33,
                                                      group == "congruent_RT" ~ 2.33,
                                                      TRUE ~ 3.33))

bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "Non-users" ~ multiplier - 0.34,
                                                      group1 == "Infrequent users" ~ multiplier - 0.11,
                                                      group1 == "Frequent users" ~ multiplier + 0.1))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                  y = case_when(group1 == "Non-users" ~ 3,
                                                group1 == "Infrequent users" ~ 3.5,
                                                group1 == "Frequent users" ~ 4))

bf_df_all$users <- bf_df_all$group1


#### calculate ANOVA bf ####

ts <- taskswitching %>%
  select(id, singleblock_1_RT, switch_RT, congruent_RT, users)  %>%
  pivot_longer(singleblock_1_RT:congruent_RT, names_to = "Group", values_to = "RT") %>%
  mutate(tasks = factor(case_when(
    Group == "singleblock_1_RT" ~ "single",
    Group == "switch_RT" ~ "switch",
    Group == "congruent_RT" ~ "congruent"
  ), levels = c("single", "congruent", "switch")))


ts$id <- as.factor(ts$id)

options(scipen = 999)

#model_ts <- anovaBF(formula = RT ~ users*tasks + id, data = ts, rscaleFixed = "wide", whichRandom = "id")
model_ts <- anovaBF(formula = RT ~ users*tasks, data = ts, rscaleFixed = "wide")

extractBF(model_ts)

r2_bayes(model_ts)

#### plot ####

ggplot(ts, aes(x = tasks, y = RT, fill = users)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Task Switching", x = "Tasks", y = "Mean RT") +
  scale_fill_manual(values = c("Non-users" = "#f8766d", 
                               "Infrequent users" = "#7caeff", 
                               "Frequent users" = "#00ba38", 
                               "High users" = "#c77cff"),
                    labels=c(paste0("Non-User\n (n=", table(taskswitching$users)["Non-users"][[1]], ")"), 
                             paste0("Infrequent User\n (n=", table(taskswitching$users)["Infrequent users"][[1]], ")"), 
                             paste0("Frequent Users\n (n=", table(taskswitching$users)["Frequent users"][[1]], ")"), 
                             paste0("High users\n (n=", table(taskswitching$users)["High users"][[1]], ")"))) +
  theme_bw() + 
  theme(legend.position = "bottom", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  stat_pvalue_manual(
    data = bf_df_all, label = "BF = {p.adj}",
    xmin = "group_1", xmax = "group_2",
    y.position = "y"
  )

#### graph for poster ####

bf_df_all$p.adj <- as.numeric(bf_df_all$p.adj)
bf_df_all <- bf_df_all[bf_df_all$p.adj > 3 | bf_df_all$p.adj < 1/3, ]
bf_df_all$p <- ifelse((bf_df_all$p.adj > 3 & bf_df_all$p.adj <= 10) | (bf_df_all$p.adj < 1/3 & bf_df_all$p.adj >= 1/10), "*", 
                      ifelse((bf_df_all$p.adj > 10 & bf_df_all$p.adj <= 30) | (bf_df_all$p.adj < 1/10 & bf_df_all$p.adj >= 1/30), "**", "***"))

## calculate positions:
m_t <- taskswitching  %>%
  group_by(users) %>%
  summarise_at(vars(singleblock_1_RT, singleblock_2_RT, switch_RT,
                    nonswitch_RT, congruent_RT, nonCongruent_RT), list(name = mean), na.rm = TRUE)

m_t <- data.frame(subset(m_t, select = c("users", "singleblock_1_RT_name", "switch_RT_name", "congruent_RT_name")))
rownames(m_t) <- m_t$users
m_t$users <- NULL
names(m_t) <- c("singleblock_1_RT", "switch_RT", "congruent_RT")

## replace groups depending on positions

for (i in 1:dim(bf_df_all)[1]) {
  bf_df_all[i, "group_2"] <- m_t[bf_df_all[i, "group1"], bf_df_all[i, "group"]]
  bf_df_all[i, "group_1"] <- m_t[bf_df_all[i, "group2"], bf_df_all[i, "group"]]
}


p4<- ts %>% group_by(users, tasks)  %>%
  mutate(count = n()) %>%
  group_by(users, tasks, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))%>%
  ggplot(aes(x=tasks, y=mean, 
             group=users,
             color=users)) +
  geom_path(size = 2) +
  geom_point(size=4) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.3, color = "black", size = 1) +
  labs(title = "Task Switching", x = "", y = "Reaction Time (s)") +
  scale_color_manual(values = c("Non-users" = "#f8766d", 
                               "Infrequent users" = "#7caeff", 
                               "Frequent users" = "#679267", 
                               "High users" = "#00FF00"),
                    labels=c(paste0("Non-Users (n=", table(taskswitching$users)["Non-users"][[1]], ")"), 
                             paste0("Infrequent Users (n=", table(taskswitching$users)["Infrequent users"][[1]], ")"), 
                             paste0("Frequent Users (n=", table(taskswitching$users)["Frequent users"][[1]], ")"), 
                             paste0("High users (n=", table(taskswitching$users)["High users"][[1]], ")"))) +
  theme_bw() + 
  theme(legend.position = c(0.3, 0.9),  # Adjust the position of the legend inside the graph
        legend.box = "horizontal",  # Set the legend box style to horizontal
        legend.title = element_blank(),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Lato", size = 24),
        panel.grid = element_blank())+
  geom_segment(aes(x = 0.85, xend = 0.85, y = 0.90626, yend = 0.9418957), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(x = 0.85, xend = 0.9, y = 0.90626, yend = 0.90626), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(x = 0.85, xend = 0.9, y = 0.9418957, yend = 0.9418957), linewidth = 0.5, color = "#f8766d") +
  geom_text(x = 0.8, y = 0.92, label = "#", size = 5, color = "#f8766d") +
  geom_segment(aes(x = 0.7, xend = 0.7, y = 0.90626, yend = 0.8753797), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(x = 0.7, xend = 0.75, y = 0.90626, yend = 0.90626), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(x = 0.7, xend = 0.75, y = 0.8753797, yend = 0.8753797), linewidth = 0.5, color = "#7caeff") +
  geom_text(x = 0.65, y = 0.889, label = "#", size = 5, color = "#7caeff") +
  geom_segment(aes(x = 0.55, xend = 0.55, y = 0.90626, yend = 0.9053950), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(x = 0.55, xend = 0.6, y = 0.90626, yend = 0.90626), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(x = 0.55, xend = 0.6, y = 0.9053950, yend = 0.9053950), linewidth = 0.5, color = "#679267") +
  geom_text(x = 0.5, y = 0.905, label = "#", size = 5, color = "#679267")+
  geom_segment(aes(x = 1.85, xend = 1.85, y = 1.06662, yend = 1.0840695), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(x = 1.85, xend = 1.9, y = 1.06662, yend = 1.06662), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(x = 1.85, xend = 1.9, y = 1.0840695, yend = 1.0840695), linewidth = 0.5, color = "#7caeff") +
  geom_text(x = 1.8, y = 1.07, label = "#", size = 5, color = "#7caeff")+
  geom_segment(aes(x = 3.15, xend = 3.15, y = 1.27264, yend = 1.3442689), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(x = 3.15, xend = 3.1, y = 1.27264, yend = 1.27264), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(x = 3.15, xend = 3.1, y = 1.3442689, yend = 1.3442689), linewidth = 0.5, color = "#679267") +
  geom_text(x = 3.2, y = 1.3, label = "#", size = 5, color = "#679267") +
  geom_segment(aes(x = 3.3, xend = 3.3, y = 1.27264, yend = 1.2934064), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(x = 3.3, xend = 3.25, y = 1.27264, yend = 1.27264), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(x = 3.3, xend = 3.25, y = 1.2934064, yend = 1.2934064), linewidth = 0.5, color = "#7caeff") +
  geom_text(x = 3.35, y = 1.28, label = "#", size = 5, color = "#7caeff")

ggsave("data/output/IE_taskswitching_300.svg", plot = p4, width=200, height=300, units = "mm", dpi = 300)




