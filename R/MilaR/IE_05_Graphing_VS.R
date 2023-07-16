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

library(stringr)


#### visualsearch ####

## looks like all passed screening?
#visualsearch <- visualsearch %>%
#  filter(passedscreening == TRUE)

# add new category for high users
visualsearch <- visualsearch %>% 
  mutate(users = cannabis_group,
         users = case_when(
           group == "experimental" ~ "High users",
           cannabis_group == "Non-users" ~ "Non-users",
           cannabis_group == "Infrequent users" ~ "Infrequent users",
           cannabis_group == "Frequent users" ~ "Frequent users"
         ))

# fill used by down
#visualsearch <- visualsearch %>% 
#  group_by(id) %>% 
#  fill(sex, physically_activity, stressed, video_games, sleep_last, 
#       concussion, music, year_of_birth, cannabis_group,
#       cannabis_freqnum, .direction = "downup")

# Reorder the 'users' factor variable according to the vector above
visualsearch$users <- factor(visualsearch$users, levels = user_order)

visualsearch %>%
  group_by(users) %>%
  summarise_at(vars(RT_6_absent, RT_12_absent, RT_18_absent,
                    RT_6_present, RT_12_present, RT_18_present), list(name = mean))

visualsearch %>%
  group_by(users) %>%
  summarise_at(vars(propcorrect_6_absent, propcorrect_12_absent, propcorrect_18_absent,
                    propcorrect_6_present, propcorrect_12_present, propcorrect_18_present), 
               list(name = mean))

#### calculate BFs ####

# BF for RT
bf_df_all <- data.frame(group = character(),
                        group1 = character(),
                        group2 = character(),
                        p.adj = numeric(),
                        stringsAsFactors = FALSE)


# loop over the variables and fill the matrix
for (k in c("RT_6_absent", "RT_12_absent", "RT_18_absent",
            "RT_6_present", "RT_12_present", "RT_18_present")) {
  
  bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(visualsearch$users), levels(visualsearch$users)))
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j | i > j) {
        bf_matrix[i,j] <- "-"
      } else {
        test <- ttestBF(x = subset(visualsearch, users == levels(visualsearch$users)[i])[[k]],
                        y = subset(visualsearch, users == levels(visualsearch$users)[j])[[k]])@numerator$`Alt., r=0.707`@analysis$bf
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

# calculate y positions and add users

vs <-visualsearch %>%
  select(RT_6_absent:RT_18_present, users)  %>%
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
  group_by(users, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))

bf_df_all <- bf_df_all %>% mutate(set_size = str_extract(group, "\\d+"))
bf_df_all <- bf_df_all %>% mutate(Present = str_extract(group, "(present|absent)"))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                        y = case_when(group1 == "Non-users" ~ as.numeric(set_size) + 0.8,
                                                      group1 == "Infrequent users" ~ as.numeric(set_size) + 1.6,
                                                      group1 == "Frequent users" ~ as.numeric(set_size) + 2.4))

bf_df_all <- bf_df_all %>%
  mutate(group_1 = vs$mean[match(paste(group1, set_size, Present), paste(vs$users, vs$set_size, vs$Present))])

bf_df_all <- bf_df_all %>%
  mutate(group_2 = vs$mean[match(paste(group2, set_size, Present), paste(vs$users, vs$set_size, vs$Present))])

bf_df_all$users <- bf_df_all$group1

bf_df_all <- bf_df_all %>%
  mutate(
    group_min = ifelse(group_1 > group_2, group_2, group_1),
    group_max = ifelse(group_1 > group_2, group_1, group_2)
  )

bf_df_all$p.adj <- as.numeric(bf_df_all$p.adj)
bf_df_all$set_size <- as.double(bf_df_all$set_size)

bf_df_all <- bf_df_all %>% 
  select(p.adj, set_size, users, y, Present, group_min, group_max)

bf_df_all$group1 <- bf_df_all$group_min
bf_df_all$group2 <- bf_df_all$group_max


#### BF for propcorrect
bf_df_all_pc <- data.frame(group = character(),
                        group1 = character(),
                        group2 = character(),
                        p.adj = numeric(),
                        stringsAsFactors = FALSE)


# loop over the variables and fill the matrix
for (k in c("propcorrect_6_absent", "propcorrect_12_absent", "propcorrect_18_absent",
            "propcorrect_6_present", "propcorrect_12_present", "propcorrect_18_present")) {
  
  bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(visualsearch$users), levels(visualsearch$users)))
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j | i > j) {
        bf_matrix[i,j] <- "-"
      } else {
        test <- ttestBF(x = subset(visualsearch, users == levels(visualsearch$users)[i])[[k]],
                        y = subset(visualsearch, users == levels(visualsearch$users)[j])[[k]])@numerator$`Alt., r=0.707`@analysis$bf
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
  bf_df_all_pc <- rbind(bf_df_all_pc, bf_df)
}

# calculate y positions and add users

vs <-visualsearch %>%
  select(propcorrect_6_absent:propcorrect_18_present, users)  %>%
  pivot_longer(propcorrect_6_absent:propcorrect_18_present, names_to = "Group", values_to = "RT") %>%
  mutate(set_size = Group,
         set_size = case_when(Group == "propcorrect_6_absent" ~ 6,
                              Group == "propcorrect_12_absent" ~ 12,
                              Group == "propcorrect_18_absent" ~ 18,
                              Group == "propcorrect_6_present" ~ 6,
                              Group == "propcorrect_12_present" ~ 12,
                              Group == "propcorrect_18_present" ~ 18)) %>%
  mutate(Present = Group,
         Present = case_when(Group == "propcorrect_6_absent" ~ "absent",
                             Group == "propcorrect_12_absent" ~ "absent",
                             Group == "propcorrect_18_absent" ~ "absent",
                             Group == "propcorrect_6_present" ~ "present",
                             Group == "propcorrect_12_present" ~ "present",
                             Group == "propcorrect_18_present" ~ "present"))  %>%
  group_by(users, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))

bf_df_all_pc <- bf_df_all_pc %>% mutate(set_size = str_extract(group, "\\d+"))
bf_df_all_pc <- bf_df_all_pc %>% mutate(Present = str_extract(group, "(present|absent)"))

bf_df_all_pc <- bf_df_all_pc %>% mutate(y = group1,
                                        y = case_when(group1 == "Non-users" ~ as.numeric(set_size) + 0.8,
                                                      group1 == "Infrequent users" ~ as.numeric(set_size) + 1.6,
                                                      group1 == "Frequent users" ~ as.numeric(set_size) + 2.4))

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(group_1 = vs$mean[match(paste(group1, set_size, Present), paste(vs$users, vs$set_size, vs$Present))])

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(group_2 = vs$mean[match(paste(group2, set_size, Present), paste(vs$users, vs$set_size, vs$Present))])

bf_df_all_pc$users <- bf_df_all_pc$group1

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(
    group_min = ifelse(group_1 > group_2, group_2, group_1),
    group_max = ifelse(group_1 > group_2, group_1, group_2)
  )

bf_df_all_pc$p.adj <- as.numeric(bf_df_all_pc$p.adj)
bf_df_all_pc$set_size <- as.double(bf_df_all_pc$set_size)

bf_df_all_pc <- bf_df_all_pc %>% 
  select(p.adj, set_size, users, y, Present, group_min, group_max)

bf_df_all_pc$group1 <- bf_df_all_pc$group_min
bf_df_all_pc$group2 <- bf_df_all_pc$group_max

#### anova ####

vs <-visualsearch %>%
  select(RT_6_absent:RT_18_present, users)  %>%
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
                             Group == "RT_18_present" ~ "present"))  

vs$set_size_factor <- as.factor(vs$set_size)
vs$Present_factor <- as.factor(vs$Present)

model_vs <- anovaBF(formula = RT ~ users*set_size_factor*Present_factor, data = vs, rscaleFixed = "wide")

extractBF(model_vs)

r2_bayes(model_vs)


#### plotting ####

vs <-visualsearch %>%
  select(propcorrect_6_absent:propcorrect_18_present, users)  %>%
  pivot_longer(propcorrect_6_absent:propcorrect_18_present, names_to = "Group", values_to = "RT") %>%
  mutate(set_size = Group,
         set_size = case_when(Group == "propcorrect_6_absent" ~ 6,
                              Group == "propcorrect_12_absent" ~ 12,
                              Group == "propcorrect_18_absent" ~ 18,
                              Group == "propcorrect_6_present" ~ 6,
                              Group == "propcorrect_12_present" ~ 12,
                              Group == "propcorrect_18_present" ~ 18)) %>%
  mutate(Present = Group,
         Present = case_when(Group == "propcorrect_6_absent" ~ "absent",
                             Group == "propcorrect_12_absent" ~ "absent",
                             Group == "propcorrect_18_absent" ~ "absent",
                             Group == "propcorrect_6_present" ~ "present",
                             Group == "propcorrect_12_present" ~ "present",
                             Group == "propcorrect_18_present" ~ "present"))  %>%
  group_by(users, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))


# plot for RT
visualsearch %>%
  select(RT_6_absent:RT_18_present, users)  %>%
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
  group_by(users, set_size, Present)  %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))%>%
  ggplot(aes(y=set_size, x=mean, 
             group=interaction(Present, users),
             shape = Present,
             color=users)) + 
  geom_errorbar(aes(xmin=mean-se, xmax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.1) +
  geom_path(aes(linetype = Present)) +
  geom_point(size=3) +
  stat_pvalue_manual(
    data = bf_df_all, label = "BF = {p.adj}",
    xmin = "group_min", xmax = "group_max",
    y.position = "y", coord.flip = TRUE,
    vjust = 0
  )+ 
  scale_y_continuous(breaks = c(6,12,18))+
  theme_bw()+
  theme(legend.position = "bottom", axis.title.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(size = 16))+
  labs(title = "Visual Search", x = "RT", y = "Set size")+ 
  scale_color_discrete(name = "Users", 
                       labels=c(paste0("Non-User\n (n=", table(visualsearch$users)["Non-users"][[1]], ")"), 
                                paste0("Infrequent User\n (n=", table(visualsearch$users)["Infrequent users"][[1]], ")"), 
                                paste0("Frequent Users\n (n=", table(visualsearch$users)["Frequent users"][[1]], ")"), 
                                paste0("High users\n (n=", table(visualsearch$users)["High users"][[1]], ")")))+
  scale_shape_discrete(name = "Condition") +
  scale_linetype_discrete(name = "Condition") +
  coord_flip()

#### graph for poster ####

bf_df_all$p.adj <- as.numeric(bf_df_all$p.adj)
bf_df_all <- bf_df_all[bf_df_all$p.adj > 3 | bf_df_all$p.adj < 1/3, ]
bf_df_all$p <- ifelse((bf_df_all$p.adj > 3 & bf_df_all$p.adj <= 10) | (bf_df_all$p.adj < 1/3 & bf_df_all$p.adj >= 1/10), "*", 
                      ifelse((bf_df_all$p.adj > 10 & bf_df_all$p.adj <= 30) | (bf_df_all$p.adj < 1/10 & bf_df_all$p.adj >= 1/30), "**", "***"))

# calculate new y
bf_df_all$distance <- bf_df_all$group_max - bf_df_all$group_min
bf_df_all <- bf_df_all %>%
  group_by(set_size, Present) %>%
  mutate(y_calc = case_when(
    distance == min(distance) ~ 0.3,
    distance == median(distance) ~ 0.2,
    distance == max(distance) ~ 0.1,
    TRUE ~ NA_real_
  )) %>%
  ungroup()
bf_df_all <- bf_df_all %>%
  mutate(y = case_when(
    set_size == 6 ~ 1 + y_calc,
    set_size == 12 ~ 2 + y_calc,
    set_size == 18 ~ 3 + y_calc,
    TRUE ~ NA_real_
  ))

p7<- visualsearch %>%
  select(RT_6_absent:RT_18_present, users)  %>%
  pivot_longer(RT_6_absent:RT_18_present, names_to = "Group", values_to = "RT") %>%
  mutate(set_size = Group,
         set_size = case_when(Group == "RT_6_absent" ~ 1,
                              Group == "RT_12_absent" ~ 2,
                              Group == "RT_18_absent" ~ 3,
                              Group == "RT_6_present" ~ 1,
                              Group == "RT_12_present" ~ 2,
                              Group == "RT_18_present" ~ 3)) %>%
  mutate(Present = Group,
         Present = case_when(Group == "RT_6_absent" ~ "absent",
                             Group == "RT_12_absent" ~ "absent",
                             Group == "RT_18_absent" ~ "absent",
                             Group == "RT_6_present" ~ "present",
                             Group == "RT_12_present" ~ "present",
                             Group == "RT_18_present" ~ "present"))  %>%
  group_by(users, set_size, Present)  %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))%>%
  ggplot(aes(y=set_size, x=mean, 
             group=interaction(Present, users),
             shape = Present,
             color=users)) +
  geom_errorbar(aes(xmin=mean-se, xmax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.3, color = "black", size = 1) +
  geom_path(aes(linetype = Present), size = 1.5) +
  geom_point(size=4) +
  #stat_pvalue_manual(
  #  data = bf_df_all, label = "{p}",
  #  xmin = "group_min", xmax = "group_max",
  #  y.position = "y", coord.flip = TRUE,
  #  vjust = 0
  #)+ 
  scale_y_continuous(breaks = c(1,2,3), labels = c("6", "12", "18"), limits = c(0.5, 3.5))+
  theme_bw()+
  theme(legend.position = c(0.3, 0.8),  # Adjust the position of the legend inside the graph
        legend.box = "horizontal",  # Set the legend box style to horizontal
        legend.title = element_blank(),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Lato", size = 24),
        panel.grid = element_blank())+
  labs(title = "Visual Search", x = "Reaction Time (s)", y = "Set size")+ 
  scale_color_manual(values = c("Non-users" = "#f8766d", 
                                  "Infrequent users" = "#7caeff", 
                                  "Frequent users" = "#679267", 
                                  "High users" = "#00FF00"),
                       labels=c(paste0("Non-User\n (n=", table(visualsearch$users)["Non-users"][[1]], ")"), 
                                paste0("Infrequent User\n (n=", table(visualsearch$users)["Infrequent users"][[1]], ")"), 
                                paste0("Frequent Users\n (n=", table(visualsearch$users)["Frequent users"][[1]], ")"), 
                                paste0("High users\n (n=", table(visualsearch$users)["High users"][[1]], ")")))+
  coord_flip()+
  geom_segment(aes(y = 0.875, yend = 0.875, x = 2.07, xend = 2.20), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 0.875, yend = 0.9, x = 2.07, xend = 2.07), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 0.875, yend = 0.9, x = 2.20, xend = 2.20), linewidth = 0.5, color = "#7caeff") +
  geom_text(y = 0.88, x = 2.235, label = "#", size = 5, color = "#7caeff")+
  geom_segment(aes(y = 0.8, yend = 0.8, x = 2.10, xend = 2.20), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 0.8, yend = 0.825, x = 2.10, xend = 2.10), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 0.8, yend = 0.825, x = 2.20, xend = 2.20), linewidth = 0.5, color = "#f8766d") +
  geom_text(y = 0.81, x = 2.235, label = "#", size = 5, color = "#f8766d")+
  geom_segment(aes(y = 0.725, yend = 0.725, x = 2.15, xend = 2.20), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 0.725, yend = 0.75, x = 2.15, xend = 2.15), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 0.725, yend = 0.75, x = 2.20, xend = 2.20), linewidth = 0.5, color = "#679267") +
  geom_text(y = 0.73, x = 2.235, label = "#", size = 5, color = "#679267")+
  geom_segment(aes(y = 0.875, yend = 0.875, x = 1.60, xend = 1.66), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 0.875, yend = 0.9, x = 1.60, xend = 1.60), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 0.875, yend = 0.9, x = 1.66, xend = 1.66), linewidth = 0.5, color = "#679267") +
  geom_text(y = 0.88, x = 1.685, label = "#", size = 5, color = "#679267")+
  geom_segment(aes(y = 0.8, yend = 0.8, x = 1.60, xend = 1.55), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 0.8, yend = 0.825, x = 1.60, xend = 1.60), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 0.8, yend = 0.825, x = 1.55, xend = 1.55), linewidth = 0.5, color = "#7caeff") +
  geom_text(y = 0.81, x = 1.635, label = "#", size = 5, color = "#7caeff")+
  geom_segment(aes(y = 0.725, yend = 0.725, x = 1.60, xend = 1.62), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 0.725, yend = 0.75, x = 1.60, xend = 1.60), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 0.725, yend = 0.75, x = 1.62, xend = 1.62), linewidth = 0.5, color = "#f8766d") +
  geom_text(y = 0.73, x = 1.655, label = "#", size = 5, color = "#f8766d")+
  geom_segment(aes(y = 1.875, yend = 1.875, x = 3.38, xend = 3.53), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 1.875, yend = 1.9, x = 3.38, xend = 3.38), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 1.875, yend = 1.9, x = 3.53, xend = 3.53), linewidth = 0.5, color = "#679267") +
  geom_text(y = 1.825, x = 3.46, label = "#", size = 5, color = "#679267")+
  geom_segment(aes(y = 1.8, yend = 1.8, x = 3.38, xend = 3.45), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 1.8, yend = 1.825, x = 3.38, xend = 3.38), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 1.8, yend = 1.825, x = 3.45, xend = 3.45), linewidth = 0.5, color = "#7caeff") +
  geom_text(y = 1.75, x = 3.415, label = "#", size = 5, color = "#7caeff")+
  geom_segment(aes(y = 1.725, yend = 1.725, x = 3.38, xend = 3.39), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 1.725, yend = 1.75, x = 3.38, xend = 3.38), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 1.725, yend = 1.75, x = 3.39, xend = 3.39), linewidth = 0.5, color = "#f8766d") +
  geom_text(y = 1.675, x = 3.385, label = "#", size = 5, color = "#f8766d")+
  geom_segment(aes(y = 1.875, yend = 1.875, x = 2.37, xend = 2.45), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 1.875, yend = 1.9, x = 2.37, xend = 2.37), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 1.875, yend = 1.9, x = 2.45, xend = 2.45), linewidth = 0.5, color = "#7caeff") +
  geom_text(y = 1.825, x = 2.485, label = "#", size = 5, color = "#7caeff")+
  geom_segment(aes(y = 1.8, yend = 1.8, x = 2.37, xend = 2.43), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 1.8, yend = 1.825, x = 2.37, xend = 2.37), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 1.8, yend = 1.825, x = 2.43, xend = 2.43), linewidth = 0.5, color = "#f8766d") +
  geom_text(y = 1.75, x = 2.43, label = "#", size = 5, color = "#f8766d")+
  geom_segment(aes(y = 3.125, yend = 3.125, x = 4.46, xend = 4.76), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 3.125, yend = 3.1, x = 4.46, xend = 4.46), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 3.125, yend = 3.1, x = 4.76, xend = 4.76), linewidth = 0.5, color = "#679267") +
  geom_text(y = 3.175, x = 4.61, label = "#", size = 5, color = "#679267")+
  geom_segment(aes(y = 3.2, yend = 3.2, x = 4.46, xend = 4.58), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 3.2, yend = 3.175, x = 4.46, xend = 4.46), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 3.2, yend = 3.175, x = 4.58, xend = 4.58), linewidth = 0.5, color = "#7caeff") +
  geom_text(y = 3.25, x = 4.52, label = "#", size = 5, color = "#7caeff")+
  geom_segment(aes(y = 3.275, yend = 3.275, x = 4.46, xend = 4.48), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 3.275, yend = 3.25, x = 4.46, xend = 4.46), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 3.275, yend = 3.25, x = 4.48, xend = 4.48), linewidth = 0.5, color = "#f8766d") +
  geom_text(y = 3.325, x = 4.47, label = "#", size = 5, color = "#f8766d")+
  geom_segment(aes(y = 3.125, yend = 3.125, x = 3.16, xend = 3.33), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 3.125, yend = 3.1, x = 3.16, xend = 3.16), linewidth = 0.5, color = "#679267") +
  geom_segment(aes(y = 3.125, yend = 3.1, x = 3.33, xend = 3.33), linewidth = 0.5, color = "#679267") +
  geom_text(y = 3.175, x = 3.245, label = "#", size = 5, color = "#679267")+
  geom_segment(aes(y = 3.2, yend = 3.2, x = 3.16, xend = 3.20), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 3.2, yend = 3.175, x = 3.16, xend = 3.16), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(y = 3.2, yend = 3.175, x = 3.20, xend = 3.20), linewidth = 0.5, color = "#f8766d") +
  geom_text(y = 3.25, x = 3.18, label = "#", size = 5, color = "#f8766d")+
  geom_segment(aes(y = 3.275, yend = 3.275, x = 3.16, xend = 3.17), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 3.275, yend = 3.25, x = 3.16, xend = 3.16), linewidth = 0.5, color = "#7caeff") +
  geom_segment(aes(y = 3.275, yend = 3.25, x = 3.17, xend = 3.17), linewidth = 0.5, color = "#7caeff") +
  geom_text(y = 3.325, x = 3.165, label = "#", size = 5, color = "#7caeff")

  

ggsave("data/output/IE_VS_300.svg", plot = p7, width=200, height=300, units = "mm", dpi = 300)

