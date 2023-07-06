library(ggplot2)
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

#### tunneling ####

## filter those that didn't pass the screening (n = 103, removed)
#tunneling <- tunneling %>%
#  filter(passedscreening == TRUE)

# add new category for high users
tunneling <- tunneling %>% 
  mutate(users = cannabis_group,
         users = case_when(
           group == "experimental" ~ "High users",
           cannabis_group == "Non-users" ~ "Non-users",
           cannabis_group == "Infrequent users" ~ "Infrequent users",
           cannabis_group == "Frequent users" ~ "Frequent users"
         ))

# fill used by down
#tunneling <- tunneling %>% 
#  group_by(id) %>% 
#  fill(sex, physically_activity, stressed, video_games, sleep_last, 
#       concussion, music, year_of_birth, cannabis_group,
#       cannabis_freqnum, .direction = "downup")

tunneling  %>%
  group_by(users) %>%
  summarise_at(vars(MT_sc40, MT_sc60, MT_sc80, MT_sc100), list(name = mean), na.rm = TRUE)

# Reorder the 'users' factor variable according to the vector above
tunneling$users <- factor(tunneling$users, levels = user_order)

#### BF calculations ####

bf_df_all <- data.frame(group = character(),
                        group1 = character(),
                        group2 = character(),
                        p.adj = numeric(),
                        stringsAsFactors = FALSE)


# loop over the variables and fill the matrix
for (k in c("MT_sc40", "MT_sc60", "MT_sc80", "MT_sc100")) {
  
  bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(tunneling$users), levels(tunneling$users)))
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j | i > j) {
        bf_matrix[i,j] <- "-"
      } else {
        a = subset(tunneling, users == levels(tunneling$users)[i])[[k]]
        b = subset(tunneling, users == levels(tunneling$users)[j])[[k]]
        test <- extractBF(ttestBF(x = a,
                                  y = b))$bf
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
                                  multiplier = case_when(group == "MT_sc40" ~ 1,
                                                         group == "MT_sc60" ~ 2,
                                                         group == "MT_sc80" ~ 3,
                                                         TRUE ~ 4))
bf_df_all <- bf_df_all %>% mutate(group_2 = group,
                                  group_2 = case_when(group == "MT_sc40" ~ 1.33,
                                                      group == "MT_sc60" ~ 2.33,
                                                      group == "MT_sc80" ~ 3.33,
                                                      TRUE ~ 4.33))

bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "Non-users" ~ multiplier - 0.34,
                                                      group1 == "Infrequent users" ~ multiplier - 0.11,
                                                      group1 == "Frequent users" ~ multiplier + 0.1))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                  y = case_when(group1 == "Non-users" ~ 9.5,
                                                group1 == "Infrequent users" ~ 10.5,
                                                group1 == "Frequent users" ~ 11.5))

bf_df_all$users <- bf_df_all$group1


#### calculate ANOVA bf ####

tn <- tunneling %>%
  select(id, MT_sc40, MT_sc60, MT_sc80, MT_sc100, users)  %>%
  pivot_longer(cols = starts_with("MT_"), names_to = "Group", values_to = "MT") %>%
  mutate(tasks = factor(case_when(
    Group == "MT_sc40" ~ "sc40",
    Group == "MT_sc60" ~ "sc60",
    Group == "MT_sc80" ~ "sc80",
    Group == "MT_sc100" ~ "sc100"
  ), levels = c("sc40", "sc60", "sc80", "sc100")))


tn$id <- as.factor(tn$id)

options(scipen = 999)

model_tn <- anovaBF(formula = MT ~ users*tasks, data = tn, rscaleFixed = "wide")

extractBF(model_tn)

r2_bayes(model_tn)

#### plot ####

ggplot(tn, aes(x = tasks, y = MT, fill = users)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Tunneling", x = "Scaled tracks %", y = "MT") +
  scale_fill_manual(values = c("Non-users" = "#f8766d", 
                               "Infrequent users" = "#7caeff", 
                               "Frequent users" = "#00ba38", 
                               "High users" = "#c77cff"),
                    labels=c(paste0("Non-User\n (n=", table(tunneling$users)["Non-users"][[1]], ")"), 
                             paste0("Infrequent User\n (n=", table(tunneling$users)["Infrequent users"][[1]], ")"), 
                             paste0("Frequent Users\n (n=", table(tunneling$users)["Frequent users"][[1]], ")"), 
                             paste0("High users\n (n=", table(tunneling$users)["High users"][[1]], ")"))) +
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

p<-tn %>% group_by(users, tasks)  %>%
  mutate(count = n()) %>%
  group_by(users, tasks, count) %>%
  summarise_at(vars(MT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))%>%
  ggplot(aes(x=tasks, y=mean, 
             group=users,
             color=users)) +
  geom_path(size = 2) +
  geom_point(size=4) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.3, color = "black", size = 1) +
  labs(title = "Tunneling", x = "Scaled tracks %", y = "Movement Time (s)") +
  scale_color_manual(values = c("Non-users" = "#f8766d", 
                                "Infrequent users" = "#7caeff", 
                                "Frequent users" = "#679267", 
                                "High users" = "#00FF00"),
                     labels=c(paste0("Non-Users (n=", table(tunneling$users)["Non-users"][[1]], ")"), 
                             paste0("Infrequent Users (n=", table(tunneling$users)["Infrequent users"][[1]], ")"), 
                             paste0("Frequent Users (n=", table(tunneling$users)["Frequent users"][[1]], ")"), 
                             paste0("High users (n=", table(tunneling$users)["High users"][[1]], ")"))) +
  scale_y_continuous(limits = c(2, 5)) +
  theme_bw() + 
  theme(legend.position = c(0.3, 0.3),  # Adjust the position of the legend inside the graph
        legend.box = "horizontal",  # Set the legend box style to horizontal
        legend.title = element_blank(),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Lato", size = 24),
        panel.grid = element_blank())

ggsave("data/output/IE_tunneling_300.svg", plot = p, width=200, height=240, units = "mm", dpi = 300)