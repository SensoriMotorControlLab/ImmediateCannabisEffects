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

#### nback ####

## filter those that didn't pass the screening (n = 103, removed)
#nback <- nback %>%
#  filter(passedscreening == TRUE)

# add new category for high users
nback <- nback %>% 
  mutate(users = cannabis_group,
         users = case_when(
           group == "experimental" ~ "High users",
           cannabis_group == "Non-users" ~ "Non-users",
           cannabis_group == "Infrequent users" ~ "Infrequent users",
           cannabis_group == "Frequent users" ~ "Frequent users"
         ))

# fill used by down
#nback <- nback %>% 
#  group_by(id) %>% 
#  fill(sex, physically_activity, stressed, video_games, sleep_last, 
#       concussion, music, year_of_birth, cannabis_group,
#       cannabis_freqnum, .direction = "downup")

nback  %>%
  group_by(users) %>%
  summarise_at(vars(N1_dprime, N2_dprime, N3_dprime), list(name = mean), na.rm = TRUE)

# Reorder the 'users' factor variable according to the vector above
nback$users <- factor(nback$users, levels = user_order)

#### BF calculations ####

bf_df_all <- data.frame(group = character(),
                        group1 = character(),
                        group2 = character(),
                        p.adj = numeric(),
                        stringsAsFactors = FALSE)


# loop over the variables and fill the matrix
for (k in c("N1_dprime", "N2_dprime", "N3_dprime")) {
  
  bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(nback$users), levels(nback$users)))
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i == j | i > j) {
        bf_matrix[i,j] <- "-"
      } else {
        a = subset(nback, users == levels(nback$users)[i])[[k]]
        b = subset(nback, users == levels(nback$users)[j])[[k]]
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
                                  multiplier = case_when(group == "N1_dprime" ~ 1,
                                                         group == "N2_dprime" ~ 2,
                                                         TRUE ~ 3))
bf_df_all <- bf_df_all %>% mutate(group_2 = group,
                                  group_2 = case_when(group == "N1_dprime" ~ 1.33,
                                                      group == "N2_dprime" ~ 2.33,
                                                      TRUE ~ 3.33))

bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "Non-users" ~ multiplier - 0.34,
                                                      group1 == "Infrequent users" ~ multiplier - 0.11,
                                                      group1 == "Frequent users" ~ multiplier + 0.1))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                  y = case_when(group1 == "Non-users" ~ 4.5,
                                                group1 == "Infrequent users" ~ 5,
                                                group1 == "Frequent users" ~ 5.5))

bf_df_all$users <- bf_df_all$group1


#### calculate ANOVA bf ####

nb <- nback %>%
  select(id, N1_dprime, N2_dprime, N3_dprime, users)  %>%
  pivot_longer(cols = ends_with("_dprime"), names_to = "Group", values_to = "dprime") %>%
  mutate(tasks = factor(case_when(
    Group == "N1_dprime" ~ "N1",
    Group == "N2_dprime" ~ "N2",
    Group == "N3_dprime" ~ "N3"
  ), levels = c("N1", "N2", "N3")))


nb$id <- as.factor(nb$id)

options(scipen = 999)

model_nb <- anovaBF(formula = dprime ~ users*tasks, data = nb, rscaleFixed = "wide")

extractBF(model_nb)

r2_bayes(model_nb)

#### plot ####

ggplot(nb, aes(x = tasks, y = dprime, fill = users)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "N-Back", x = "Tasks", y = "dprime") +
  scale_fill_manual(values = c("Non-users" = "#f8766d", 
                               "Infrequent users" = "#7caeff", 
                               "Frequent users" = "#00ba38", 
                               "High users" = "#c77cff"),
                    labels=c(paste0("Non-User\n (n=", table(nback$users)["Non-users"][[1]], ")"), 
                             paste0("Infrequent User\n (n=", table(nback$users)["Infrequent users"][[1]], ")"), 
                             paste0("Frequent Users\n (n=", table(nback$users)["Frequent users"][[1]], ")"), 
                             paste0("High users\n (n=", table(nback$users)["High users"][[1]], ")"))) +
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

p3<-nb %>%
  group_by(users, tasks)  %>%
    mutate(count = n()) %>%
    group_by(users, tasks, count) %>%
    summarise_at(vars(dprime), list(mean = mean, sd = sd)) %>%
    mutate(se = sd/sqrt(count)) %>%
    ggplot(aes(x=tasks, y=mean, 
               group= users,
               color=users)) +
  geom_path(size = 1.5) +
  geom_point(size=4) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.3, color = "black", size = 1) +
  labs(title = "N-Back", x = "", y = "dprime (d')") +
  scale_color_manual(values = c("Non-users" = "#f8766d", 
                               "Infrequent users" = "#7caeff", 
                               "Frequent users" = "#679267", 
                               "High users" = "#00FF00"),
                    labels=c(paste0("Non-Users (n=", table(nback$users)["Non-users"][[1]], ")"), 
                             paste0("Infrequent Users (n=", table(nback$users)["Infrequent users"][[1]], ")"), 
                             paste0("Frequent Users (n=", table(nback$users)["Frequent users"][[1]], ")"), 
                             paste0("High users (n=", table(nback$users)["High users"][[1]], ")"))) +
  scale_x_discrete(labels = c("1-Back", "2-Back", "3-Back"))+
  theme_bw() + 
  theme(legend.position = c(0.3, 0.2),  # Adjust the position of the legend inside the graph
        legend.box = "horizontal",  # Set the legend box style to horizontal
        legend.title = element_blank(),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Lato", size = 24),
        panel.grid = element_blank()) +
  geom_segment(aes(x = 0.85, xend = 0.85, y = 2.78, yend = 3.19), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(x = 0.85, xend = 0.9, y = 2.78, yend = 2.78), linewidth = 0.5, color = "#f8766d") +
  geom_segment(aes(x = 0.85, xend = 0.9, y = 3.19, yend = 3.19), linewidth = 0.5, color = "#f8766d") +
  geom_text(x = 0.8, y = 2.98, label = "***", size = 5, color = "#f8766d", angle = 90) 

ggsave("data/output/IE_nback_300.svg", plot = p3, width=200, height=300, units = "mm", dpi = 300)