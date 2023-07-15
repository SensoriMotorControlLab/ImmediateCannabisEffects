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


#### calculate mean by groups ####

#### trailmaking ####

# remove observations that didn't pass the screening (0)
#trailmaking <- trailmaking %>%
#  filter(passedscreening == TRUE)

# add new category for high users
trailmaking <- trailmaking %>% 
    mutate(users = cannabis_group,
           users = case_when(
             group == "experimental" ~ "High users",
             cannabis_group == "Non-users" ~ "Non-users",
             cannabis_group == "Infrequent users" ~ "Infrequent users",
             cannabis_group == "Frequent users" ~ "Frequent users"
           ))

# fill used by down
#trailmaking <- trailmaking %>% 
#  group_by(id) %>% 
#  fill(sex, physically_activity, stressed, video_games, sleep_last, 
#       concussion, music, year_of_birth, cannabis_group,
#       cannabis_freqnum, .direction = "downup")

# remove one outlier among infrequent users who took >600 time
trailmaking <- trailmaking[trailmaking$MoveTime_1 <= 600, ]

trailmaking  %>%
  group_by(users) %>%
  summarise_at(vars(MoveTime_1, MoveTime_2, MoveTime_3, MoveTime_4, MoveTime_5), 
               list(name = mean), na.rm = TRUE)

#### Bivariate Analysis ####

# Reorder the 'users' factor variable according to the vector above
trailmaking$users <- factor(trailmaking$users, levels = user_order)

# Box plot by group
ggplot(trailmaking, aes(x = users, y = MoveTime_1, fill= users)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  labs(title = "Go/No-Go", x = "")+ 
  scale_x_discrete(labels=c(paste0("Non-User\n (n=", table(trailmaking$users)["Non-users"][[1]], ")"), 
                            paste0("Infrequent User\n (n=", table(trailmaking$users)["Infrequent users"][[1]], ")"), 
                            paste0("Frequent Users\n (n=", table(trailmaking$users)["Frequent users"][[1]], ")"), 
                            paste0("High users\n (n=", table(trailmaking$users)["High users"][[1]], ")")))+ 
  theme(legend.position = "none") 



#### calculate BFs ####

bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(trailmaking$users), levels(trailmaking$users)))

for (i in 1:4) {
  for (j in 1:4) {
    if (i == j | i > j) {
      bf_matrix[i,j] <- "-"
    } else {
      test <- ttestBF(x = subset(trailmaking, users == levels(trailmaking$users)[i])$MoveTime_1,
                      y = subset(trailmaking, users == levels(trailmaking$users)[j])$MoveTime_1)@numerator$`Alt., r=0.707`@analysis$bf
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

# keep only data for high users
bf_df <- bf_df[(bf_df$group1 == "High users") | (bf_df$group2 == "High users"), ]




#set.seed(123)

#ggbetweenstats(
#  data  = trailmaking,
#  x     = users,
#  y     = MoveTime_1,
#  title = "Go/No-Go", 
#  type = "bayes",
#  bf.prior = "medium",
#  centrality.type = "parametric"
#)

#### anova BF ####

trailmaking$id <- as.factor(trailmaking$id)

model_trailmaking <- anovaBF(formula = MoveTime_1 ~ users, data = trailmaking, rscaleFixed = "wide")

extractBF(model_trailmaking)

r2_bayes(model_trailmaking)


# add significance symbols to the plot
ggplot(trailmaking, aes(x = users, y = MoveTime_1)) + 
  geom_violin(width = 0.5) +
  #stat_boxplot(geom = "errorbar",
  #             width = 0.25) + 
  #geom_boxplot(width = 0.2) +
  geom_jitter(aes(col = users), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Trailmaking", x = "",
       y = "Move Time 1",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
       ) + 
  scale_x_discrete(labels=c(paste0("Non-User\n (n=", table(trailmaking$users)["Non-users"][[1]], ")"), 
                            paste0("Infrequent User\n (n=", table(trailmaking$users)["Infrequent users"][[1]], ")"), 
                            paste0("Frequent Users\n (n=", table(trailmaking$users)["Frequent users"][[1]], ")"), 
                            paste0("High Users\n (n=", table(trailmaking$users)["High users"][[1]], ")")))+
  theme_bw() + 
  theme(legend.position = "none", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank()) +
  stat_summary(
    fun = mean, 
    geom = "label_repel", 
    aes(label = sprintf("μ = %.2f", after_stat(y))), 
    position = position_nudge_repel(x = 0.4, y = 0.2)
  ) +
  #stat_compare_means(method = "anova", label.y = 6.5)+ # Add global p-value
  #stat_compare_means(aes(label = paste0("p = ", after_stat(p.format))),
  #                   method = "t.test", ref.group = "High users", label.y = 5.6)
  ## manually
  #geom_segment(aes(x = 3, xend = 4, y = 5.8, yend = 5.8), linewidth = 0.3) +
  #geom_segment(aes(x = 3, xend = 3, y = 5.7, yend = 5.8), linewidth = 0.3) +
  #geom_segment(aes(x = 4, xend = 4, y = 5.7, yend = 5.8), linewidth = 0.3) +
  #geom_text(x = 3.5, y = 5.9, label = expression("BF"[10]*" = 59.74"), size = 3) +
  #geom_segment(aes(x = 2, xend = 4, y = 6, yend = 6), linewidth = 0.3) +
  #geom_segment(aes(x = 2, xend = 2, y = 5.9, yend = 6), linewidth = 0.3) +
  #geom_segment(aes(x = 4, xend = 4, y = 5.9, yend = 6), linewidth = 0.3) +
#geom_text(x = 3, y = 6.1, label = expression("BF"[10]*" = 0.73"), size = 3) +
#geom_segment(aes(x = 1, xend = 4, y = 6.2, yend = 6.2), linewidth = 0.3) +
#geom_segment(aes(x = 1, xend = 1, y = 6.1, yend = 6.2), linewidth = 0.3) +
#geom_segment(aes(x = 4, xend = 4, y = 6.1, yend = 6.2), linewidth = 0.3) +
#geom_text(x = 2.5, y = 6.3, label = expression("BF"[10]*" = 0.41"), size = 3)
## custom
stat_pvalue_manual(
  data = bf_df, label = "BF = {p.adj}",
  xmin = "group1", xmax = "group2",
  y.position = c(160, 170, 180)
)

#### graph for poster ####

bf_df$p.adj <- as.numeric(bf_df$p.adj)
bf_df <- bf_df[bf_df$p.adj > 3 | bf_df$p.adj < 1/3, ]
bf_df$p <- ifelse((bf_df$p.adj > 3 & bf_df$p.adj <= 10) | (bf_df$p.adj < 1/3 & bf_df$p.adj >= 1/10), "*", 
                  ifelse((bf_df$p.adj > 10 & bf_df$p.adj <= 30) | (bf_df$p.adj < 1/10 & bf_df$p.adj >= 1/30), "**", "***"))

bf_df$users <- bf_df$group1

p5<-ggplot(trailmaking, aes(x = users, y = MoveTime_1, fill = users)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot(outlier.shape = "") +
  labs(title = "Trailmaking", x = "",
       y = "Movement Time (s)",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
  ) +  
  scale_fill_manual(values = c("Non-users" = "#f8766d", 
                               "Infrequent users" = "#7caeff", 
                               "Frequent users" = "#679267", 
                               "High users" = "#00FF00"),
  ) +
  scale_x_discrete(labels=c(paste0("Non-Users\n (n=", table(trailmaking$users)["Non-users"][[1]], ")"), 
                            paste0("Infrequent\n Users\n (n=", table(trailmaking$users)["Infrequent users"][[1]], ")"), 
                            paste0("Frequent\n Users\n (n=", table(trailmaking$users)["Frequent users"][[1]], ")"), 
                            paste0("High Users\n (n=", table(trailmaking$users)["High users"][[1]], ")")))+
  scale_y_continuous(limits = c(0, 150)) +
  theme_bw() + 
  theme(legend.position = "none", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Lato", size = 24),
        panel.grid = element_blank()) +
  geom_segment(aes(x = 1, xend = 4, y = 120, yend = 120), linewidth = 0.3, color = "#f8766d") +
  geom_segment(aes(x = 1, xend = 1, y = 115, yend = 120), linewidth = 0.3, color = "#f8766d") +
  geom_segment(aes(x = 4, xend = 4, y = 115, yend = 120), linewidth = 0.3, color = "#f8766d") +
  geom_text(x = 2.5, y = 125, label = "**", size = 5, color = "#f8766d")
  #stat_pvalue_manual(
  #  data = bf_df, label = "{p}",
  #  xmin = "group1", xmax = "group2",
  #  y.position = c(120)
  #)

ggsave("data/output/IE_trailmaking_300.svg", plot = p5, width=200, height=300, units = "mm", dpi = 300)
