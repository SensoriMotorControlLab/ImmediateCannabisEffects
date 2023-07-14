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

#### mirror ####

# remove observations that didn't pass the screening (0)
#mirror <- mirror %>%
#  filter(passedscreening == TRUE)

# add new category for high users
mirror <- mirror %>% 
  mutate(users = cannabis_group,
         users = case_when(
           group == "experimental" ~ "High users",
           cannabis_group == "Non-users" ~ "Non-users",
           cannabis_group == "Infrequent users" ~ "Infrequent users",
           cannabis_group == "Frequent users" ~ "Frequent users"
         ))

# fill used by down
#mirror <- mirror %>% 
#  group_by(id) %>% 
#  fill(sex, physically_activity, stressed, video_games, sleep_last, 
#       concussion, music, year_of_birth, cannabis_group,
#       cannabis_freqnum, .direction = "downup")

mirror  %>%
  group_by(users) %>%
  summarise_at(vars(meanMT), 
               list(name = mean), na.rm = TRUE)

#### Bivariate Analysis ####

# Reorder the 'users' factor variable according to the vector above
mirror$users <- factor(mirror$users, levels = user_order)

# Box plot by group
ggplot(mirror, aes(x = users, y = meanMT, fill= users)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  labs(title = "Mirror Reversal", x = "")+ 
  scale_x_discrete(labels=c(paste0("Non-User\n (n=", table(mirror$users)["Non-users"][[1]], ")"), 
                            paste0("Infrequent User\n (n=", table(mirror$users)["Infrequent users"][[1]], ")"), 
                            paste0("Frequent Users\n (n=", table(mirror$users)["Frequent users"][[1]], ")"), 
                            paste0("High users\n (n=", table(mirror$users)["High users"][[1]], ")")))+ 
  theme(legend.position = "none") 



#### calculate BFs ####

bf_matrix <- matrix(0, nrow = 4, ncol = 4, dimnames = list(levels(mirror$users), levels(mirror$users)))

for (i in 1:4) {
  for (j in 1:4) {
    if (i == j | i > j) {
      bf_matrix[i,j] <- "-"
    } else {
      test <- ttestBF(x = subset(mirror, users == levels(mirror$users)[i])$meanMT,
                      y = subset(mirror, users == levels(mirror$users)[j])$meanMT)@numerator$`Alt., r=0.707`@analysis$bf
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
#  data  = mirror,
#  x     = users,
#  y     = meanMT,
#  title = "Go/No-Go", 
#  type = "bayes",
#  bf.prior = "medium",
#  centrality.type = "parametric"
#)

#### anova BF ####

mirror$id <- as.factor(mirror$id)

model_mirror <- anovaBF(formula = meanMT ~ users, data = mirror, rscaleFixed = "wide")

extractBF(model_mirror)

r2_bayes(model_mirror)


# add significance symbols to the plot
ggplot(mirror, aes(x = users, y = meanMT)) + 
  geom_violin(width = 0.5) +
  #stat_boxplot(geom = "errorbar",
  #             width = 0.25) + 
  #geom_boxplot(width = 0.2) +
  geom_jitter(aes(col = users), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Mirror Reversal", x = "",
       y = "Avg. Move Time",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
  ) + 
  scale_x_discrete(labels=c(paste0("Non-User\n (n=", table(mirror$users)["Non-users"][[1]], ")"), 
                            paste0("Infrequent User\n (n=", table(mirror$users)["Infrequent users"][[1]], ")"), 
                            paste0("Frequent Users\n (n=", table(mirror$users)["Frequent users"][[1]], ")"), 
                            paste0("High Users\n (n=", table(mirror$users)["High users"][[1]], ")")))+
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
  stat_pvalue_manual(
    data = bf_df, label = "BF = {p.adj}",
    xmin = "group1", xmax = "group2",
    y.position = c(15, 17, 19)
  )

#### graph for poster ####

bf_df$p.adj <- as.numeric(bf_df$p.adj)
bf_df <- bf_df[bf_df$p.adj > 3 | bf_df$p.adj < 1/3, ]
bf_df$p <- ifelse((bf_df$p.adj > 3 & bf_df$p.adj <= 10) | (bf_df$p.adj < 1/3 & bf_df$p.adj >= 1/10), "*", 
                  ifelse((bf_df$p.adj > 10 & bf_df$p.adj <= 30) | (bf_df$p.adj < 1/10 & bf_df$p.adj >= 1/30), "**", "***"))

bf_df$users <- bf_df$group1

p<-ggplot(mirror, aes(x = users, y = meanMT, fill = users)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot(outlier.shape = "") +
  labs(title = "Mirror Reversal", x = "",
       y = "Avg. Movement Time (s)",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
  ) +  
  scale_fill_manual(values = c("Non-users" = "#f8766d", 
                               "Infrequent users" = "#7caeff", 
                               "Frequent users" = "#679267", 
                               "High users" = "#00FF00"),
  ) +
  scale_x_discrete(labels=c(paste0("Non-Users\n (n=", table(mirror$users)["Non-users"][[1]], ")"), 
                            paste0("Infrequent\n Users\n (n=", table(mirror$users)["Infrequent users"][[1]], ")"), 
                            paste0("Frequent\n Users\n (n=", table(mirror$users)["Frequent users"][[1]], ")"), 
                            paste0("High Users\n (n=", table(mirror$users)["High users"][[1]], ")")))+
  scale_y_continuous(limits = c(0, 7)) +
  theme_bw() + 
  theme(legend.position = "none", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Lato", size = 24),
        panel.grid = element_blank()) +
  stat_pvalue_manual(
    data = bf_df, label = "{p}",
    xmin = "group1", xmax = "group2",
    y.position = c(5)
  )

ggsave("data/output/IE_mirror_300.svg", plot = p, width=200, height=240, units = "mm", dpi = 300)
