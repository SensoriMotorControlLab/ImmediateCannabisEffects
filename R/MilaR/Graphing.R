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


#### calculate mean by groups ####

#### gonogo ####

gonogo <- gonogo %>%
  filter(passedscreening == TRUE)

# add new category for high users
gonogo <- gonogo %>% 
  mutate(users = cannabis_freqnum,
         users = case_when(
           group == "experimental" ~ "High users",
           cannabis_freqnum == 0 ~ "Non-users",
           cannabis_freqnum > 5 ~ "Frequent users",
           TRUE ~ "Infrequent users",
         ))

# fill used by down
gonogo <- gonogo %>% 
  group_by(id) %>% 
  fill(sex, physically_activity, stressed, video_games, sleep_last, 
       concussion, music, year_of_birth, cannabis_group,
       go_prop.error, go_RT, nogo_RT, dprime, sensitivity, totaltime, 
      passedscreening,  cannabis_freqnum, .direction = "downup")

gonogo  %>%
  group_by(users) %>%
  summarise_at(vars(dprime), list(name = mean), na.rm = TRUE)

# Create a vector of user groups in the desired order
user_order <- c("Non-users", "Infrequent users", "Frequent users", "High users")

# Reorder the 'users' factor variable according to the vector above
gonogo$users <- factor(gonogo$users, levels = user_order)

# drop missing on sex and gonogo (n = 23)
gonogo <- gonogo[complete.cases(gonogo[, c("sex", "nogo_RT")]), ]

# Box plot by group
ggplot(gonogo, aes(x = users, y = dprime, fill= users)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot() +
  labs(title = "Go/No-Go", x = "")+ 
  scale_x_discrete(labels=c(paste("Non-User\n (n=", table(gonogo$users)["Non-users"][[1]], ")"), 
                            paste("Infrequent User\n (n=", table(gonogo$users)["Infrequent users"][[1]], ")"), 
                            paste("Frequent Users\n (n=", table(gonogo$users)["Frequent users"][[1]], ")"), 
                            paste("High users\n (n=", table(gonogo$users)["High users"][[1]], ")")))+ 
  theme(legend.position = "none") 

## bar plots for error proportions

group_summary <- gonogo %>%
  group_by(users) %>%
  summarize(mean_gpe = mean(go_prop.error),
            se_gpe = sd(go_prop.error) / sqrt(n()))

# Bar plot with error bars
ggplot(group_summary, aes(x = users, y = mean_gpe, fill = users)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_gpe - se_gpe, ymax = mean_gpe + se_gpe),
                width = 0.25, position = position_dodge()) +
  labs(title = "Go/No-Go: Go Error Proportion", x = "",
       y = "d'") +
  scale_x_discrete(labels=c(paste("Non-User\n (n=", table(gonogo$users)["Non-users"][[1]], ")"), 
                            paste("Infrequent User\n (n=", table(gonogo$users)["Infrequent users"][[1]], ")"), 
                            paste("Frequent Users\n (n=", table(gonogo$users)["Frequent users"][[1]], ")"), 
                            paste("High users\n (n=", table(gonogo$users)["High users"][[1]], ")")))+
  theme(legend.position = "none")


group_summary <- gonogo %>%
  group_by(users) %>%
  summarize(mean_gpe = mean(nogo_prop.error),
            se_gpe = sd(nogo_prop.error) / sqrt(n()))

# Bar plot with error bars
ggplot(group_summary, aes(x = users, y = mean_gpe, fill = users)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_gpe - se_gpe, ymax = mean_gpe + se_gpe),
                width = 0.25, position = position_dodge()) +
  labs(title = "Go/No-Go: No-Go Error Proportion", x = "",
       y = "d'") +
  scale_x_discrete(labels=c(paste("Non-User\n (n=", table(gonogo$users)["Non-users"][[1]], ")"), 
                            paste("Infrequent User\n (n=", table(gonogo$users)["Infrequent users"][[1]], ")"), 
                            paste("Frequent Users\n (n=", table(gonogo$users)["Frequent users"][[1]], ")"), 
                            paste("High users\n (n=", table(gonogo$users)["High users"][[1]], ")")))+
  theme(legend.position = "none")

## line segments for Response Times

# Calculate means and standard errors for go_RT and nogo_RT by users
data_summary <- gonogo %>%
  group_by(users) %>%
  summarize(mean_go_RT = mean(go_RT),
            se_go_RT = sd(go_RT) / sqrt(n()),
            mean_nogo_RT = mean(nogo_RT),
            se_nogo_RT = sd(nogo_RT) / sqrt(n()))

# Create scatter plot with lines for go_RT and nogo_RT by users
ggplot(data_summary, aes(x = users, y = mean_go_RT, group = 1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_go_RT - se_go_RT, ymax = mean_go_RT + se_go_RT),
                width = 0.2) +
  geom_line(aes(linetype = "go_RT"), size = 1) +
  geom_point(aes(y = mean_nogo_RT), size = 3, shape = 21, fill = "white") +
  geom_errorbar(aes(ymin = mean_nogo_RT - se_nogo_RT, ymax = mean_nogo_RT + se_nogo_RT),
                width = 0.2) +
  geom_line(aes(y = mean_nogo_RT, linetype = "nogo_RT"), size = 1) +
  labs(title = "Go/No-Go: Response Times",
       x = "",
       y = "Mean RT (ms)",
       linetype = "Variable") +
  scale_x_discrete(labels = c(paste("Non-User\n (n=", table(gonogo$users)["Non-users"][[1]], ")"), 
                              paste("Infrequent User\n (n=", table(gonogo$users)["Infrequent users"][[1]], ")"), 
                              paste("Frequent Users\n (n=", table(gonogo$users)["Frequent users"][[1]], ")"), 
                              paste("High users\n (n=", table(gonogo$users)["High users"][[1]], ")")))+
  scale_linetype_manual(name = "Variable",
                        values = c("dashed", "solid"),
                        labels = c("go_RT", "nogo_RT")) +
  theme_bw()


#### prepare descriptive table ####

tbl_svysummary <-
  survey::svydesign(~1, data = gonogo) %>%
  tbl_svysummary(by = users, percent = "column",
                 include = c(sex, video_games, concussion,
                             year_of_birth, physically_activity,
                             stressed, go_prop.error, nogo_prop.error,
                             go_RT, nogo_RT,
                             dprime, sensitivity, totaltime),
                 type = list(year_of_birth ~ 'continuous',
                             physically_activity ~ 'continuous',
                             stressed ~ 'continuous',
                             go_prop.error ~ 'continuous',
                             nogo_prop.error ~ 'continuous',
                             go_RT ~ 'continuous',
                             nogo_RT ~ 'continuous',
                             dprime ~ 'continuous',
                             sensitivity ~ 'continuous', 
                             totaltime ~ 'continuous'),
                 statistic = list(all_continuous() ~ "{mean} ({sd})",
                                   all_categorical() ~ "{n} ({p}%)"),
                  digits = list(all_continuous() ~ 3,
                                all_categorical() ~ 0)) %>%
  modify_caption("Table 1. Descriptive Statistics") %>%
  as_flex_table()

tbl_svysummary

#### saving output in a Word document ####

save_as_docx(tbl_svysummary, path = "data/output/Table_1.docx")

#### gonogo model ####

# Perform linear regression
model1 <- lm(dprime ~ users, data = gonogo)
model2 <- lm(dprime ~ users + sex + year_of_birth, data = gonogo)
model3 <- lm(dprime ~ users + sex + year_of_birth + 
               video_games + concussion + physically_activity + stressed,
             data = gonogo)

summ(model1)
summ(model2)
summ(model3)
plot_summs(model1, model2, model3, plot.distributions = TRUE, inner_ci_level = 0.95)

export_summs(model1, model2, model3, 
             to.file = "docx", file.name = "data/output/Table_2.docx", number_format = "%.3g")

stargazer(model, title = "Table 2. Regression Model", type="text", out = "data/output/Table_2.doc")

model <- aov(dprime ~ users, data = gonogo)
summary(model)

# Perform Tukey's HSD test
my_tukey <- TukeyHSD(model)

# Print the results
print(my_tukey)

# create a table using the Tukey HSD results
tukey_tbl <- my_tukey[[1]]
tukey_tbl <- as.data.frame(tukey_tbl[, c(1, 4)])

# Add index as column
tukey_tbl <- tukey_tbl %>% rownames_to_column(var = "index")

# create a flextable object
flex_tbl <- flextable(tukey_tbl)

# create a Word document
save_as_docx(flex_tbl, path = "data/output/flex_tbl_1.docx")

#### visualsearch ####

visualsearch <- visualsearch %>%
  filter(passedscreening == TRUE)

# add new category for high users
visualsearch <- visualsearch %>% 
  mutate(users = cannabis_freqnum,
         users = case_when(
           is.na(cannabis_freqnum) ~ "High users",
           cannabis_freqnum == 0 ~ "Non-users",
           cannabis_freqnum > 5 ~ "Frequent users",
           TRUE ~ "Infrequent users"
         ))

# fill used by down
visualsearch <- visualsearch %>% 
  group_by(id) %>% 
  fill(sex, physically_activity, stressed, video_games, sleep_last, 
       concussion, music, year_of_birth, cannabis_group,
       RT_6_absent, RT_12_absent, RT_18_absent,
       RT_6_present, RT_12_present, RT_18_present, totaltime,
       passedscreening,  cannabis_freqnum, .direction = "downup")

# Reorder the 'users' factor variable according to the vector above
visualsearch$users <- factor(visualsearch$users, levels = user_order)

visualsearch %>%
  group_by(users) %>%
  summarise_at(vars(RT_6_absent, RT_12_absent, RT_18_absent,
                    RT_6_present, RT_12_present, RT_18_present), list(name = mean))

# line segment graph
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
  group_by(users, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(users, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count)) %>%
  ggplot(aes(x=factor(set_size), y=mean, 
             group=interaction(Present, users),
             shape = Present,
             color=users)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.1) +
  geom_line(aes(linetype = Present)) +
  geom_point(size=3) +
  labs(title = "Visual Search", x = "")+ 
  scale_y_continuous(limits = c(1, 5), name = "Reaction Time (s)") +
  xlab("Set Size")

#### prepare descriptive table ####

tbl_svysummary <-
  survey::svydesign(~1, data = visualsearch) %>%
  tbl_svysummary(by = users,
                 include = c(RT_6_absent, RT_12_absent, RT_18_absent,
                             RT_6_present, RT_12_present, RT_18_present, totaltime),
                 type = list(RT_6_absent ~ 'continuous',
                             RT_12_absent ~ 'continuous', 
                             RT_18_absent ~ 'continuous', 
                             RT_6_present ~ 'continuous',
                             RT_12_present ~ 'continuous', 
                             RT_18_present ~ 'continuous', 
                             totaltime ~ 'continuous'),
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") %>%
  as_flex_table()

tbl_svysummary

#### saving output in a Word document ####

save_as_docx(tbl_svysummary, path = "data/output/Table_vs_1.docx")

#### vs model ####

# Perform linear regression
model <- lm(dprime ~ users, data = visualsearch)

# View summary of regression model
model_1 <- lm(RT_6_absent ~ users, data = visualsearch)
model_2 <- lm(RT_12_absent ~ users, data = visualsearch)
model_3 <- lm(RT_18_absent ~ users, data = visualsearch)
model_4 <- lm(RT_6_present ~ users, data = visualsearch)
model_5 <- lm(RT_12_present ~ users, data = visualsearch)
model_6 <- lm(RT_18_present ~ users, data = visualsearch)

stargazer(model_1, model_2, model_3, 
          model_4, model_5, model_6, title = "Table 2. Regression Model", type="text", out = "data/output/Table_vs_2.doc")

## anova

model_1 <- aov(RT_6_absent ~ users, data = visualsearch)
model_2 <- aov(RT_12_absent ~ users, data = visualsearch)
model_3 <- aov(RT_18_absent ~ users, data = visualsearch)
model_4 <- aov(RT_6_present ~ users, data = visualsearch)
model_5 <- aov(RT_12_present ~ users, data = visualsearch)
model_6 <- aov(RT_18_present ~ users, data = visualsearch)

stargazer(model_1, model_2, model_3, model_4, model_5, model_6, 
          out = "data/output/Table_vs_3.doc")

# Perform Tukey's HSD test
my_tukey_1 <- TukeyHSD(model_1)
my_tukey_2 <- TukeyHSD(model_2)
my_tukey_3 <- TukeyHSD(model_3)
my_tukey_4 <- TukeyHSD(model_4)
my_tukey_5 <- TukeyHSD(model_5)
my_tukey_6 <- TukeyHSD(model_6)

# Combine the Tukey HSD results into a single data frame
tukey_tbl <- bind_rows(
  as.data.frame(my_tukey_1[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_2[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_3[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_4[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_5[[1]][, c(1, 4)]),
  as.data.frame(my_tukey_6[[1]][, c(1, 4)])
)

# Add index as column
tukey_tbl <- tukey_tbl %>% rownames_to_column(var = "index")

# create a flextable object
flex_tbl <- flextable(tukey_tbl)

# create a Word document
save_as_docx(flex_tbl, path = "data/output/flex_tbl_2.docx")


#### taskswitching ####

taskswitching <- taskswitching %>%
  filter(passedscreening == TRUE)

# add new category for high users
taskswitching <- taskswitching %>% 
  mutate(users = cannabis_freqnum,
         users = case_when(
           is.na(cannabis_freqnum) ~ "High users",
           cannabis_freqnum == 0 ~ "Non-users",
           cannabis_freqnum > 5 ~ "Frequent users",
           TRUE ~ "Infrequent users"
         ))

# fill used by down
taskswitching <- taskswitching %>% 
  group_by(id) %>% 
  fill(sex, physically_activity, stressed, video_games, sleep_last, 
       concussion, music, year_of_birth, cannabis_group,
       singleblock_1_RT, singleblock_2_RT, switch_RT,
       nonswitch_RT, congruent_RT, nonCongruent_RT, totaltime,
       passedscreening,  cannabis_freqnum, .direction = "downup")

taskswitching  %>%
  group_by(users) %>%
  summarise_at(vars(singleblock_1_RT, singleblock_2_RT, switch_RT,
                    nonswitch_RT, congruent_RT, nonCongruent_RT), list(name = mean), na.rm = TRUE)

# Reorder the 'users' factor variable according to the vector above
taskswitching$users <- factor(taskswitching$users, levels = user_order)


taskswitching %>%
  select(id, singleblock_1_RT, switch_RT, congruent_RT, users)  %>%
  pivot_longer(singleblock_1_RT:congruent_RT, names_to = "Group", values_to = "RT") %>%
  mutate(tasks = factor(case_when(
    Group == "singleblock_1_RT" ~ "single",
    Group == "switch_RT" ~ "switch",
    Group == "congruent_RT" ~ "congruent"
  ), levels = c("single", "congruent", "switch"))) %>%
  ggplot(aes(x = tasks, y = RT, fill = users)) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Task Switching", x = "Tasks", y = "Mean") +
  scale_fill_manual(values = c("Non-users" = "#f8766d", "Infrequent users" = "#7caeff", "Frequent users" = "#00ba38", "High users" = "#c77cff")) +
  theme(legend.position = "bottom")


#### prepare descriptive table ####

tbl_svysummary <-
  survey::svydesign(~1, data = taskswitching) %>%
  tbl_svysummary(by = users,
                 include = c(singleblock_1_RT, switch_RT, congruent_RT, totaltime),
                 type = list(singleblock_1_RT ~ 'continuous',
                             congruent_RT ~ 'continuous', 
                             switch_RT ~ 'continuous', 
                             totaltime ~ 'continuous'),
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") %>%
  as_flex_table()

tbl_svysummary

#### saving output in a Word document ####

save_as_docx(tbl_svysummary, path = "data/output/Table_ts_1.docx")

#### ts model ####

# Perform linear regression
model_1 <- lm(singleblock_1_RT ~ users, data = taskswitching)
model_2 <- lm(congruent_RT ~ users, data = taskswitching)
model_3 <- lm(switch_RT ~ users, data = taskswitching)

stargazer(model_1, model_2, model_3, title = "Table 2. Regression Model", type="text", out = "data/output/Table_ts_2.doc")


####