#### matching by sober/high id ####

# Subset the data frame where users == "High users"
high_users <- subset(taskswitching, users == "High users")

# Extract the corresponding id values
high_users_id <- unique(high_users$id)

# Subset the data frame to find all other users with the same id values
other_users <- subset(taskswitching, id %in% high_users_id & users != "High users")

# View the result
taskswitching_subset <- taskswitching[taskswitching$id %in% other_users$id, ]

# Identify duplicated rows
dup_rows <- duplicated(taskswitching_subset[c("id", "group")])

# Keep only the non-duplicated rows
taskswitching_subset <- taskswitching_subset[!dup_rows, ]

# Create a vector of user groups in the desired order
group_order <- c("control", "experimental")

# Reorder the 'users' factor variable according to the vector above
taskswitching_subset$group <- factor(taskswitching_subset$group, levels = group_order)

#### BFs ####

bf_df_all <- data.frame(group1 = character(),
                           group2 = character(),
                           p.adj = numeric(), 
                           condition = character())


# loop over the variables and fill the matrix
for (k in c("singleblock_1_RT", "switch_RT", "congruent_RT")) {
  
  test <- extractBF(ttestBF(x = subset(taskswitching_subset, group == "control")[[k]],
                            y = subset(taskswitching_subset, group == "experimental")[[k]]))$bf
  
  bf_df <- data.frame(group1 = c("control"),
                      group2 = c("experimental"),
                      p.adj = round(test, 2),
                      condition = k)
  
  # append bf_df to bf_df_all_new
  bf_df_all <- rbind(bf_df_all, bf_df)
}

bf_df_all <- bf_df_all %>% mutate(multiplier = condition,
                                  multiplier = case_when(condition == "singleblock_1_RT" ~ 1,
                                                         condition == "congruent_RT" ~ 2,
                                                         TRUE ~ 3))
bf_df_all <- bf_df_all %>% mutate(group_2 = condition,
                                  group_2 = case_when(condition == "singleblock_1_RT" ~ 1.33,
                                                      condition == "congruent_RT" ~ 2.33,
                                                      TRUE ~ 3.33))
bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                        group_1 = case_when(group1 == "control" ~ multiplier - 0.34))

bf_df_all <- bf_df_all %>% mutate(group1 = group1,
                                  y = case_when(group1 == "control" ~ 2))

bf_df_all$group <- bf_df_all$group1

#### plotting ####

ts <- taskswitching_subset %>%
  select(id, singleblock_1_RT, switch_RT, congruent_RT, group)  %>%
  pivot_longer(singleblock_1_RT:congruent_RT, names_to = "Group", values_to = "RT") %>%
  mutate(tasks = factor(case_when(
    Group == "singleblock_1_RT" ~ "single",
    Group == "switch_RT" ~ "switch",
    Group == "congruent_RT" ~ "congruent"
  ), levels = c("single", "congruent", "switch")))

ggplot(ts, aes(x = tasks, y = RT, fill = group)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Same Sample Task Switching", x = "Tasks", y = "RT") +
  scale_fill_manual(values = c("control" = "#00ba38", 
                               "experimental" = "#c77cff"),
                    labels=c(paste0("Control\n (n=", table(taskswitching_subset$group)["control"][[1]], ")"), 
                             paste0("Experimental User\n (n=", table(taskswitching_subset$group)["experimental"][[1]], ")"))) +
  theme_bw() + 
  theme(legend.position = "bottom", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  stat_pvalue_manual(
    data = bf_df_all, label = "BF = {p.adj}",
    xmin = "group_1", xmax = "group_2",
    y.position = "y"
  )

#### psm matched ####

# Create a binary treatment variable for "High users"
taskswitching$treatment <- ifelse((taskswitching$users == "High users"), 1, ifelse((taskswitching$users != "Infrequent users") & (taskswitching$users != "Frequent users"), 0, NA))

taskswitching_ps <- taskswitching[!is.na(taskswitching$treatment) & !is.na(taskswitching$sex) & !is.na(taskswitching$age), ]

# Fit a propensity score model
psm_model <- glm(treatment ~ as.factor(sex) + age, 
                 data = taskswitching_ps, family = binomial())

# Compute propensity scores
taskswitching_ps$ps <- predict(psm_model, type = "response")

# Match "High users" with "Non-users" based on their propensity scores
matched_data <- matchit(treatment ~ ps, data = taskswitching_ps, method = "nearest", 
                        ratio = 1, caliper = 0.1)

# Extract the matched data
matched_taskswitching <- match.data(matched_data)

#### BFs ####

bf_df_all <- data.frame(group1 = character(),
                        group2 = character(),
                        p.adj = numeric(), 
                        condition = character())


# loop over the variables and fill the matrix
for (k in c("singleblock_1_RT", "switch_RT", "congruent_RT")) {
  
  test <- extractBF(ttestBF(x = subset(matched_taskswitching, group == "control")[[k]],
                            y = subset(matched_taskswitching, group == "experimental")[[k]]))$bf
  
  bf_df <- data.frame(group1 = c("control"),
                      group2 = c("experimental"),
                      p.adj = round(test, 2),
                      condition = k)
  
  # append bf_df to bf_df_all_new
  bf_df_all <- rbind(bf_df_all, bf_df)
}

bf_df_all <- bf_df_all %>% mutate(multiplier = condition,
                                  multiplier = case_when(condition == "singleblock_1_RT" ~ 1,
                                                         condition == "congruent_RT" ~ 2,
                                                         TRUE ~ 3))
bf_df_all <- bf_df_all %>% mutate(group_2 = condition,
                                  group_2 = case_when(condition == "singleblock_1_RT" ~ 1.33,
                                                      condition == "congruent_RT" ~ 2.33,
                                                      TRUE ~ 3.33))
bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "control" ~ multiplier - 0.34))

bf_df_all <- bf_df_all %>% mutate(group1 = group1,
                                  y = case_when(group1 == "control" ~ 2.25))

bf_df_all$group <- bf_df_all$group1

#### plotting ####

ts <- matched_taskswitching %>%
  select(id, singleblock_1_RT, switch_RT, congruent_RT, group)  %>%
  pivot_longer(singleblock_1_RT:congruent_RT, names_to = "Group", values_to = "RT") %>%
  mutate(tasks = factor(case_when(
    Group == "singleblock_1_RT" ~ "single",
    Group == "switch_RT" ~ "switch",
    Group == "congruent_RT" ~ "congruent"
  ), levels = c("single", "congruent", "switch")))

ggplot(ts, aes(x = tasks, y = RT, fill = group)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Matched Task Switching", x = "Tasks", y = "RT") +
  scale_fill_manual(values = c("control" = "#00ba38", 
                               "experimental" = "#c77cff"),
                    labels=c(paste0("Control\n (n=", table(matched_taskswitching$group)["control"][[1]], ")"), 
                             paste0("Experimental User\n (n=", table(matched_taskswitching$group)["experimental"][[1]], ")"))) +
  theme_bw() + 
  theme(legend.position = "bottom", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  stat_pvalue_manual(
    data = bf_df_all, label = "BF = {p.adj}",
    xmin = "group_1", xmax = "group_2",
    y.position = "y"
  )
