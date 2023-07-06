#### matching by sober/high id ####

# Subset the data frame where users == "High users"
high_users <- subset(nback, users == "High users")

# Extract the corresponding id values
high_users_id <- unique(high_users$id)

# Subset the data frame to find all other users with the same id values
other_users <- subset(nback, id %in% high_users_id & users != "High users")

# View the result
nback_subset <- nback[nback$id %in% other_users$id, ]

# Identify duplicated rows
dup_rows <- duplicated(nback_subset[c("id", "group")])

# Keep only the non-duplicated rows
nback_subset <- nback_subset[!dup_rows, ]

# Create a vector of user groups in the desired order
group_order <- c("control", "experimental")

# Reorder the 'users' factor variable according to the vector above
nback_subset$group <- factor(nback_subset$group, levels = group_order)

#### BFs ####

bf_df_all <- data.frame(group1 = character(),
                        group2 = character(),
                        p.adj = numeric(), 
                        condition = character())


# loop over the variables and fill the matrix
for (k in c("N1_dprime", "N2_dprime", "N3_dprime")) {
  
  test <- extractBF(ttestBF(x = subset(nback_subset, group == "control")[[k]],
                            y = subset(nback_subset, group == "experimental")[[k]]))$bf
  
  bf_df <- data.frame(group1 = c("control"),
                      group2 = c("experimental"),
                      p.adj = round(test, 2),
                      condition = k)
  
  # append bf_df to bf_df_all_new
  bf_df_all <- rbind(bf_df_all, bf_df)
}

bf_df_all <- bf_df_all %>% mutate(multiplier = condition,
                                  multiplier = case_when(condition == "N1_dprime" ~ 1,
                                                         condition == "N2_dprime" ~ 2,
                                                         TRUE ~ 3))
bf_df_all <- bf_df_all %>% mutate(group_2 = condition,
                                  group_2 = case_when(condition == "N1_dprime" ~ 1.33,
                                                      condition == "N2_dprime" ~ 2.33,
                                                      TRUE ~ 3.33))

bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "control" ~ multiplier - 0.34))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                  y = case_when(group1 == "control" ~ 4.5))

bf_df_all$group <- bf_df_all$group1

#### plotting ####

nb <- nback_subset %>%
  select(id, N1_dprime, N2_dprime, N3_dprime, group)  %>%
  pivot_longer(cols = ends_with("_dprime"), names_to = "Group", values_to = "dprime") %>%
  mutate(tasks = factor(case_when(
    Group == "N1_dprime" ~ "N1",
    Group == "N2_dprime" ~ "N2",
    Group == "N3_dprime" ~ "N3"
  ), levels = c("N1", "N2", "N3")))


ggplot(nb, aes(x = tasks, y = dprime, fill = group)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Same Sample N-Back", x = "Tasks", y = "dprime") +
  scale_fill_manual(values = c("control" = "#00ba38", 
                               "experimental" = "#c77cff"),
                    labels=c(paste0("Control\n (n=", table(nback_subset$group)["control"][[1]], ")"), 
                             paste0("Experimental\n (n=", table(nback_subset$group)["experimental"][[1]], ")"))) +
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
nback$treatment <- ifelse((nback$users == "High users"), 1, ifelse((nback$users != "Infrequent users") & (nback$users != "Frequent users"), 0, NA))

nback_ps <- nback[!is.na(nback$treatment) & !is.na(nback$sex), ]

# Fit a propensity score model
psm_model <- glm(treatment ~ as.factor(sex) + physically_activity + stressed + video_games + age, 
                 data = nback_ps, family = binomial())

# Compute propensity scores
nback_ps$ps <- predict(psm_model, type = "response")

# Match "High users" with "Non-users" based on their propensity scores
matched_data <- matchit(treatment ~ ps, data = nback_ps, method = "nearest", 
                        ratio = 1, caliper = 0.1)

# Extract the matched data
matched_nback <- match.data(matched_data)

#### BFs ####

bf_df_all <- data.frame(group1 = character(),
                        group2 = character(),
                        p.adj = numeric(), 
                        condition = character())


# loop over the variables and fill the matrix
for (k in c("N1_dprime", "N2_dprime", "N3_dprime")) {
  
  test <- extractBF(ttestBF(x = subset(matched_nback, group == "control")[[k]],
                            y = subset(matched_nback, group == "experimental")[[k]]))$bf
  
  bf_df <- data.frame(group1 = c("control"),
                      group2 = c("experimental"),
                      p.adj = round(test, 2),
                      condition = k)
  
  # append bf_df to bf_df_all_new
  bf_df_all <- rbind(bf_df_all, bf_df)
}

bf_df_all <- bf_df_all %>% mutate(multiplier = condition,
                                  multiplier = case_when(condition == "N1_dprime" ~ 1,
                                                         condition == "N2_dprime" ~ 2,
                                                         TRUE ~ 3))
bf_df_all <- bf_df_all %>% mutate(group_2 = condition,
                                  group_2 = case_when(condition == "N1_dprime" ~ 1.33,
                                                      condition == "N2_dprime" ~ 2.33,
                                                      TRUE ~ 3.33))

bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "control" ~ multiplier - 0.34))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                  y = case_when(group1 == "control" ~ 4.5))

bf_df_all$group <- bf_df_all$group1

#### plotting ####

nb <- matched_nback %>%
  select(id, N1_dprime, N2_dprime, N3_dprime, group)  %>%
  pivot_longer(cols = ends_with("_dprime"), names_to = "Group", values_to = "dprime") %>%
  mutate(tasks = factor(case_when(
    Group == "N1_dprime" ~ "N1",
    Group == "N2_dprime" ~ "N2",
    Group == "N3_dprime" ~ "N3"
  ), levels = c("N1", "N2", "N3")))


ggplot(nb, aes(x = tasks, y = dprime, fill = group)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Matched N-Back", x = "Tasks", y = "dprime") +
  scale_fill_manual(values = c("control" = "#00ba38", 
                               "experimental" = "#c77cff"),
                    labels=c(paste0("Control\n (n=", table(matched_nback$group)["control"][[1]], ")"), 
                             paste0("Experimental\n (n=", table(matched_nback$group)["experimental"][[1]], ")"))) +
  theme_bw() + 
  theme(legend.position = "bottom", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank()) +
  stat_pvalue_manual(
    data = bf_df_all, label = "BF = {p.adj}",
    xmin = "group_1", xmax = "group_2",
    y.position = "y"
  )
