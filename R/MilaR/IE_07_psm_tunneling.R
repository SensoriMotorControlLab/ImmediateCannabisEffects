#### matching by sober/high id ####

# Subset the data frame where users == "High users"
high_users <- subset(tunneling, users == "High users")

# Extract the corresponding id values
high_users_id <- unique(high_users$id)

# Subset the data frame to find all other users with the same id values
other_users <- subset(tunneling, id %in% high_users_id & users != "High users")

# View the result
tunneling_subset <- tunneling[tunneling$id %in% other_users$id, ]

# Identify duplicated rows
dup_rows <- duplicated(tunneling_subset[c("id", "group")])

# Keep only the non-duplicated rows
tunneling_subset <- tunneling_subset[!dup_rows, ]

# Create a vector of user groups in the desired order
group_order <- c("control", "experimental")

# Reorder the 'users' factor variable according to the vector above
tunneling_subset$group <- factor(tunneling_subset$group, levels = group_order)

#### BFs ####

bf_df_all <- data.frame(group1 = character(),
                        group2 = character(),
                        p.adj = numeric(), 
                        condition = character())


# loop over the variables and fill the matrix
for (k in c("MT_sc40", "MT_sc60", "MT_sc80", "MT_sc100")) {
  
  test <- extractBF(ttestBF(x = subset(tunneling_subset, group == "control")[[k]],
                            y = subset(tunneling_subset, group == "experimental")[[k]]))$bf
  
  bf_df <- data.frame(group1 = c("control"),
                      group2 = c("experimental"),
                      p.adj = round(test, 2),
                      condition = k)
  
  # append bf_df to bf_df_all_new
  bf_df_all <- rbind(bf_df_all, bf_df)
}

bf_df_all <- bf_df_all %>% mutate(multiplier = condition,
                                  multiplier = case_when(condition == "MT_sc40" ~ 1,
                                                         condition == "MT_sc60" ~ 2,
                                                         condition == "MT_sc80" ~ 3,
                                                         TRUE ~ 4))
bf_df_all <- bf_df_all %>% mutate(group_2 = condition,
                                  group_2 = case_when(condition == "MT_sc40" ~ 1.33,
                                                      condition == "MT_sc60" ~ 2.33,
                                                      condition == "MT_sc80" ~ 3.33,
                                                      TRUE ~ 4.33))

bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "control" ~ multiplier - 0.34))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                  y = case_when(group1 == "control" ~ 9.5))

bf_df_all$group <- bf_df_all$group1



#### calculate ANOVA bf ####

tn <- tunneling_subset %>%
  select(id, MT_sc40, MT_sc60, MT_sc80, MT_sc100, group)  %>%
  pivot_longer(cols = starts_with("MT_"), names_to = "Group", values_to = "MT") %>%
  mutate(tasks = factor(case_when(
    Group == "MT_sc40" ~ "sc40",
    Group == "MT_sc60" ~ "sc60",
    Group == "MT_sc80" ~ "sc80",
    Group == "MT_sc100" ~ "sc100"
  ), levels = c("sc40", "sc60", "sc80", "sc100")))


tn$id <- as.factor(tn$id)

options(scipen = 999)

model_tn <- anovaBF(formula = MT ~ group*tasks, data = tn, rscaleFixed = "wide")

extractBF(model_tn)

r2_bayes(model_tn)

#### plot ####

ggplot(tn, aes(x = tasks, y = MT, fill = group)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Same Sample: Tunneling", x = "Scaled tracks %", y = "MT") +
  scale_fill_manual(values = c("control" = "#00ba38", 
                               "experimental" = "#c77cff"),
                    labels=c(paste0("Control\n (n=", table(tunneling_subset$group)["control"][[1]], ")"), 
                             paste0("Experimental\n (n=", table(tunneling_subset$group)["experimental"][[1]], ")"))) +
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
tunneling$treatment <- ifelse((tunneling$users == "High users"), 1, ifelse((tunneling$users != "Infrequent users") & (tunneling$users != "Frequent users"), 0, NA))

tunneling_ps <- tunneling[!is.na(tunneling$treatment) & !is.na(tunneling$sex) & !is.na(tunneling$sex), ]

# Fit a propensity score model
psm_model <- glm(treatment ~ as.factor(sex) + age, 
                 data = tunneling_ps, family = binomial())

# Compute propensity scores
tunneling_ps$ps <- predict(psm_model, type = "response")

# Match "High users" with "Non-users" based on their propensity scores
matched_data <- matchit(treatment ~ ps, data = tunneling_ps, method = "nearest", 
                        ratio = 1, caliper = 0.1)

# Extract the matched data
matched_tunneling <- match.data(matched_data)

#### BFs ####

bf_df_all <- data.frame(group1 = character(),
                        group2 = character(),
                        p.adj = numeric(), 
                        condition = character())


# loop over the variables and fill the matrix
for (k in c("MT_sc40", "MT_sc60", "MT_sc80", "MT_sc100")) {
  
  test <- extractBF(ttestBF(x = subset(matched_tunneling, group == "control")[[k]],
                            y = subset(matched_tunneling, group == "experimental")[[k]]))$bf
  
  bf_df <- data.frame(group1 = c("control"),
                      group2 = c("experimental"),
                      p.adj = round(test, 2),
                      condition = k)
  
  # append bf_df to bf_df_all_new
  bf_df_all <- rbind(bf_df_all, bf_df)
}

bf_df_all <- bf_df_all %>% mutate(multiplier = condition,
                                  multiplier = case_when(condition == "MT_sc40" ~ 1,
                                                         condition == "MT_sc60" ~ 2,
                                                         condition == "MT_sc80" ~ 3,
                                                         TRUE ~ 4))
bf_df_all <- bf_df_all %>% mutate(group_2 = condition,
                                  group_2 = case_when(condition == "MT_sc40" ~ 1.33,
                                                      condition == "MT_sc60" ~ 2.33,
                                                      condition == "MT_sc80" ~ 3.33,
                                                      TRUE ~ 4.33))

bf_df_all <- bf_df_all %>% mutate(group_1 = group1,
                                  group_1 = case_when(group1 == "control" ~ multiplier - 0.34))

bf_df_all <- bf_df_all %>% mutate(y = group1,
                                  y = case_when(group1 == "control" ~ 9.5))

bf_df_all$group <- bf_df_all$group1



tn <- matched_tunneling %>%
  select(id, MT_sc40, MT_sc60, MT_sc80, MT_sc100, group)  %>%
  pivot_longer(cols = starts_with("MT_"), names_to = "Group", values_to = "MT") %>%
  mutate(tasks = factor(case_when(
    Group == "MT_sc40" ~ "sc40",
    Group == "MT_sc60" ~ "sc60",
    Group == "MT_sc80" ~ "sc80",
    Group == "MT_sc100" ~ "sc100"
  ), levels = c("sc40", "sc60", "sc80", "sc100")))


tn$id <- as.factor(tn$id)

options(scipen = 999)

#### plot ####

ggplot(tn, aes(x = tasks, y = MT, fill = group)) +
  stat_boxplot(geom = "errorbar",
               width = 0.25, position = position_dodge(width = 0.9)) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Matched: Tunneling", x = "Scaled tracks %", y = "MT") +
  scale_fill_manual(values = c("control" = "#00ba38", 
                               "experimental" = "#c77cff"),
                    labels=c(paste0("Control\n (n=", table(matched_tunneling$group)["control"][[1]], ")"), 
                             paste0("Experimental\n (n=", table(matched_tunneling$group)["experimental"][[1]], ")"))) +
  theme_bw() + 
  theme(legend.position = "bottom", axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  stat_pvalue_manual(
    data = bf_df_all, label = "BF = {p.adj}",
    xmin = "group_1", xmax = "group_2",
    y.position = "y"
  )
