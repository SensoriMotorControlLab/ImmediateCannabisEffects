#### matching by sober/high id ####

# Subset the data frame where users == "High users"
high_users <- subset(gonogo, users == "High users")

# Extract the corresponding id values
high_users_id <- unique(high_users$id)

# Subset the data frame to find all other users with the same id values
other_users <- subset(gonogo, id %in% high_users_id & users != "High users")

# View the result
gonogo_subset <- gonogo[gonogo$id %in% other_users$id, ]

# Identify duplicated rows
dup_rows <- duplicated(gonogo_subset[c("id", "group")])

# Keep only the non-duplicated rows
gonogo_subset <- gonogo_subset[!dup_rows, ]

# Create a vector of user groups in the desired order
group_order <- c("control", "experimental")

# Reorder the 'users' factor variable according to the vector above
gonogo_subset$group <- factor(gonogo_subset$group, levels = group_order)

#### calculating BFs ####


test <- extractBF(ttestBF(x = subset(gonogo_subset, group == "control")$dprime,
                y = subset(gonogo_subset, group == "experimental")$dprime))$bf


bf_df <- data.frame(group1 = c("control"),
                    group2 = c("experimental"),
                    p.adj = c(round(test, 2)))

#### plots ####

ggplot(gonogo_subset, aes(x = group, y = dprime)) + 
  geom_violin(width = 0.5) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot(width = 0.2) +
  geom_jitter(aes(col = users), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Same Sample Go/No-Go", x = "",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
  ) + 
  scale_x_discrete(labels=c(paste0("Control\n (n=", table(gonogo_subset$group)["control"][[1]], ")"), 
                            paste0("Experimental\n (n=", table(gonogo_subset$group)["experimental"][[1]], ")")))+
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
    y.position = c(5.5)
  )

#### create psm matched with Non-users ####

# Create a binary treatment variable for "High users"
gonogo$treatment <- ifelse((gonogo$users == "High users"), 1, ifelse((gonogo$users != "Infrequent users") & (gonogo$users != "Frequent users"), 0, NA))

gonogo_ps <- gonogo[!is.na(gonogo$treatment) & !is.na(gonogo$sex) & !is.na(gonogo$age), ]

# Fit a propensity score model
psm_model <- glm(treatment ~ as.factor(sex) + age, 
                 data = gonogo_ps, family = binomial())

# Compute propensity scores
gonogo_ps$ps <- predict(psm_model, type = "response")

# Match "High users" with "Non-users" based on their propensity scores
matched_data <- matchit(treatment ~ ps, data = gonogo_ps, method = "nearest", 
                        ratio = 1, caliper = 0.1)

# Extract the matched data
matched_gonogo <- match.data(matched_data)

#### BF for matched ####
test <- extractBF(ttestBF(x = subset(matched_gonogo, group == "control")$dprime,
                y = subset(matched_gonogo, group == "experimental")$dprime))$bf


bf_df <- data.frame(group1 = c("control"),
                    group2 = c("experimental"),
                    p.adj = c(round(test, 2)))

#### plots ####

ggplot(matched_gonogo, aes(x = group, y = dprime)) + 
  geom_violin(width = 0.5) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot(width = 0.2) +
  geom_jitter(aes(col = users), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Matched Go/No-Go", x = "",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
  ) + 
  scale_x_discrete(labels=c(paste0("Control\n (n=", table(gonogo_subset$group)["control"][[1]], ")"), 
                            paste0("Experimental\n (n=", table(gonogo_subset$group)["experimental"][[1]], ")")))+
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
    y.position = c(5.5)
  )
