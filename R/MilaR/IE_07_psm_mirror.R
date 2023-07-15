#### matching by sober/high id ####

# Subset the data frame where users == "High users"
high_users <- subset(mirror, users == "High users")

# Extract the corresponding id values
high_users_id <- unique(high_users$id)

# Subset the data frame to find all other users with the same id values
other_users <- subset(mirror, id %in% high_users_id & users != "High users")

# View the result
mirror_subset <- mirror[mirror$id %in% other_users$id, ]

# Identify duplicated rows
dup_rows <- duplicated(mirror_subset[c("id", "group")])

# Keep only the non-duplicated rows
mirror_subset <- mirror_subset[!dup_rows, ]

# Create a vector of user groups in the desired order
group_order <- c("control", "experimental")

# Reorder the 'users' factor variable according to the vector above
mirror_subset$group <- factor(mirror_subset$group, levels = group_order)

#### calculating BFs ####


test <- extractBF(ttestBF(x = subset(mirror_subset, group == "control")$meanMT,
                          y = subset(mirror_subset, group == "experimental")$meanMT))$bf


bf_df <- data.frame(group1 = c("control"),
                    group2 = c("experimental"),
                    p.adj = c(round(test, 2)))

#### plots ####

ggplot(mirror_subset, aes(x = group, y = meanMT)) + 
  geom_violin(width = 0.5) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot(width = 0.2) +
  geom_jitter(aes(col = users), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Same Sample Mirror Reversal", x = "",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
  ) + 
  scale_x_discrete(labels=c(paste0("Control\n (n=", table(mirror_subset$group)["control"][[1]], ")"), 
                            paste0("Experimental\n (n=", table(mirror_subset$group)["experimental"][[1]], ")")))+
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
mirror$treatment <- ifelse((mirror$users == "High users"), 1, ifelse((mirror$users != "Infrequent users") & (mirror$users != "Frequent users"), 0, NA))

mirror_ps <- mirror[!is.na(mirror$treatment) & !is.na(mirror$sex) & !is.na(mirror$age), ]

# Fit a propensity score model
psm_model <- glm(treatment ~ as.factor(sex) + age, 
                 data = mirror_ps, family = binomial())

# Compute propensity scores
mirror_ps$ps <- predict(psm_model, type = "response")

# Match "High users" with "Non-users" based on their propensity scores
matched_data <- matchit(treatment ~ ps, data = mirror_ps, method = "nearest", 
                        ratio = 1, caliper = 0.1)

# Extract the matched data
matched_mirror <- match.data(matched_data)

#### BF for matched ####
test <- extractBF(ttestBF(x = subset(matched_mirror, group == "control")$meanMT,
                          y = subset(matched_mirror, group == "experimental")$meanMT))$bf


bf_df <- data.frame(group1 = c("control"),
                    group2 = c("experimental"),
                    p.adj = c(round(test, 2)))

#### plots ####

ggplot(matched_mirror, aes(x = group, y = meanMT)) + 
  geom_violin(width = 0.5) +
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot(width = 0.2) +
  geom_jitter(aes(col = users), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Matched Mirror Reversal", x = "",
       #subtitle = bquote(ANOVA[BF] ~ ": " ~ BF[10] ~ " = 0.60, " ~ R[Bayesian]^2 ~ " = 0.02, " ~ "95% CI = [0.00, 0.04]")
  ) + 
  scale_x_discrete(labels=c(paste0("Control\n (n=", table(mirror_subset$group)["control"][[1]], ")"), 
                            paste0("Experimental\n (n=", table(mirror_subset$group)["experimental"][[1]], ")")))+
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
