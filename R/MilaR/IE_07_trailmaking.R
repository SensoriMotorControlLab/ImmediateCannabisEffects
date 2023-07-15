#### matching by sober/high id ####

# Subset the data frame where users == "High users"
high_users <- subset(trailmaking, users == "High users")

# Extract the corresponding id values
high_users_id <- unique(high_users$id)

# Subset the data frame to find all other users with the same id values
other_users <- subset(trailmaking, id %in% high_users_id & users != "High users")

# View the result
trailmaking_subset <- trailmaking[trailmaking$id %in% other_users$id, ]

# Identify duplicated rows
dup_rows <- duplicated(trailmaking_subset[c("id", "group")])

# Keep only the non-duplicated rows
trailmaking_subset <- trailmaking_subset[!dup_rows, ]

# Create a vector of user groups in the desired order
group_order <- c("control", "experimental")

# Reorder the 'users' factor variable according to the vector above
trailmaking_subset$group <- factor(trailmaking_subset$group, levels = group_order)

#### calculating BFs ####


test <- extractBF(ttestBF(x = subset(trailmaking_subset, group == "control")$MoveTime_1,
                y = subset(trailmaking_subset, group == "experimental")$MoveTime_1))$bf


bf_df <- data.frame(group1 = c("control"),
                    group2 = c("experimental"),
                    p.adj = c(round(test, 2)))

#### plotting ####

# add significance symbols to the plot
ggplot(trailmaking_subset, aes(x = group, y = MoveTime_1)) + 
  geom_violin(width = 0.5) +
  geom_jitter(aes(col = group), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Same Sample Trailmaking", x = "",
       y = "Move Time 1",
       ) + 
  scale_x_discrete(labels=c(paste0("Control\n (n=", table(trailmaking_subset$group)["control"][[1]], ")"), 
                            paste0("Experimental\n (n=", table(trailmaking_subset$group)["experimental"][[1]], ")")))+
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
    y.position = c(140)
  )

#### psm matched ####

# Create a binary treatment variable for "High users"
trailmaking$treatment <- ifelse((trailmaking$users == "High users"), 1, ifelse((trailmaking$users != "Infrequent users") & (trailmaking$users != "Frequent users"), 0, NA))

trailmaking_ps <- trailmaking[!is.na(trailmaking$treatment) & !is.na(trailmaking$sex) & !is.na(trailmaking$age), ]

# Fit a propensity score model
psm_model <- glm(treatment ~ as.factor(sex) + age, 
                 data = trailmaking_ps, family = binomial())

# Compute propensity scores
trailmaking_ps$ps <- predict(psm_model, type = "response")

# Match "High users" with "Non-users" based on their propensity scores
matched_data <- matchit(treatment ~ ps, data = trailmaking_ps, method = "nearest", 
                        ratio = 1, caliper = 0.1)

# Extract the matched data
matched_trailmaking <- match.data(matched_data)

#### calculating BFs ####


test <- extractBF(ttestBF(x = subset(matched_trailmaking, group == "control")$MoveTime_1,
                          y = subset(matched_trailmaking, group == "experimental")$MoveTime_1))$bf


bf_df <- data.frame(group1 = c("control"),
                    group2 = c("experimental"),
                    p.adj = c(round(test, 2)))

#### plotting ####

# add significance symbols to the plot
ggplot(matched_trailmaking, aes(x = group, y = MoveTime_1)) + 
  geom_violin(width = 0.5) +
  geom_jitter(aes(col = group), height = 0, width = 0.05, alpha = 0.3) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(title = "Matched Trailmaking", x = "",
       y = "Move Time 1",
  ) + 
  scale_x_discrete(labels=c(paste0("Control\n (n=", table(matched_trailmaking$group)["control"][[1]], ")"), 
                            paste0("Experimental\n (n=", table(matched_trailmaking$group)["experimental"][[1]], ")")))+
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
    y.position = c(140)
  )
