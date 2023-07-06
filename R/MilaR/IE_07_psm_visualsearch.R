#### matching by sober/high id ####

# Subset the data frame where users == "High users"
high_users <- subset(visualsearch, users == "High users")

# Extract the corresponding id values
high_users_id <- unique(high_users$id)

# Subset the data frame to find all other users with the same id values
other_users <- subset(visualsearch, id %in% high_users_id & users != "High users")

# View the result
visualsearch_subset <- visualsearch[visualsearch$id %in% other_users$id, ]

# Identify duplicated rows
dup_rows <- duplicated(visualsearch_subset[c("id", "group")])

# Keep only the non-duplicated rows
visualsearch_subset <- visualsearch_subset[!dup_rows, ]

# Create a vector of user groups in the desired order
group_order <- c("control", "experimental")

# Reorder the 'users' factor variable according to the vector above
visualsearch_subset$group <- factor(visualsearch_subset$group, levels = group_order)

#### BFs ####

bf_df_all_pc <- data.frame(group1 = character(),
                    group2 = character(),
                    p.adj = numeric(), 
                    group = character())

# loop over the variables and fill the matrix
for (k in c("RT_6_absent", "RT_12_absent", "RT_18_absent",
            "RT_6_present", "RT_12_present", "RT_18_present")) {
  
  test <- extractBF(ttestBF(x = subset(visualsearch_subset, group == "control")[[k]],
                  y = subset(visualsearch_subset, group == "experimental")[[k]]))$bf

  bf_df <- data.frame(group1 = c("control"),
                      group2 = c("experimental"),
                      p.adj = round(test, 2),
                      group = k)
  
  # append bf_df to bf_df_all_new
  bf_df_all_pc <- rbind(bf_df_all_pc, bf_df)
}

# calculate y positions and add users

vs <-visualsearch_subset %>%
  select(RT_6_absent:RT_18_present, group)  %>%
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
  group_by(group, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(group, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))

bf_df_all_pc <- bf_df_all_pc %>% mutate(set_size = str_extract(group, "\\d+"))
bf_df_all_pc <- bf_df_all_pc %>% mutate(Present = str_extract(group, "(present|absent)"))

bf_df_all_pc <- bf_df_all_pc %>% mutate(y = group1,
                                        y = case_when(group1 == "control" ~ as.numeric(set_size) + 0.8))

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(group_1 = vs$mean[match(paste(group1, set_size, Present), paste(vs$group, vs$set_size, vs$Present))])

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(group_2 = vs$mean[match(paste(group2, set_size, Present), paste(vs$group, vs$set_size, vs$Present))])

bf_df_all_pc$users <- bf_df_all_pc$group1

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(
    group_min = ifelse(group_1 > group_2, group_2, group_1),
    group_max = ifelse(group_1 > group_2, group_1, group_2)
  )

bf_df_all_pc$p.adj <- as.numeric(bf_df_all_pc$p.adj)
bf_df_all_pc$set_size <- as.double(bf_df_all_pc$set_size)

bf_df_all_pc <- bf_df_all_pc %>% 
  select(p.adj, set_size, group, y, Present, group_min, group_max)

bf_df_all_pc$group1 <- bf_df_all_pc$group_min
bf_df_all_pc$group2 <- bf_df_all_pc$group_max


#### plotting ####

vs %>%
  ggplot(aes(y=set_size, x=mean, 
             group=interaction(Present, group),
             shape = Present,
             color=group)) + 
  geom_errorbar(aes(xmin=mean-se, xmax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.1) +
  geom_path(aes(linetype = Present)) +
  geom_point(size=3) +
  stat_pvalue_manual(
    data = bf_df_all_pc, label = "BF = {p.adj}",
    xmin = "group_min", xmax = "group_max",
    y.position = "y", coord.flip = TRUE,
    vjust = 0
  )+ 
  scale_y_continuous(breaks = c(6,12,18))+
  theme_bw()+
  theme(legend.position = "bottom", axis.title.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  labs(title = "Same Sample: Visual Search", x = "RT", y = "Set size")+ 
  scale_color_discrete(name = "Group", 
                       labels=c(paste0("Control\n (n=", table(visualsearch_subset$group)["control"][[1]], ")"), 
                                paste0("Experimental\n (n=", table(visualsearch_subset$group)["experimental"][[1]], ")")))+
  scale_shape_discrete(name = "Condition") +
  scale_linetype_discrete(name = "Condition") +
  coord_flip()

#### psm matched ####

# Create a binary treatment variable for "High users"
visualsearch$treatment <- ifelse((visualsearch$users == "High users"), 1, ifelse((visualsearch$users != "Infrequent users") & (visualsearch$users != "Frequent users"), 0, NA))

visualsearch_ps <- visualsearch[!is.na(visualsearch$treatment) & !is.na(visualsearch$sex), ]

# Fit a propensity score model
psm_model <- glm(treatment ~ as.factor(sex) + physically_activity + stressed + video_games + age, 
                 data = visualsearch_ps, family = binomial())

# Compute propensity scores
visualsearch_ps$ps <- predict(psm_model, type = "response")

# Match "High users" with "Non-users" based on their propensity scores
matched_data <- matchit(treatment ~ ps, data = visualsearch_ps, method = "nearest", 
                        ratio = 1, caliper = 0.1)

# Extract the matched data
matched_visualsearch <- match.data(matched_data)


#### BFs ####

bf_df_all_pc <- data.frame(group1 = character(),
                           group2 = character(),
                           p.adj = numeric(), 
                           group = character())

# loop over the variables and fill the matrix
for (k in c("RT_6_absent", "RT_12_absent", "RT_18_absent",
            "RT_6_present", "RT_12_present", "RT_18_present")) {
  
  
  
  test <- extractBF(ttestBF(x = subset(matched_visualsearch, group == "control")[[k]],
                            y = subset(matched_visualsearch, group == "experimental")[[k]]))$bf
  
  bf_df <- data.frame(group1 = c("control"),
                      group2 = c("experimental"),
                      p.adj = round(test, 2),
                      group = k)
  
  # append bf_df to bf_df_all_new
  bf_df_all_pc <- rbind(bf_df_all_pc, bf_df)
}

# calculate y positions and add users

vs <-matched_visualsearch %>%
  select(RT_6_absent:RT_18_present, group)  %>%
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
  group_by(group, set_size, Present) %>%
  mutate(count = n()) %>%
  group_by(group, set_size, Present, count) %>%
  summarise_at(vars(RT), list(mean = mean, sd = sd)) %>%
  mutate(se = sd/sqrt(count))

bf_df_all_pc <- bf_df_all_pc %>% mutate(set_size = str_extract(group, "\\d+"))
bf_df_all_pc <- bf_df_all_pc %>% mutate(Present = str_extract(group, "(present|absent)"))

bf_df_all_pc <- bf_df_all_pc %>% mutate(y = group1,
                                        y = case_when(group1 == "control" ~ as.numeric(set_size) + 0.8))

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(group_1 = vs$mean[match(paste(group1, set_size, Present), paste(vs$group, vs$set_size, vs$Present))])

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(group_2 = vs$mean[match(paste(group2, set_size, Present), paste(vs$group, vs$set_size, vs$Present))])

bf_df_all_pc$users <- bf_df_all_pc$group1

bf_df_all_pc <- bf_df_all_pc %>%
  mutate(
    group_min = ifelse(group_1 > group_2, group_2, group_1),
    group_max = ifelse(group_1 > group_2, group_1, group_2)
  )

bf_df_all_pc$p.adj <- as.numeric(bf_df_all_pc$p.adj)
bf_df_all_pc$set_size <- as.double(bf_df_all_pc$set_size)

bf_df_all_pc <- bf_df_all_pc %>% 
  select(p.adj, set_size, group, y, Present, group_min, group_max)

bf_df_all_pc$group1 <- bf_df_all_pc$group_min
bf_df_all_pc$group2 <- bf_df_all_pc$group_max


#### plotting ####

vs %>%
  ggplot(aes(y=set_size, x=mean, 
             group=interaction(Present, group),
             shape = Present,
             color=group)) + 
  geom_errorbar(aes(xmin=mean-se, xmax=mean+se), 
                position=position_dodge(width=0.05),
                width = 0.1) +
  geom_path(aes(linetype = Present)) +
  geom_point(size=3) +
  stat_pvalue_manual(
    data = bf_df_all_pc, label = "BF = {p.adj}",
    xmin = "group_min", xmax = "group_max",
    y.position = "y", coord.flip = TRUE,
    vjust = 0
  )+ 
  scale_y_continuous(breaks = c(6,12,18))+
  theme_bw()+
  theme(legend.position = "bottom", axis.title.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.border = element_blank())+
  labs(title = "Matched: Visual Search", x = "RT", y = "Set size")+ 
  scale_color_discrete(name = "Group", 
                       labels=c(paste0("Control\n (n=", table(matched_visualsearch$group)["control"][[1]], ")"), 
                                paste0("Experimental\n (n=", table(matched_visualsearch$group)["experimental"][[1]], ")")))+
  scale_shape_discrete(name = "Condition") +
  scale_linetype_discrete(name = "Condition") +
  coord_flip()