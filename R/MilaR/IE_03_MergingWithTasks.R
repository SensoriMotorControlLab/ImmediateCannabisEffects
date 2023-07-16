#### merge questionnaire with tasks ####

#### gonogo ####

## duplicates done on the same day (-70)
df_list[[1]] <- df_list[[1]][!duplicated(df_list[[1]][c("id", "sdate")]), ]

#df_list[[1]] <- df_list[[1]] %>% 
#  select(-startdate, -enddate)

df_gonogo <- merge(df_combined, df_list[[1]], by = c("id"))
#2636 -> 2753

df_gonogo <- df_gonogo[df_gonogo$date_1 >= df_gonogo$startdate & df_gonogo$date_1 <= df_gonogo$enddate, ]
#984 -> 1011

df_gonogo <- df_gonogo[!duplicated(df_gonogo[c("id", "startdate", "enddate", "date_1")], fromLast = TRUE), ]
#984 -> 1011

df_gonogo <- df_gonogo[!duplicated(df_gonogo[c("id", "date_1")], fromLast = TRUE), ]
#956 -> 980


## wrangling

# replace NA in column in informed consent with values from column informed_consent2
df_gonogo$informed_consent <- ifelse(df_gonogo$informed_consent == "", NA, df_gonogo$informed_consent)
df_gonogo$informed_consent <- ifelse(is.na(df_gonogo$informed_consent), df_gonogo$informed_consent2, df_gonogo$informed_consent)

# keep those who agreed to participate
df_gonogo <- df_gonogo[which(df_gonogo$informed_consent == 'I agree to participate in this study'),]
#956 -> 980

df_gonogo <- df_gonogo %>% 
  select(-informed_consent2)

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_gonogo[columns_to_process] <- lapply(df_gonogo[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the stress by down
df_gonogo <- df_gonogo %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
      handedness, glasses_contacts, wearing_glasses_now, 
      physically_activity, opiates, video_games, used, use_frequency,
      use_last, use_dose,
      concussion, music, .direction = "downup")

# keep only finished sessions
#df_gonogo <- df_gonogo[which(df_gonogo$finished == 'TRUE'),]
#926

df_gonogo <- df_gonogo[which(df_gonogo$passedscreening == 'TRUE'),]
#895 - 924 -> 948

# remove those who need to wear corrective devices to see screen and not wearing them now
df_gonogo <- df_gonogo[which(df_gonogo$wearing_glasses_now != "No" | is.na(df_gonogo$wearing_glasses_now)), ]
#879 -905 -> 929

df_gonogo <- df_gonogo %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# only without neurological conditions
# remove people with neurological conditions, except migraine
df_gonogo <- subset(df_gonogo, (neurological_conditions == "No" | 
                                      neurological_condition_description == "headache and migraine " | 
                                      neurological_condition_description == "Migraines" |
                                      neurological_condition_description == "Chronic Migraines" |
                                      neurological_condition_description == "Chronic Migraines " |
                                      neurological_condition_description == "no" |
                                      neurological_condition_description == "No" |
                                      neurological_condition_description == "none"))
#837 - 859 -> 881

df_gonogo <- df_gonogo %>% 
  select(-neurological_conditions, -neurological_condition_description, -neurological_condition_choice)

# remove opiate users
df_gonogo <- df_gonogo[which(df_gonogo$opiates == "No" | is.na(df_gonogo$opiates)),]
#783 - 803 -> 824

# subset depending on use_last -- this part will not be implemented in July 2023 ver.
# remove people who used on the day
#df_gonogo <- df_gonogo[df_gonogo$sample == "experimental" | 
#                         !(df_gonogo$use_last %in% c("1 hour ago",
#                                                    "10 minutes ago",
#                                                    "12 hours ago",
#                                                    "14 mins ago",
#                                                    "15 minutes ago",
#                                                    "30 minutes ago",
#                                                    "7:30",
#                                                    "a second ago",
#                                                    "an hour ago",
#                                                    "Right before this questionnaire. ",
#                                                    "Right now",
#                                                    "This evening",
#                                                    "this morning",
#                                                    "This morning",
#                                                    "This morning ",
#                                                    "today",
#                                                    "Today",
#                                                    "Tonight",
#                                                    "Within the last 12 hours",
#                                                    "Within the last 24 hours",
#                                                    "Within the last hour")), ]



# change to yes's if at least once responded as yes
df_gonogo <- df_gonogo %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_gonogo$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_gonogo$cannabis_freqnum[which(df_gonogo$use_frequency == freqlist[idx])] <- idx
}

# because some are NAs change them back to NA
df_gonogo$cannabis_freqnum <- ifelse(is.na(df_gonogo$used), NA, df_gonogo$cannabis_freqnum)

df_gonogo %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_gonogo <- df_gonogo %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_gonogo <- df_gonogo %>% mutate(cannabis_group = cannabis_freqnum,
                                  cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                                             cannabis_freqnum > 5 ~ "Frequent users",
                                                             TRUE ~ "Infrequent users"))
df_gonogo$cannabis_group <- factor(df_gonogo$cannabis_group, ordered = FALSE)
df_gonogo$cannabis_group <- relevel(df_gonogo$cannabis_group, ref = "Non-users")


df_gonogo$group <- df_gonogo$sample
gonogo <- df_gonogo
#824

#### visual search ####

## duplicates done on the same day (-70)
df_list[[2]] <- df_list[[2]][!duplicated(df_list[[2]][c("id", "sdate")]), ]

#df_list[[2]] <- df_list[[2]] %>% 
#  select(-startdate, -enddate)

df_visualsearch <- merge(df_combined, df_list[[2]], by = c("id"))
#2535 -> 2649

df_visualsearch <- df_visualsearch[df_visualsearch$date_1 >= df_visualsearch$startdate & df_visualsearch$date_1 <= df_visualsearch$enddate, ]
#943 -> 966

df_visualsearch <- df_visualsearch[!duplicated(df_visualsearch[c("id", "startdate", "enddate", "date_1")], fromLast = TRUE), ]
#943 -> 966

df_visualsearch <- df_visualsearch[!duplicated(df_visualsearch[c("id", "date_1")], fromLast = TRUE), ]
#913 -> 934


## wrangling 

# replace NA in column in informed consent with values from column informed_consent2
df_visualsearch$informed_consent <- ifelse(df_visualsearch$informed_consent == "", NA, df_visualsearch$informed_consent)
df_visualsearch$informed_consent <- ifelse(is.na(df_visualsearch$informed_consent), df_visualsearch$informed_consent2, df_visualsearch$informed_consent)

# keep those who agreed to participate
df_visualsearch <- df_visualsearch[which(df_visualsearch$informed_consent == 'I agree to participate in this study'),]
#913 -> 934

df_visualsearch <- df_visualsearch %>% 
  select(-informed_consent2)

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_visualsearch[columns_to_process] <- lapply(df_visualsearch[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the stress by down
df_visualsearch <- df_visualsearch %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
       handedness, glasses_contacts, wearing_glasses_now, 
       physically_activity, opiates, video_games, used, use_frequency,
       use_last, use_dose,
       concussion, music, .direction = "downup")

# keep only finished sessions
#df_visualsearch <- df_visualsearch[which(df_visualsearch$finished == 'TRUE'),]
#892

df_visualsearch <- df_visualsearch[which(df_visualsearch$passedscreening == 'TRUE'),]
#892 - 913 -> 934

# remove those who need to wear corrective devices to see screen and not wearing them now
df_visualsearch <- df_visualsearch[which(df_visualsearch$wearing_glasses_now != "No" | is.na(df_visualsearch$wearing_glasses_now)), ]
#874 - 895 -> 916

df_visualsearch <- df_visualsearch %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# only without neurological conditions
# remove people with neurological conditions, except migraine
df_visualsearch <- subset(df_visualsearch, (neurological_conditions == "No" | 
                                  neurological_condition_description == "headache and migraine " | 
                                  neurological_condition_description == "Migraines" |
                                  neurological_condition_description == "Chronic Migraines" |
                                  neurological_condition_description == "Chronic Migraines " |
                                  neurological_condition_description == "no" |
                                  neurological_condition_description == "No" |
                                  neurological_condition_description == "none"))
#831 - 851 -> 870

df_visualsearch <- df_visualsearch %>% 
  select(-neurological_conditions, -neurological_condition_description, -neurological_condition_choice)

# remove opiate users
df_visualsearch <- df_visualsearch[which(df_visualsearch$opiates == "No" | is.na(df_visualsearch$opiates)),]
#784 - 801 -> 817

# change to yes's if at least once responded as yes
df_visualsearch <- df_visualsearch %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_visualsearch$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_visualsearch$cannabis_freqnum[which(df_visualsearch$use_frequency == freqlist[idx])] <- idx
}

# because some are NAs change them back to NA
df_visualsearch$cannabis_freqnum <- ifelse(is.na(df_visualsearch$used), NA, df_visualsearch$cannabis_freqnum)

df_visualsearch %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_visualsearch <- df_visualsearch %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_visualsearch <- df_visualsearch %>% mutate(cannabis_group = cannabis_freqnum,
                                  cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                                             cannabis_freqnum > 5 ~ "Frequent users",
                                                             TRUE ~ "Infrequent users"))
df_visualsearch$cannabis_group <- factor(df_visualsearch$cannabis_group, ordered = FALSE)
df_visualsearch$cannabis_group <- relevel(df_visualsearch$cannabis_group, ref = "Non-users")


df_visualsearch$group <- df_visualsearch$sample
visualsearch <- df_visualsearch
#784 -> 817

#### taskswitching ####

## duplicates done on the same day 
df_list[[3]] <- df_list[[3]][!duplicated(df_list[[3]][c("id", "sdate")]), ]

#df_list[[3]] <- df_list[[3]] %>% 
#  select(-startdate, -enddate)

df_taskswitching <- merge(df_combined, df_list[[3]], by = c("id"))
#2540 -> 2651

df_taskswitching <- df_taskswitching[df_taskswitching$date_1 >= df_taskswitching$startdate & df_taskswitching$date_1 <= df_taskswitching$enddate, ]
#935 -> 956

df_taskswitching <- df_taskswitching[!duplicated(df_taskswitching[c("id", "startdate", "enddate", "date_1")], fromLast = TRUE), ]
#935 -> 956

df_taskswitching <- df_taskswitching[!duplicated(df_taskswitching[c("id", "date_1")], fromLast = TRUE), ]
#904 -> 923


## wrangling 

# replace NA in column in informed consent with values from column informed_consent2
df_taskswitching$informed_consent <- ifelse(df_taskswitching$informed_consent == "", NA, df_taskswitching$informed_consent)
df_taskswitching$informed_consent <- ifelse(is.na(df_taskswitching$informed_consent), df_taskswitching$informed_consent2, df_taskswitching$informed_consent)

# keep those who agreed to participate
df_taskswitching <- df_taskswitching[which(df_taskswitching$informed_consent == 'I agree to participate in this study'),]
#904 -> 923

df_taskswitching <- df_taskswitching %>% 
  select(-informed_consent2)

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_taskswitching[columns_to_process] <- lapply(df_taskswitching[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the stress by down
df_taskswitching <- df_taskswitching %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
       handedness, glasses_contacts, wearing_glasses_now, 
       physically_activity, opiates, video_games, used, use_frequency,
       use_last, use_dose,
       concussion, music, .direction = "downup")

# keep only finished sessions -- not implemented in July 2023
#df_taskswitching <- df_taskswitching[which(df_taskswitching$finished == 'TRUE'),]
#876

df_taskswitching <- df_taskswitching[which(df_taskswitching$passedscreening == 'TRUE'),]
#736 -> 779

# remove those who need to wear corrective devices to see screen and not wearing them now
df_taskswitching <- df_taskswitching[which(df_taskswitching$wearing_glasses_now != "No" | is.na(df_taskswitching$wearing_glasses_now)), ]
#726 -> 768

df_taskswitching <- df_taskswitching %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# only without neurological conditions
# remove people with neurological conditions, except migraine
df_taskswitching <- subset(df_taskswitching, (neurological_conditions == "No" | 
                                              neurological_condition_description == "headache and migraine " | 
                                              neurological_condition_description == "Migraines" |
                                              neurological_condition_description == "Chronic Migraines" |
                                              neurological_condition_description == "Chronic Migraines " |
                                              neurological_condition_description == "no" |
                                              neurological_condition_description == "No" |
                                              neurological_condition_description == "none"))
#687 -> 727

df_taskswitching <- df_taskswitching %>% 
  select(-neurological_conditions, -neurological_condition_description, -neurological_condition_choice)

# remove opiate users
df_taskswitching <- df_taskswitching[which(df_taskswitching$opiates == "No" | is.na(df_taskswitching$opiates)),]
#647 -> 682

## set up users

# change to yes's if at least once responded as yes
df_taskswitching <- df_taskswitching %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_taskswitching$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_taskswitching$cannabis_freqnum[which(df_taskswitching$use_frequency == freqlist[idx])] <- idx
}

# because some are NAs change them back to NA
df_taskswitching$cannabis_freqnum <- ifelse(is.na(df_taskswitching$used), NA, df_taskswitching$cannabis_freqnum)

df_taskswitching %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_taskswitching <- df_taskswitching %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_taskswitching <- df_taskswitching %>% mutate(cannabis_group = cannabis_freqnum,
                                              cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                                                         cannabis_freqnum > 5 ~ "Frequent users",
                                                                         TRUE ~ "Infrequent users"))
df_taskswitching$cannabis_group <- factor(df_taskswitching$cannabis_group, ordered = FALSE)
df_taskswitching$cannabis_group <- relevel(df_taskswitching$cannabis_group, ref = "Non-users")


df_taskswitching$group <- df_taskswitching$sample
taskswitching <- df_taskswitching
#647 -> 682

#### tunneling ####

## duplicates done on the same day
df_list[[4]] <- df_list[[4]][!duplicated(df_list[[4]][c("id", "sdate")]), ]

#df_list[[4]] <- df_list[[4]] %>% 
#  select(-startdate, -enddate)

df_tunneling <- merge(df_combined, df_list[[4]], by = c("id"))
#1974 -> 2081

df_tunneling <- df_tunneling[df_tunneling$date_1 >= df_tunneling$startdate & df_tunneling$date_1 <= df_tunneling$enddate, ]
#672 -> 692

df_tunneling <- df_tunneling[!duplicated(df_tunneling[c("id", "startdate", "enddate", "date_1")], fromLast = TRUE), ]
#666 -> 686

df_tunneling <- df_tunneling[!duplicated(df_tunneling[c("id", "date_1")], fromLast = TRUE), ]
#663 -> 683


## wrangling 

# replace NA in column in informed consent with values from column informed_consent2
df_tunneling$informed_consent <- ifelse(df_tunneling$informed_consent == "", NA, df_tunneling$informed_consent)
df_tunneling$informed_consent <- ifelse(is.na(df_tunneling$informed_consent), df_tunneling$informed_consent2, df_tunneling$informed_consent)

# keep those who agreed to participate
df_tunneling <- df_tunneling[which(df_tunneling$informed_consent == 'I agree to participate in this study'),]
#662 -> 682

df_tunneling <- df_tunneling %>% 
  select(-informed_consent2)

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_tunneling[columns_to_process] <- lapply(df_tunneling[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the stress by down
df_tunneling <- df_tunneling %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
       handedness, glasses_contacts, wearing_glasses_now, 
       physically_activity, opiates, video_games, used, use_frequency,
       use_last, use_dose,
       concussion, music, .direction = "downup")

# keep only finished sessions
#df_tunneling <- df_tunneling[which(df_tunneling$finished == 'TRUE'),]
#643

df_tunneling <- df_tunneling[which(df_tunneling$passedscreening == 'TRUE'),]
#643 -> 682

# remove those who need to wear corrective devices to see screen and not wearing them now
df_tunneling <- df_tunneling[which(df_tunneling$wearing_glasses_now != "No" | is.na(df_tunneling$wearing_glasses_now)), ]
#633 -> 672

df_tunneling <- df_tunneling %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# only without neurological conditions
# remove people with neurological conditions, except migraine
# because in tunneling more were from full questionnaire 2, many weren't ask the question about neurological
df_tunneling <- subset(df_tunneling, (neurological_conditions == "No" | 
                                                neurological_condition_description == "headache and migraine " | 
                                                neurological_condition_description == "Migraines" |
                                                neurological_condition_description == "Chronic Migraines" |
                                                neurological_condition_description == "Chronic Migraines " |
                                                neurological_condition_description == "no" |
                                                neurological_condition_description == "No" |
                                                neurological_condition_description == "none"))
#597 -> 632

df_tunneling <- df_tunneling %>% 
  select(-neurological_conditions, -neurological_condition_description, -neurological_condition_choice)

# remove opiate users
df_tunneling <- df_tunneling[which(df_tunneling$opiates == "No" | is.na(df_tunneling$opiates)),]
#563 -> 594

## set up users

# change to yes's if at least once responded as yes
df_tunneling <- df_tunneling %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_tunneling$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_tunneling$cannabis_freqnum[which(df_tunneling$use_frequency == freqlist[idx])] <- idx
}

# because some are NAs change them back to NA
df_tunneling$cannabis_freqnum <- ifelse(is.na(df_tunneling$used), NA, df_tunneling$cannabis_freqnum)

df_tunneling %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_tunneling <- df_tunneling %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_tunneling <- df_tunneling %>% mutate(cannabis_group = cannabis_freqnum,
                                                cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                                                           cannabis_freqnum > 5 ~ "Frequent users",
                                                                           TRUE ~ "Infrequent users"))
df_tunneling$cannabis_group <- factor(df_tunneling$cannabis_group, ordered = FALSE)
df_tunneling$cannabis_group <- relevel(df_tunneling$cannabis_group, ref = "Non-users")


df_tunneling$group <- df_tunneling$sample
tunneling <- df_tunneling
#563 -> 594

#### trailMaking ####

## duplicates done on the same day 
df_list[[5]] <- df_list[[5]][!duplicated(df_list[[5]][c("id", "sdate")]), ]

#df_list[[5]] <- df_list[[5]] %>% 
#  select(-startdate, -enddate)

df_trailmaking <- merge(df_combined, df_list[[5]], by = c("id"))
#1947 -> 2074

df_trailmaking <- df_trailmaking[df_trailmaking$date_1 >= df_trailmaking$startdate & df_trailmaking$date_1 <= df_trailmaking$enddate, ]
#663 -> 677

df_trailmaking <- df_trailmaking[!duplicated(df_trailmaking[c("id", "startdate", "enddate", "date_1")], fromLast = TRUE), ]
#663 -> 677

df_trailmaking <- df_trailmaking[!duplicated(df_trailmaking[c("id", "date_1")], fromLast = TRUE), ]
#659 -> 673


## wrangling 

# replace NA in column in informed consent with values from column informed_consent2
df_trailmaking$informed_consent <- ifelse(df_trailmaking$informed_consent == "", NA, df_trailmaking$informed_consent)
df_trailmaking$informed_consent <- ifelse(is.na(df_trailmaking$informed_consent), df_trailmaking$informed_consent2, df_trailmaking$informed_consent)

# keep those who agreed to participate
df_trailmaking <- df_trailmaking[which(df_trailmaking$informed_consent == 'I agree to participate in this study'),]
#659 -> 673

df_trailmaking <- df_trailmaking %>% 
  select(-informed_consent2)

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_trailmaking[columns_to_process] <- lapply(df_trailmaking[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the stress by down
df_trailmaking <- df_trailmaking %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
       handedness, glasses_contacts, wearing_glasses_now, 
       physically_activity, opiates, video_games, used, use_frequency,
       use_last, use_dose,
       concussion, music, .direction = "downup")

# keep only finished sessions
#df_trailmaking <- df_trailmaking[which(df_trailmaking$finished == 'TRUE'),]
#642

df_trailmaking <- df_trailmaking[which(df_trailmaking$passedscreening == 'TRUE'),]
#642 -> 673

# remove those who need to wear corrective devices to see screen and not wearing them now
df_trailmaking <- df_trailmaking[which(df_trailmaking$wearing_glasses_now != "No" | is.na(df_trailmaking$wearing_glasses_now)), ]
#633 -> 664

df_trailmaking <- df_trailmaking %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# only without neurological conditions
# remove people with neurological conditions, except migraine
# because in trailmaking more were from full questionnaire 2, many weren't ask the question about neurological
df_trailmaking <- subset(df_trailmaking, (neurological_conditions == "No" | 
                                        neurological_condition_description == "headache and migraine " | 
                                        neurological_condition_description == "Migraines" |
                                        neurological_condition_description == "Chronic Migraines" |
                                        neurological_condition_description == "Chronic Migraines " |
                                        neurological_condition_description == "no" |
                                        neurological_condition_description == "No" |
                                        neurological_condition_description == "none"))
#597 -> 626

df_trailmaking <- df_trailmaking %>% 
  select(-neurological_conditions, -neurological_condition_description, -neurological_condition_choice)

# remove opiate users
df_trailmaking <- df_trailmaking[which(df_trailmaking$opiates == "No" | is.na(df_trailmaking$opiates)),]
#562 -> 588

## set up users

# change to yes's if at least once responded as yes
df_trailmaking <- df_trailmaking %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_trailmaking$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_trailmaking$cannabis_freqnum[which(df_trailmaking$use_frequency == freqlist[idx])] <- idx
}

# because some are NAs change them back to NA
df_trailmaking$cannabis_freqnum <- ifelse(is.na(df_trailmaking$used), NA, df_trailmaking$cannabis_freqnum)

df_trailmaking %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_trailmaking <- df_trailmaking %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_trailmaking <- df_trailmaking %>% mutate(cannabis_group = cannabis_freqnum,
                                        cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                                                   cannabis_freqnum > 5 ~ "Frequent users",
                                                                   TRUE ~ "Infrequent users"))
df_trailmaking$cannabis_group <- factor(df_trailmaking$cannabis_group, ordered = FALSE)
df_trailmaking$cannabis_group <- relevel(df_trailmaking$cannabis_group, ref = "Non-users")


df_trailmaking$group <- df_trailmaking$sample
trailmaking <- df_trailmaking
#562 -> 588

#### nBack ####

## duplicates done on the same day 
df_list[[6]] <- df_list[[6]][!duplicated(df_list[[6]][c("id", "sdate")]), ]

#df_list[[6]] <- df_list[[6]] %>% 
#  select(-startdate, -enddate)

df_nback <- merge(df_combined, df_list[[6]], by = c("id"))
#2203 -> 2303

df_nback <- df_nback[df_nback$date_1 >= df_nback$startdate & df_nback$date_1 <= df_nback$enddate, ]
#764 -> 779

df_nback <- df_nback[!duplicated(df_nback[c("id", "startdate", "enddate", "date_1")], fromLast = TRUE), ]
#761 -> 776

df_nback <- df_nback[!duplicated(df_nback[c("id", "date_1")], fromLast = TRUE), ]
#757 -> 772


## wrangling 

# replace NA in column in informed consent with values from column informed_consent2
df_nback$informed_consent <- ifelse(df_nback$informed_consent == "", NA, df_nback$informed_consent)
df_nback$informed_consent <- ifelse(is.na(df_nback$informed_consent), df_nback$informed_consent2, df_nback$informed_consent)

# keep those who agreed to participate
df_nback <- df_nback[which(df_nback$informed_consent == 'I agree to participate in this study'),]
#756 -> 771

df_nback <- df_nback %>% 
  select(-informed_consent2)

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_nback[columns_to_process] <- lapply(df_nback[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the stress by down
df_nback <- df_nback %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
       handedness, glasses_contacts, wearing_glasses_now, 
       physically_activity, opiates, video_games, used, use_frequency,
       use_last, use_dose,
       concussion, music, .direction = "downup")

# keep only finished sessions
#df_nback <- df_nback[which(df_nback$finished == 'TRUE'),]
#722

df_nback <- df_nback[which(df_nback$passedscreening == 'TRUE'),]
#590 -> 633

# remove those who need to wear corrective devices to see screen and not wearing them now
df_nback <- df_nback[which(df_nback$wearing_glasses_now != "No" | is.na(df_nback$wearing_glasses_now)), ]
#582 -> 624

df_nback <- df_nback %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# only without neurological conditions
# remove people with neurological conditions, except migraine
# because in nback more were from full questionnaire 2, many weren't ask the question about neurological
df_nback <- subset(df_nback, (neurological_conditions == "No" | 
                                            neurological_condition_description == "headache and migraine " | 
                                            neurological_condition_description == "Migraines" |
                                            neurological_condition_description == "Chronic Migraines" |
                                            neurological_condition_description == "Chronic Migraines " |
                                            neurological_condition_description == "no" |
                                            neurological_condition_description == "No" |
                                            neurological_condition_description == "none"))
#555 -> 594

df_nback <- df_nback %>% 
  select(-neurological_conditions, -neurological_condition_description, -neurological_condition_choice)

# remove opiate users
df_nback <- df_nback[which(df_nback$opiates == "No" | is.na(df_nback$opiates)),]
#526 -> 560

## set up users

# change to yes's if at least once responded as yes
df_nback <- df_nback %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_nback$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_nback$cannabis_freqnum[which(df_nback$use_frequency == freqlist[idx])] <- idx
}

# because some are NAs change them back to NA
df_nback$cannabis_freqnum <- ifelse(is.na(df_nback$used), NA, df_nback$cannabis_freqnum)

df_nback %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_nback <- df_nback %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_nback <- df_nback %>% mutate(cannabis_group = cannabis_freqnum,
                                            cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                                                       cannabis_freqnum > 5 ~ "Frequent users",
                                                                       TRUE ~ "Infrequent users"))
df_nback$cannabis_group <- factor(df_nback$cannabis_group, ordered = FALSE)
df_nback$cannabis_group <- relevel(df_nback$cannabis_group, ref = "Non-users")


df_nback$group <- df_nback$sample
nback <- df_nback
#526 -> 560

#### mirrorReversal (July 2023) ####

## duplicates done on the same day 
df_list[[7]] <- df_list[[7]][!duplicated(df_list[[7]][c("id", "sdate")]), ]

#df_list[[6]] <- df_list[[6]] %>% 
#  select(-startdate, -enddate)

df_mirror <- merge(df_combined, df_list[[7]], by = c("id"))
#2798

df_mirror <- df_mirror[df_mirror$date_1 >= df_mirror$startdate & df_mirror$date_1 <= df_mirror$enddate, ]
#984

df_mirror <- df_mirror[!duplicated(df_mirror[c("id", "startdate", "enddate", "date_1")], fromLast = TRUE), ]
#984

df_mirror <- df_mirror[!duplicated(df_mirror[c("id", "date_1")], fromLast = TRUE), ]
#953


## wrangling 

# replace NA in column in informed consent with values from column informed_consent2
df_mirror$informed_consent <- ifelse(df_mirror$informed_consent == "", NA, df_mirror$informed_consent)
df_mirror$informed_consent <- ifelse(is.na(df_mirror$informed_consent), df_mirror$informed_consent2, df_mirror$informed_consent)

# keep those who agreed to participate
df_mirror <- df_mirror[which(df_mirror$informed_consent == 'I agree to participate in this study'),]
#953

df_mirror <- df_mirror %>% 
  select(-informed_consent2)

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_mirror[columns_to_process] <- lapply(df_mirror[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the stress by down
df_mirror <- df_mirror %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
       handedness, glasses_contacts, wearing_glasses_now, 
       physically_activity, opiates, video_games, used, use_frequency,
       use_last, use_dose,
       concussion, music, .direction = "downup")

# keep only finished sessions
#df_mirror <- df_mirror[which(df_mirror$finished == 'TRUE'),]

# there is no screening test in mirrorReversal (NOTE: to develop, if needed)
#df_mirror <- df_mirror[which(df_mirror$passedscreening == 'TRUE'),]


# remove those who need to wear corrective devices to see screen and not wearing them now
df_mirror <- df_mirror[which(df_mirror$wearing_glasses_now != "No" | is.na(df_mirror$wearing_glasses_now)), ]
#934

df_mirror <- df_mirror %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# only without neurological conditions
# remove people with neurological conditions, except migraine
# because in nback more were from full questionnaire 2, many weren't ask the question about neurological
df_mirror <- subset(df_mirror, (neurological_conditions == "No" | 
                                neurological_condition_description == "headache and migraine " | 
                                neurological_condition_description == "Migraines" |
                                neurological_condition_description == "Chronic Migraines" |
                                neurological_condition_description == "Chronic Migraines " |
                                neurological_condition_description == "no" |
                                neurological_condition_description == "No" |
                                neurological_condition_description == "none"))
#889

df_mirror <- df_mirror %>% 
  select(-neurological_conditions, -neurological_condition_description, -neurological_condition_choice)

# remove opiate users
df_mirror <- df_mirror[which(df_mirror$opiates == "No" | is.na(df_mirror$opiates)),]
#829

## set up users

# change to yes's if at least once responded as yes
df_mirror <- df_mirror %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_mirror$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_mirror$cannabis_freqnum[which(df_mirror$use_frequency == freqlist[idx])] <- idx
}

# because some are NAs change them back to NA
df_mirror$cannabis_freqnum <- ifelse(is.na(df_mirror$used), NA, df_mirror$cannabis_freqnum)

df_mirror %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_mirror <- df_mirror %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_mirror <- df_mirror %>% mutate(cannabis_group = cannabis_freqnum,
                                cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                                           cannabis_freqnum > 5 ~ "Frequent users",
                                                           TRUE ~ "Infrequent users"))
df_mirror$cannabis_group <- factor(df_mirror$cannabis_group, ordered = FALSE)
df_mirror$cannabis_group <- relevel(df_mirror$cannabis_group, ref = "Non-users")


df_mirror$group <- df_mirror$sample
mirror <- df_mirror
#829



