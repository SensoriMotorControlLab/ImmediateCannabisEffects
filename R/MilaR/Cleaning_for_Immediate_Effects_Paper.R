library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
# install.packages("lme4", dependencies=TRUE)
library(lme4)
# install.packages("lmerTest", dependencies=TRUE)
library(lmerTest)

library(data.table)
library(lubridate)

# load helpers
source('R/parseTaskData.R')
source('R/data.R')
source('R/statistics.R')

source('R/MilaR/helper.R')

#### identify (full) controls only ####

## Cleaning the tasks data, Loading to dataframes

tasks <- c("gonogo", "visualsearch", "taskswitching", "tunneling", "trailMaking", "nBack")

for (task in tasks) {
  start_time <- system.time({
    df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", task)
    assign(paste0(task, "_df"), df)
  })
  
  # print the elapsed time
  print(paste0("Elapsed time for task ", task, " was: ",  as.numeric(start_time["elapsed"]), " seconds"))
}

## next: adding timestamps to output

# list of data frames
df_list <- list(gonogo_df, visualsearch_df, taskswitching_df, 
                tunneling_df, trailMaking_df, nBack_df)

# iterate over data frames and modify date column
df_list <- lapply(df_list, function(df) {
  df$date_1 <- as.POSIXct(df$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
  return(df)
})

for (i in seq_along(df_list)) {
  write.csv(df_list[[i]], file.path("data", paste0("df_", tasks[i], ".csv")), row.names = FALSE)
}


#### create data based on Assel's ####

source('R/MilaR/CleaningQ1.R')

#### creating codebook for q1 ####

# create data frame with column names, numbers, and values
col_info <- data.frame(
  num = 1:ncol(q1),
  name = names(q1),
  values = sapply(q1, function(x) paste(unique(x), collapse = ", "))
)

# write to csv file
write.csv(col_info, "data/codebook/q1_codebook.csv", row.names = FALSE)

col_info <- data.frame(
  num = 1:ncol(q2),
  name = names(q2),
  values = sapply(q2, function(x) paste(unique(x), collapse = ", "))
)

# write to csv file
write.csv(col_info, "data/codebook/q2_codebook.csv", row.names = FALSE)


#### using q1 ####

# calculate use frequency
# 7: daily
# 6: a few times a week
# 5: once a week
# 4: a few times a month
# 3: around once a month
# 2: once or twice within these past 3 months
# 1: not in the past 3 months
# 0: [empty / never]

q1$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  q1$cannabis_freqnum[which(q1$cannabis_use_frequency == freqlist[idx])] <- idx
}

# calculate cannabis groups

q1 <- q1 %>% mutate(cannabis_group = cannabis_freqnum,
                    cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                               cannabis_freqnum > 5 ~ "Frequent users",
                                               TRUE ~ "Infrequent users"))

## next: figure out how to combine using date information

#### merging with tasks ####

gng_df <- gng_df[which(gng_df$passedscreening == TRUE),]
vs_df <- vs_df[which(vs_df$passedscreening == TRUE),]
df   <- merge(q1, gng_df,   by.x='id', by.y='participant', all=FALSE)
df   <- merge(df, vs_df,   by.x='id', by.y='participant', all=FALSE)






#### get tasks ####
tasks <- c("gonogo", "visualsearch", "taskswitching", "tunneling", "trailmaking")
suffixes <- c("_high", "_control")

for (task in tasks) {
  for (suffix in suffixes) {
    file_list <- tasks_lists[[paste0(task, suffix)]]
    df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_anonymized", task, file_list = file_list)
    assign(paste0(task, suffix, "_df"), df)
  }
}

tasks <- c("nback")
suffixes <- c("_high", "_control")

for (task in tasks) {
  for (suffix in suffixes) {
    file_list <- tasks_lists[[paste0(task, suffix)]]
    df <- getGroupPerformance("pavlovia", "SensoriMotorBattery_raw", task, file_list = file_list)
    assign(paste0(task, suffix, "_df"), df)
  }
}

df_imm <- merge(Part3_1, gng_df,   by.x='id', by.y='participant', all=FALSE)

#### combining data ####

tasks <- c("gonogo", "taskswitching", "nback", "trailmaking", "tunneling", "visualsearch")

for(task in tasks){
  control_df <- get(paste0(task, "_control_df"))
  high_df <- get(paste0(task, "_high_df"))
  control_df$treatment <- "control"
  high_df$treatment <- "experiment"
  combined_df <- rbind(control_df, high_df)
  assign(paste0(task, "_combined_df"), combined_df)
}


nback_combined_df$dprime <- nback_combined_df$N1_dprime + 
  nback_combined_df$N2_dprime + nback_combined_df$N3_dprime

#### output of models ####

tasks <- c("gonogo", "taskswitching", "nback", "trailmaking", "tunneling", "visualsearch")

for (task in tasks) {
  print(cat("\n\n", task, "\n\n", sep = ""))
  
  #Set up model formula with random intercepts for each participant
  
  if (task %in% c("gonogo", "nback")){
    model <- lmer(dprime ~ treatment + (1|participant), data = get(paste0(task, "_combined_df")))
  } else {
    model <- lmer(totaltime ~ treatment + (1|participant), data = get(paste0(task, "_combined_df")))
  }
  
  # Fit the model
  fit <- summary(model)
  
  # View the model results
  print(fit)
}