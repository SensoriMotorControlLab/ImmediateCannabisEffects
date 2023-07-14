library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(lubridate)

# load helpers
source('R/parseTaskData.R')
source('R/data.R')
source('R/statistics.R')

source('R/MilaR/helper.R')

#### Cleaning the tasks data, Loading to dataframes ####

tasks <- c("gonogo", "visualsearch", "taskswitching", "tunneling", "nBack", "mirrorReversal")

for (task in tasks) {
  start_time <- system.time({
    df <- getGroupPerformance("pavlovia", "2023-07", task)
    assign(paste0(task, "_df"), df)
  })
  
  # print the elapsed time
  print(paste0("Elapsed time for task ", task, " was: ",  as.numeric(start_time["elapsed"]), " seconds"))
}

#### Fixing Trailmaking File Naming ####
# trailMaking didn't run in the first instance
# some task names were TrailMaking instead of trailMaking <- changed to latter
folder_path <- file.path('data', 'pavlovia', '2023-07', 'trailMaking')

# Get the list of .csv files in the folder
file_list <- list.files(folder_path,pattern='*.csv')

# Iterate through each file and rename if necessary
for (file_path in file_list) {
  file_name <- basename(file_path)
  
  # Check if the file name contains 'TrailMaking'
  if (grepl("TrailMaking", file_name)) {
    new_file_name <- gsub("TrailMaking", "trailMaking", file_name)
    f_p <- file.path(folder_path, file_name)
    new_file_path <- file.path(folder_path, new_file_name)
    
    # Rename the file
    file.rename(f_p, new_file_path)
  }
}

##
# additionally manually removed 3 obs where there were no participant ID in the filename string

tasks <- c("trailMaking")

for (task in tasks) {
  start_time <- system.time({
    df <- getGroupPerformance("pavlovia", "2023-07", task)
    assign(paste0(task, "_df"), df)
  })
  
  # print the elapsed time
  print(paste0("Elapsed time for task ", task, " was: ",  as.numeric(start_time["elapsed"]), " seconds"))
}

#### Fixing Mirror Tasks Files Naming ####
folder_path <- file.path('data', 'pavlovia', '2023-07', 'mirrorgeneralizationhor')

# Get the list of .csv files in the folder
file_list <- list.files(folder_path,pattern='*.csv')

# Iterate through each file and rename if necessary
for (file_path in file_list) {
  file_name <- basename(file_path)
  
  # Check if the file name contains different namings
  if (grepl("MirrorGeneralizationHor", file_name)) {
    new_file_name <- gsub("MirrorGeneralizationHor", "mirrorgeneralizationhor", file_name)
    f_p <- file.path(folder_path, file_name)
    new_file_path <- file.path(folder_path, new_file_name)
    
    # Rename the file
    file.rename(f_p, new_file_path)
  }
}

## 

tasks <- c("mirrorgeneralizationhor")

for (task in tasks) {
  start_time <- system.time({
    df <- getGroupPerformance("pavlovia", "2023-07", task)
    assign(paste0(task, "_df"), df)
  })
  
  # print the elapsed time
  print(paste0("Elapsed time for task ", task, " was: ",  as.numeric(start_time["elapsed"]), " seconds"))
}

## combined with mirrorReversal_df
mirrorReversal_df <- bind_rows(mirrorReversal_df, mirrorgeneralizationhor_df)


# list of data frames
df_list <- list(gonogo_df, visualsearch_df, taskswitching_df, 
                tunneling_df, trailMaking_df, nBack_df, mirrorReversal_df)

# iterate over data frames and modify date column
df_list <- lapply(df_list, function(df) {
  df$date_1 <- as.POSIXct(df$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
  return(df)
})

##

df_list <- lapply(df_list, function(df) {
  df$date_1 <- as.POSIXct(df$date, format = "%Y-%m-%d_%Hh%M.%S.%OS")
  df$date_date <- as.Date(df$date)
  df$sdate <- as.Date(df$date)
  df$edate <- as.Date(df$date)
  df$id <- df$participant
  return(df)
})

for (i in seq_along(df_list)) {
  write.csv(df_list[[i]], file.path("data", paste0("df_", tasks[i], ".csv")), row.names = FALSE)
}