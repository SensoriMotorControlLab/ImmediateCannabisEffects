df <- getGroupPerformance(2023, "spring", "gonogo")
df_control <- getGroupPerformance(2020, "fall", "gonogo")

## test

# get list of file names
folder <- file.path('data','2020','fall','gonogo')
files <- list.files(folder,pattern='*.csv')

# get list of file names
folder <- file.path('data','2023','spring','gonogo')
files_exp <- list.files(folder,pattern='*.csv')
intersection <- Reduce(intersect,list(files, files_exp))
out <- files_exp[!(files_exp %in% intersection)]
