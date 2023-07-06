library(dplyr)
library(lubridate)

#' Get output files
#' 
#' Get a list of output files that match the specified pattern and are within or outside of 5 days of EndDate.
#' 
#' @param folder_path The path to the folder containing the output files.
#' @param Part3_or_4 A data frame containing the IDs to match against.
#' @param t A string indicating whether to return the files within or outside of the specified period. Default is "within".
#' @return A vector of file names matching the specified criteria.
#' @examples
#' getOutputFiles("/path/to/files", Part3_or_4, "within")
#' getOutputFiles("/path/to/files", Part3_or_4, "outside")
getOutputFiles <- function(folder_path, Part3_or_4, t = "within") {
  # Get list of file names
  files <- list.files(folder_path, pattern = '*.csv')
  
  # Create an empty dataframe to store the results
  result_df <- data.frame(id = character(length(files)),
                          task = character(length(files)),
                          date = character(length(files)),
                          time = character(length(files)),
                          stringsAsFactors = FALSE)
  
  # Loop through the file list and add each list of substrings to the result dataframe
  for (i in seq_along(files)) {
    # Split the string into substrings using "_" as the delimiter
    substrings <- strsplit(files[i], "_")[[1]]
    
    # Extract the relevant substrings and add them to the result dataframe
    result_df[i, "id"] <- substrings[1]
    result_df[i, "task"] <- substrings[2]
    result_df[i, "date"] <- substrings[3]
    result_df[i, "time"] <- substrings[4]
  }
  
  # Convert the date and time columns to a datetime object
  result_df$datetime <- ymd(result_df$date)
  
  # Join the two dataframes based on the id column
  df <- inner_join(result_df, Part3_or_4, by = "id")
  
  # Filter the joined dataframe to keep only the rows where the datetime column is within 1 days of EndDate
  df_imm <- df %>%
    filter(datetime >= as_datetime(EndDate) - days(1),
           datetime <= as_datetime(EndDate))
  
  # remaining observations 
  remaining_df <- df %>%
    filter(!row.names(.) %in% row.names(df_imm))

  if(t == "within"){
    return(paste(df_imm$id, df_imm$task, df_imm$date, df_imm$time, sep = "_"))
  }else if (t == "outside"){
    return(paste(remaining_df$id, remaining_df$task, remaining_df$date, remaining_df$time, sep = "_"))
  }
}

moveFiles <- function(files_to_move, source_folder, destination_folder) {
  # Check if the destination folder exists, and create it if it does not
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder)
  }
  
  # Loop through each file and move it to the destination folder
  for (file in files_to_move) {
    file_path <- file.path(source_folder, file)
    if (file.exists(file_path)) {
      file_destination <- file.path(destination_folder, file)
      file.copy(file_path, file_destination)
    } else {
      print(paste0("File ", file, " not found in the source folder."))
    }
  }
  print("Files have been moved.")
}
