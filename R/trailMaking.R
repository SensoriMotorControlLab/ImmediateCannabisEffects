# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(6)

## KK: optimized function using data table
## improved from many hours
## to 118.260000000002 seconds
trailMaking <- function(filename) {
  
  use <- TRUE
  
  # first we read the data file:
  dt <- fread(filename)
  
  thisparticipant <- as.character(dt$participant[1])
  thistotaltime <- dt$cumulativetime[dim(dt)[1]]
  thisOS <- dt$OS[1]
  
  # Convert stimulus columns to numeric vectors
  stimX <- lapply(dt$stimulusX, convertCellToNumVector)
  stimY <- lapply(dt$stimulusY, convertCellToNumVector)
  
  SP <- c()
  
  # Calculate shortest path and store in SP list
  for (i in 1:5) {
    shortest_route <- sum(sqrt(diff(stimX[[i]])^2 + diff(stimY[[i]])^2 ))
    SP[[i]] <- shortest_route
  }
  
  # Extract trial columns and calculate metrics
  trialMouseX <- lapply(dt$trialMouse.x, convertCellToNumVector)
  trialMouseY <- lapply(dt$trialMouse.y, convertCellToNumVector)
  trialMouseTime <- lapply(dt$trialMouse.time, convertCellToNumVector)
  step <- lapply(dt$step, convertCellToNumVector)
  
  # Calculate path length, movement time, and other variables for each trial
  path_lengths <- sapply(seq_along(trialMouseX), function(trialno) {
    start.idx <- which(step[[trialno]] == 1)[1]
    x <- trialMouseX[[trialno]][start.idx:length(trialMouseX[[trialno]])]
    y <- trialMouseY[[trialno]][start.idx:length(trialMouseY[[trialno]])]
    sum(sqrt(diff(x)^2 + diff(y)^2))
  })
  
  movement_times <- sapply(seq_along(trialMouseTime), function(trialno) {
    start.idx <- which(step[[trialno]] == 1)[1]
    s <- trialMouseTime[[trialno]][start.idx:length(trialMouseTime[[trialno]])]
    tail(s, 1) - s[1]
  })
  
  shortest_paths <- unlist(SP)
  
  time_per_lengths <- movement_times / path_lengths
  
  # Combine trial data into a vector
  TMdata_new <- c(movement_times, path_lengths, shortest_paths, time_per_lengths)
  names(TMdata_new) <- paste(rep(c('MoveTime', 'PathLength', 'ShortestPath', 'TimePerLength'), 
                                 each = 5), rep(1:5, times = 4), sep = '_')
  
  
  # create named output vector
  output <- as.list(TMdata_new)
  if (!use) {
    output[1:length(output)] <- NA
  }
  
  #print(output)
  
  output[['participant']]     <- thisparticipant
  output[['totaltime']]       <- thistotaltime
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use
  
  ## KK: date to distinguish double participations
  output[['date']] <- dt$date[1]
  
  return(output)
  
}


