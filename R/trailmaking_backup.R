# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(6)


trailmaking <- function(filename) {
  
  use <- TRUE
  
  print(filename)
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  thisparticipant <- as.character(df$participant[1])
  thistotaltime <- df$cumulativetime[dim(df)[1]]
  thisOS <- df$OS[1]
  
  TMdata <- c()
  MT <- list('MoveTime_1'=c(),  'MoveTime_2'=c(),    'MoveTime_3'=c(),   'MoveTime_4'=c(),   'MoveTime_5'=c())
  PL <- list('PathLength_1'=c(), 'PathLength_2'=c(), 'PathLength_3'=c(), 'PathLength_4'=c(), 'PathLength_5'=c())
  SP <- list('ShortPath_1'=c(),  'ShortPath_2'=c(),  'ShortPath_3'=c(),  'ShortPath_4'=c(),  'ShortPath_5'=c())

  for (trialno in c(1:5)) {
    
    # this is where the stimuli are:
    stimX <- convertCellToNumVector(df$stimulusX[trialno])
    stimY <- convertCellToNumVector(df$stimulusY[trialno])
    # shortest path from first to last stimulus in order:
    shortest_route <- sum(sqrt(diff(stimX)^2 + diff(stimY)^2 ))
    # store:
    SP[[trialno]] <- c(SP[[trialno]], shortest_route)

    # this is the actual path:
    x <- convertCellToNumVector(df$trialMouse.x[trialno])
    y <- convertCellToNumVector(df$trialMouse.y[trialno])
    s <- convertCellToNumVector(df$trialMouse.time[trialno])
    step <- convertCellToNumVector(df$step[trialno])
    # but we only count from the point the participants reaches the first position
    # which is when step == 1
    start.idx <- which(step == 1)[1]
    
    x <- x[start.idx:length(x)]
    y <- y[start.idx:length(y)]
    s <- s[start.idx:length(s)]
    
    path_length <- sum(sqrt(diff(x)^2 + diff(y)^2))
    
    movement_time <- s[length(s)]-s[1]
    
    TMdata[sprintf('MoveTime_%d',trialno)] <- movement_time
    TMdata[sprintf('PathLength_%d',trialno)] <- path_length
    TMdata[sprintf('ShortestPath_%d',trialno)] <- shortest_route
    TMdata[sprintf('TimePerLength_%d',trialno)] <- movement_time / path_length
    
  }
  
  #print(thisparticipant)
  
  # create named output vector
  output <- as.list(TMdata)
  if (!use) {
    output[1:length(output)] <- NA
  }
  
  #print(output)
  
  output[['participant']]     <- thisparticipant
  output[['totaltime']]       <- thistotaltime
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use
  
  return(output)
  
}


