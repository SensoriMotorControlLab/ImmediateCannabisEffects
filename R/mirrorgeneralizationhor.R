nlines <- c(143)

mirrorgeneralizationhor <- function(filename) {  
  step <- 2
  
  # if the file can't be read, return empty list for now
  dt <- NULL
  dt <- fread(filename)
  if (is.null(dt)) {
    return(list())
  }
  
  # set up vectors for relevant data:
  trialno <- c()            #trialNum
  targetangle_deg <- c()
  mirror <-c()              #trialsType
  taskno <- c()             #trialsNum
  participant <- c()
  time <- c()
  
  # remove empty lines:
  dt <- dt[which(!is.na(dt$trialsNum)),]

  
  # loop through all trials
  for (trialnum in c(1:dim(dt)[1])) {
    s <- convertCellToNumVector(dt$step[trialnum])
    m <- dt$trialsType[trialnum]
    a <- dt$targetangle_deg[trialnum]
    p <- dt$participant[trialnum]
    t <- convertCellToNumVector(dt$trialMouse.time[trialnum])
    d <- dt$date[trialnum]
    o <- dt$OS[trialnum]
    
    # remove stuff that is not step==2
    stepidx <- which(s == step)
    if(all(stepidx == 0)){
      return(list())
    }
    t <- t[stepidx]
    startt <- t[1]
    endt <- t[length(t)]
    mt <- endt - startt
    
    
    # store in vectors:
    trialno <- c(trialno, trialnum)
    targetangle_deg <- c(targetangle_deg, a)
    mirror <-c(mirror, m)
    taskno <- c(taskno, dt$trialsNum[trialnum])
    time <- c(time, mt)
  }
  
  # vectors as data frame columns:
  dtmt <- data.frame(trialno, targetangle_deg, mirror, taskno, time)
  
  dtmt <- head(subset(dtmt, taskno == 2), 20)

  # create named output vector
  output <- c()
  if (nrow(dtmt) >= 3) {
    dtmt <- dtmt[3:nrow(dtmt), ]
    output[['meanMT']] <- mean(dtmt$time)
    output[['sdMT']] <- sd(dtmt$time)
  } else {
    dtmt <- data.frame()  # Empty data frame
    output[['meanMT']] <- NA
    output[['sdMT']] <- NA
  }
  
  output[['participant']]     <- p
  output[['OS']]              <- o
  output[['date']] <- d
  
  return(output)
}


#Function helps to organize data within each cell, so that they are numeric and we can work with them.
convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}