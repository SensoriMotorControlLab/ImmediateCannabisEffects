# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(25)

## KK: optimized version using data table
tunneling <- function(filename) {

  use <- TRUE
  
  # first we read the data file:
  dt <- fread(filename)
  
  thisparticipant <- as.character(dt$participant[1])
  thistotaltime <- dt$cumulativetime[dim(dt)[1]]
  thisOS <- dt$OS[1]
  
  
  scale <- intrack <- time_s <- c()
  
  for (rowidx in c(1:dim(dt)[1])) {
    
    step        <- convertCellToNumVector(dt$step[rowidx])
    inOrOut     <- convertCellToNumVector(dt$inOrOut[rowidx])
    mousetime   <- convertCellToNumVector(dt$trialMouse.time[rowidx])
    trackScaleX <- dt$trackScaleX[rowidx]
    trackScaleY <- dt$trackScaleY[rowidx]
    
    if (trackScaleY == trackScaleX) {
      scale <- c(scale, abs(trackScaleX)*100)
    } else {
      cat('scale in X and Y not equal!\n')
    }
    
    step.idx <- which(step > 0)
    intrack  <- c(intrack, mean(inOrOut[step.idx]))
    time_s   <- c(time_s, diff(range(mousetime[step.idx])))
  }
  
  df <- aggregate(cbind(intrack, time_s) ~ scale, data=data.frame(scale, intrack, time_s), FUN=median)
  
  intrack <- df$intrack
  names(intrack) <- sprintf('intrack_sc%d', df$scale)
  time_s <- df$time_s
  names(time_s) <- sprintf('MT_sc%d', df$scale)
  
  output <- as.list( c(intrack, time_s) )
  
  output[['participant']]     <- thisparticipant
  output[['totaltime']]       <- thistotaltime
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use # although there is no real screening implemented yet...
  
  ## KK: date to distinguish double participations
  output[['date']] <- dt$date[1]
  
  return(output)
  
}