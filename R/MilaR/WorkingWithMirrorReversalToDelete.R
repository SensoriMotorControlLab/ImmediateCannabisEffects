source('R/mirrorReversal.R')

year <- "pavlovia"
semester <- "2023-07"
task <- "mirrorReversal"

# get list of file names
folder <- file.path('data',year,semester,task)
files <- list.files(folder,pattern='*.csv')

# use readLines and weed out those with too few lines
filelines <- unlist(lapply(sprintf('%s%s%s',folder,.Platform$file.sep,files), function(x){length(readLines(x))}))
# kk: keep track of how many were deleted
l <- length(files)
files <- files[which(filelines %in% nlines)]
print(sprintf("Deleted %d files.\n", l - length(files) ))

# extract participant IDs and timestamps
# participants <- as.data.frame(do.call("rbind", lapply(files, getIDtimestamp, task)), stringsAsFactors=F)
# KK: strings separated by tasknames b/c there are unusual IDs
participants <- as.data.frame(do.call("rbind", lapply(files, getIDtimestamp_KK, task)), stringsAsFactors=F)
participants <- participants[order(participants$timestamp),]
row.names(participants) <- NULL

# get relative filenames:
#participants$filename <- sprintf('data/%s/%s/%s/%s_%s_%s.csv',year,semester,task,participants$participant,task,participants$timestamp)
# fixing non-harmonized task names
participants$filename <- sprintf('data/%s/%s/%s/%s_%s_%s',year,semester,task,participants$participant,participants$task,participants$timestamp)


#### figuring out for one task ####

mirrorReversal <- function(filename) {
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
    #print(trialnum)
    s <- convertCellToNumVector(dt$step[trialnum])
    m <- dt$trialsType[trialnum]
    a <- dt$targetangle_deg[trialnum]
    p <- dt$participant[trialnum]
    t <- convertCellToNumVector(dt$trialMouse.time[trialnum])
    d <- dt$date[trialnum]
    o <- dt$OS[trialnum]
    
    # remove stuff that is not step==2
    stepidx <- which(s == step)
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
  
  dtmt <- tail(subset(dtmt, taskno == 2), 40)

  # create named output vector
  output <- c()
  output[['meanMT']] <- mean(dtmt$time)
  output[['sdMT']] <- sd(dtmt$time)
  
  output[['participant']]     <- p
  output[['OS']]              <- o
  output[['date']] <- d
  
  return(output)
}

test <- mirrorReversal(participants$filename[1])

#### for all files ####


f <- match.fun(task)

# and use lapply to run stuff on all participants
functionoutput <- as.data.frame(do.call("rbind", lapply(participants$filename, f)))


# this will be a complicated format, so we simplify it a little here
colnames <- names(functionoutput)
for (colname in colnames) {
  functionoutput[colname] <- unlist(functionoutput[colname])
}





