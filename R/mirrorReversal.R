mirrorReversal <- function(set, step = 2){
  
  if (set == 'su2020'){
    datafilenames <- list.files('data/mReversalNewAlpha3-master/raw', pattern = '*.csv')
  } else if (set == 'fa2020'){
    datafilenames <- list.files('data/mirrorreversal-fall/raw', pattern = '*.csv')
  }
  
  participant <- c()#create place holder
  meanMT <- c()
  sdMT <- c()
  for(datafilenum in c(1:length(datafilenames))){
    if (set == 'su2020'){
      datafilename <- sprintf('data/mReversalNewAlpha3-master/raw/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    } else if (set == 'fa2020'){
      datafilename <- sprintf('data/mirrorreversal-fall/raw/%s', datafilenames[datafilenum]) #change this, depending on location in directory
    }
    #cat(sprintf('file %d / %d     (%s)\n',datafilenum,length(datafilenames),datafilename))
    dat <- handleOneMTFile(filename = datafilename, step = step)
    
    #then get only the last 40 trials of the mirror reversal (trials 71 to 110)
    dat <- dat[which(dat$taskno == 2),]
    dat <- tail(dat, n = 40)
    
    #get variables for participant, mean, SD of measure
    ppname <- unique(dat$participant)
    mean <- mean(dat$time)
    stdev <- sd(dat$time)
    
    participant <- c(participant, ppname)
    meanMT <- c(meanMT, mean)
    sdMT <- c(sdMT, stdev)
    
  }
  
  dfmt <- data.frame(participant, meanMT, sdMT)
  
  #IMPORTANT: Summer data - These participants have mirror data but no qualtrics data:
  # 215797, Tiffany, Victoria, Yue Hu
  # dfmt <- dfmt[-which(dfmt$participant == '215797'),]
  # dfmt <- dfmt[-which(dfmt$participant == 'Tiffany'),]
  # dfmt <- dfmt[-which(dfmt$participant == 'Victoria'),]
  # dfmt <- dfmt[-which(dfmt$participant == 'Yue Hu'),]
  
  return(dfmt)
  
}


handleOneMTFile <- function(filename, step) {
  
  # if the file can't be read, return empty list for now
  df <- NULL
  try(df <- read.csv(filename, stringsAsFactors = F), silent = TRUE)
  if (is.null(df)) {
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
  df <- df[which(!is.na(df$trialsNum)),]
  
  # loop through all trials
  for (trialnum in c(1:dim(df)[1])) {
    #print(trialnum)
    s <- convertCellToNumVector(df$step[trialnum])
    m <- df$trialsType[trialnum]
    a <- df$targetangle_deg[trialnum]
    p <- df$participant[trialnum]
    t <- convertCellToNumVector(df$trialMouse.time[trialnum])
    
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
    taskno <- c(taskno, df$trialsNum[trialnum])
    participant <- c(participant, p)
    time <- c(time, mt)
  }
  
  # vectors as data frame columns:
  dfmt <- data.frame(trialno, targetangle_deg, mirror, taskno, participant, time)
  
  
  return(dfmt)
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