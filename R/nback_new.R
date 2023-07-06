
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(183,190)

nback <- function(filename) {
  
  use <- TRUE
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  thisparticipant <- as.character(df$participant[1])
  #thistotaltime <- df$cumulativetime[dim(df)[1]]
  thisOS <- df$OS[1]
  
  # remove break lines:
  df <- df[which(!is.na(df$trials.thisN)),]
  
  if ('reactiontime' %in% names(df)) {   # new file format
    
    medRT <- median(df$reactiontime, na.rm=TRUE)
    if (!is.na(medRT) & medRT < 0) {
      df$reactiontime[!is.na(df$reactiontime)] <- df$reactiontime[!is.na(df$reactiontime)] + 1
    }
    
    # remove initial non-response trials:
    df <- df[which(df$use == 1),]
    
    # set up column for correct responses:
    df$response <- as.numeric(df$response) # first get the response column in shape
    df$response[which(is.nan(df$response))] <- NA
    df$correct <- NA
    # idx <- which(!is.na(df$response))      
    df$correct <- (df$response == df$target)
    
    
  } else {  # old file format
    
    if ('trialResp.rt' %in% names(df) ) {
      df$reactiontime <- df$trialResp.rt
    }
    
    # set up column for correct responses:
    df$correct <- (df$trialResp.keys == df$corrAns)
    
    # set up column to indicate if a target was presented:
    df$target <- 0
    df$target[which(df$corrAns == 'space')] <- 1
    
    df$nback <- df$trialsNum
    
  }
  
  # remove very low RTs:
  if ('reactiontime' %in% names(df)) {
    df <- df[which(is.na(df$reactiontime) | df$reactiontime > 0.1),]
  }
  
  
  # get proportion correct for target-presence BY N-back:
  if (dim(df)[1] > 0 & 'reactiontime' %in% names(df) & length(which(!is.na(df$correct))) > 0) {
    
    correct <- aggregate(correct ~ target + nback, data=df, FUN=mean)
    
    if ( any( correct$correct[which(correct$target == 1)] < c(0.50, 0.25, 0.01) ) ) {
      # low performance!
      use <- FALSE
    }
    if (dim(correct)[1] < 6) {
      # missing performance for at least 1 condition
      use <- FALSE
    }
  } else {
    use <- FALSE # seriously? this happens?
  }
  
  if (use) {
    correctOutput <- as.vector(unlist(correct$correct))
    names(correctOutput) <- sprintf('N%d_%s_prop.correct',correct$nback,c('absent','present')[correct$target+1])
  } else {
    correctOutput <- rep(NA, 6)
    names(correctOutput) <- sprintf('N%d_%s_prop.correct',c(1,1,2,2,3,3),rep(c('absent','present'),3))
  }
  
  # get d-primes and Cowan's K's for each N (1,2,3)
  if (use) {
    
    sigdectOutput <- c()
    RToutput <- c()
    
    for (N in c(1,2,3)) {
      
      # select only data for N=N
      Ndf <- df[which(df$nback == N),]
      
      # get the numbers of hits, misses, false alarms and correct rejections:
      hits   <- length(which(Ndf$correct == TRUE  & Ndf$target == 1))
      misses <- length(which(Ndf$correct == FALSE & Ndf$target == 1))
      fas    <- length(which(Ndf$correct == FALSE & Ndf$target == 0))
      crs    <- length(which(Ndf$correct == TRUE  & Ndf$target == 0))
      
      # get dprime
      Ndpr <- dprime(hits=hits, misses=misses, fas=fas, crs=crs, hautus=TRUE)$dprime
      # get Cowan's K
      Ncwk <- cowan.k(hits=hits, misses=misses, fas=fas, crs=crs, N=N)
      # add to output
      newsigdectOutput <- c(Ndpr,Ncwk)
      names(newsigdectOutput) <- sprintf('N%d_%s',rep(N,2),c('dprime','cowan.k'))
      sigdectOutput <- c(sigdectOutput, newsigdectOutput)
      
      # get RTs for hits
      hit_idx <- which(Ndf$correct == TRUE  & Ndf$target == 1) 
      if (length(hit_idx) > 0) {
        hit_RT <- mean(Ndf$reactiontime[hit_idx], na.rm=TRUE)
      } else {hit_RT <- NA}
      
      # get RTs for false alarms
      fa_idx  <- which(Ndf$correct == FALSE & Ndf$target == 0)
      if (length(fa_idx) > 0) {
        fa_RT <- mean(Ndf$reactiontime[fa_idx], na.rm=TRUE)
      } else {fa_RT <- NA}
      
      # add to output:
      newRToutput <- c(hit_RT, fa_RT)
      names(newRToutput) <- sprintf('N%d_%s',rep(N,2),c('hits_RT','falsealarm_RT'))
      RToutput <- c(RToutput, newRToutput)
      
    }
  } else {
    
    sigdectOutput <- rep(NA,6)
    names(sigdectOutput) <- sprintf('N%d_%s',c(1,1,2,2,3,3),rep(c('dprime','cowan.k'),3))
    
    RToutput <- rep(NA,6)
    names(RToutput) <- sprintf('N%d_%s',c(1,1,2,2,3,3),rep(c('hits_RT','falsealarm_RT'),3))
    
  }
  
  
  # create named output vector
  output <- as.list(c(correctOutput, RToutput, sigdectOutput))
  if (!use) {
    output[1:length(output)] <- NA
  }
  
  output[['participant']]     <- thisparticipant
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use
  
  return(output)
  
}
