
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(183)

nback <- function(filename) {
  
  use <- TRUE
  
  print(filename)
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  thisparticipant <- as.character(df$participant[1])
  #thistotaltime <- df$cumulativetime[dim(df)[1]]
  thisOS <- df$OS[1]
  
  # remove break lines:
  df <- df[which(!is.na(df$trialsNum)),]
  
  # remove very low RTs:
  if ('trialResp.rt' %in% names(df)) {
    df <- df[which(is.na(df$trialResp.rt) | df$trialResp.rt > 0.1),]
  } else {
    use <- FALSE
  }
  
  # get a column indicating whether or not the response is correct:
  df$correct <- (df$trialResp.keys == df$corrAns)
  
  # get a column indicating whether or not a target trial was presented:
  df$target <- 'absent'
  df$target[which(df$corrAns == 'space')] <- 'present'
  
  # get proportion correct for target-presence BY N-back:
  if (dim(df)[1] > 0 & 'trialResp.rt' %in% names(df)) {
    #print(dim(df))
    correct <- aggregate(correct ~ target + trialsNum, data=df, FUN=mean)
    if ( any( correct$correct[which(correct$target == 'present')] < c(0.50, 0.25, 0.01) ) ) {
      use <- FALSE
    }
    # if (any(correct$correct[which(correct$target == 'present')] < 0.50)) {
    #   use <- FALSE
    # }
  } else {
    use <- FALSE
  }
  
  if (use) {
    correctOutput <- as.vector(unlist(correct$correct))
    names(correctOutput) <- sprintf('N%d_%s_prop.correct',correct$trialsNum,correct$target)
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
      Ndf <- df[which(df$trialsNum == N),]
      
      # get the numbers of hits, misses, false alarms and correct rejections:
      hits   <- length(which(Ndf$correct == TRUE  & Ndf$target == 'present'))
      misses <- length(which(Ndf$correct == FALSE & Ndf$target == 'present'))
      fas    <- length(which(Ndf$correct == FALSE & Ndf$target == 'absent'))
      crs    <- length(which(Ndf$correct == TRUE  & Ndf$target == 'absent'))
      
      # get dprime
      Ndpr <- dprime(hits=hits, misses=misses, fas=fas, crs=crs, hautus=TRUE)$dprime
      # get Cowan's K
      Ncwk <- cowan.k(hits=hits, misses=misses, fas=fas, crs=crs, N=N)
      # add to output
      newsigdectOutput <- c(Ndpr,Ncwk)
      names(newsigdectOutput) <- sprintf('N%d_%s',rep(N,2),c('dprime','cowan.k'))
      sigdectOutput <- c(sigdectOutput, newsigdectOutput)
      
      # get RTs for hits
      hit_idx <- which(Ndf$correct == TRUE  & Ndf$target == 'present') 
      if (length(hit_idx) > 0) {
        hit_RT <- mean(Ndf$trialResp.rt[hit_idx], na.rm=TRUE)
      } else {hit_RT <- NA}
      
      # get RTs for false alarms
      fa_idx  <- which(Ndf$correct == FALSE & Ndf$target == 'absent')
      if (length(fa_idx) > 0) {
        fa_RT <- mean(Ndf$trialResp.rt[fa_idx], na.rm=TRUE)
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
