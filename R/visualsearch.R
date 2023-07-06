
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(165)

visualsearch <- function(filename) {

  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  thisparticipant <- as.character(df$participant[1])
  thistotaltime <- df$cumulativetime[dim(df)[1]]
  thisOS <- df$OS[1]
  
  # set up a vector for output, must be a named vector:
  output <- c()
  use = TRUE
  
  # remove lines for breaks:
  df <- df[-which(is.na(df$trialResp.corr)),]
  
  # remove lines with very low RTs:
  df <- df[which(df$trialResp.rt > 0.1),]
  # remove lines with ridiculously high RTs:
  df <- df[which(df$trialResp.rt < 40),]
  
  # get target presence as a variable:
  df$counttrials <- 1
  df$targetpresent <- NA
  df$targetpresent[which(df$trialResp.keys == 'm' & df$trialResp.corr == 1)] <- 'absent'
  df$targetpresent[which(df$trialResp.keys == 'x' & df$trialResp.corr == 1)] <- 'present'
  df$targetpresent[which(df$trialResp.keys == 'm' & df$trialResp.corr == 0)] <- 'present'
  df$targetpresent[which(df$trialResp.keys == 'x' & df$trialResp.corr == 0)] <- 'absent'
  
  
  # check if there are enough good trials left
  counttrials <- aggregate(counttrials ~ arraysize + targetpresent, data=df, FUN=sum)
  if (any(counttrials$counttrials < 18)) {
    #print(filename)
    use = FALSE
    #cat('removed because of low performance\n')
  }
  
  # KK: if not enough good trials return nothing b/c models won't run
  if (!use) {
    return()
  }
  
  # get proportion correct scores to data:
  correct <- aggregate(trialResp.corr ~ arraysize + targetpresent, data=df, FUN=mean)
  # remove people who are less then 66.7% correct in any condition
  # if (any(correct$trialResp.corr < (2/3))) {
  #   use = FALSE
  #   cat('what is going on?\n')
  # }
  correctOutput <- as.vector(unlist(correct$trialResp.corr))
  names(correctOutput) <- sprintf('propcorrect_%d_%s',correct$arraysize,correct$targetpresent)
  
  # we remove the trials without a correct response:
  df[which(df$trialResp.corr == 1),]
  
  # now get the average RTs:
  RTs <- aggregate(trialResp.rt ~ arraysize + targetpresent, data=df, FUN=mean)
  
  # actually, we're going to go with the median, which is less sensitive to outliers:
  #RTs <- aggregate(trialResp.rt ~ arraysize + targetpresent, data=df, FUN=median)
  
  RToutput <- as.vector(unlist(RTs$trialResp.rt))
  names(RToutput) <- sprintf('RT_%d_%s',RTs$arraysize,RTs$targetpresent)
  
  # fit 2 linear models:
  for (target in c('absent','present')) {
    
    model <- lm(trialResp.rt ~ arraysize, data=RTs[which(RTs$targetpresent==target),])
    lmoutput <- as.vector(unlist(coef(model)))
    if (lmoutput[1] > 40) {use <- FALSE} # intercept above the maximum RT
    names(lmoutput) <- sprintf('lm_%s_%s',c('intercept','slope'),target)
    output <- c(output, lmoutput)
    
  }
  
  # create named output vector
  output <- as.list(c(output, correctOutput, RToutput))
  if (!use) {
    output[1:length(output)] <- NA
  }
  output[['participant']]     <- thisparticipant
  output[['totaltime']]       <- thistotaltime
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use
  
  ## KK: date to distinguish double participations
  output[['date']] <- df$date[1]
  
  # return to caller:
  return(output)
  
}