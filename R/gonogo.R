
# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(303)

gonogo <- function(filename) {

  use <- TRUE

  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  thisparticipant <- as.character(df$participant[1])
  thistotaltime <- df$cumulativetime[dim(df)[1]]
  thisOS <- df$OS[1]
  
  # get a more useful column with trialtype:
  df$trialtype <- NA
  df$trialtype[which(df$colorName == 'blue')]   <- 'go'
  df$trialtype[which(df$colorName == 'orange')] <- 'nogo'
  
  # set up a vector for output, must be a named vector:
  output <- c()
  
  # remove lines for breaks:
  df <- df[-which(is.na(df$trialResp.corr)),]
  
  # remove people who hit space in ALL trials:
  notspace <- length(which(df$trialResp.keys != 'space'))
  if (notspace < 1) {
    #cat('all space?\n')
  }
  
  # remove lines with very low RTs:
  df <- df[which(df$trialResp.rt > 0.1 | is.na(df$trialResp.rt)),]
  
  # do the calculations only if there are observations left (MK)
  if (nrow(df)>0){
  
    # get proportion correct scores to data:
    correct <- aggregate(trialResp.corr ~ trialtype, data=df, FUN=mean)
    # remove people who are less then 66.7% correct in any condition
    if (any(correct$trialResp.corr[which(correct$trialtype == 'go')] < 0.95)) {
      use <- FALSE
      #cat('too many errors\n')
    }
    
    # get dprime, we need counts:
    hits   <- length(which(df$trialtype == 'go'   & df$trialResp.corr == 1))
    misses <- length(which(df$trialtype == 'go'   & df$trialResp.corr == 0))
    fas    <- length(which(df$trialtype == 'nogo' & df$trialResp.corr == 0))
    crs    <- length(which(df$trialtype == 'nogo' & df$trialResp.corr == 1))
    
    dpr    <- dprime(hits=hits, 
                     misses=misses, 
                     fas=fas, 
                     crs=crs, 
                     hautus=TRUE)
    
    sigdectOutput <- list('dprime'=dpr$dprime,'sensitivity'=dpr$sensitivity)
    
    # do we use prop correct, or prop error?
    correctOutput <- as.vector(unlist(correct$trialResp.corr))
    names(correctOutput) <- sprintf('%s_prop.correct',correct$trialtype)
    errorOutput <- 1 - as.vector(unlist(correct$trialResp.corr))
    names(errorOutput) <- sprintf('%s_prop.error',correct$trialtype)
    
    # %error (or false alarms), and RTs for hits and false alarms.
    
    # get RTs (for trials that have them):
    df <- df[which(!is.na(df$trialResp.rt)),]
    RTs <- aggregate(trialResp.rt ~ trialtype, data=df, FUN=mean)
    RToutput <- as.vector(unlist(RTs$trialResp.rt))
    names(RToutput) <- sprintf('%s_RT',RTs$trialtype)
    
    if (!('go_RT' %in% names(RToutput))) {
      RToutput <- c(c('go_RT'=NA), RToutput)
    }
    if (!('nogo_RT' %in% names(RToutput))) {
      RToutput <- c(RToutput, c('nogo_RT'=NA))
    }
    
    # create named output vector
    output <- as.list(c(errorOutput, RToutput, sigdectOutput))
    if (!use) {
      output[1:length(output)] <- NA
    }
    
    output[['participant']]     <- thisparticipant
    output[['totaltime']]       <- thistotaltime
    output[['OS']]              <- thisOS
    output[['passedscreening']] <- use
    
    ## KK: date to distinguish double participations
    output[['date']] <- df$date[1]
    
    return(output)
  }
}
