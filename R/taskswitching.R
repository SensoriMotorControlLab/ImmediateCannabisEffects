# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(77)

## KK: optimized version using data.table
taskswitching <- function(filename) {

  use <- TRUE
  
  # first we read the data file:
  dt <- fread(filename)
  
  thisparticipant <- as.character(dt$participant[1])
  thistotaltime <- dt$cumulativetime[dim(dt)[1]]
  thisOS <- dt$OS[1]
  
  if (dim(dt)[1]==76) {
    dt <- dt[-which(is.na(dt$dots)),]
    dt <- dt %>%
      mutate(block = c(rep(1,12), rep(2,12), rep(3,50)))
  }else if (dim(dt)[1]==102) {
    dt <- dt[-which(is.na(dt$dots)),]
    dt <- dt %>%
      mutate(block = c(rep(1,12), rep(2,12), rep(3,50), rep(4,12), rep(5,12)))
  }
  
  
  # remove very low RTs:
  dt$key_resp.rt[dt$key_resp.rt < 0.1] <- NA
  dt$key_resp.rt[dt$key_resp.rt == ""] <- NA
  
  
  ## KK: simplify applied functions for correct response and conguence
  dt$correctResponse <- apply(dt[,c('correctAnswer','key_resp.keys')], 1, function(x) as.numeric(x[1] == x[2]))
  dt$congruent <- apply(dt[,c('shape','dots')], 1, function(x) as.numeric((x[1] == 'diamond' & x[2] == '2') | (x[1] == 'square' & x[2] == '3')))
  
  ## new
  dt$grid_loc_y <- NA
  dt$switch <- NA
  for (block in unique(dt$block)) {
    
    trial_idx <- which(dt$block == block)
    
    for (trial in trial_idx) {
      dt$grid_loc_y[trial] <- convertCellToNumVector(dt$gridLocation[trial])[2]
    }
    
    dt$switch[trial_idx[2:length(trial_idx)]] <- as.logical(diff(dt$grid_loc_y[trial_idx]))
    
  }
  
  
  # get proportion correct scores to data for switch/non-switch trials:
  correct <- aggregate(correctResponse ~ switch, data=dt, FUN=mean)
  correct$correctResponse <- round(correct$correctResponse, digits = 3)
  
  # get proportion correct scores to data for congruent/non-congruent trials:
  congruent <- aggregate(correctResponse ~ congruent, data = dt, FUN = mean)
  congruent$correctResponse <- round(congruent$correctResponse, digits= 3)
  
  #### which percent threshold for error to keep 
  if (any(correct$correctResponse[which(correct$switch == 1)] < 0.60)) {
    use <- FALSE
    #cat('too many errors\n')
  }
  
  #correct response output for switch vs.non-switch trials  
  correctOutput <- as.vector(unlist(correct$correctResponse))
  names(correctOutput) <- sprintf('switch_%s_prop.correct',as.numeric(correct$switch))
  
  congruentOutput <- as.vector(unlist(congruent$correctResponse))
  names(congruentOutput) <- sprintf('congruent_%s_prop.correct', as.numeric(congruent$congruent))
  
  # data frame for single trials 
  singledf <- dt[c(4:12, 17:25 ),]
  
  # print (singledf)
  # get proportion correct scores to data for single block trials:
  correctsingle <- aggregate(correctResponse ~ block, data = singledf, FUN = mean)
  
  correctsingle$correctResponse <- round(correctsingle$correctResponse, digits = 3)
  
  if (any(correctsingle$correctResponse[which(correctsingle$block == 1 | correctsingle$block == 2)] < 0.65)) {
    use <- FALSE
    #cat('too many errors\n')
  }
  
  #correct response output for single block trials  
  correctOutputSingle <- as.vector(unlist(correctsingle$correctResponse))
  names(correctOutputSingle) <- sprintf('block_%s_prop.correct',correctsingle$block)
  
  
  RToutput <- c()
  
  
  
  #get RTs for single block 1
  singleBlock1_idx <- which(singledf$correctResponse == 1  & singledf$block == 1) 
  if (length(singleBlock1_idx) > 0) {
    singleBlock1_RT <- mean(singledf$key_resp.rt[singleBlock1_idx], na.rm=TRUE)
  } else {singleBlock1_RT <- NA}
  
  singleBlock1_RT <- round(singleBlock1_RT, digits = 3)
  
  
  
  #get RTs for single block 2
  singleBlock2_idx <- which(singledf$correctResponse == 1  &  singledf$block==2) 
  if (length(singleBlock2_idx) > 0) {
    singleBlock2_RT <- mean(singledf$key_resp.rt[singleBlock2_idx], na.rm=TRUE)
  } else {singleBlock2_RT <- NA}
  
  singleBlock2_RT <- round(singleBlock2_RT, digits = 3)
  
  
  # get RTs for switch trials
  switch_idx <- which(dt$correctResponse == 1  & dt$switch == TRUE & dt$block == 3) 
  if (length(switch_idx) > 0) {
    switch_RT <- mean(dt$key_resp.rt[switch_idx], na.rm=TRUE)
  } else {switch_RT <- NA}
  
  switch_RT <- round(switch_RT, digits = 3)
  
  
  
  
  # get RTs for non-switch trials
  nonswitch_idx <- which(dt$correctResponse == 1  & dt$switch == FALSE & dt$block == 3) 
  if (length(nonswitch_idx) > 0) {
    nonswitch_RT <- mean(dt$key_resp.rt[nonswitch_idx], na.rm=TRUE)
  } else {nonswitch_RT <- NA}
  
  nonswitch_RT <- round(nonswitch_RT, digits = 3)
  
  
  #get RT for congurent trials
  congruent_idx <- which(dt$correctResponse == 1  & dt$congruent == 1 & dt$block == 3) 
  if (length(congruent_idx) > 0) {
    congruent_RT <- mean(dt$key_resp.rt[congruent_idx], na.rm=TRUE)
  } else {congruent_RT <- NA}
  
  congruent_RT <- round(congruent_RT, digits = 3)
  
  #get RT for non-congurent trials
  nonCongruent_idx <- which(dt$correctResponse == 1  & dt$congruent == 0 & dt$block == 3) 
  if (length(nonCongruent_idx) > 0) {
    nonCongruent_RT <- mean(dt$key_resp.rt[nonCongruent_idx], na.rm=TRUE)
  } else {nonCongruent_RT <- NA}
  
  nonCongruent_RT <- round(nonCongruent_RT, digits = 3)
  
  newRToutput <- c(singleBlock1_RT,singleBlock2_RT, switch_RT, nonswitch_RT, congruent_RT, nonCongruent_RT )
  names(newRToutput) <- sprintf(c('singleblock_1_RT','singleblock_2_RT','switch_RT', 'nonswitch_RT', 'congruent_RT', 'nonCongruent_RT' ))
  RToutput <- c(RToutput, newRToutput)
  
  
  # remove break lines:
  dt <- dt[which(!is.na(dt$trials.thisRepN)),]
  
  
  # create named output vector
  output <- as.list(c(correctOutput, congruentOutput, correctOutputSingle,RToutput))
  if (!use) {
    output[1:length(output)] <- NA
  }
  
  
  output[['participant']]     <- thisparticipant
  output[['totaltime']]       <- thistotaltime
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use
  
  ## KK: date to distinguish double participations
  output[['date']] <- dt$date[1]
  
  return(output)
  
}
  
  
  