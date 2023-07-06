# use the first (or alternatively the last) of several files for the same participant?
usefirst <- TRUE

# how many lines should be in a data file? (there may be multiple correct options)
nlines <- c(77)

taskswitching <- function(filename) {
  
  use <- TRUE
  
  # first we read the data file:
  df <- read.csv(filename, stringsAsFactors=F)
  
  thisparticipant <- as.character(df$participant[1])
  thistotaltime <- df$cumulativetime[dim(df)[1]]
  thisOS <- df$OS[1]
  
  
  if (dim(df)[1]==77) {
    # remove lines for breaks:
    df <- df[-which(is.na(df$dots)),]
    block <- c(rep(1,12), rep(2,12), rep(3,50))
  }
  if (dim(df)[1]==77) {
    # remove lines for breaks:
    df <- df[-which(is.na(df$dots)),]
    block <- c(rep(1,12), rep(2,12), rep(3,50), rep(4,12), rep(5,12))
  }
  
  df <- cbind(block,df)
  
  
  # remove very low RTs:
  df$key_resp.rt[df$key_resp.rt < 0.1] <- NA
  df$key_resp.rt[df$key_resp.rt == ""] <- NA
  
  # get a column indicating whether the response is correct 
  apply_correct <- function(answer_response){
    if (answer_response[1]== answer_response[2])
      return(1)
    else 
      return(0)
  }
  
  df$correctResponse <- apply(df[,c('correctAnswer','key_resp.keys')], 1, FUN=apply_correct)
  
  
  # get a column indicating whether trial is congruent
  apply_congruent <- function(shape_dots) {
    # print(shape_dots)
    # d2 <- as.logical((shape_dots[1] == 'diamond' & shape_dots[2] == '2'))
    # s3 <- as.logical((shape_dots[1] == 'square'  & shape_dots[2] == '3'))
    return(( as.logical((shape_dots[1] == 'diamond' & shape_dots[2] == '2')) | as.logical((shape_dots[1] == 'square'  & shape_dots[2] == '3')) ))
  }
  
  df$congruent <- apply(df[,c('shape','dots')], 1 , FUN = apply_congruent)
  
  # print(df$congruent)
  
  # Switch vs. non-switch trials 
  # given a string of numbers from pavlovia experiment, makes an array and ONLY RETURNS THE SECOND VALUE
  # online_string_to_array <- function(str_array){
  #   
  #   x <- str_sub(str_array, 2, str_length(str_array)-1) # subset string to get rid of beginning and end
  #   x <- simplify(lapply(str_split(x, ','), as.double)) # make this a list of doubles (simplify gets rid of unnecessary levels)
  #   
  #   x <- x[2] # we only need the second value
  #   return(x)
  # }
  
  
  df$grid_loc_y <- NA
  df$switch <- NA
  for (block in unique(df$block)) {
    
    trial_idx <- which(df$block == block)
    
    for (trial in trial_idx) {
      df$grid_loc_y[trial] <- convertCellToNumVector(df$gridLocation[trial])[2]
    }
    
    df$switch[trial_idx[2:length(trial_idx)]] <- as.logical(diff(df$grid_loc_y[trial_idx]))
    
  }
  
  # library(tidyverse)
  
  # #finding switch vs. non-switch trials 
  # df$grid_loc_y <- apply(df[,'gridLocation', drop=F], 
  #                        1, FUN = convertCellToNumVec)
  # 
  # df <- df %>% 
  #   mutate(grid_diff = lag(grid_loc_y) + grid_loc_y) %>%
  #   mutate(switch = recode(grid_diff, 
  #                          "0" = "1",
  #                          .default = "0")) %>% select(-grid_diff,  -contains("phase"))
  # 
  
  
  # get proportion correct scores to data for switch/non-switch trials:
  correct <- aggregate(correctResponse ~ switch, data=df, FUN=mean)
  correct$correctResponse <- round(correct$correctResponse, digits = 3)
  
  # get proportion correct scores to data for congruent/non-congruent trials:
  congruent <- aggregate(correctResponse ~ congruent, data = df, FUN = mean)
  congruent$correctResponse <- round(congruent$correctResponse, digits= 3)
  
  #### which percent threshold for error to keep 
  if (any(correct$correctResponse[which(correct$switch == 1)] < 0.60)) {
    use <- FALSE
    #cat('too many errors\n')
  }
  
  #correct response output for switch vs.non-switch trials  
  correctOutput <- as.vector(unlist(correct$correctResponse))
  names(correctOutput) <- sprintf('switch_%s_prop.correct',correct$switch)
  
  congruentOutput <- as.vector(unlist(congruent$correctResponse))
  names(congruentOutput) <- sprintf('congruent_%s_prop.correct', congruent$congruent)
  
  # data frame for single trials 
  
  
  singledf <- df[c(4:12, 17:25 ),]
  
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
  switch_idx <- which(df$correctResponse == 1  & df$switch == TRUE & df$block == 3)
  if (length(switch_idx) > 0) {
    switch_RT <- mean(df$key_resp.rt[switch_idx], na.rm=TRUE)
  } else {switch_RT <- NA}
  
  switch_RT <- round(switch_RT, digits = 3)
  
  
  
  
  # get RTs for non-switch trials
  nonswitch_idx <- which(df$correctResponse == 1  & df$switch == FALSE & df$block == 3) 
  if (length(nonswitch_idx) > 0) {
    nonswitch_RT <- mean(df$key_resp.rt[nonswitch_idx], na.rm=TRUE)
  } else {nonswitch_RT <- NA}
  
  nonswitch_RT <- round(nonswitch_RT, digits = 3)
  
  
  #get RT for congurent trials
  # print(which(df$correctResponse == 1))
  # print(which(df$congruent == 1))
  congruent_idx <- which(df$correctResponse == 1  & df$congruent == 1 & df$block == 3) 
  # print(congruent_idx)
  if (length(congruent_idx) > 0) {
    congruent_RT <- mean(df$key_resp.rt[congruent_idx], na.rm=TRUE)
  } else {congruent_RT <- NA}
  
  congruent_RT <- round(congruent_RT, digits = 3)
  
  #get RT for non-congurent trials
  nonCongruent_idx <- which(df$correctResponse == 1  & df$congruent == 0 & df$block == 3) 
  if (length(nonCongruent_idx) > 0) {
    nonCongruent_RT <- mean(df$key_resp.rt[nonCongruent_idx], na.rm=TRUE)
  } else {nonCongruent_RT <- NA}
  
  nonCongruent_RT <- round(nonCongruent_RT, digits = 3)
  
  newRToutput <- c(singleBlock1_RT,singleBlock2_RT, switch_RT, nonswitch_RT, congruent_RT, nonCongruent_RT )
  names(newRToutput) <- sprintf(c('singleblock_1_RT','singleblock_2_RT','switch_RT', 'nonswitch_RT', 'congruent_RT', 'nonCongruent_RT' ))
  RToutput <- c(RToutput, newRToutput)
  
  
  # remove break lines:
  df <- df[which(!is.na(df$trials.thisRepN)),]
  
  
  # create named output vector
  output <- as.list(c(correctOutput, congruentOutput, correctOutputSingle,RToutput))
  if (!use) {
    output[1:length(output)] <- NA
  }
  
  
  output[['participant']]     <- thisparticipant
  output[['totaltime']]       <- thistotaltime
  output[['OS']]              <- thisOS
  output[['passedscreening']] <- use
  
  return(output)
  
}


