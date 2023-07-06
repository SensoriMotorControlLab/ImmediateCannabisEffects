# want these bits of info:

# age
# sex
# handedness
# cannabis use frequency
# use opiates
# no neurological conditions

# tasks are only in part 1

# age, sex and handedness, opiate use and neurological conditions are in demographics.csv

source('R/parseTaskData.R')
source('R/statistics.R')

getAsselData <- function() {
  
  demographics <- read.csv('data/2020/fall/demographics_1.csv', stringsAsFactors=F)
  columns <- c("What.is.your.age.",
               "Sex....Selected.Choice",
               "Sex....Other...Text",
               "What.is.your.handedness.",
               "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
               "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
               "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
               "id")
  demographics <- demographics[columns]
  names(demographics) <- c('age','sex','sex_descript','handedness','recreational_opiate_use','neurological_condition','neurological_condition_descript','id')
  
  # cannabis use:
  
  cannabis <- read.csv('data/2020/fall/cannabis_1.csv', stringsAsFactors = F)
  columns <- c("Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
               "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
               "id")
  cannabis <- cannabis[columns]
  names(cannabis) <- c('use_cannabis','cannabis_use_frequency','id')
  
  # are there some exclusion criteria in the generic session info?
  
  session <- read.csv('data/2020/fall/session_1.csv', stringsAsFactors = F)
  columns <- c("Start.Date",
               "Progress",
               "Finished",
               "Informed.Consent.Form",
               "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
               "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
               "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.",
               "id")
  session <- session[columns]
  names(session) <- c('start_date','progress','finished','informed_consent','need_corrective_device','wear_corrective_device','session_problems','id')
  
  # combine all the info:
  info <- cbind(demographics, cannabis, session)
  info <- info[!duplicated(names(info))]
  
  # select participants:
  info <- info[which(info$informed_consent == 'I agree to participate in this study'),]
  info <- info[which(info$wear_corrective_device != 'No'),]
  # 55 duplicate participants left... which ones have the right info?
  # let's remove unfinished ones (as there is missing info):
  # or do we remove all participants that have any duplicated info?
  info <- info[which(info$finished == 'True'),]
  # now there are 6 participants with duplicate info left... they finished it multiple times?
  
  # remove later instances or remove completely?
  
  # remove completely: (what are they DOING?!)
  double_info <- unique(info$id[which(duplicated(info$id))])
  info <- info[which(!(info$id %in% double_info)),]
  
  # remove people with neurological conditions, except migraine
  idx <- which(info$neurological_condition == 'No' | info$neurological_condition_descript == "headache and migraine ")
  info <- info[idx,]
  
  # remove those using opiates
  info <- info[which(info$recreational_opiate_use == "No"),]
  
  # assign two cannabis use groups:
  info$cannabis_group <- NA
  info$cannabis_group[which(info$use_cannabis == 'No')] <- 'non-user'
  info$cannabis_group[which(info$cannabis_use_frequency %in% c('Daily', 'A few times a week'))] <- 'frequent-user'
  
  # remove all the intermediate users:
  info <- info[which(info$cannabis_group %in% c('non-user', 'frequent-user')),]
  
  cat('GENERIC\n')
  print(table(info$cannabis_group))
  
  # now merge with task data...
  vs <- getGroupPerformance('2020', 'fall', 'visualsearch')
  vs <- vs[which(vs$passedscreening == TRUE),]
  gng <- getGroupPerformance('2020', 'fall', 'gonogo')
  gng <- gng[which(gng$passedscreening == TRUE),]
  nback <- getGroupPerformance('2020', 'fall', 'nback')
  nback <- nback[which(nback$passedscreening == TRUE),]
  
  vs    <- merge(info, vs,    by.x='id', by.y='participant', all=FALSE)
  gng   <- merge(info, gng,   by.x='id', by.y='participant', all=FALSE)
  nback <- merge(info, nback, by.x='id', by.y='participant', all=FALSE)
  
  write.csv(vs, 'data/2020/assel_visualsearch.csv', row.names=FALSE)
  write.csv(gng, 'data/2020/assel_gonogo.csv', row.names=FALSE)
  write.csv(nback, 'data/2020/assel_nback.csv', row.names=FALSE)
  
  cat('\nVISUAL SEARCH\n')
  print(table(vs$cannabis_group))
  cat('\nGO/NOGO\n')
  print(table(gng$cannabis_group))
  cat('\nN-BACK\n')
  print(table(nback$cannabis_group))
  
  return(list('visualsearch'=vs,
              'gonogo'=gng,
              'nback'=nback))
  
}


getAsselRegressionData <- function() {
  
  demographics <- read.csv('data/2020/fall/demographics_1.csv', stringsAsFactors=F)
  columns <- c("What.is.your.age.",
               "Sex....Selected.Choice",
               "Sex....Other...Text",
               "What.is.your.handedness.",
               "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
               "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
               "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
               "id")
  demographics <- demographics[columns]
  names(demographics) <- c('age','sex','sex_descript','handedness','recreational_opiate_use','neurological_condition','neurological_condition_descript','id')
  
  # cannabis use:
  
  cannabis <- read.csv('data/2020/fall/cannabis_1.csv', stringsAsFactors = F)
  columns <- c("Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
               "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
               "id")
  cannabis <- cannabis[columns]
  names(cannabis) <- c('use_cannabis','cannabis_use_frequency','id')
  
  # are there some exclusion criteria in the generic session info?
  
  session <- read.csv('data/2020/fall/session_1.csv', stringsAsFactors = F)
  columns <- c("Start.Date",
               "Progress",
               "Finished",
               "Informed.Consent.Form",
               "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
               "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
               "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.",
               "id")
  session <- session[columns]
  names(session) <- c('start_date','progress','finished','informed_consent','need_corrective_device','wear_corrective_device','session_problems','id')
  
  # combine all the info:
  info <- cbind(demographics, cannabis, session)
  info <- info[!duplicated(names(info))]
  
  # print(str(info))
  
  # select participants:
  info <- info[which(info$informed_consent == 'I agree to participate in this study'),]
  info <- info[which(info$wear_corrective_device != 'No'),]
  # 55 duplicate participants left... which ones have the right info?
  # let's remove unfinished ones (as there is missing info):
  # or do we remove all participants that have any duplicated info?
  info <- info[which(info$finished == 'True'),]
  # now there are 6 participants with duplicate info left... they finished it multiple times?
  
  # remove later instances or remove completely?
  
  # remove completely: (what are they DOING?!)
  double_info <- unique(info$id[which(duplicated(info$id))])
  info <- info[which(!(info$id %in% double_info)),]
  
  # remove people with neurological conditions, except migraine
  idx <- which(info$neurological_condition == 'No' | info$neurological_condition_descript == "headache and migraine ")
  info <- info[idx,]
  
  # remove those using opiates
  info <- info[which(info$recreational_opiate_use == "No"),]
  
  # assign two cannabis use groups:
  info$cannabis_freqnum <- 0
  
  freqlist <-    c('Not in the past 3 months',
                   'Once or twice within these past 3 months',
                   'Around once a month',
                   'A few times a month',
                   'Once a week',
                   'A few times a week',
                   'Daily')
  
  for (idx in c(1:length(freqlist))) {
    info$cannabis_freqnum[which(info$cannabis_use_frequency == freqlist[idx])] <- idx
  }
  
  # 7: daily
  # 6: a few times a week
  # 5: once a week
  # 4: a few times a month
  # 3: around once a month
  # 2: once or twice within these past 3 months
  # 1: not in the past 3 months
  # 0: [empty / never]
  
  
  
  # remove all the intermediate users:
  #info <- info[which(info$cannabis_group %in% c('non-user', 'frequent-user')),]
  
  cat('GENERIC\n')
  print(table(info$cannabis_freqnum))
  
  # now merge with task data...
  # vs <- getGroupPerformance('2020', 'fall', 'visualsearch')
  # vs <- vs[which(vs$passedscreening == TRUE),]
  # gng <- getGroupPerformance('2020', 'fall', 'gonogo')
  # gng <- gng[which(gng$passedscreening == TRUE),]
  nback <- getGroupPerformance('2020', 'fall', 'nback')
  nback <- nback[which(nback$passedscreening == TRUE),]
  
  # vs    <- merge(info, vs,    by.x='id', by.y='participant', all=FALSE)
  # gng   <- merge(info, gng,   by.x='id', by.y='participant', all=FALSE)
  nback <- merge(info, nback, by.x='id', by.y='participant', all=FALSE)
  
  # write.csv(vs, 'data/2020/assel_visualsearch.csv', row.names=FALSE)
  # write.csv(gng, 'data/2020/assel_gonogo.csv', row.names=FALSE)
  # write.csv(nback, 'data/2020/assel_nback.csv', row.names=FALSE)
  
  # cat('\nVISUAL SEARCH\n')
  # print(table(vs$cannabis_group))
  # cat('\nGO/NOGO\n')
  # print(table(gng$cannabis_group))
  cat('\nN-BACK\n')
  #print(table(nback$cannabis_group))
  
  return(list('nback'=nback))
  
}