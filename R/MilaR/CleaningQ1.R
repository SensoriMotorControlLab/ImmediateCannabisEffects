#### create data based on Assel's ####

# reading questionnaire_1
q1 <- read.csv('data/2020/fall/questionnaire_1.csv', stringsAsFactors=F)

# columns that will be used
columns <- c("Start.Date",
             "What.is.your.age.",
             "Sex....Selected.Choice",
             "Sex....Other...Text",
             "What.is.your.handedness.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "Start.Date",
             "Progress",
             "Finished",
             "Informed.Consent.Form",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.",
             "id")

# keeping only the columns that will be used
q1 <- q1[columns]

names(q1) <- c('date', 'age','sex','sex_descript','handedness','recreational_opiate_use','neurological_condition','neurological_condition_descript',
               'use_cannabis','cannabis_use_frequency','start_date','progress','finished','informed_consent','need_corrective_device','wear_corrective_device','session_problems','id')

# start obs = 463

# remove those who need to wear corrective devices to see screen and not wearing them now
q1 <- q1[which(q1$wear_corrective_device != 'No'),]

# keep those who have finished the session
q1 <- q1[which(q1$finished == 'True'),]

# remove duplicated 
## weed out one by one BY HAND
# q1[which(q1$id == "218395"),]
double_info <- unique(q1$id[which(duplicated(q1$id))])
#q1 <- q1 %>% slice(-174)
#q1 <- q1 %>% slice(-c(134, 136))
#q1 <- q1 %>% slice(-178)
#q1 <- q1 %>% slice(-172)
#q1 <- q1 %>% slice(-184)
#q1 <- q1 %>% slice(-c(251, 252)) #same id but different age and sex
#q1 <- q1 %>% slice(-161)

# check if there are still duplicates
#double_info <- unique(q1$id[which(duplicated(q1$id))]) # should be empty

# remove people with neurological conditions, except migraine
q1 <- subset(q1, (neurological_condition == "No" | neurological_condition_descript == "headache and migraine "))

# remove opiate users
q1 <- q1[which(q1$recreational_opiate_use == "No"),]
