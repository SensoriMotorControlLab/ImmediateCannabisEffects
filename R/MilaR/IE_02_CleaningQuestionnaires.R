#### create data ####

# in each questionnaire downloaded from qualtrics remove 1st and 3rd line -- to keep only one line of column names

q_full_1 <- read.csv('data/questionnaires/Canbyork-Fall2020-Part #1-FINAL_July 13, 2023_10.44.csv', stringsAsFactors=F)
q_full_2 <- read.csv('data/questionnaires/Canbyork-Fall2020-Part #2-FINAL_July 13, 2023_10.45.csv', stringsAsFactors=F)

# additional questionnaires (July 2023)

q_1 <- read.csv('data/questionnaires/Canbyork-Fall2021-Part 1_July 13, 2023_10.45.csv', stringsAsFactors=F)
q_2 <- read.csv('data/questionnaires/Canbyork-Fall2021-Part 2_July 13, 2023_10.47.csv', stringsAsFactors=F)
q_3 <- read.csv('data/questionnaires/Canbyork-SU2021-Part #1_July 13, 2023_10.48.csv', stringsAsFactors=F)
q_4 <- read.csv('data/questionnaires/Canbyork-SU2021-Part #2_July 13, 2023_10.50.csv', stringsAsFactors=F)
q_5 <- read.csv('data/questionnaires/Canbyork-Winter2022-Part 1_July 13, 2023_10.51.csv', stringsAsFactors=F)
q_6 <- read.csv('data/questionnaires/Canbyork-Winter2022-Part 2_July 13, 2023_10.52.csv', stringsAsFactors=F)

# reading questionnaire
q_ie_part1_fall <- read.csv('data/questionnaires/Immediate Cannabis Effects- Fall21-Part 1_July 13, 2023_10.59.csv', stringsAsFactors=F)
q_ie_part2_fall <- read.csv('data/questionnaires/Immediate Cannabis Effects- Fall21-Part 2_July 13, 2023_11.00.csv', stringsAsFactors=F)
q_ie_part1_winter <- read.csv('data/questionnaires/Immediate Cannabis Effects- Winter22-Part 1_July 13, 2023_12.08.csv', stringsAsFactors=F)
q_ie_part2_winter <- read.csv('data/questionnaires/Immediate Cannabis Effects- Winter22-Part 2_July 13, 2023_12.10.csv', stringsAsFactors=F)

# part 3 and 4
q_ie_part3_fall <- read.csv('data/questionnaires/Immediate Cannabis Effects- Part 3_July 13, 2023_11.01.csv', stringsAsFactors=F)
q_ie_part4_fall <- read.csv('data/questionnaires/Immediate Cannabis Effects- Part 4_July 13, 2023_12.07.csv', stringsAsFactors=F)
q_ie_part3_winter <- read.csv('data/questionnaires/Immediate Cannabis Effects- Winter22-Part 3_July 13, 2023_12.11.csv', stringsAsFactors=F)
q_ie_part4_winter <- read.csv('data/questionnaires/Immediate Cannabis Effects- Winter22-Part 4_July 13, 2023_12.11.csv', stringsAsFactors=F)

# additional ie (July 2023)

q_ie_part1_summer <- read.csv('data/questionnaires/Immediate Cannabis Effects - Summer 23 - Part 1_July 13, 2023_10.55.csv', stringsAsFactors=F)
q_ie_part2_summer <- read.csv('data/questionnaires/Immediate Cannabis Effects - Summer23 - Part 2_July 13, 2023_10.56.csv', stringsAsFactors=F)
q_ie_part3_summer <- read.csv('data/questionnaires/Immediate Cannabis Effects - Summer23 - Part 3_July 13, 2023_10.57.csv', stringsAsFactors=F)
q_ie_part4_summer <- read.csv('data/questionnaires/Immediate Cannabis Effects - Summer23 - Part 4_July 13, 2023_10.58.csv', stringsAsFactors=F)


#### create codebooks ####

# list of data frames
df_list_q <- list(q_ie_part1_fall,
                q_ie_part2_fall,
                q_ie_part1_winter,
                q_ie_part2_winter,
                q_ie_part3_fall,
                q_ie_part4_fall,
                q_ie_part3_winter,
                q_ie_part4_winter,
                q_full_1,
                q_full_2,
                q_1, q_2, q_3,
                q_4, q_5, q_6,
                q_ie_part1_summer,
                q_ie_part2_summer,
                q_ie_part3_summer,
                q_ie_part4_summer)

df_names <- c("q_ie_part1_fall",
             "q_ie_part2_fall",
             "q_ie_part1_winter",
             "q_ie_part2_winter",
             "q_ie_part3_fall",
             "q_ie_part4_fall",
             "q_ie_part3_winter",
             "q_ie_part4_winter",
             "q_full_1",
             "q_full_2",
             "q_1", "q_2", "q_3",
             "q_4", "q_5", "q_6",
             "q_ie_part1_summer",
             "q_ie_part2_summer",
             "q_ie_part3_summer",
             "q_ie_part4_summer")

# iterate over data frames
for (i in seq_along(df_list_q)) {
# create data frame with column names, numbers, and values
  col_info <- data.frame(
    num = 1:ncol(df_list_q[[i]]),
    name = names(df_list_q[[i]]),
    values = sapply(df_list_q[[i]], function(x) paste(unique(x), collapse = ", "))
  )

# write to csv file
  write.csv(col_info, file.path("data", "codebook", paste0(df_names[i], "_codebook.csv")), row.names = FALSE)
}

#### change the dates ####

# dates end
q_full_1$enddate <- as.POSIXct(q_full_1$End.Date, format = "%Y-%m-%d %H:%M")
q_full_2$enddate <- as.POSIXct(q_full_2$End.Date, format = "%Y-%m-%d %H:%M")

# ie part 1 and 2
q_ie_part1_fall$enddate <- as.POSIXct(q_ie_part1_fall$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part2_fall$enddate <- as.POSIXct(q_ie_part2_fall$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part1_winter$enddate <- as.POSIXct(q_ie_part1_winter$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part2_winter$enddate <- as.POSIXct(q_ie_part2_winter$End.Date, format = "%Y-%m-%d %H:%M")

# part 3 and 4
q_ie_part3_fall$enddate <- as.POSIXct(q_ie_part3_fall$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part4_fall$enddate <- as.POSIXct(q_ie_part4_fall$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part3_winter$enddate <- as.POSIXct(q_ie_part3_winter$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part4_winter$enddate <- as.POSIXct(q_ie_part4_winter$End.Date, format = "%Y-%m-%d %H:%M")

# dates start
q_full_1$startdate <- as.POSIXct(q_full_1$Start.Date, format = "%Y-%m-%d %H:%M")
q_full_2$startdate <- as.POSIXct(q_full_2$Start.Date, format = "%Y-%m-%d %H:%M")

# ie part 1 and 2
q_ie_part1_fall$startdate <- as.POSIXct(q_ie_part1_fall$Start.Date, format = "%Y-%m-%d %H:%M")
q_ie_part2_fall$startdate <- as.POSIXct(q_ie_part2_fall$Start.Date, format = "%Y-%m-%d %H:%M")
q_ie_part1_winter$startdate <- as.POSIXct(q_ie_part1_winter$Start.Date, format = "%Y-%m-%d %H:%M")
q_ie_part2_winter$startdate <- as.POSIXct(q_ie_part2_winter$Start.Date, format = "%Y-%m-%d %H:%M")

# part 3 and 4
q_ie_part3_fall$startdate <- as.POSIXct(q_ie_part3_fall$Start.Date, format = "%Y-%m-%d %H:%M")
q_ie_part4_fall$startdate <- as.POSIXct(q_ie_part4_fall$Start.Date, format = "%Y-%m-%d %H:%M")
q_ie_part3_winter$startdate <- as.POSIXct(q_ie_part3_winter$Start.Date, format = "%Y-%m-%d %H:%M")
q_ie_part4_winter$startdate <- as.POSIXct(q_ie_part4_winter$Start.Date, format = "%Y-%m-%d %H:%M")

# additional ones (July 2023)
q_1$enddate <- as.POSIXct(q_1$End.Date, format = "%Y-%m-%d %H:%M")
q_1$startdate <- as.POSIXct(q_1$Start.Date, format = "%Y-%m-%d %H:%M")

q_2$enddate <- as.POSIXct(q_2$End.Date, format = "%Y-%m-%d %H:%M")
q_2$startdate <- as.POSIXct(q_2$Start.Date, format = "%Y-%m-%d %H:%M")

q_3$enddate <- as.POSIXct(q_3$End.Date, format = "%Y-%m-%d %H:%M")
q_3$startdate <- as.POSIXct(q_3$Start.Date, format = "%Y-%m-%d %H:%M")

q_4$enddate <- as.POSIXct(q_4$End.Date, format = "%Y-%m-%d %H:%M")
q_4$startdate <- as.POSIXct(q_4$Start.Date, format = "%Y-%m-%d %H:%M")

q_5$enddate <- as.POSIXct(q_5$End.Date, format = "%Y-%m-%d %H:%M")
q_5$startdate <- as.POSIXct(q_5$Start.Date, format = "%Y-%m-%d %H:%M")

q_6$enddate <- as.POSIXct(q_6$End.Date, format = "%Y-%m-%d %H:%M")
q_6$startdate <- as.POSIXct(q_6$Start.Date, format = "%Y-%m-%d %H:%M")

q_ie_part1_summer$enddate <- as.POSIXct(q_ie_part1_summer$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part1_summer$startdate <- as.POSIXct(q_ie_part1_summer$Start.Date, format = "%Y-%m-%d %H:%M")

q_ie_part2_summer$enddate <- as.POSIXct(q_ie_part2_summer$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part2_summer$startdate <- as.POSIXct(q_ie_part2_summer$Start.Date, format = "%Y-%m-%d %H:%M")

q_ie_part3_summer$enddate <- as.POSIXct(q_ie_part3_summer$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part3_summer$startdate <- as.POSIXct(q_ie_part3_summer$Start.Date, format = "%Y-%m-%d %H:%M")

q_ie_part4_summer$enddate <- as.POSIXct(q_ie_part4_summer$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part4_summer$startdate <- as.POSIXct(q_ie_part4_summer$Start.Date, format = "%Y-%m-%d %H:%M")

#q_full_1$date_date <- as.Date(q_full_1$date)
#q_full_2$date_date <- as.Date(q_full_2$date)


#q_ie_part1_fall$date_date <- as.Date(q_ie_part1_fall$date)
#q_ie_part2_fall$date_date <- as.Date(q_ie_part2_fall$date)
#q_ie_part1_winter$date_date <- as.Date(q_ie_part1_winter$date)
#q_ie_part2_winter$date_date <- as.Date(q_ie_part2_winter$date)

# part 3 and 4
#q_ie_part3_fall$date_date <- as.Date(q_ie_part3_fall$date)
#q_ie_part4_fall$date_date <- as.Date(q_ie_part4_fall$date)
#q_ie_part3_winter$date_date <- as.Date(q_ie_part3_winter$date)
#q_ie_part4_winter$date_date <- as.Date(q_ie_part4_winter$date)

#### clean each of the questionnaires dataframes ####

#### full part 1 ####

#q_full_1

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Sex....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "If.possible..describe.the.dose..THC.mg..or.number.of.hits.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_full_1 <- q_full_1[columns]

names(q_full_1) <- c("id",
                     "startdate",
                     "enddate",
                     "finished",
                     "informed_consent",
                     "age",
                     "sex",
                     "neurological_conditions",
                     "neurological_condition_description",
                     "handedness",
                     "glasses_contacts",
                     "wearing_glasses_now",
                     "physically_activity",
                     "stressed",
                     "opiates",
                     "video_games",
                     "used",
                     "use_frequency",
                     "use_last",
                     "use_dose",
                     "sleep_last",
                     "concussion",
                     "music",
                     "problems")

q_full_1$sample <- "control"


#### full part 2 ####

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "If.possible..describe.the.dose..THC.mg..or.number.of.hits.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_full_2 <- q_full_2[columns]

names(q_full_2) <- c("id",
                     "startdate",
                     "enddate",
                     "finished",
                     "informed_consent",
                     "glasses_contacts",
                     "wearing_glasses_now",
                     "stressed",
                     "opiates",
                     "used",
                     "use_frequency",
                     "use_last",
                     "use_dose",
                     "sleep_last",
                     "concussion",
                     "problems")

q_full_2$sample <- "control"

#### q_ie_part1_fall ####

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "What.sex.were.you.assigned.at.birth.",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Selected.Choice",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Other...Text",
             "What.best.describes.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part1_fall <- q_ie_part1_fall[columns]

names(q_ie_part1_fall) <- c("id",
                            "startdate",
                            "enddate",
                            "finished",
                            "informed_consent",
                            "age",
                            "sex",
                            "ancestry",
                            "ancestry_other",
                            "education",
                            "neurological_conditions",
                            "neurological_condition_description",
                            "handedness",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "physically_activity",
                            "stressed",
                            "opiates",
                            "video_games",
                            "used",
                            "use_frequency",
                            "use_last",
                            "sleep_last",
                            "concussion",
                            "music",
                            "problems")

q_ie_part1_fall$id <- as.character(q_ie_part1_fall$id)

q_ie_part1_fall$sample <- "control"

#### q_ie_part1_winter ####

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "What.sex.were.you.assigned.at.birth.",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Selected.Choice",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Other...Text",
             "What.best.describes.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part1_winter <- q_ie_part1_winter[columns]

names(q_ie_part1_winter) <- c("id",
                              "startdate",
                              "enddate",
                              "finished",
                              "informed_consent",
                              "age",
                              "sex",
                              "ancestry",
                              "ancestry_other",
                              "education",
                              "neurological_conditions",
                              "neurological_condition_choice",
                              "neurological_condition_description",
                              "handedness",
                              "glasses_contacts",
                              "wearing_glasses_now",
                              "physically_activity",
                              "stressed",
                              "opiates",
                              "video_games",
                              "used",
                              "use_frequency",
                              "use_last",
                              "concussion",
                              "music",
                              "problems")

q_ie_part1_winter$id <- as.character(q_ie_part1_winter$id)

q_ie_part1_winter$sample <- "control"

#### q_ie_part2_fall ####

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part2_fall <- q_ie_part2_fall[columns]

names(q_ie_part2_fall) <- c("id",
                            "startdate",
                            "enddate",
                            "finished",
                            "informed_consent",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "used",
                            "use_frequency",
                            "use_last",
                            "sleep_last",
                            "concussion",
                            "problems")

q_ie_part2_fall$id <- as.character(q_ie_part2_fall$id)

q_ie_part2_fall$sample <- "control"

#### q_ie_part2_winter ####

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part2_winter <- q_ie_part2_winter[columns]

names(q_ie_part2_winter) <- c("id",
                              "startdate",
                              "enddate",
                              "finished",
                              "informed_consent",
                              "glasses_contacts",
                              "wearing_glasses_now",
                              "stressed",
                              "opiates",
                              "used",
                              "use_frequency",
                              "use_last",
                              "concussion",
                              "problems")

q_ie_part2_winter$id <- as.character(q_ie_part2_winter$id)

q_ie_part2_winter$sample <- "control"

#### q_ie_part3_fall ####

names(q_ie_part3_fall)[18] <- "Consent"
names(q_ie_part3_fall)[173] <- "Consent2"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Consent",
             "Consent2",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..please.specify..if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "When.was.the.last.time.you.used.marijuana.",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.1",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.2",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High.....100...Highest.I.ve.ever.been.on.Marijuana",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part3_fall <- q_ie_part3_fall[columns]

names(q_ie_part3_fall) <- c("id",
                            "startdate",
                            "enddate",
                            "finished",
                            "informed_consent",
                            "informed_consent2",
                            "education",
                            "neurological_conditions",
                            "neurological_condition_description",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "sleep_last",
                            "concussion",
                            "use_last",
                            "how_high1",
                            "how_high2",
                            "how_high3",
                            "how_high4",
                            "problems")

q_ie_part3_fall$sample <- "experimental"

#### q_ie_part3_winter ####

names(q_ie_part3_winter)[18] <- "Consent"
names(q_ie_part3_winter)[189] <- "Consent2"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Consent",
             "Consent2",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.experienced.a.concussion.before.",
             "When.was.the.last.time.you.used.marijuana.",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.1",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.2",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High.....100...Highest.I.ve.ever.been.on.Marijuana",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part3_winter <- q_ie_part3_winter[columns]

names(q_ie_part3_winter) <- c("id",
                              "startdate",
                              "enddate",
                              "finished",
                              "informed_consent",
                              "informed_consent2",
                              "education",
                              "neurological_conditions",
                              "neurological_condition_choice",
                              "neurological_condition_description",
                              "glasses_contacts",
                              "wearing_glasses_now",
                              "stressed",
                              "opiates",
                              "concussion",
                              "use_last",
                              "how_high1",
                              "how_high2",
                              "how_high3",
                              "how_high4",
                              "problems")

q_ie_part3_winter$sample <- "experimental"

#### q_ie_part4_fall ####

names(q_ie_part4_fall)[18] <- "Consent"
names(q_ie_part4_fall)[172] <- "Consent2"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Consent",
             "Consent2",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..please.specify..if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "When.was.the.last.time.you.used.marijuana.",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.1",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.2",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.high.....100...Highest.I.ve.ever.been.on.Marijuana",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part4_fall <- q_ie_part4_fall[columns]

names(q_ie_part4_fall) <- c("id",
                            "startdate",
                            "enddate",
                            "finished",
                            "informed_consent",
                            "informed_consent2",
                            "education",
                            "neurological_conditions",
                            "neurological_condition_description",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "sleep_last",
                            "concussion",
                            "use_last",
                            "how_high1",
                            "how_high2",
                            "how_high3",
                            "how_high4",
                            "problems")

q_ie_part4_fall$sample <- "experimental"

#### q_ie_part4_winter ####

names(q_ie_part4_winter)[18] <- "Consent"
names(q_ie_part4_winter)[186] <- "Consent2"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Consent",
             "Consent2",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.experienced.a.concussion.before.",
             "When.was.the.last.time.you.used.marijuana.",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.1",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.2",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.high.....100...Highest.I.ve.ever.been.on.Marijuana",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part4_winter <- q_ie_part4_winter[columns]

names(q_ie_part4_winter) <- c("id",
                              "startdate",
                              "enddate",
                              "finished",
                              "informed_consent",
                              "informed_consent2",
                              "education",
                              "neurological_conditions",
                              "neurological_condition_choice",
                              "neurological_condition_description",
                              "glasses_contacts",
                              "wearing_glasses_now",
                              "stressed",
                              "opiates",
                              "concussion",
                              "use_last",
                              "how_high1",
                              "how_high2",
                              "how_high3",
                              "how_high4",
                              "problems")

q_ie_part4_winter$id <- as.character(q_ie_part4_winter$id)

q_ie_part4_winter$sample <- "experimental"

## additional questionnaires (July 2023)

#### q_1 ####

names(q_1)[18] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "What.sex.were.you.assigned.at.birth.",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Selected.Choice",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Other...Text",
             "What.best.describes.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "How.many.hours.of.sleep.did.you.get.last.night.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_1 <- q_1[columns]

names(q_1) <- c("id",
                            "startdate",
                            "enddate",
                            "finished",
                            "informed_consent",
                            "age",
                            "sex",
                            "ancestry",
                            "ancestry_other",
                            "education",
                            "neurological_conditions",
                            "neurological_condition_description",
                            "handedness",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "physically_activity",
                            "stressed",
                            "opiates",
                            "video_games",
                            "used",
                            "use_frequency",
                            "use_last",
                            "sleep_last",
                            "concussion",
                            "music",
                            "problems")

q_1$id <- as.character(q_1$id)

q_1$sample <- "control"

#### q_2 ####

names(q_2)[18] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "How.many.hours.of.sleep.did.you.get.last.night.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_2 <- q_2[columns]

names(q_2) <- c("id",
                            "startdate",
                            "enddate",
                            "finished",
                            "informed_consent",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "used",
                            "use_frequency",
                            "use_last",
                            "sleep_last",
                            "concussion",
                            "problems")

q_2$id <- as.character(q_2$id)

q_2$sample <- "control"

#### q_3 ####

names(q_3)[18] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "What.sex.were.you.assigned.at.birth.",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Selected.Choice",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Other...Text",
             "What.best.describes.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "How.many.hours.of.sleep.did.you.get.last.night.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_3 <- q_3[columns]

names(q_3) <- c("id",
                "startdate",
                "enddate",
                "finished",
                "informed_consent",
                "age",
                "sex",
                "ancestry",
                "ancestry_other",
                "education",
                "neurological_conditions",
                "neurological_condition_description",
                "handedness",
                "glasses_contacts",
                "wearing_glasses_now",
                "physically_activity",
                "stressed",
                "opiates",
                "video_games",
                "used",
                "use_frequency",
                "use_last",
                "sleep_last",
                "concussion",
                "music",
                "problems")

q_3$id <- as.character(q_3$id)

q_3$sample <- "control"

#### q_4 ####

names(q_4)[18] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_4 <- q_4[columns]

names(q_4) <- c("id",
                "startdate",
                "enddate",
                "finished",
                "informed_consent",
                "glasses_contacts",
                "wearing_glasses_now",
                "stressed",
                "opiates",
                "used",
                "use_frequency",
                "use_last",
                "sleep_last",
                "concussion",
                "problems")

q_4$id <- as.character(q_4$id)

q_4$sample <- "control"

#### q_5 ####

names(q_5)[18] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "What.sex.were.you.assigned.at.birth.",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Selected.Choice",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Other...Text",
             "What.best.describes.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "SleepDuration",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_5 <- q_5[columns]

names(q_5) <- c("id",
                "startdate",
                "enddate",
                "finished",
                "informed_consent",
                "age",
                "sex",
                "ancestry",
                "ancestry_other",
                "education",
                "neurological_conditions",
                "neurological_condition_choice",
                "neurological_condition_description",
                "handedness",
                "glasses_contacts",
                "wearing_glasses_now",
                "physically_activity",
                "stressed",
                "opiates",
                "video_games",
                "used",
                "use_frequency",
                "use_last",
                "sleep_last",
                "concussion",
                "music",
                "problems")

q_5$id <- as.character(q_5$id)

q_5$sample <- "control"

#### q_6 ####

names(q_6)[18] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "SleepDuration",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_6 <- q_6[columns]

names(q_6) <- c("id",
                "startdate",
                "enddate",
                "finished",
                "informed_consent",
                "glasses_contacts",
                "wearing_glasses_now",
                "stressed",
                "opiates",
                "used",
                "use_frequency",
                "use_last",
                "sleep_last",
                "concussion",
                "problems")

q_6$id <- as.character(q_6$id)

q_6$sample <- "control"

#### q_ie_part1_summer ####

names(q_ie_part1_summer)[123] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "What.sex.were.you.assigned.at.birth.",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Selected.Choice",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Other...Text",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "SleepDuration",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part1_summer <- q_ie_part1_summer[columns]

names(q_ie_part1_summer) <- c("id",
                            "startdate",
                            "enddate",
                            "finished",
                            "informed_consent",
                            "age",
                            "sex",
                            "ancestry",
                            "ancestry_other",
                            "education",
                            "neurological_conditions",
                            "neurological_condition_choice",
                            "neurological_condition_description",
                            "handedness",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "used",
                            "use_frequency",
                            "use_last",
                            "sleep_last",
                            "concussion",
                            "problems")

q_ie_part1_summer$id <- as.character(q_ie_part1_summer$id)

q_ie_part1_summer$sample <- "experimental"

#### q_ie_part2_summer ####

names(q_ie_part2_summer)[105] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "SleepDuration",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part2_summer <- q_ie_part2_summer[columns]

names(q_ie_part2_summer) <- c("id",
                              "startdate",
                              "enddate",
                              "finished",
                              "informed_consent",
                              "neurological_conditions",
                              "neurological_condition_choice",
                              "neurological_condition_description",
                              "glasses_contacts",
                              "wearing_glasses_now",
                              "stressed",
                              "opiates",
                              "used",
                              "use_frequency",
                              "use_last",
                              "sleep_last",
                              "concussion",
                              "problems")

q_ie_part2_summer$id <- as.character(q_ie_part2_summer$id)

q_ie_part2_summer$sample <- "experimental"

#### q_ie_part3_summer ####

names(q_ie_part3_summer)[18] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "SleepDuration",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part3_summer <- q_ie_part3_summer[columns]

names(q_ie_part3_summer) <- c("id",
                              "startdate",
                              "enddate",
                              "finished",
                              "informed_consent",
                              "neurological_conditions",
                              "neurological_condition_choice",
                              "neurological_condition_description",
                              "glasses_contacts",
                              "wearing_glasses_now",
                              "physically_activity",
                              "stressed",
                              "opiates",
                              "video_games",
                              "used",
                              "use_frequency",
                              "use_last",
                              "sleep_last",
                              "concussion",
                              "music",
                              "problems")

q_ie_part3_summer$id <- as.character(q_ie_part3_summer$id)

q_ie_part3_summer$sample <- "control"

#### q_ie_part4_summer ####

names(q_ie_part4_summer)[18] <- "Informed.Consent"

columns <- c("id",
             "startdate",
             "enddate",
             "Finished",
             "Informed.Consent",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "When.was.the.last.time.you.used.marijuana.",
             "SleepDuration",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part4_summer <- q_ie_part4_summer[columns]

names(q_ie_part4_summer) <- c("id",
                              "startdate",
                              "enddate",
                              "finished",
                              "informed_consent",
                              "neurological_conditions",
                              "neurological_condition_choice",
                              "neurological_condition_description",
                              "glasses_contacts",
                              "wearing_glasses_now",
                              "stressed",
                              "opiates",
                              "used",
                              "use_frequency",
                              "use_last",
                              "sleep_last",
                              "concussion",
                              "problems")

q_ie_part4_summer$id <- as.character(q_ie_part4_summer$id)

q_ie_part4_summer$sample <- "control"

#### combine all questionnaires into one ####

df_combined <- bind_rows(q_full_1, q_full_2)

#do those separately
df_combined <- bind_rows(df_combined, q_ie_part1_fall)
df_combined <- bind_rows(df_combined, q_ie_part1_winter)
df_combined <- bind_rows(df_combined, q_ie_part2_fall)
df_combined <- bind_rows(df_combined, q_ie_part2_winter)
df_combined <- bind_rows(df_combined, q_ie_part3_fall)
df_combined <- bind_rows(df_combined, q_ie_part3_winter)
df_combined <- bind_rows(df_combined, q_ie_part4_fall)
df_combined <- bind_rows(df_combined, q_ie_part4_winter)

## additional (July 2023)
df_combined <- bind_rows(df_combined, q_1)
df_combined <- bind_rows(df_combined, q_2)
df_combined <- bind_rows(df_combined, q_3)
df_combined <- bind_rows(df_combined, q_4)

# sleep_last changes, so we need to change it to character, just in case
df_combined$sleep_last <- as.character(df_combined$sleep_last)

df_combined <- bind_rows(df_combined, q_5)
df_combined <- bind_rows(df_combined, q_6)

df_combined <- bind_rows(df_combined, q_ie_part1_summer)
df_combined <- bind_rows(df_combined, q_ie_part2_summer)
df_combined <- bind_rows(df_combined, q_ie_part3_summer)
df_combined <- bind_rows(df_combined, q_ie_part4_summer)

#### subset each ####

#df_combined <- df_combined[!duplicated(df_combined, fromLast = TRUE), ]
df_combined$sdate <- as.Date(df_combined$startdate)
df_combined$edate <- as.Date(df_combined$enddate)

#df_combined <- df_combined[df_combined$finished == "TRUE", ]


#### filling in the unknowns by id ####

## NAs instead of ""
columns_to_process <- c("sex", "neurological_conditions", "neurological_condition_description", 
                        "handedness", "glasses_contacts", "wearing_glasses_now", 
                        "physically_activity", "opiates", "video_games", "used",
                        "use_frequency", "concussion", "music")

# Replace empty strings with NA for each specified column
df_combined[columns_to_process] <- lapply(df_combined[columns_to_process], function(x) ifelse(x == "", NA, x))


# fill in the some variables by downup
df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(sex, stressed, age,
       neurological_conditions, neurological_condition_description,
       handedness, glasses_contacts, wearing_glasses_now, 
       physically_activity, opiates, video_games, used, 
       use_frequency, use_last, use_dose,
       concussion, music, .direction = "downup")

df_combined <- df_combined[order(df_combined$id), ]

df_combined <- df_combined[df_combined$id != "", ]
