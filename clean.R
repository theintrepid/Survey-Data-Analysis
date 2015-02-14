#Step 1. Read the data
data <- read.csv("data_day4.csv") #type filepath

#Step 2. Load this function by running the whole chunk up to "}" ONCE
clean <- function(df, part2=T, binary.code=F) {
        names(df)[names(df)=="X_Answer.ID"] <- "ID" 
        
        names(df)[names(df)=="X3..Gender"] <- "gender"
        
        names(df)[names(df)=="X4..How.old.are.you."] <- "age"
        
        names(df)[names(df)=="X5..Ethnicity"] <- "ethnicity"
        
        names(df)[names(df)=="X6..What.language.do.you.use.most.often."] <- "primary.language"
        
        names(df)[names(df)=="X7..Are.you.currently.working.full.time."] <- "employed"
        names(df)[names(df)=="X8..What.industry.do.you.work.in."] <- "occupation"
        
        #X9..What.is.your.highest.level.of.education. is a wrapper
        names(df)[names(df)=="edu.level"] <- "edu.level"
        df[,"edu.level"] <- factor(df[,"edu.level"],labels=c("No qualification","Primary","Secondary","Upper Secondary","Tertiary","University"),ordered=T)
        
        #---------------------SES---------------------
        #X10..Which.of.the.following.best.describes.your.total...household...monthly.income. is a wrapper
        names(df)[names(df)=="mth.income"]
        df[,"mth.income"] <- factor(df[,"mth.income"],labels=c("No income","Less than 3000","3000-5000","5000-8000","8000-12000","12000 and above","Refused"),ordered=T)
        
        #X11..Type.of.residence is a wrapper
        names(df)[names(df)=="residence.type"]
        df[,"residence.type"] <- factor(df[,"residence.type"],labels=c("Rental","1 or 2 Room Flat","3 Room Flat","4 Room Flat","5 Room Flat or above"),ordered=T)
        
        #-----------------Smoking status--------------
        names(df)[names(df)=="X12..DURING.THE.HAZE.PERIOD..did.you.smoke.tobacco.on.a.daily.basis..less.than.daily.or.not.at.all."] <- "current.smoke"
        df[,"current.smoke"] <- factor(df[,"current.smoke"],ordered=T)
        
        names(df)[names(df)=="X13..BEFORE.THE.2013.HAZE.PERIOD..have.you.smoked.tobacco.on.a.daily.basis..less.than.daily..or.not.at.all."] <- "past.smoke"
        df[,"past.smoke"] <- factor(df[,"past.smoke"],ordered=T)
        
        #------------------Chronic disease-------------
        names(df)[names(df)=="X14..DURING.THE.HAZE.OF.2013..WERE.YOU.suffering.from.Asthma..Chronic.Lung.Disease..COPD..or.Heart.Disease."] <- "chronic.disease"
        
        binary <- function(vector) {
                vector <- as.numeric(vector)
                output <- sapply(vector,FUN= function(x) {
                        if (is.na(x)==T) {
                                return(NA)
                        }
                        if (x %in% c(4,5)) {
                                output <- "Yes"
                        }
                        else if (x %in% c(1,2,3)) {
                                output <- "No"
                        }
                },simplify=T)
                output <- unlist(output)
                factor(output)
        }
        
        #-----------------Perceptions----------------
        #X15..Please.answer.the.following.question. is a wrapper
        names(df)[names(df)=="How.severe.is.haze.as.a.problem.in.Singapore."] <- "haze.severe"
        df[,"haze.severe"] <- factor(df[,"haze.severe"],labels=c("Not severe at all","Somewhat not severe","Neutral","Somewhat severe","Very severe"))
        
        #X16..The.following.are.some.questions.about.YOUR.OPINIONS.about.haze..They.may.be.different.for.different.people.and.there.are.no.correct.or.wrong.answers..For.each.question..please.indicate.if.you..Strongly.Agree....Agree....Neither.agree.nor.disagree....Disagree..or..Strongly.Disagree.. is a wrapper
        names(df)[names(df)=="I.believe.haze.has.a.damaging.effect.on.my.health"] <- "r1"
        df[,"r1"] <- factor(df[,"r1"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="I.am.at.risk.of.lung.disease.from.haze"] <- "r2"
        df[,"r2"] <- factor(df[,"r2"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="I.am.at.risk.of.heart.disease.from.haze"] <- "r3"
        df[,"r3"] <- factor(df[,"r3"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="I.am.at.risk.of.eye.disease.from.haze"] <- "r4"
        df[,"r4"] <- factor(df[,"r4"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="I.need.to.take.protective.measure.for.myself.during.haze"] <- "r5"
        df[,"r5"] <- factor(df[,"r5"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        #------------------Practices------------------
        #X17..The.following.are.questions.about.ACTIONS.YOU.ACTUALLY.TOOK.DURING.THE.2013.HAZE..For.each.action.please.indicate.whether.you.took.them..Not.at.all....Less.than.weekly....Weekly....Once.every.few.days....Everyday...Please.answer..Not.at.all..if.you.only.wanted.to.take.them.but.did.not.actually.take.them is a wrapper
        
        names(df)[names(df)=="I.sought.updates.about.the.severity.of.haze..for.example..on.the.news..internet.or.radio."] <- "p1"
        df[,"p1"] <- factor(df[,"p1"],labels=c("Not at all","Less than weekly","Weekly","Once every few days","Almost daily"),ordered=T)
        
        names(df)[names(df)=="I.wore.an.N95.mask"] <- "p2"
        df[,"p2"] <- factor(df[,"p2"],labels=c("Not at all","Less than weekly","Weekly","Once every few days","Almost daily"),ordered=T)
        
        names(df)[names(df)=="I.stayed.indoors.and.avoided.outdoor.activites"] <- "p3"
        df[,"p3"] <- factor(df[,"p3"],labels=c("Not at all","Less than weekly","Weekly","Once every few days","Almost daily"),ordered=T)
        
        names(df)[names(df)=="I.cleaned.my.house.more.frequently.than.usual"] <- "p4"
        df[,"p4"] <- factor(df[,"p4"],labels=c("Not at all","Less than weekly","Weekly","Once every few days","Almost daily"),ordered=T)
        
        names(df)[names(df)=="I.used.an.air.purifier.at.home"] <- "p5"
        df[,"p5"] <- factor(df[,"p5"],labels=c("Not at all","Less than weekly","Weekly","Once every few days","Almost daily"),ordered=T)
        
        names(df)[names(df)=="I.took.showers.more.frequently.than.usual"] <- "p6"
        df[,"p6"] <- factor(df[,"p6"])
        #df[,"p6"] <- factor(df[,"p6"],labels=c("Not at all","Less than weekly","Weekly","Once every few days","Almost daily"),ordered=T)
        
        names(df)[names(df)=="I.kept.myself.hydrated.more.than.usual"] <- "p7"
        df[,"p7"] <- factor(df[,"p7"])
        #df[,"p7"] <- factor(df[,"p7"],labels=c("Not at all","Less than weekly","Weekly","Once every few days","Almost daily"),ordered=T)
        
        #-------------------Knowledge-------------------
        #X18..The.following.are.some.facts.about.haze..they.could.be.correct.or.incorrect..For.each.question..please.indicate.if.you..Strongly.Agree....Agree....Neither.agree.nor.disagree....Disagree....Strongly.Disagree.. is a wrapper
        
        names(df)[names(df)=="Haze.is.caused.by.forest.fires.in.neighboring.countries"] <- "k1"
        df[,"k1"] <- factor(df[,"k1"])
        #df[,"k1"] <- factor(df[,"k1"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="PSI..Pollutant.Standard.Index..can.measure.the.severity.of.haze"] <- "k2"
        df[,"k2"] <- factor(df[,"k2"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="The.elderly.and.children.do.not.have.a.higher.risk.of.harm.during.haze"] <- "k3" #reverse coding
        reverse <- function(x) {
                x <- as.numeric(factor(x))
                x <- 1/x 
        }
        df[,"k3"] <- reverse(df[,"k3"])
        df[,"k3"] <- factor(df[,"k3"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="N95.masks.can.be.used.for.children"] <- "k4" #reverse coding
        df[,"k4"] <- reverse(df[,"k4"])
        df[,"k4"] <- factor(df[,"k4"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="Health.effects.of.haze.depends.on.how.long.one.has.been.exposed.to.it"] <- "k5"
        df[,"k5"] <- factor(df[,"k5"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="Wearing.more.than.one.N95.mask.at.the.same.time.offers.better.protection"] <- "k6" #reverse coding
        df[,"k6"] <- reverse(df[,"k6"])
        df[,"k6"] <- factor(df[,"k6"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="The.main.pollutant.during.haze.is.particulate.matter..for.example.PM10.and.PM2.5."] <- "k7"
        df[,"k7"] <- factor(df[,"k7"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="Individuals.who.spend.a.lot.time.outdoors.need.to.be.protected"] <- "k8"
        df[,"k8"] <- factor(df[,"k8"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="N95.masks.can.be.reused.as.long.as.they.are.not.dirtied.or.damaged"] <- "k9" #reverse coding
        df[,"k9"] <- reverse(df[,"k9"])
        df[,"k9"] <- factor(df[,"k9"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        names(df)[names(df)=="Surgical.masks.provide.protection.from.haze"] <- "k10"
        df[,"k10"] <- factor(df[,"k10"],labels=c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree"),ordered=T)
        
        #--------------------Recode to binary----------------
        if (binary.code == T) {
                df[,"haze.severe"] <- binary(df[,"haze.severe"])
                df[,"r1"] <- binary(df[,"r1"])
                df[,"r2"] <- binary(df[,"r2"])
                df[,"r3"] <- binary(df[,"r3"])
                df[,"r4"] <- binary(df[,"r4"])
                df[,"r5"] <- binary(df[,"r5"])
                
                df[,"p1"] <- binary(df[,"p1"])
                df[,"p2"] <- binary(df[,"p2"])
                df[,"p3"] <- binary(df[,"p3"])
                df[,"p4"] <- binary(df[,"p4"])
                df[,"p5"] <- binary(df[,"p5"])
                df[,"p6"] <- binary(df[,"p6"])
                df[,"p7"] <- binary(df[,"p7"])
                
                df[,"k1"] <- binary(df[,"k1"])
                df[,"k2"] <- binary(df[,"k2"])
                df[,"k3"] <- binary(df[,"k3"])
                df[,"k4"] <- binary(df[,"k4"])
                df[,"k5"] <- binary(df[,"k5"])
                df[,"k6"] <- binary(df[,"k6"])
                df[,"k7"] <- binary(df[,"k7"])
                df[,"k8"] <- binary(df[,"k8"])
                df[,"k9"] <- binary(df[,"k9"])
                df[,"k10"] <- binary(df[,"k10"])
        }
        
        #-------------------Visual Mask Fit---------------
        names(df)[names(df)=="X19..Subject.referred.to.instruction.slip.provided"] <- "vmf.instructions"
        
        #X20..Visual.Mask.Fit.Assessment is a wrapper
        names(df)[names(df)=="Mask.donned.right.side.up"] <- "vmft1"
        names(df)[names(df)=="Mask.is.straight..Not.tilted."] <- "vmft2"
        names(df)[names(df)=="Both.straps.were.used"] <- "vmft3"
        names(df)[names(df)=="Straps.were.correctly.placed"] <- "vmft4"
        names(df)[names(df)=="Nose.clip.was.tightened"] <- "vmft5"
        names(df)[names(df)=="No.visible.gap.between.respiratory.and.skin"] <- "vmft6"
        names(df)[names(df)=="No.visible.beard"] <- "vmft7"
        names(df)[names(df)=="No.leakage.of.air.around.mask.when.subject.asked.to.blow.out"] <- "vmft8"
        names(df)[names(df)=="Breathing.in.to.create.a.vaccum"] <- "vmft9"
        
        pass.fail.vmf <- function(df) {
                score <- apply(df[,names(df)[grep("vmft",names(df))]], 1, sum)
                result <- sapply(score, FUN=function(x) {
                        if (is.na(x)==T) {
                                return(NA)
                        }
                        if (x == 9) {
                                result <- "pass"
                        }
                        else {
                                result <- "fail"
                        }
                },simplify=T)  
                factor(result)
        }
        df$vmft.result <- pass.fail.vmf(df) 
        
        #-----------------Past training-----------------
        #X21..Have.you.ever.received.any.N95.mask.fit.training. is a wrapper
        names(df)[names(df)=="No"] <- "past.training"
        names(df)[names(df)=="Read.instruction.manual"] <- "instruction.training"
        names(df)[names(df)=="Video.and.Multi.media"] <- "video.training"
        names(df)[names(df)=="Formal.mask.fit.training.with.a.saccharin.spray.test"] <- "formal.training"
        names(df)[names(df)=="Others"] <- "other.training"
        
        #-----------------N95 questions----------------
        names(df)[names(df)=="X22..Did.you.own.N95.masks.during.the.2013.haze.period."] <- "own.masks"
        names(df)[names(df)=="X23..Did.you.wear.an.N95.mask.during.the.2013.haze.period."] <- "wore.masks"
        
        #X24..The.following.are.possible.reasons.why.you.did.not.obtain.any.N95.masks.during.the.2013.haze.period..there.are.no.correct.or.incorrect.answers..For.each.reason..please.state.if.you..Strongly.Agree....Agree....Neither.Agree.nor.Disagree....Disagree..or..Strongly.Disagree..        
        names(df)[names(df)=="N95.masks.were.too.expensive"]
        names(df)[names(df)=="Queues.at.the.stores.selling.N95.masks.were.too.long"]
        names(df)[names(df)=="Stores.selling.N95.masks.were.out.of.stock"]
        names(df)[names(df)=="N95.masks.were.not.useful.during.haze"]
        names(df)[names(df)=="I.did.not.look.good.wearing.an.N95.mask"]
        names(df)[names(df)=="N95.masks.were.uncomfortable"]
        names(df)[names(df)=="I.did.not.know.how.to.wear.an.N95.mask"]
        
        #X25..How.did.you.obtain.your.N95.masks. is a wrapper                                                                                                                                                                                        
        names(df)[names(df)=="Was.given.to.me.by.government.charity"]
        names(df)[names(df)=="I.purchased.the.mask"]
        names(df)[names(df)=="Others.1"]
        
        
        #X26..The.following.are.possible.reasons.why.you.did.not.wear.an.N95.mask..there.are.no.correct.of.incorrect.answers..For.each.reason..please.state.if.you..Strong.Agree....Agree....Neither.Agree.nor.Disagree....Disagree..or..Strongly.Disagree.. is a wrapper
        names(df)[names(df)=="It.was.too.troublesome.to.put.on.an.N95.mask"]
        names(df)[names(df)=="N95.masks.were.not.comfortable.to.wear"] 
        names(df)[names(df)=="N95.masks.were.not.useful.during.haze.1"]
        names(df)[names(df)=="I.did.not.look.good.wearing.an.N95.mask.1"]
        names(df)[names(df)=="I.did.not.know.how.to.wear.an.N95.mask.1 "]
        
        #X27..There.should.be.a.mask.fit.education.campaign.to.teach.proper.mask.fit.techniques is a wrapper
        names(df)[names(df)=="mask.campaign"]
        
        #X28..Which.of.the.following.educational.campaigns.would.you.be.interested.in.participating.in. is a wrapper
        names(df)[names(df)=="Internet.Website"]
        names(df)[names(df)=="Television"]
        names(df)[names(df)=="Printed.media"]
        names(df)[names(df)=="Face.to.face.at.a.Community.Center"]
        names(df)[names(df)=="Door.to.door.visits"]
        names(df)[names(df)=="Not.interested.in.any.training"]
        
        #-----------Some rearragements-----------
        if (part2 == F) {
                df <- df[,c(1:grep("vmft9",names(df)),grep("vmft.result",names(df)))]
        }
        df <- df[,-grep("X",names(df))]
        df <- df[,c(1,grep("gender",names(df)):ncol(df))]
        df <- df[,c(1,3,2,4:ncol(df))]
        df
}

#Step 3. Chose the line you need to clean the data you input in Step 1
data <- clean(data) #if you want the FULL dataset
data <- clean(data, part2=F) #if you want it cleaned up to the visual mask fit
data <- clean(data, part2=F, binary.code=T) #for binary coding

#Step 4. Check missing values, run this ONCE to load the function
missing.var <- function(df) {
        output <- apply(df, 1, function(x) names(x)[which(is.na(x)==T)])
        vector <- c()
        for (i in 1:length(output)) {
                if (length(output[[i]] > 0)) {
                        vector <- c(i,vector)
                }
        }
        output <- output[vector]
        names(output) <- vector
        output
}

#Run these 2 lines to get a list of rows and variables names with missing values
list <- missing.var(data)
list

#Step 5. Output data (run these steps)
missing <- as.numeric(names(list))
factors <- grep("r1",names(data)):grep("k10",names(data))
vmft <- grep("vmft1",names(data)):grep("vmft.result",names(data))
demographics <- which(names(data) %in% c("age","gender","edu.level","mth.income","chronic.disease"))

#write to a csv file (Run this once to output to "data.csv")
write.csv(data.matrix(data[-missing,c(demographics,factors)]),file="day4databinary.dat",quote=F,row.names=T)
