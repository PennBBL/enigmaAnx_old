##########################
### LOAD DATA & SUBSET ###
##########################

#Demographics
data.demo <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv", header=TRUE, na.strings=".")

#Health Exclusion
data.healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE, na.strings=".")

#FS Exclusion (FS QA criteria = the basis for all enigma studies)
data.fsExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header=TRUE, na.strings=".")

#GOASSESS clinical groups
data.clinical <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_psych_summary_vars_20131014.csv", header=TRUE, na.strings=".")

#Psychosis clinical group
data.psychosis <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_diagnosis_dxpmr_20170509.csv", header=TRUE, na.strings=".")


##################
### MERGE DATA ###
##################

#WARNING: Merging files with unequal cases will cause the non-matched bblids to be deleted; to merge all cases and create NAs for missing data, add all = TRUE).
data.merge1 <- merge(data.demo, data.healthExclude, by=c("bblid","scanid"), all=TRUE) #n=1629
data.merge2 <- merge(data.merge1, data.fsExclude, by=c("bblid","scanid"), all=TRUE) #n=1601
data.merge3 <- merge(data.merge2, data.clinical, by=c("bblid","scanid"), all=TRUE) #n=1601
data.merge4 <- merge(data.merge3, data.psychosis, by=c("bblid","scanid"), all=TRUE) #n=1601

#Retain only the n=1601 bblids
data.n1601 <- data.merge4[match(data.clinical$bblid, data.merge4$bblid, nomatch=0),] 

#Put bblids in ascending order
data.ordered <- data.n1601[order(data.n1601$bblid),]


################################
### APPLY EXCLUSION CRITERIA ###
################################

#The exclusion criteria are: healthExcludev2=1 (1=Excludes those with medical rating 3/4, major incidental findings that distort anatomy, psychoactive medical medications); fsFinalExclude=1 (1=problems with their FreeSurfer data).
#NOTE: the new "healthExcludev2" replaces the old more stringent exclusion criteria "healthExclude"
data.ordered$ACROSS.INCLUDE <- 1
data.ordered$ACROSS.INCLUDE[data.ordered$healthExcludev2==1] <- 0
data.ordered$ACROSS.INCLUDE[data.ordered$fsFinalExclude==1] <- 0

##Exclude everyone that has one or more of these exclusion criteria
data.exclude <- data.ordered[which(data.ordered$ACROSS.INCLUDE == 1), ]

##After exclusion criteria, there are 1384 subjects
subjData <- data.exclude


###########################
### TRANSFORM VARIABLES ###
###########################

#Transform the age variable from months to years ("ageAtGo1Scan" changed to "ageAtScan1" in new demographics file)
subjData$age <- (subjData$ageAtScan1)/12

#Label sex
subjData$sex[which(subjData$sex==1)] <- "male"
subjData$sex[which(subjData$sex==2)] <- "female"
subjData$sex <- as.factor(subjData$sex)

#race (make white vs non-white)
subjData$white <- NA
subjData$white[which(subjData$race==1)] <- "Caucasian"
subjData$white[which(subjData$race!=1)] <- "notCaucasian"
subjData$white <- as.factor(subjData$white)


#########################################
#### MAKE DIAGNOSIS FACTOR VARIABLES ####
#########################################

##Make variables where 1 = diagnosis.

#ADHD
subjData$Add <- NA
subjData$Add[which(subjData$goassessSmryAdd==4)] <- 1

#Agoraphobia
subjData$Agr <- NA
subjData$Agr[which(subjData$goassessSmryAgr==4)] <- 1

#Anorexia
subjData$Ano <- NA
subjData$Ano[which(subjData$goassessSmryAno==4)] <- 1

#Bulimia
subjData$Bul <- NA
subjData$Bul[which(subjData$goassessSmryBul==4)] <- 1

#Conduct Disorder
subjData$Con <- NA
subjData$Con[which(subjData$goassessSmryCon==4)] <- 1

#Generalized Anxiety Disorder
subjData$Gad <- NA
subjData$Gad[which(subjData$goassessSmryGad==4)] <- 1

#Mania
subjData$Man <- NA
subjData$Man[which(subjData$goassessSmryMan==4)] <- 1

#Major Depressive Disorder
subjData$Mdd <- NA
subjData$Mdd[which(subjData$goassessSmryDep==4)] <- 1

#OCD
subjData$Ocd <- NA
subjData$Ocd[which(subjData$goassessSmryOcd==4)] <- 1

#Oppositional Defiant Disorder
subjData$Odd <- NA
subjData$Odd[which(subjData$goassessSmryOdd==4)] <- 1

#Panic Disorder
subjData$Pan <- NA
subjData$Pan[which(subjData$goassessSmryPan==4)] <- 1

#Psychosis
subjData$Ps <- NA
subjData$Ps[which(subjData$goassessDxpmr4=="4PS")] <- 1

#Posttraumatic Stress Disorder
subjData$Ptd <- NA
subjData$Ptd[which(subjData$goassessSmryPtd==4)] <- 1

#Separation Anxiety Disorder
subjData$Sep <- NA
subjData$Sep[which(subjData$goassessSmrySep==4)] <- 1

#Social Anxiety Disorder
subjData$Soc <- NA
subjData$Soc[which(subjData$goassessSmrySoc==4)] <- 1

#Specific Phobia
subjData$Sph <- NA
subjData$Sph[which(subjData$goassessSmryPhb==4)] <- 1

#Typically Developing
dxNames <- c("bblid","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf <- data.matrix(subjData[,dxNames])
subjData$totDx <- rowSums(dxDf[,2:17], na.rm=TRUE) #This is how many people have how many diagnoses: sum(subjData$totDx==0):420, sum(subjData$totDx==1):316, sum(subjData$totDx>=2):648
subjData$Td <- 0
subjData$Td[which(subjData$totDx==0)] <- 1


#####################################
#### MAKE TD THE REFERENCE GROUP ####
#####################################

subjData$Add[which(subjData$Td==1)] <- 0
subjData$Agr[which(subjData$Td==1)] <- 0
subjData$Ano[which(subjData$Td==1)] <- 0
subjData$Bul[which(subjData$Td==1)] <- 0
subjData$Con[which(subjData$Td==1)] <- 0
subjData$Gad[which(subjData$Td==1)] <- 0
subjData$Man[which(subjData$Td==1)] <- 0
subjData$Mdd[which(subjData$Td==1)] <- 0
subjData$Ocd[which(subjData$Td==1)] <- 0
subjData$Odd[which(subjData$Td==1)] <- 0
subjData$Pan[which(subjData$Td==1)] <- 0
subjData$Ps[which(subjData$Td==1)] <- 0
subjData$Ptd[which(subjData$Td==1)] <- 0
subjData$Sep[which(subjData$Td==1)] <- 0
subjData$Soc[which(subjData$Td==1)] <- 0
subjData$Sph[which(subjData$Td==1)] <- 0


##############################################
### SUBSET TO THOSE WITH ANXIETY DIAGNOSES ###
##############################################

#Create a variable that represents only the anxiety groups
subjData$AllAnxTd <- NA
subjData$AllAnxTd[subjData$Agr==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Gad==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Ocd==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Pan==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Ptd==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Sep==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Soc==1] <- "AllAnx"
subjData$AllAnxTd[subjData$Sph==1] <- "AllAnx"
subjData$AllAnxTd <- as.factor(subjData$AllAnxTd)

#Subset to just the anxiety groups (n=690)
AllAnxSubjData <- subjData
AllAnxSubjData <- AllAnxSubjData[which(AllAnxSubjData$AllAnx != "NA"), ]


######################################
### DETERMINE COMMON COMORBIDITIES ###
######################################

#Frequencies of each comorbid disorder
Add_freq <- sum(AllAnxSubjData$Add, na.rm=TRUE)
Ano_freq <- sum(AllAnxSubjData$Ano, na.rm=TRUE)
Bul_freq <- sum(AllAnxSubjData$Bul, na.rm=TRUE)
Con_freq <- sum(AllAnxSubjData$Con, na.rm=TRUE)
Mdd_freq <- sum(AllAnxSubjData$Mdd, na.rm=TRUE)
Man_freq <- sum(AllAnxSubjData$Man, na.rm=TRUE)
Odd_freq <- sum(AllAnxSubjData$Odd, na.rm=TRUE)
Ps_freq <- sum(AllAnxSubjData$Ps, na.rm=TRUE)

#Determine how many of the anxiety subjects have more than 1 diagnosis (1 diagnosis = 164; 2 or more diagnoses = 526)
oneDx <- sum(AllAnxSubjData$totDx==1)
twoOrMoreDx <- sum(AllAnxSubjData$totDx>=2)

#Determine how many subjects were on psychiatric meds at the time of imaging
subjData_sensitivity <- AllAnxSubjData
subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS <- 1
subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS[subjData_sensitivity$psychoactiveMedPsychv2==1] <- 0
NoMeds <- sum(subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS)
Meds <- 690-NoMeds

#Mean age and range
meanAge<-mean(AllAnxSubjData$age)
rangeAge<-range(AllAnxSubjData$age)


#################
### SAVE DATA ###
#################

#Save RDS file with all variables
saveRDS(AllAnxSubjData,"/data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_enigmaAnx_subjData.rds")

#Save the .csv with just the bblids and scanids
IDs <- c("bblid", "scanid")
bblidsScanids <- AllAnxSubjData[IDs]

#Remove header
names(bblidsScanids) <- NULL

#Save list
write.csv(bblidsScanids, file="/data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_enigmaAnx_bblids_scanids.csv", row.names=FALSE)


######################################
### SUBSET TO THOSE UNDER 18 YEARS ###
######################################

#Create under 21 inclusion variable (for ENIGMA-ANXIETY sample sizes)
subjData_under21 <- subjData
subjData_under21$ACROSS.INCLUDE.UNDER21 <- NA
subjData_under21$ACROSS.INCLUDE.UNDER21[subjData_under21$age<21] <- 1

#Subset the data to just those under 21
subjData_under21 <- subjData_under21[which(subjData_under21$ACROSS.INCLUDE.UNDER21 != "NA"), ]

#Calculate n per anixety group

#Agoraphobia
nAgr<-sum(subjData_under21$Agr, na.rm=TRUE)

#Generalized Anxiety Disorder
nGad<-sum(subjData_under21$Gad, na.rm=TRUE)

#OCD
nOcd<-sum(subjData_under21$Ocd, na.rm=TRUE)

#Panic Disorder
nPan<-sum(subjData_under21$Pan, na.rm=TRUE)

#Posttraumatic Stress Disorder
nPtd<-sum(subjData_under21$Ptd, na.rm=TRUE)

#Separation Anxiety Disorder
nSep<-sum(subjData_under21$Sep, na.rm=TRUE)

#Social Anxiety Disorder
nSoc<-sum(subjData_under21$Soc, na.rm=TRUE)

#Specific Phobia
nSph<-sum(subjData_under21$Sph, na.rm=TRUE)

#Typically Developing
nTd<-sum(subjData_under21$Td, na.rm=TRUE)

#Determine number of subjects with any anxiety diagnosis (n=)
nAllAnx <- table(subjData_under21$AllAnxTd)


#########################################
#### MEANS, SDS, %FEMALE, %WHITE, Ns ####
#########################################

##Calculate means, sds, percentage of females, percentage of White, and Ns for entire sample. 

#ADHD
numAdd<-sum(subjData$Add, na.rm=TRUE)
ageAddMean<-mean(subjData$age[which(subjData$Add==1)],na.rm=T)
ageAddSd<-sd(subjData$age[which(subjData$Add==1)],na.rm=T)
femAdd<-length(which(subjData$sex=="female" & subjData$Add==1))/numAdd
whiteAdd<-(length(which(subjData$race==1 & subjData$Add==1)))/numAdd
meduAddMean<-mean(subjData$medu1[which(subjData$Add==1)],na.rm=T)
meduAddSd<-sd(subjData$medu1[which(subjData$Add==1)],na.rm=T)

#Agoraphobia
numAgr<-sum(subjData$Agr, na.rm=TRUE)
ageAgrMean<-mean(subjData$age[which(subjData$Agr==1)],na.rm=T)
ageAgrSd<-sd(subjData$age[which(subjData$Agr==1)],na.rm=T)
femAgr<-length(which(subjData$sex=="female" & subjData$Agr==1))/numAgr
whiteAgr<-(length(which(subjData$race==1 & subjData$Agr==1)))/numAgr
meduAgrMean<-mean(subjData$medu1[which(subjData$Agr==1)],na.rm=T)
meduAgrSd<-sd(subjData$medu1[which(subjData$Agr==1)],na.rm=T)

#Anorexia
numAno<-sum(subjData$Ano, na.rm=TRUE)
ageAnoMean<-mean(subjData$age[which(subjData$Ano==1)],na.rm=T)
ageAnoSd<-sd(subjData$age[which(subjData$Ano==1)],na.rm=T)
femAno<-length(which(subjData$sex=="female" & subjData$Ano==1))/numAno
whiteAno<-(length(which(subjData$race==1 & subjData$Ano==1)))/numAno
meduAnoMean<-mean(subjData$medu1[which(subjData$Ano==1)],na.rm=T)
meduAnoSd<-sd(subjData$medu1[which(subjData$Ano==1)],na.rm=T)

#Bulimia
numBul<-sum(subjData$Bul, na.rm=TRUE)
ageBulMean<-mean(subjData$age[which(subjData$Bul==1)],na.rm=T)
ageBulSd<-sd(subjData$age[which(subjData$Bul==1)],na.rm=T)
femBul<-length(which(subjData$sex=="female" & subjData$Bul==1))/numBul
whiteBul<-(length(which(subjData$race==1 & subjData$Bul==1)))/numBul
meduBulMean<-mean(subjData$medu1[which(subjData$Bul==1)],na.rm=T)
meduBulSd<-sd(subjData$medu1[which(subjData$Bul==1)],na.rm=T)

#Conduct Disorder
numCon<-sum(subjData$Con, na.rm=TRUE)
ageConMean<-mean(subjData$age[which(subjData$Con==1)],na.rm=T)
ageConSd<-sd(subjData$age[which(subjData$Con==1)],na.rm=T)
femCon<-length(which(subjData$sex=="female" & subjData$Con==1))/numCon
whiteCon<-(length(which(subjData$race==1 & subjData$Con==1)))/numCon
meduConMean<-mean(subjData$medu1[which(subjData$Con==1)],na.rm=T)
meduConSd<-sd(subjData$medu1[which(subjData$Con==1)],na.rm=T)

#Generalized Anxiety Disorder
numGad<-sum(subjData$Gad, na.rm=TRUE)
ageGadMean<-mean(subjData$age[which(subjData$Gad==1)],na.rm=T)
ageGadSd<-sd(subjData$age[which(subjData$Gad==1)],na.rm=T)
femGad<-length(which(subjData$sex=="female" & subjData$Gad==1))/numGad
whiteGad<-(length(which(subjData$race==1 & subjData$Gad==1)))/numGad
meduGadMean<-mean(subjData$medu1[which(subjData$Gad==1)],na.rm=T)
meduGadSd<-sd(subjData$medu1[which(subjData$Gad==1)],na.rm=T)

#Major Depressive Disorder
numMdd<-sum(subjData$Mdd, na.rm=TRUE)
ageMddMean<-mean(subjData$age[which(subjData$Mdd==1)],na.rm=T)
ageMddSd<-sd(subjData$age[which(subjData$Mdd==1)],na.rm=T)
femMdd<-length(which(subjData$sex=="female" & subjData$Mdd==1))/numMdd
whiteMdd<-(length(which(subjData$race==1 & subjData$Mdd==1)))/numMdd
meduMddMean<-mean(subjData$medu1[which(subjData$Mdd==1)],na.rm=T)
meduMddSd<-sd(subjData$medu1[which(subjData$Mdd==1)],na.rm=T)

#Mania
numMan<-sum(subjData$Man, na.rm=TRUE)
ageManMean<-mean(subjData$age[which(subjData$Man==1)],na.rm=T)
ageManSd<-sd(subjData$age[which(subjData$Man==1)],na.rm=T)
femMan<-length(which(subjData$sex=="female" & subjData$Man==1))/numMan
whiteMan<-(length(which(subjData$race==1 & subjData$Man==1)))/numMan
meduManMean<-mean(subjData$medu1[which(subjData$Man==1)],na.rm=T)
meduManSd<-sd(subjData$medu1[which(subjData$Man==1)],na.rm=T)

#OCD
numOcd<-sum(subjData$Ocd, na.rm=TRUE)
ageOcdMean<-mean(subjData$age[which(subjData$Ocd==1)],na.rm=T)
ageOcdSd<-sd(subjData$age[which(subjData$Ocd==1)],na.rm=T)
femOcd<-length(which(subjData$sex=="female" & subjData$Ocd==1))/numOcd
whiteOcd<-(length(which(subjData$race==1 & subjData$Ocd==1)))/numOcd
meduOcdMean<-mean(subjData$medu1[which(subjData$Ocd==1)],na.rm=T)
meduOcdSd<-sd(subjData$medu1[which(subjData$Ocd==1)],na.rm=T)

#Oppositional Defiant Disorder
numOdd<-sum(subjData$Odd, na.rm=TRUE)
ageOddMean<-mean(subjData$age[which(subjData$Odd==1)],na.rm=T)
ageOddSd<-sd(subjData$age[which(subjData$Odd==1)],na.rm=T)
femOdd<-length(which(subjData$sex=="female" & subjData$Odd==1))/numOdd
whiteOdd<-(length(which(subjData$race==1 & subjData$Odd==1)))/numOdd
meduOddMean<-mean(subjData$medu1[which(subjData$Odd==1)],na.rm=T)
meduOddSd<-sd(subjData$medu1[which(subjData$Odd==1)],na.rm=T)

#Panic Disorder
numPan<-sum(subjData$Pan, na.rm=TRUE)
agePanMean<-mean(subjData$age[which(subjData$Pan==1)],na.rm=T)
agePanSd<-sd(subjData$age[which(subjData$Pan==1)],na.rm=T)
femPan<-length(which(subjData$sex=="female" & subjData$Pan==1))/numPan
whitePan<-(length(which(subjData$race==1 & subjData$Pan==1)))/numPan
meduPanMean<-mean(subjData$medu1[which(subjData$Pan==1)],na.rm=T)
meduPanSd<-sd(subjData$medu1[which(subjData$Pan==1)],na.rm=T)

#Psychosis
numPs<-sum(subjData$Ps, na.rm=TRUE)
agePsMean<-mean(subjData$age[which(subjData$Ps==1)],na.rm=T)
agePsSd<-sd(subjData$age[which(subjData$Ps==1)],na.rm=T)
femPs<-length(which(subjData$sex=="female" & subjData$Ps==1))/numPs
whitePs<-(length(which(subjData$race==1 & subjData$Ps==1)))/numPs
meduPsMean<-mean(subjData$medu1[which(subjData$Ps==1)],na.rm=T)
meduPsSd<-sd(subjData$medu1[which(subjData$Ps==1)],na.rm=T)

#Posttraumatic Stress Disorder
numPtd<-sum(subjData$Ptd, na.rm=TRUE)
agePtdMean<-mean(subjData$age[which(subjData$Ptd==1)],na.rm=T)
agePtdSd<-sd(subjData$age[which(subjData$Ptd==1)],na.rm=T)
femPtd<-length(which(subjData$sex=="female" & subjData$Ptd==1))/numPtd
whitePtd<-(length(which(subjData$race==1 & subjData$Ptd==1)))/numPtd
meduPtdMean<-mean(subjData$medu1[which(subjData$Ptd==1)],na.rm=T)
meduPtdSd<-sd(subjData$medu1[which(subjData$Ptd==1)],na.rm=T)

#Separation Anxiety Disorder
numSep<-sum(subjData$Sep, na.rm=TRUE)
ageSepMean<-mean(subjData$age[which(subjData$Sep==1)],na.rm=T)
ageSepSd<-sd(subjData$age[which(subjData$Sep==1)],na.rm=T)
femSep<-length(which(subjData$sex=="female" & subjData$Sep==1))/numSep
whiteSep<-(length(which(subjData$race==1 & subjData$Sep==1)))/numSep
meduSepMean<-mean(subjData$medu1[which(subjData$Sep==1)],na.rm=T)
meduSepSd<-sd(subjData$medu1[which(subjData$Sep==1)],na.rm=T)

#Social Anxiety Disorder
numSoc<-sum(subjData$Soc, na.rm=TRUE)
ageSocMean<-mean(subjData$age[which(subjData$Soc==1)],na.rm=T)
ageSocSd<-sd(subjData$age[which(subjData$Soc==1)],na.rm=T)
femSoc<-length(which(subjData$sex=="female" & subjData$Soc==1))/numSoc
whiteSoc<-(length(which(subjData$race==1 & subjData$Soc==1)))/numSoc
meduSocMean<-mean(subjData$medu1[which(subjData$Soc==1)],na.rm=T)
meduSocSd<-sd(subjData$medu1[which(subjData$Soc==1)],na.rm=T)

#Specific Phobia
numSph<-sum(subjData$Sph, na.rm=TRUE)
ageSphMean<-mean(subjData$age[which(subjData$Sph==1)],na.rm=T)
ageSphSd<-sd(subjData$age[which(subjData$Sph==1)],na.rm=T)
femSph<-length(which(subjData$sex=="female" & subjData$Sph==1))/numSph
whiteSph<-(length(which(subjData$race==1 & subjData$Sph==1)))/numSph
meduSphMean<-mean(subjData$medu1[which(subjData$Sph==1)],na.rm=T)
meduSphSd<-sd(subjData$medu1[which(subjData$Sph==1)],na.rm=T)

#Typically Developing
numTd<-sum(subjData$Td, na.rm=TRUE)
ageTdMean<-mean(subjData$age[which(subjData$Td==1)],na.rm=T)
ageTdSd<-sd(subjData$age[which(subjData$Td==1)],na.rm=T)
femTd<-length(which(subjData$sex=="female" & subjData$Td==1))/numTd
whiteTd<-(length(which(subjData$race==1 & subjData$Td==1)))/numTd
meduTdMean<-mean(subjData$medu1[which(subjData$Td==1)],na.rm=T)
meduTdSd<-sd(subjData$medu1[which(subjData$Td==1)],na.rm=T)


############################
#### DEMOGRAPHICS TABLE ####
############################

#Combine variables
dxNames<-c("bblid","Td","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf<-subjData[,dxNames]

numComb<-c(numTd,numAdd,numAgr,numAno,numBul,numCon,numGad,numMdd,numMan,numOcd,numOdd,numPan,numPs,numPtd,numSep,numSoc,numSph)

ageMeanComb<-round(c(ageTdMean,ageAddMean,ageAgrMean,ageAnoMean,ageBulMean,ageConMean,ageGadMean,ageMddMean,ageManMean,ageOcdMean,ageOddMean,agePanMean,agePsMean,agePtdMean,ageSepMean,ageSocMean,ageSphMean),2)

ageSdComb<-round(c(ageTdSd,ageAddSd,ageAgrSd,ageAnoSd,ageBulSd,ageConSd,ageGadSd,ageMddSd,ageManSd,ageOcdSd,ageOddSd,agePanSd,agePsSd,agePtdSd,ageSepSd,ageSocSd,ageSphSd),2)

femComb<-round(c(femTd,femAdd,femAgr,femAno,femBul,femCon,femGad,femMdd,femMan,femOcd,femOdd,femPan,femPs,femPtd,femSep,femSoc,femSph),3)*100

whiteComb<-round(c(whiteTd,whiteAdd,whiteAgr,whiteAno,whiteBul,whiteCon,whiteGad,whiteMdd,whiteMan,whiteOcd,whiteOdd,whitePan,whitePs,whitePtd,whiteSep,whiteSoc,whiteSph),3)*100

meduMeanComb<-round(c(meduTdMean,meduAddMean,meduAgrMean,meduAnoMean,meduBulMean,meduConMean,meduGadMean,meduMddMean,meduManMean,meduOcdMean,meduOddMean,meduPanMean,meduPsMean,meduPtdMean,meduSepMean,meduSocMean,meduSphMean),2)

meduSdComb<-round(c(meduTdSd,meduAddSd,meduAgrSd,meduAnoSd,meduBulSd,meduConSd,meduGadSd,meduMddSd,meduManSd,meduOcdSd,meduOddSd,meduPanSd,meduPsSd,meduPtdSd,meduSepSd,meduSocSd,meduSphSd),2)

#Make table
dxNamesFull<-c("Typically Developing","ADHD","Agoraphobia","Anorexia","Bulimia","Conduct Disorder","Generalized Anxiety Disorder","Major Depression","Mania","Obsessive-Compulsive Disorder","Oppositional Defiant Disorder","Panic","Psychosis-spectrum","PTSD","Separation Anxiety","Social Phobia","Specific Phobia")
table1<-as.data.frame(matrix(nrow=17,ncol=7))
row.names(table1)<-dxNamesFull
colnames(table1)[1]<-"N"
colnames(table1)[2]<-"Female (%)"
colnames(table1)[3]<-"Caucasian (%)"
colnames(table1)[4]<-"Mean Age"
colnames(table1)[5]<-"SD Age"
colnames(table1)[6]<-"Mean Maternal Education (Years)"
colnames(table1)[7]<-"SD Maternal Education"
table1[,1]<-numComb
table1[,2]<-femComb
table1[,3]<-whiteComb
table1[,4]<-ageMeanComb
table1[,5]<-ageSdComb
table1[,6]<-meduMeanComb
table1[,7]<-meduSdComb

#Save table
write.csv(table1,"/data/joy/BBL/projects/enigmaAnxiety/tablesFigures/Table1_Demographics.csv",row.names=TRUE,quote=FALSE)

