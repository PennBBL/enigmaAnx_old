#################
### LOAD DATA ###
#################

##Demographic data (n=1629)
data.demo <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv", header=TRUE, na.strings="") 

##Clinical data
#Screening diagnoses (n=1601) (no missing values)
data.diag <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_psych_summary_vars_20131014.csv", header=TRUE)

#Psychosis clinical group (n=1601)
data.psychosis <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_diagnosis_dxpmr_20170509.csv", header=TRUE, na.strings="")

##Exclusion data
#Health exclusion (use the new healthExcludev2 variable) (n=1601; no missing values)
data.healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)

#T1 QA exclusion (n=1601)
data.t1QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header=TRUE, na.strings="NA")

#################
### DATA PREP ###
#################

#Transform the age variable from months to years
data.demo$age <- (data.demo$ageAtScan1)/12

#Recode male as 0 and female as 1 (0=male, 1=female)
data.demo$sex[which(data.demo$sex==1)] <- 0
data.demo$sex[which(data.demo$sex==2)] <- 1

#Make sex a factor
data.demo$sex <- as.factor(data.demo$sex)




##################
### MERGE DATA ###
##################
dataMerge1 <-merge(data.demo,data.diag, by=c("bblid","scanid"), all=TRUE) 
dataMerge2 <-merge(dataMerge1,data.psychosis, by=c("bblid","scanid"), all=TRUE) 
dataMerge3 <-merge(dataMerge2,data.healthExclude, by=c("bblid","scanid"), all=TRUE)
dataMerge4 <-merge(dataMerge3,data.t1QA, by=c("bblid","scanid"), all=TRUE)

#Retain only the 1601 bblids (demographics has 1629)
data.n1601 <- dataMerge4[match(data.t1QA$bblid, dataMerge4$bblid, nomatch=0),] 

#Put bblids in ascending order
data.ordered <- data.n1601[order(data.n1601$bblid),]

#Count the number of subjects (should be 1601)
n <- nrow(data.ordered)

########################
### APPLY EXCLUSIONS ### 
########################
##Count the total number excluded for healthExcludev2=1 (1=Excludes those with medical rating 3/4, major incidental findings that distort anatomy, psychoactive medical medications)
#Included: n=1447; Excluded: n=154, but medical.exclude (n=81) + incidental.exclude (n=20) + medicalMed.exclude (n=64) = 165, so 11 people were excluded on the basis of two or more of these criteria
data.final <- data.ordered
data.final$ACROSS.INCLUDE.health <- 1
data.final$ACROSS.INCLUDE.health[data.final$healthExcludev2==1] <- 0
health.include<-sum(data.final$ACROSS.INCLUDE.health)
health.exclude<-1601-health.include

#Count the number excluded just medical rating 3/4 (GOAssess Medial History and CHOP EMR were used to define one summary rating for overall medical problems) (n=81)
data.final$ACROSS.INCLUDE.medical <- 1
data.final$ACROSS.INCLUDE.medical[data.final$medicalratingExclude==1] <- 0
medical.include<-sum(data.final$ACROSS.INCLUDE.medical)
medical.exclude<-1601-medical.include

#Count the number excluded for just major incidental findings that distort anatomy (n=20)
data.final$ACROSS.INCLUDE.incidental <- 1
data.final$ACROSS.INCLUDE.incidental[data.final$incidentalFindingExclude==1] <- 0
incidental.include<-sum(data.final$ACROSS.INCLUDE.incidental)
incidental.exclude<-1601-incidental.include

#Count the number excluded for just psychoactive medical medications (n=64)
data.final$ACROSS.INCLUDE.medicalMed <- 1
data.final$ACROSS.INCLUDE.medicalMed[data.final$psychoactiveMedMedicalv2==1] <- 0
medicalMed.include<-sum(data.final$ACROSS.INCLUDE.medicalMed)
medicalMed.exclude<-1601-medicalMed.include

#Subset the data to just those who pass healthExcludev2 (n=1447)
data.subset <-data.final[which(data.final$ACROSS.INCLUDE.health == 1), ]
n_health <- nrow(data.subset)

##Count the number excluded for failing to meet structural image quality assurance protocols
#Included: n=1396; Excluded: n=51
data.subset$ACROSS.INCLUDE.t1QA <- 1
data.subset$ACROSS.INCLUDE.t1QA[data.subset$t1Exclude==1] <- 0
t1QA.include<-sum(data.subset$ACROSS.INCLUDE.t1QA)
t1QA.exclude<-1447-t1QA.include

###Exclude those with ALL problems (health problems and problems with their t1 data) (included n=1396)
data.exclude <- data.subset[which(data.subset$healthExcludev2==0 & data.subset$t1Exclude == 0 ),]
n_health_t1 <- nrow(data.exclude)
subjData <- data.exclude

##################################################
### DEFINE PSYCHOPATHOLOGY SCREENING DIAGNOSES ###
##################################################

##Make variables where 1 = diagnosis

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
subjData$totDx <- rowSums(dxDf[,2:17], na.rm=TRUE) #This is how many people have how many diagnoses: sum(subjData$totDx==0): 428, sum(subjData$totDx==1): 321, sum(subjData$totDx>=2): 647.
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

#################
### SAVE DATA ###
#################

#Subset to just GAD and TD
data.TdGad <- subjData[which(subjData$Td==1 | subjData$Gad==1),]
n_TdGad <- nrow(data.TdGad)

#Save subject level data
#saveRDS(data.TdGad,"/data/jux/BBL/projects/enigmaAnxiety/subjectData/n455_TD_GAD.rds")

##################
### SAVE LISTS ###
##################

##TD
#Subset only TD for zipping files
data.TD <- subjData[which(subjData$Td==1),]

#Save the bblids and scanids for the final sample
IDs <- c("bblid", "scanid")
TD_IDs <- data.TD[IDs]

#Remove header
names(TD_IDs) <- NULL

#Save a list of bblids and scanids
#write.table(TD_IDs, file="/data/jux/BBL/projects/enigmaAnxiety/subjectData/TD_bblids_scanids.csv", row.names=FALSE, col.names=FALSE, sep=",")

##GAD
#Subset only GAD for zipping files
data.GAD <- subjData[which(subjData$Gad==1),]

#Save the bblids and scanids for the final sample
IDs <- c("bblid", "scanid")
GAD_IDs <- data.GAD[IDs]

#Remove header
names(GAD_IDs) <- NULL

#Save a list of bblids and scanids
#write.table(GAD_IDs, file="/data/jux/BBL/projects/enigmaAnxiety/subjectData/GAD_bblids_scanids.csv", row.names=FALSE, col.names=FALSE, sep=",")
