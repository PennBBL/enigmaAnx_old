###This script will prep the data for the ENIGMA-Anxiety projects, including creating covariates files with the consortium requested variable names and lists of bblids for zipping nifti files for transferring.

#################
### LOAD DATA ###
#################

#Demographic data (n=1629)
demo <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv", header=TRUE, na.strings="") 

#WRAT scores (n=1601)
wrat <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/cnb/n1601_cnb_wrat_scores_20161215.csv", header=TRUE, na.strings="")

#Clinical screening diagnoses (n=1601) (no missing values)
diag <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_psych_summary_vars_20131014.csv", header=TRUE)

#Psychosis clinical group (n=1601)
psychosis <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_diagnosis_dxpmr_20170509.csv", header=TRUE, na.strings="")

#Meds and health exclusion (use the new healthExcludev2 variable) (n=1601) (no missing values)
healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)

#State trait anxiety inventory (STAI) (n=1391)
stai <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_stai_pre_post_itemwise_smry_factors_20170131.csv", header=TRUE, na.strings="")

#T1 QA exclusion (n=1601)
t1QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header=TRUE, na.strings="NA")

####################
### DEMOGRAPHICS ###
####################

#Rename bblid
demo$SubjID <- demo$bblid

#Transform the age variable from months to years
demo$Age <- (demo$ageAtScan1)/12

#Make age into integers
demo$Age <- as.integer(demo$Age)

#Recode male as 0 and female as 1 (0=male, 1=female)
demo$Sex <- demo$sex
demo$Sex[which(demo$Sex==1)] <- 0
demo$Sex[which(demo$Sex==2)] <- 1

#Rename edu1
demo$Edu <- demo$edu1

#Extract variables of interest
demo_final <- demo[c(grep("SubjID|Age|Sex|Edu",names(demo)))]

##########
### IQ ###
##########

#Rename bblid
wrat$SubjID <- wrat$bblid

#Rename wrat4CrStd
wrat$IQ <- wrat$wrat4CrStd

#Extract variables of interest
wrat_final <- wrat[c(grep("SubjID|IQ",names(wrat)))]

##################################################
### DEFINE PSYCHOPATHOLOGY SCREENING DIAGNOSES ###
##################################################

##Make variables where 1 = diagnosis

#ADHD
diag$Add <- NA
diag$Add[which(diag$goassessSmryAdd==4)] <- 1
diag$Add[which(diag$goassessSmryAdd<4)] <- 0

#Agoraphobia
diag$Agr <- NA
diag$Agr[which(diag$goassessSmryAgr==4)] <- 1
diag$Agr[which(diag$goassessSmryAgr<4)] <- 0

#Anorexia
diag$Ano <- NA
diag$Ano[which(diag$goassessSmryAno==4)] <- 1
diag$Ano[which(diag$goassessSmryAno<4)] <- 0

#Bulimia
diag$Bul <- NA
diag$Bul[which(diag$goassessSmryBul==4)] <- 1
diag$Bul[which(diag$goassessSmryBul<4)] <- 0

#Conduct Disorder
diag$Con <- NA
diag$Con[which(diag$goassessSmryCon==4)] <- 1
diag$Con[which(diag$goassessSmryCon<4)] <- 0

#Generalized Anxiety Disorder
diag$Gad <- NA
diag$Gad[which(diag$goassessSmryGad==4)] <- 1
diag$Gad[which(diag$goassessSmryGad<4)] <- 0

#Mania
diag$Man <- NA
diag$Man[which(diag$goassessSmryMan==4)] <- 1
diag$Man[which(diag$goassessSmryMan<4)] <- 0

#Major Depressive Disorder
diag$Mdd <- NA
diag$Mdd[which(diag$goassessSmryDep==4)] <- 1
diag$Mdd[which(diag$goassessSmryDep<4)] <- 0

#OCD
diag$Ocd <- NA
diag$Ocd[which(diag$goassessSmryOcd==4)] <- 1
diag$Ocd[which(diag$goassessSmryOcd<4)] <- 0

#Oppositional Defiant Disorder
diag$Odd <- NA
diag$Odd[which(diag$goassessSmryOdd==4)] <- 1
diag$Odd[which(diag$goassessSmryOdd<4)] <- 0

#Panic Disorder
diag$Pan <- NA
diag$Pan[which(diag$goassessSmryPan==4)] <- 1
diag$Pan[which(diag$goassessSmryPan<4)] <- 0

#Psychosis
diag$Ps <- NA
diag$Ps[which(psychosis$goassessDxpmr4=="4PS")] <- 1
diag$Ps[which(psychosis$goassessDxpmr4!="4PS")] <- 0

#Posttraumatic Stress Disorder
diag$Ptd <- NA
diag$Ptd[which(diag$goassessSmryPtd==4)] <- 1
diag$Ptd[which(diag$goassessSmryPtd<4)] <- 0

#Separation Anxiety Disorder
diag$Sep <- NA
diag$Sep[which(diag$goassessSmrySep==4)] <- 1
diag$Sep[which(diag$goassessSmrySep<4)] <- 0

#Social Anxiety Disorder
diag$Soc <- NA
diag$Soc[which(diag$goassessSmrySoc==4)] <- 1
diag$Soc[which(diag$goassessSmrySoc<4)] <- 0

#Specific Phobia
diag$Sph <- NA
diag$Sph[which(diag$goassessSmryPhb==4)] <- 1
diag$Sph[which(diag$goassessSmryPhb<4)] <- 0

#Typically Developing
dxNames <- c("bblid","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf <- data.matrix(diag[,dxNames])
diag$totDx <- rowSums(dxDf[,2:17], na.rm=TRUE)
diag$Td <- 0
diag$Td[which(diag$totDx==0)] <- 1

##########################
### CLINICAL DIAGNOSES ###
##########################

#Rename variables
diag$SubjID <- diag$bblid
diag$SAD <- diag$Soc
diag$PD <- diag$Pan
diag$AG <- diag$Agr
diag$GAD <- diag$Gad
diag$SPH <- diag$Sph

#Other anxiety disorders (the only other anxiety disorder we assess that isn't already accounted for is Separation Anxiety)
diag$OtherANXD <- diag$Sep

#Specify other anxiety diagnoses
diag$Specify_OtherANXD <- NA
diag$Specify_OtherANXD[diag$Sep==1] <- "Separation Anxiety Disorder"

#Age at onset (we don't have this)
diag$AgeO <- NA

#Rename variables
diag$MDD <- diag$Mdd
diag$OCD <- diag$Ocd
diag$PTSD <- diag$Ptd

#Substance Use Disorder (we don't have this)
diag$SUD <- NA

#Other diagnoses (non-anxiety)
diag$OtherD <- NA
diag$OtherD[which(diag$Add==1|diag$Ano==1|diag$Bul==1|diag$Con==1|diag$Man==1|diag$Odd==1|diag$Ps==1)] <- 1
diag$OtherD[which(diag$Add==0&diag$Ano==0&diag$Bul==0&diag$Con==0&diag$Man==0&diag$Odd==0&diag$Ps==0)] <- 0

#Specify other diagnoses (non-anxiety)
diag$Specify_Add <- NA
diag$Specify_Add[diag$OtherD==1 & diag$Add==1] <- "ADHD"
diag$Specify_Ano <- NA
diag$Specify_Ano[diag$OtherD==1 & diag$Ano==1] <- "Anorexia Nervosa"
diag$Specify_Bul <- NA
diag$Specify_Bul[diag$OtherD==1 & diag$Bul==1] <- "Bulimia Nervosa"
diag$Specify_Con <- NA
diag$Specify_Con[diag$OtherD==1 & diag$Con==1] <- "Conduct Disorder"
diag$Specify_Man <- NA
diag$Specify_Man[diag$OtherD==1 & diag$Man==1] <- "Mania"
diag$Specify_Odd <- NA
diag$Specify_Odd[diag$OtherD==1 & diag$Odd==1] <- "Oppositional Defiant Disorder"
diag$Specify_Ps <- NA
diag$Specify_Ps[diag$OtherD==1 & diag$Ps==1] <- "Psychosis"

#Combine the other diagnosis labels into one variable
diag$Specify_OtherD <- NA
diag$Specify_OtherD <- paste(diag$Specify_Add, diag$Specify_Ano, diag$Specify_Bul, diag$Specify_Con, diag$Specify_Man, diag$Specify_Odd, diag$Specify_Ps, sep=";")
diag$Specify_OtherD <- gsub("NA;", "", diag$Specify_OtherD)
diag$Specify_OtherD <- gsub(";NA", "", diag$Specify_OtherD)

#Dx (0=Healthy Control, 1=Anxiety disorder)
diag$Dx <- NA
diag$Dx[diag$SAD==1|diag$PD==1|diag$AG==1|diag$GAD==1|diag$SPH==1|diag$OtherANXD==1] <- 1
diag$Dx[diag$Td==1] <- 0

#Create a variable that represents only the anxiety groups and the TD youth
diag$AllAnxTd <- NA
diag$AllAnxTd[diag$Agr==1] <- "Anx"
diag$AllAnxTd[diag$Gad==1] <- "Anx"
diag$AllAnxTd[diag$Ocd==1] <- "Anx"
diag$AllAnxTd[diag$Pan==1] <- "Anx"
diag$AllAnxTd[diag$Ptd==1] <- "Anx"
diag$AllAnxTd[diag$Sep==1] <- "Anx"
diag$AllAnxTd[diag$Soc==1] <- "Anx"
diag$AllAnxTd[diag$Sph==1] <- "Anx"
diag$AllAnxTd[diag$Td==1] <- "TD"
diag$AllAnxTd <- as.factor(diag$AllAnxTd)

#Extract variables of interest
diag_final <- diag[c(grep("SubjID|SAD|PD|AG|GAD|SPH|OtherANXD|Specify_OtherANXD|AgeO|MDD|OCD|PTSD|SUD|OtherD|Specify_OtherD|^Dx$|^Td$|^Add$|^Agr$|^Ano$|^Bul$|^Con$|^Gad$|^Man$|^Mdd$|^Ocd$|^Odd$|^Pan$|^Ps$|^Ptd$|^Sep$|^Soc$|^Sph$|AllAnxTd",names(diag)))]

##################
### MEDICATION ###
##################

#Rename bblids
healthExclude$SubjID <- healthExclude$bblid

#Rename variables
healthExclude$SSRI_SNRI <- healthExclude$medclass_Antidepressant
healthExclude$Benzo <- healthExclude$medclass_Benzodiazepine
healthExclude$Apsy <- healthExclude$medclass_Antipsychotic

#Other medications
healthExclude$OtherMed <- NA
healthExclude$OtherMed[which(healthExclude$medclass_Anticonvulsant==1|healthExclude$medclass_Stimulant==1|healthExclude$medclass_NonstimulantADHDmed==1|healthExclude$medclass_Other==1|healthExclude$medclass_Lithium==1)] <- 1
healthExclude$OtherMed[which(healthExclude$medclass_Anticonvulsant==0&healthExclude$medclass_Stimulant==0&healthExclude$medclass_NonstimulantADHDmed==0&healthExclude$medclass_Other==0&healthExclude$medclass_Lithium==0)] <- 0

#Specify other meds
healthExclude$Specify_Anticonvulsant <- NA
healthExclude$Specify_Anticonvulsant[healthExclude$OtherMed==1 & healthExclude$medclass_Anticonvulsant==1] <- "Anticonvulsant"
healthExclude$Specify_Stimulant <- NA
healthExclude$Specify_Stimulant[healthExclude$OtherMed==1 & healthExclude$medclass_Stimulant==1] <- "Stimulant"
healthExclude$Specify_NonStim <- NA
healthExclude$Specify_NonStim[healthExclude$OtherMed==1 & healthExclude$medclass_NonstimulantADHDmed==1] <- "Non-stimulant ADHD med"
healthExclude$Specify_Other <- NA
healthExclude$Specify_Other[healthExclude$OtherMed==1 & healthExclude$medclass_Other==1] <- "Other"
healthExclude$Specify_Lithium <- NA
healthExclude$Specify_Lithium[healthExclude$OtherMed==1 & healthExclude$medclass_Lithium==1] <- "Lithium"

#Combine the other med labels into one variable
healthExclude$Specify_OtherMed <- NA
healthExclude$Specify_OtherMed <- paste(healthExclude$Specify_Anticonvulsant, healthExclude$Specify_Stimulant, healthExclude$Specify_NonStim, healthExclude$Specify_Other, healthExclude$Specify_Lithium, sep=";")
healthExclude$Specify_OtherMed <- gsub("NA;", "", healthExclude$Specify_OtherMed)
healthExclude$Specify_OtherMed <- gsub(";NA", "", healthExclude$Specify_OtherMed)

#Duration of medication that is currently used (we don't have this)
healthExclude$MedDur <- NA

#Merge healthExclude and diag so we can define the Medicated and Patient variables
healthExclude_merged <- merge(healthExclude,diag_final, by="SubjID", all=TRUE)

#Define medicated=1 vs unmedicated=0
healthExclude_merged$Medicated <- NA
healthExclude_merged$Medicated[healthExclude_merged$medclass_Antidepressant==1|healthExclude_merged$medclass_Benzodiazepine==1|healthExclude_merged$medclass_Antipsychotic==1|healthExclude_merged$OtherMed==1] <- 1
healthExclude_merged$Medicated[healthExclude_merged$medclass_Antidepressant==0&healthExclude_merged$medclass_Benzodiazepine==0&healthExclude_merged$medclass_Antipsychotic==0&healthExclude_merged$OtherMed==0] <- 0

#Define patient=1 vs control=0
healthExclude_merged$Patient <- NA
healthExclude_merged$Patient[healthExclude_merged$Dx==1|healthExclude_merged$MDD==1|healthExclude_merged$OCD==1|healthExclude_merged$PTSD==1|healthExclude_merged$SUD==1|healthExclude_merged$OtherD==1] <- 1
healthExclude_merged$Patient[healthExclude_merged$Td==1] <- 0

#Create a Med variable
healthExclude_merged$Med <- NA
healthExclude_merged$Med[healthExclude_merged$Medicated==0 & healthExclude_merged$Patient==0] <- 0
healthExclude_merged$Med[healthExclude_merged$Medicated==0 & healthExclude_merged$Patient==1] <- 1
healthExclude_merged$Med[healthExclude_merged$Medicated==1 & healthExclude_merged$Patient==1] <- 2
healthExclude_merged$Med[healthExclude_merged$Medicated==1 & healthExclude_merged$Patient==0] <- 3

#Extract variables of interest
healthExclude_final <- healthExclude_merged[c(grep("SubjID|SSRI_SNRI|Benzo|Apsy|OtherMed|Specify_OtherMed|MedDur|healthExcludev2|Medicated|Patient|^Med$|medclass_Antidepressant|medclass_Benzodiazepine|medclass_Antipsychotic|medclass_Anticonvulsant|medclass_Stimulant|medclass_NonstimulantADHDmed|medclass_Lithium|medclass_Other",names(healthExclude_merged)))]

#####################################
### STAI AND OTHER QUESTIONNAIRES ###
#####################################

#Rename bblids
stai$SubjID <- stai$bblid

#The stai scores are coded as strings in the datafreeze and need to be converted to numbers.
stai$STAI_T <- as.numeric(as.character(stai$staiPreTrait))

#We didn't collect any of these other questionnaires in the PNC data; make variables but fill with NAs
stai$ASI <- NA
stai$BAI <- NA
stai$LSAS_T <- NA
stai$LSAS_F <- NA
stai$LSAS_A <- NA
stai$PAS <- NA
stai$ACQ <- NA
stai$PDSS <- NA
stai$HAM_A <- NA
stai$PSWQ <- NA
stai$GAD_7 <- NA
stai$BDI_II <- NA
stai$HAM_D <- NA
stai$CDI <- NA
stai$SCARED_T_P <- NA
stai$SCARED_T_C <- NA
stai$SCARED_SC_P <- NA
stai$SCARED_SC_C <- NA
stai$SCARED_PN_P <- NA
stai$SCARED_PN_C <- NA
stai$SCARED_GD_P <- NA
stai$SCARED_GD_C <- NA
stai$SCARED_SH_P <- NA
stai$SCARED_SH_C <- NA

#Extract variables of interest
stai_final <- stai[c(grep("SubjID|STAI_T|ASI|BAI|LSAS_T|LSAS_F|LSAS_A|PAS|ACQ|PDSS|HAM_A|PSWQ|GAD_7|BDI_II|HAM_D|CDI|SCARED_T_P|SCARED_T_C|SCARED_SC_P|SCARED_SC_C|SCARED_PN_P|SCARED_PN_C|SCARED_GD_P|SCARED_GD_C|SCARED_SH_P|SCARED_SH_C",names(stai)))]

#############
### T1 QA ###
#############

#Rename bblids
t1QA$SubjID <- t1QA$bblid

#Extract variables of interest
#We need bblids ans scanids for making subject lists for zipping files
#We need T1QA for our exclusion criteria
t1QA_final <- t1QA[c(grep("SubjID|bblid|scanid|t1Exclude",names(t1QA)))]

##################
### MERGE DATA ###
##################
dataMerge1 <-merge(demo_final,wrat_final, by="SubjID", all=TRUE) 
dataMerge2 <-merge(dataMerge1,diag_final, by="SubjID", all=TRUE) 
dataMerge3 <-merge(dataMerge2,healthExclude_final, by="SubjID", all=TRUE)
dataMerge4 <-merge(dataMerge3,stai_final, by="SubjID", all=TRUE)
dataMerge5 <-merge(dataMerge4,t1QA_final, by="SubjID", all=TRUE)

#Retain only the 1601 bblids (demographics has 1629)
data.n1601 <- dataMerge5[match(t1QA$bblid, dataMerge5$bblid, nomatch=0),] 

#Put bblids in ascending order
data.n1601 <- data.n1601[order(data.n1601$bblid),]

#Count the number of subjects (should be 1601)
n <- nrow(data.n1601)

########################
### APPLY EXCLUSIONS ### 
########################
##Count the total number excluded for healthExcludev2=1 (1=Excludes those with medical rating 3/4, major incidental findings that distort anatomy, psychoactive medical medications)
#Included: n=1447; Excluded: n=154, but medical.exclude (n=81) + incidental.exclude (n=20) + medicalMed.exclude (n=64) = 165, so 11 people were excluded on the basis of two or more of these criteria
data.n1601$ACROSS.INCLUDE.health <- 1
data.n1601$ACROSS.INCLUDE.health[data.n1601$healthExcludev2==1] <- 0
health.include<-sum(data.n1601$ACROSS.INCLUDE.health)
health.exclude<-1601-health.include

#Count the number excluded just medical rating 3/4 (GOAssess Medial History and CHOP EMR were used to define one summary rating for overall medical problems) (n=81)
data.n1601$ACROSS.INCLUDE.medical <- 1
data.n1601$ACROSS.INCLUDE.medical[data.n1601$medicalratingExclude==1] <- 0
medical.include<-sum(data.n1601$ACROSS.INCLUDE.medical)
medical.exclude<-1601-medical.include

#Count the number excluded for just major incidental findings that distort anatomy (n=20)
data.n1601$ACROSS.INCLUDE.incidental <- 1
data.n1601$ACROSS.INCLUDE.incidental[data.n1601$incidentalFindingExclude==1] <- 0
incidental.include<-sum(data.n1601$ACROSS.INCLUDE.incidental)
incidental.exclude<-1601-incidental.include

#Count the number excluded for just psychoactive medical medications (n=64)
data.n1601$ACROSS.INCLUDE.medicalMed <- 1
data.n1601$ACROSS.INCLUDE.medicalMed[data.n1601$psychoactiveMedMedicalv2==1] <- 0
medicalMed.include<-sum(data.n1601$ACROSS.INCLUDE.medicalMed)
medicalMed.exclude<-1601-medicalMed.include

#Subset the data to just those who pass healthExcludev2 (n=1447)
data.subset <-data.n1601[which(data.n1601$ACROSS.INCLUDE.health == 1), ]
n_health <- nrow(data.subset)

##Count the number excluded for failing to meet structural image quality assurance protocols
#Included: n=1396; Excluded: n=51
data.subset$ACROSS.INCLUDE.t1QA <- 1
data.subset$ACROSS.INCLUDE.t1QA[data.subset$t1Exclude==1] <- 0
t1QA.include<-sum(data.subset$ACROSS.INCLUDE.t1QA)
t1QA.exclude<-1447-t1QA.include

###Exclude those with ALL problems (health problems and problems with their t1 data) (included n=1396)
data.final <- data.subset[which(data.subset$healthExcludev2==0 & data.subset$t1Exclude == 0 ),]
n_health_t1 <- nrow(data.final)

##################################################
### SUBSET TO THOSE WITH ANY ANXIETY DIAGNOSIS ###
##################################################

#This is used to create a master LandRvolumes.csv file for all anxiety and TD subjects before subsetting by specific diagnoses for different projects.

#Subset to just the anxiety groups and the TD (n=1120)
data.AllAnxTd <- data.final[which(data.final$AllAnxTd != "NA"), ]
n_AllAnxTd <- nrow(data.AllAnxTd)

#See n's by group (Anx=692; TD=428)
grp_AllAnxTd <- table(data.AllAnxTd$AllAnxTd)

#Save the bblids and scanids for the final sample
IDs <- c("bblid", "scanid")
AllAnxTd_IDs <- data.AllAnxTd[IDs]

#Remove header
names(AllAnxTd_IDs) <- NULL

#Save a list of bblids and scanids
write.table(AllAnxTd_IDs, file="/data/jux/BBL/projects/enigmaAnxiety/subjectData/AllAnxTd_bblids_scanids.csv", row.names=FALSE, col.names=FALSE, sep=",")

#Pull variables of interest
data.AllAnxTd <- data.AllAnxTd[c("SubjID","Dx","Age","Sex","Edu","IQ","SAD","PD","AG","GAD","SPH","OtherANXD","Specify_OtherANXD","AgeO","MDD","OCD","PTSD","SUD","OtherD","Specify_OtherD","Med","SSRI_SNRI","Benzo","Apsy","OtherMed","Specify_OtherMed","MedDur","STAI_T","ASI","BAI","LSAS_T","LSAS_F","LSAS_A","PAS","ACQ","PDSS","HAM_A","PSWQ","GAD_7","BDI_II","HAM_D","CDI","SCARED_T_P","SCARED_T_C","SCARED_SC_P","SCARED_SC_C","SCARED_PN_P","SCARED_PN_C","SCARED_GD_P","SCARED_GD_C","SCARED_SH_P","SCARED_SH_C","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph","Td","AllAnxTd","medclass_Antidepressant","medclass_Benzodiazepine","medclass_Antipsychotic","medclass_Anticonvulsant","medclass_Stimulant","medclass_NonstimulantADHDmed","medclass_Lithium","medclass_Other")]

#Save file
saveRDS(data.AllAnxTd,"/data/jux/BBL/projects/enigmaAnxiety/subjectData/n1120_AllAnxTd_subjData.rds")

##################################
### ENIGMA GAD COVARIATES FILE ###
##################################

#Subset to just GAD and TD
data.TdGad <- data.final[which(data.final$Td==1 | data.final$GAD==1),]
n_TdGad <- nrow(data.TdGad)

##TD
#Subset TD bblids for zipping files
data.TD <- data.final[which(data.final$Td==1),]

#Save the bblids and scanids for the final sample
IDs <- c("bblid", "scanid")
TD_IDs <- data.TD[IDs]

#Remove header
names(TD_IDs) <- NULL

#Save a list of bblids and scanids for zipping files
write.table(TD_IDs, file="/data/jux/BBL/projects/enigmaAnxiety/subjectData/TD_bblids_scanids.csv", row.names=FALSE, col.names=FALSE, sep=",")

##GAD
#Subset only GAD for zipping files
data.GAD <- data.final[which(data.final$GAD==1),]

#Save the bblids and scanids for the final sample
IDs <- c("bblid", "scanid")
GAD_IDs <- data.GAD[IDs]

#Remove header
names(GAD_IDs) <- NULL

#Save a list of bblids and scanids for zipping files
write.table(GAD_IDs, file="/data/jux/BBL/projects/enigmaAnxiety/subjectData/GAD_bblids_scanids.csv", row.names=FALSE, col.names=FALSE, sep=",")

##Covariates file
#Pull variables of interest in the requested order
data.TdGad <- data.TdGad[c("SubjID","Dx","Age","Sex","Edu","IQ","SAD","PD","AG","GAD","SPH","OtherANXD","Specify_OtherANXD","AgeO","MDD","OCD","PTSD","SUD","OtherD","Specify_OtherD","Med","SSRI_SNRI","Benzo","Apsy","OtherMed","Specify_OtherMed","MedDur","STAI_T","ASI","BAI","LSAS_T","LSAS_F","LSAS_A","PAS","ACQ","PDSS","HAM_A","PSWQ","GAD_7","BDI_II","HAM_D","CDI","SCARED_T_P","SCARED_T_C","SCARED_SC_P","SCARED_SC_C","SCARED_PN_P","SCARED_PN_C","SCARED_GD_P","SCARED_GD_C","SCARED_SH_P","SCARED_SH_C")]

#Save subject level data as a csv file
write.csv(data.TdGad,file="/data/jux/BBL/projects/enigmaAnxiety/covariates/PNC_n455_TD_GAD_covariates.csv", row.names=FALSE)

##################################
### ENIGMA SAD COVARIATES FILE ###
##################################

#Subset to just SAD and TD
data.TdSad <- data.final[which(data.final$Td==1 | data.final$SAD==1),]
n_TdSad <- nrow(data.TdSad)

##TD
#Subset TD bblids for zipping files
data.TD <- data.final[which(data.final$Td==1),]

#Save the bblids and scanids for the final sample
IDs <- c("bblid", "scanid")
TD_IDs <- data.TD[IDs]

#Remove header
names(TD_IDs) <- NULL

#Save a list of bblids and scanids for zipping files (this is identical to the TDs in ENIGMA GAD)
#write.table(TD_IDs, file="/data/jux/BBL/projects/enigmaAnxiety/subjectData/TD_bblids_scanids.csv", row.names=FALSE, col.names=FALSE, sep=",")

##SAD
#Subset only SAD for zipping files
data.SAD <- data.final[which(data.final$SAD==1),]

#Save the bblids and scanids for the final sample
IDs <- c("bblid", "scanid")
SAD_IDs <- data.SAD[IDs]

#Remove header
names(SAD_IDs) <- NULL

#Save a list of bblids and scanids for zipping files
write.table(SAD_IDs, file="/data/jux/BBL/projects/enigmaAnxiety/subjectData/SAD_bblids_scanids.csv", row.names=FALSE, col.names=FALSE, sep=",")

##Covariates file
#Pull variables of interest in the requested order
data.TdSad <- data.TdSad[c("SubjID","Dx","Age","Sex","Edu","IQ","SAD","PD","AG","GAD","SPH","OtherANXD","Specify_OtherANXD","AgeO","MDD","OCD","PTSD","SUD","OtherD","Specify_OtherD","Med","SSRI_SNRI","Benzo","Apsy","OtherMed","Specify_OtherMed","MedDur","STAI_T","ASI","BAI","LSAS_T","LSAS_F","LSAS_A","PAS","ACQ","PDSS","HAM_A","PSWQ","GAD_7","BDI_II","HAM_D","CDI","SCARED_T_P","SCARED_T_C","SCARED_SC_P","SCARED_SC_C","SCARED_PN_P","SCARED_PN_C","SCARED_GD_P","SCARED_GD_C","SCARED_SH_P","SCARED_SH_C")]

#Save subject level data as a csv file
write.csv(data.TdSad,file="/data/jux/BBL/projects/enigmaAnxiety/covariates/PNC_n756_TD_SAD_covariates.csv", row.names=FALSE)



