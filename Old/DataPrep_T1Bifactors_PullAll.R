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

#Suicide variables (n=1601)
data.suicide <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_itemwise_smryvars_suicide_20170209.csv", header=TRUE, na.strings="")

#Item level GOASSESS (n=1601)
data.goItems <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_112_itemwise_vars_20161214.csv", header=TRUE, na.strings="")

#Bifactors (n=1601)
data.bifactors <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_itemwise_bifactor_scores_20161219.csv", header=TRUE, na.strings="")

#Correlated traits (n=1601)
data.corrTraits <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_itemwise_corrtraits_scores_20161219.csv", header=TRUE, na.strings="")

#Correlated traits self regressed (n=1601)
data.corrTraits_sr <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_goassess_itemwise_fscores_self_regressed_20170131.csv", header=TRUE, na.strings="")

#State trait anxiety data (n=1391)
data.stai <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/clinical/n1601_stai_pre_post_itemwise_smry_factors_20170131.csv", header=TRUE, na.strings="")

##Cognitive data
#Summary factor scores (n=1601)
data.cogFactors <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/cnb/n1601_cnb_factor_scores_tymoore_20151006.csv", header=TRUE, na.strings="")

#z-scores for the 14 subtests (n=1601)
data.cogZscores <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/cnb/n1601_cnb_zscores_all_fr_20161215.csv", header=TRUE, na.strings="")

#WRAT scores (n=1601)
data.wrat <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/cnb/n1601_cnb_wrat_scores_20161215.csv", header=TRUE, na.strings="")

##Environment 
#SES (n=1601)
data.envir <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/environment/n1601_go1_environment_factor_scores_tymoore_20150909.csv", header=TRUE, na.strings="NA")

##Exclusion data
#Health exclusion (use the new healthExcludev2 variable) (n=1601; no missing values)
data.healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)

#T1 QA exclusion (n=1601)
data.t1QA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header=TRUE, na.strings="NA")

#Asl QA exclusion (n=1601)
data.aslQA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/asl/n1601_PcaslQaData_20170403.csv", header=TRUE, na.strings="NA")

#Resting state connectivity QA exclusion (n=1601)
data.restQA <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/rest/n1601_RestQAData_20170714.csv", header=TRUE, na.strings="NA")

##Brain data
#NMF components (n=1396) (no missing values)
data.CtNMF <- read.csv("/data/jux/BBL/projects/pncNmf/subjectData/n1396_Nmf18Bases_CT_bblids.csv", header=TRUE)
data.RavensNMF <- read.csv("/data/jux/BBL/projects/pncNmf/subjectData/n1396_Nmf26Bases_Ravens_bblids.csv", header=TRUE)

#JLF T1 ROIs (n=1601; no missing values)
data.ct <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv", header=TRUE)
data.vol <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv", header=TRUE)

#T1 Lobes (n=1601)
data.ctLobes <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1601_jlfWmCt_20180328.csv", header=TRUE)
data.volLobes <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfWmVol_20170412.csv", header=TRUE)

#JLF total brain volume (TBV) (n=1601; no missing values)
data.tbv <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_ctVol20170412.csv", header=TRUE)

#JLF ASL ROIs (n=1601)
data.asl <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/asl/n1601_jlfAntsCTIntersectionPcaslValues_20170403.csv", header=TRUE, na.strings="NA")

#ASL Lobes (n=1601)
data.aslLobes <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/asl/n1601_jlfWMPcasl_20170412.csv", header=TRUE, na.strings="NA")

#JLF resting state connectivity ROIs (n=1601)
data.rest.alff <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/rest/n1601_jlfALFFValues_20170714.csv", header=TRUE, na.strings="NA")
data.rest.reho <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/rest/n1601_jlfReHoValues_20170714.csv", header=TRUE, na.strings="NA")

#Freesurfer SurfaceArea 
data.fs.sa <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1601_freesurferSurfaceArea_20180213.csv", header=TRUE)

#Freesurfer CorticalThickness
data.fs.ct <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1601_freesurferCt_20180213.csv", header=TRUE)

#Jacobian volume (18 CT NMF components applied to Jacobian volume images)
data.jacobian <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/cmpWeightedAverageNumBases_18_jacobian.csv", header=FALSE)

#18 Ravens components (18 CT NMF components applied to Ravens volume images)
data.18Ravens <- read.csv("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/cmpWeightedAverageNumBases_18_ravens.csv", header=FALSE)

#################
### DATA PREP ###
#################

#Transform the age variable from months to years
data.demo$age <- (data.demo$ageAtScan1)/12

#Define age squared (de-mean age)
data.demo$ageSq <- I(scale(data.demo$age, scale=FALSE, center=TRUE)^2)

#Recode male as 0 and female as 1 (0=male, 1=female)
data.demo$sex[which(data.demo$sex==1)] <- 0
data.demo$sex[which(data.demo$sex==2)] <- 1

#Make sex a factor
data.demo$sex <- as.factor(data.demo$sex)

#Define white vs nonwhite (white=1, non-white=0)
data.demo$white <- 0
data.demo$white[which(data.demo$race==1)] <- 1

#Make white a factor
data.demo$white <- as.factor(data.demo$white)

#Make the suicide variables into factors
data.suicide$sui001 <- as.factor(data.suicide$sui001)
data.suicide$sui002 <- as.factor(data.suicide$sui002)

#Remove the suicide variables from data.goItems (they are exactly the same as those in data.suicide)
data.goItems$sui001 <- NULL
data.goItems$sui002 <- NULL

#####################################################
### REMOVE REDUNDANT VARIABLES IN FREESURFER DATA ###
#####################################################

#These variables are identical in both freesufer files; only need to keep one copy
data.fs.sa$LThickness <- NULL
data.fs.sa$RThickness <- NULL
data.fs.sa$LSurfArea <- NULL
data.fs.sa$RSurfArea <- NULL
data.fs.sa$ICV <- NULL

#############################
### PREPARE JACOBIAN DATA ###
#############################

#Remove path to get scan ID only
data.jacobian[] <- lapply(data.jacobian, function(x) gsub("/cbica/projects/pncNmf/n1396_t1NMF/images/Jacobian_ReslicedDownsampledSmoothed2mm/", "", x))
data.jacobian[] <- lapply(data.jacobian, function(x) gsub("_SubjectToTemplateLogJacobian_isotropic2mm_smoothed2mm.nii.gz", "", x))

#Rename variables
colnames(data.jacobian) <- c("scanid","jacobian_Nmf18C1","jacobian_Nmf18C2","jacobian_Nmf18C3","jacobian_Nmf18C4","jacobian_Nmf18C5","jacobian_Nmf18C6","jacobian_Nmf18C7","jacobian_Nmf18C8","jacobian_Nmf18C9","jacobian_Nmf18C10","jacobian_Nmf18C11","jacobian_Nmf18C12","jacobian_Nmf18C13","jacobian_Nmf18C14","jacobian_Nmf18C15","jacobian_Nmf18C16","jacobian_Nmf18C17","jacobian_Nmf18C18")

#Make Jacobian variables numeric
data.jacobian <- data.frame(lapply(data.jacobian, function(x) as.numeric(as.character(x))))

#########################################
### PREPARE 18 RAVENS COMPONENTS DATA ###
#########################################

#Remove path to get scan ID only
data.18Ravens[] <- lapply(data.18Ravens, function(x) gsub("/cbica/projects/pncNmf/n1396_t1NMF/images/Ravens_smoothed8mm/", "", x))
data.18Ravens[] <- lapply(data.18Ravens, function(x) gsub("_RAVENS_2GM_2mm_smoothed8mm.nii.gz", "", x))

#Rename variables
colnames(data.18Ravens) <- c("scanid","Ravens_Nmf18C1","Ravens_Nmf18C2","Ravens_Nmf18C3","Ravens_Nmf18C4","Ravens_Nmf18C5","Ravens_Nmf18C6","Ravens_Nmf18C7","Ravens_Nmf18C8","Ravens_Nmf18C9","Ravens_Nmf18C10","Ravens_Nmf18C11","Ravens_Nmf18C12","Ravens_Nmf18C13","Ravens_Nmf18C14","Ravens_Nmf18C15","Ravens_Nmf18C16","Ravens_Nmf18C17","Ravens_Nmf18C18")

#Make Ravens variables numeric
data.18Ravens <- data.frame(lapply(data.18Ravens, function(x) as.numeric(as.character(x))))

###################################
### CREATE TOTAL LOBE VARIABLES ###
###################################

#For cortical thickness, AVERAGE the left and right hemispheres (ct units = mm)
data.ctLobes$mprage_jlfLobe_ct_Limbic_Lobe <- (data.ctLobes$mprage_jlfLobe_ct_L_Limbic_Lobe + data.ctLobes$mprage_jlfLobe_ct_R_Limbic_Lobe)/2
data.ctLobes$mprage_jlfLobe_ct_Insular_Lobe <- (data.ctLobes$mprage_jlfLobe_ct_L_Insular_Lobe + data.ctLobes$mprage_jlfLobe_ct_R_Insular_Lobe)/2
data.ctLobes$mprage_jlfLobe_ct_Frontal_Lobe <- (data.ctLobes$mprage_jlfLobe_ct_L_Frontal_Lobe + data.ctLobes$mprage_jlfLobe_ct_R_Frontal_Lobe)/2
data.ctLobes$mprage_jlfLobe_ct_Parietal_Lobe <- (data.ctLobes$mprage_jlfLobe_ct_L_Parietal_Lobe + data.ctLobes$mprage_jlfLobe_ct_R_Parietal_Lobe)/2
data.ctLobes$mprage_jlfLobe_ct_Occipital_Lobe <- (data.ctLobes$mprage_jlfLobe_ct_L_Occipital_Lobe + data.ctLobes$mprage_jlfLobe_ct_R_Occipital_Lobe)/2
data.ctLobes$mprage_jlfLobe_ct_Temporal_Lobe <- (data.ctLobes$mprage_jlfLobe_ct_L_Temporal_Lobe + data.ctLobes$mprage_jlfLobe_ct_R_Temporal_Lobe)/2

#For volume, SUM the left and right hemispheres and then divide by 1000 to change the units from cubic millimeters (mm3) to cubic centimeters (cc3); 1 cc3 = 1,000 mm3
data.volLobes$mprage_jlf_vol_Limbic_Lobe_WM <- (data.volLobes$mprage_jlf_vol_L_Limbic_Lobe_WM + data.volLobes$mprage_jlf_vol_R_Limbic_Lobe_WM)/1000
data.volLobes$mprage_jlf_vol_Insular_Lobe_WM <- (data.volLobes$mprage_jlf_vol_L_Insular_Lobe_WM + data.volLobes$mprage_jlf_vol_R_Insular_Lobe_WM)/1000
data.volLobes$mprage_jlf_vol_Frontal_Lobe_WM <- (data.volLobes$mprage_jlf_vol_L_Frontal_Lobe_WM + data.volLobes$mprage_jlf_vol_R_Frontal_Lobe_WM)/1000
data.volLobes$mprage_jlf_vol_Parietal_Lobe_WM <- (data.volLobes$mprage_jlf_vol_L_Parietal_Lobe_WM + data.volLobes$mprage_jlf_vol_R_Parietal_Lobe_WM)/1000
data.volLobes$mprage_jlf_vol_Occipital_Lobe_WM <- (data.volLobes$mprage_jlf_vol_L_Occipital_Lobe_WM + data.volLobes$mprage_jlf_vol_R_Occipital_Lobe_WM)/1000
data.volLobes$mprage_jlf_vol_Temporal_Lobe_WM <- (data.volLobes$mprage_jlf_vol_L_Temporal_Lobe_WM + data.volLobes$mprage_jlf_vol_R_Temporal_Lobe_WM)/1000

##################
### MERGE DATA ###
##################
dataMerge1 <-merge(data.demo,data.diag, by=c("bblid","scanid"), all=TRUE) 
dataMerge2 <-merge(dataMerge1,data.psychosis, by=c("bblid","scanid"), all=TRUE) 
dataMerge3 <-merge(dataMerge2,data.suicide, by=c("bblid","scanid"), all=TRUE)
dataMerge4 <-merge(dataMerge3,data.goItems, by=c("bblid","scanid"), all=TRUE)
dataMerge5 <-merge(dataMerge4,data.bifactors, by=c("bblid","scanid"), all=TRUE)
dataMerge6 <-merge(dataMerge5,data.corrTraits, by=c("bblid","scanid"), all=TRUE)
dataMerge7 <-merge(dataMerge6,data.corrTraits_sr, by=c("bblid","scanid") ,all=TRUE)
dataMerge8 <-merge(dataMerge7,data.stai, by=c("bblid","scanid"), all=TRUE) 
dataMerge9 <-merge(dataMerge8,data.cogFactors, by=c("bblid","scanid"), all=TRUE)
dataMerge10 <-merge(dataMerge9,data.cogZscores, by=c("bblid","scanid"), all=TRUE)
dataMerge11 <-merge(dataMerge10,data.wrat, by=c("bblid","scanid"), all=TRUE)
dataMerge12 <-merge(dataMerge11,data.envir, by=c("bblid","scanid"), all=TRUE)
dataMerge13 <-merge(dataMerge12,data.healthExclude, by=c("bblid","scanid"), all=TRUE)
dataMerge14 <-merge(dataMerge13,data.t1QA, by=c("bblid","scanid"), all=TRUE)
dataMerge15 <-merge(dataMerge14,data.aslQA, by=c("bblid","scanid"), all=TRUE)
dataMerge16 <-merge(dataMerge15,data.restQA, by=c("bblid","scanid"), all=TRUE)
dataMerge17 <- merge(dataMerge16,data.CtNMF, by=c("bblid","scanid"), all=TRUE)
dataMerge18 <- merge(dataMerge17,data.RavensNMF, by=c("bblid","scanid"), all=TRUE)
dataMerge19 <- merge(dataMerge18,data.ct, by=c("bblid","scanid"), all=TRUE)
dataMerge20 <- merge(dataMerge19,data.vol, by=c("bblid","scanid"), all=TRUE)
dataMerge21 <- merge(dataMerge20,data.ctLobes, by=c("bblid","scanid"), all=TRUE)
dataMerge22 <- merge(dataMerge21,data.volLobes, by=c("bblid","scanid"), all=TRUE)
dataMerge23 <- merge(dataMerge22,data.tbv, by=c("bblid","scanid"), all=TRUE)
dataMerge24 <- merge(dataMerge23,data.asl, by=c("bblid","scanid"), all=TRUE)
dataMerge25 <- merge(dataMerge24,data.aslLobes, by=c("bblid","scanid"), all=TRUE)
dataMerge26 <- merge(dataMerge25,data.rest.alff, by=c("bblid","scanid"), all=TRUE)
dataMerge27 <- merge(dataMerge26,data.rest.reho, by=c("bblid","scanid"), all=TRUE)
dataMerge28 <- merge(dataMerge27,data.fs.sa, by=c("bblid","scanid"), all=TRUE)
dataMerge29 <- merge(dataMerge28,data.fs.ct, by=c("bblid","scanid"), all=TRUE)
dataMerge30 <- merge(dataMerge29,data.jacobian, by="scanid", all=TRUE)
dataMerge31 <- merge(dataMerge30,data.18Ravens, by="scanid", all=TRUE)

#Retain only the 1601 bblids (demographics has 1629)
data.n1601 <- dataMerge31[match(data.t1QA$bblid, dataMerge31$bblid, nomatch=0),] 

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

##Count the number missing clinical data
#Included: n=1394; Excluded: n=2
data.exclude$ACROSS.INCLUDE.clinical <- 1
data.exclude$ACROSS.INCLUDE.clinical[is.na(data.exclude$overall_psychopathology_4factorv2)] <- 0
clinical.include<-sum(data.exclude$ACROSS.INCLUDE.clinical)
clinical.exclude<-1396-clinical.include

#Exclude those missing clinical data
subjData <- data.exclude[!is.na(data.exclude$overall_psychopathology_4factorv2),]

#Check that number of subjects = 1394
n_final <- nrow(subjData)

##Count the number taking psychotropic psychiatric medications
#Included: n=1239; Excluded for meds: n=155
subjData$ACROSS.INCLUDE.psychMeds <- 1
subjData$ACROSS.INCLUDE.psychMeds[subjData$psychoactiveMedPsychv2==1] <- 0
psychMeds.include<-sum(subjData$ACROSS.INCLUDE.psychMeds)
psychMeds.exclude<-1394-psychMeds.include


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

#######################################################
### CREATE SUMMARY DIAGNOSTIC VARIABLES FOR FIGURES ###
#######################################################

#Anxious-misery disorders
subjData$GadMdd <- NA
subjData$GadMdd[which(subjData$Gad==1 | subjData$Mdd==1)] <- 1
subjData$GadMdd[which(subjData$Td==1)] <- 0

#Psychotic disorders
subjData$Psychosis <- subjData$Ps

#Behavioral disorders
subjData$AddConOdd <- NA
subjData$AddConOdd[which(subjData$Add==1 | subjData$Con==1 | subjData$Odd==1)] <- 1
subjData$AddConOdd[which(subjData$Td==1)] <- 0

#Fear disorders
subjData$AgrPtdSepSocSph <- NA
subjData$AgrPtdSepSocSph[which(subjData$Agr==1 | subjData$Ptd==1 | subjData$Sep==1 |subjData$Soc==1 | subjData$Sph==1)] <- 1
subjData$AgrPtdSepSocSph[which(subjData$Td==1)] <- 0

#All disorders
subjData$AllDiag <- 1
subjData$AllDiag[which(subjData$Td==1)] <- 0

#################
### SAVE DATA ###
#################

saveRDS(subjData,"/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

############################
### SENSITIVITY ANALYSES ###
############################

#Exclude those who were on psychiatric medications (excluded n=155)
data.sensitivity <- subjData[which(subjData$ACROSS.INCLUDE.psychMeds==1),]

#Total sample size after excluding those on psychiatric meds (n=1239)
n_NoPsychMeds <- nrow(data.sensitivity)

#Save sensitivity dataset
saveRDS(data.sensitivity,"/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1239_T1_subjData_NoPsychMeds.rds")

#Count the number missing medu1 from sensitivity dataset.
#Included: n=1226; Excluded: n=13
data.sensitivity$ACROSS.INCLUDE.medu <- 1
data.sensitivity$ACROSS.INCLUDE.medu[is.na(data.sensitivity$medu1)] <- 0
medu.include<-sum(data.sensitivity$ACROSS.INCLUDE.medu)
medu.exclude<-1239-medu.include
