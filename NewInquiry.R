##Use this script to fill out the excel "ENIGMA_ANX_overviewsites_final.xlsx" for inquiries about new ENIGMA projects.
##Run with source("NewInquiry.R", echo=TRUE) to see output

###################
#### LOAD DATA ####
###################

#All anxiety disorders and TD sample
subjData <- readRDS("/data/jux/BBL/projects/enigmaAnxiety/subjectData/n1120_AllAnxTd_subjData.rds")


#####################################
#### N FOR EACH ANXIETY DISORDER ####
#####################################

#Frequency of each anxiety disorder
Agr_freq <- sum(subjData$Agr, na.rm=TRUE)
Agr_freq

Gad_freq <- sum(subjData$Gad, na.rm=TRUE)
Gad_freq

Ocd_freq <- sum(subjData$Ocd, na.rm=TRUE)
Ocd_freq

Pan_freq <- sum(subjData$Pan, na.rm=TRUE)
Pan_freq

Ptd_freq <- sum(subjData$Ptd, na.rm=TRUE)
Ptd_freq

Sep_freq <- sum(subjData$Sep, na.rm=TRUE)
Sep_freq

Soc_freq <- sum(subjData$Soc, na.rm=TRUE)
Soc_freq

Sph_freq <- sum(subjData$Sph, na.rm=TRUE)
Sph_freq


##########################################
#### N FOR TYPICALLY DEVELOPING YOUTH ####
##########################################

Td_freq <- sum(subjData$Td, na.rm=TRUE)
Td_freq


###################################
#### N WITH COMORBID DIAGNOSES ####
###################################

#Determine how many of the anxiety subjects have more than 1 diagnosis
dxNames <- c("SubjID","Add","Agr","Ano","Bul","Con","Gad","Man","Mdd","Ocd","Odd","Pan","Ps","Ptd","Sep","Soc","Sph")
dxDf <- data.matrix(subjData[,dxNames])
subjData$totDx <- rowSums(dxDf[,2:17], na.rm=TRUE)

#One diagnosis
oneDx <- sum(subjData$totDx==1)
oneDx

#Two or more diagnoses
twoOrMoreDx <- sum(subjData$totDx>=2)
twoOrMoreDx


#############################################
#### FREQUENCIES OF COMMON COMORBIDITIES ####
#############################################

#Frequency of each comorbid disorder
Add_freq <- sum(subjData$Add, na.rm=TRUE)
Add_freq

Ano_freq <- sum(subjData$Ano, na.rm=TRUE)
Ano_freq

Bul_freq <- sum(subjData$Bul, na.rm=TRUE)
Bul_freq

Con_freq <- sum(subjData$Con, na.rm=TRUE)
Con_freq

Mdd_freq <- sum(subjData$Mdd, na.rm=TRUE)
Mdd_freq

Man_freq <- sum(subjData$Man, na.rm=TRUE)
Man_freq

Odd_freq <- sum(subjData$Odd, na.rm=TRUE)
Odd_freq

Ps_freq <- sum(subjData$Ps, na.rm=TRUE)
Ps_freq


###############################
#### N ON PSYCHIATRIC MEDS ####
###############################

#Determine how many subjects were on psychiatric meds at the time of imaging
n_Meds <- sum(subjData$Medicated)
n_Meds


##########################################
#### COMMON PSYCHIATRIC MEDS REPORTED ####
##########################################

antiPsy <- sum(subjData$medclass_Antipsychotic)
antiPsy

antiCon <- sum(subjData$medclass_Anticonvulsant)
antiCon

antiDep <- sum(subjData$medclass_Antidepressant)
antiDep

benzo <- sum(subjData$medclass_Benzodiazepine)
benzo

stim <- sum(subjData$medclass_Stimulant)
stim

nonStimADHD <- sum(subjData$medclass_NonstimulantADHDmed)
nonStimADHD

Lithium <- sum(subjData$medclass_Lithium)
Lithium

Other <- sum(subjData$medclass_Other)
Other
