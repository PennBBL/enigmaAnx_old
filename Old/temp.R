subjData <- readRDS("/data/jux/BBL/projects/enigmaAnxiety/subjectData/n1120_AllAnxTd_subjData.rds")
healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)

healthExclude$SubjID <- healthExclude$bblid

AllAnxSubjData <- merge(subjData, healthExclude, by="SubjID")
nrow(AllAnxSubjData)

subjData_sensitivity <- AllAnxSubjData
subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS <- 1
subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS[subjData_sensitivity$psychoactiveMedPsychv2==1] <- 0
NoMeds <- sum(subjData_sensitivity$ACROSS.INCLUDE.PSYCMEDS)
Meds <- 1120-NoMeds
