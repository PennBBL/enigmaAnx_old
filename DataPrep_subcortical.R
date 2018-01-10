###################################################
#### Prepare Subject-Level Data For Histograms ####
###################################################

#Read in subcortical file
subjData <- read.csv("/data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_anxiety_subCorticalVals.csv", header = TRUE)

##Read in Pan or Td file
subjData <- read.csv("/data/joy/BBL/projects/enigmaAnxiety/figures/pan/LandRvolumes.csv", header = TRUE)
subjData <- read.csv("/data/joy/BBL/projects/enigmaAnxiety/figures/td/LandRvolumes.csv", header = TRUE)

#Read in anxiety variables file (this has the data regarding patient vs. control)
anxData <- readRDS("/data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_enigmaAnx_subjData.rds")

##########################################################################
#### Pull the bblids and dataxscanid variables for a list of subjects ####
##########################################################################
ids <- subjData[c(grep("bblid|datexscanid", names(subjData)))]

write.table(ids, file="/data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_enigmaAnx_bblids_datexscanid.csv", row.names=F, col.names=F, quote=F)

#####################################
#### Pull the relevant variables ####
#####################################

#Subset to only subcortical regions needed for analyses
subjData_short <- subjData[c(grep("bblid|Left.Lateral.Ventricle|Right.Lateral.Ventricle|Left.Thalamus.Proper|Right.Thalamus.Proper|Left.Caudate|Right.Caudate|Left.Putamen|Right.Putamen|Left.Pallidum|Right.Pallidum|Left.Hippocampus|Right.Hippocampus|Left.Amygdala|Right.Amygdala|Left.Accumbens.area|Right.Accumbens.area|EstimatedTotalIntraCranialVol",names(subjData)))]

#Create a variable that will designate patient vs. control
anxData$PanTd <- NA
anxData$PanTd[anxData$Pan==1] <- 1
anxData$PanTd[is.na(anxData$PanTd)] <- 0

#Subset the anxiety data file to only the column designating panic disorder patient vs. control (and keep bblids for merging)
anxData_short <- anxData[c(grep("bblid|PanTd",names(anxData)))]

#Merge data
dataMerge <-merge(subjData_short,anxData_short, by="bblid")

#Change names
library(data.table)
setnames(subjData_short, old = c('bblid','Left.Lateral.Ventricle','Right.Lateral.Ventricle','Left.Thalamus.Proper','Right.Thalamus.Proper','Left.Caudate','Right.Caudate','Left.Putamen','Right.Putamen','Left.Pallidum','Right.Pallidum','Left.Hippocampus','Right.Hippocampus','Left.Amygdala','Right.Amygdala','Left.Accumbens.area','Right.Accumbens.area','EstimatedTotalIntraCranialVol'), new = c('SubjID','LLatVent','RLatVent','Lthal','Rthal','Lcaud','Rcaud','Lput','Rput','Lpal','Rpal','Lhippo','Rhippo','Lamyg','Ramyg','Laccumb','Raccumb','ICV'))

#Reorder the columns
subjData_short<-subjData_short[c(1,2,10,3,11,4,12,5,13,6,14,7,15,8,16,9,17,18)]

#Save .csv
write.csv(dataMerge, file="/data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_anxiety_subCorticalVals_short.csv", row.names=F, quote=F)

## save .csv for Pan or td subgroup
write.csv(subjData_short, file="/data/joy/BBL/projects/enigmaAnxiety/figures/pan/LandRvolumes.csv", row.names=F, quote=F)
write.csv(subjData_short, file="/data/joy/BBL/projects/enigmaAnxiety/figures/td/LandRvolumes.csv", row.names=F, quote=F)

#########################
#### Split the file #####
#########################

#Subset to just the panic group (n=14)
PanData <- dataMerge
PanData <- dataMerge[which(dataMerge$PanTd == 1), ]

write.csv(PanData, file="/data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_anxiety_subCorticalVals_short_Pan.csv", row.names=F, quote=F)

#Subset to just the panic group (n=14)
TdData <- dataMerge
TdData <- dataMerge[which(dataMerge$PanTd == 0), ]

write.csv(TdData, file="/data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_anxiety_subCorticalVals_short_Td.csv", row.names=F, quote=F)
