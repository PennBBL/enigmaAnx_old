##This script will set the subcortical values to NA for the subjects previously flagged with bad Freesurfer data (originally determined by Megan Quarmely and reproduced by Lauren Beard)

###################
#### LOAD DATA ####
###################

#Read in data
volume <- read.csv("/data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd/LandRvolumes.csv", header=TRUE)
fsExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header=TRUE, na.strings=".")

###################################
#### RENAME AND PULL VARIABLES ####
###################################

#Rename bblid
fsExclude$SubjID <- fsExclude$bblid

#Subset to only the variables needed
fsExclude_short <- fsExclude[c(grep("SubjID|fsFinalExclude",names(fsExclude)))]

####################
#### Merge data ####
####################

subjData <- merge(volume,fsExclude_short,by="SubjID",all=FALSE)

############################
#### SET BAD DATA TO NA ####
############################

subjData$LLatVent <- ifelse(subjData$fsFinalExclude == 0, subjData$LLatVent, NA)
subjData$RLatVent <- ifelse(subjData$fsFinalExclude == 0, subjData$RLatVent, NA)
subjData$Lthal <- ifelse(subjData$fsFinalExclude == 0, subjData$Lthal, NA)
subjData$Rthal <- ifelse(subjData$fsFinalExclude == 0, subjData$Rthal, NA)
subjData$Lcaud <- ifelse(subjData$fsFinalExclude == 0, subjData$Lcaud, NA)
subjData$Rcaud <- ifelse(subjData$fsFinalExclude == 0, subjData$Rcaud, NA)
subjData$Lput <- ifelse(subjData$fsFinalExclude == 0, subjData$Lput, NA)
subjData$Rput <- ifelse(subjData$fsFinalExclude == 0, subjData$Rput, NA)
subjData$Lpal <- ifelse(subjData$fsFinalExclude == 0, subjData$Lpal, NA)
subjData$Rpal <- ifelse(subjData$fsFinalExclude == 0, subjData$Rpal, NA)
subjData$Lhippo <- ifelse(subjData$fsFinalExclude == 0, subjData$Lhippo, NA)
subjData$Rhippo <- ifelse(subjData$fsFinalExclude == 0, subjData$Rhippo, NA)
subjData$Lamyg <- ifelse(subjData$fsFinalExclude == 0, subjData$Lamyg, NA)
subjData$Ramyg <- ifelse(subjData$fsFinalExclude == 0, subjData$Ramyg, NA)
subjData$Laccumb <- ifelse(subjData$fsFinalExclude == 0, subjData$Laccumb, NA)
subjData$Raccumb <- ifelse(subjData$fsFinalExclude == 0, subjData$Raccumb, NA)
subjData$ICV <- ifelse(subjData$fsFinalExclude == 0, subjData$ICV, NA)

###################
#### SAVE FILE ####
###################

#Remove fsFinalExclude
subjData$fsFinalExclude <- NULL

#Save .csv
write.csv(subjData,"/data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd_QA/fsFinalExclude/LandRvolumes.csv",row.names=FALSE, quote=F)
