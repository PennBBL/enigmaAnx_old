##This script will update the LandRvolumes.csv to include the QA performed by Sophia Park. 

###################
#### LOAD DATA ####
###################

library(plyr)

#Read in data
volume <- read.csv("/data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd_QA/fsFinalExclude/LandRvolumes.csv", header=TRUE)
SophiaQA <- read.csv("/data/jux/BBL/projects/enigmaAnxiety/figures/Old/allQA/LandRvolumes.csv", header=TRUE)

###################################
#### RENAME AND PULL VARIABLES ####
###################################

#Rename ICV
SophiaQA <- rename(SophiaQA, c("ICV" = "ICV_old"))

#Subset to only the variables needed
volume_short <- volume[c(grep("SubjID|ICV",names(volume)))]

####################
#### Merge data ####
####################

#all=TRUE to keep all subjects
subjData <- merge(SophiaQA,volume_short,by="SubjID",all=TRUE)

#Retain only the 1120 sample
subjData <- subjData[match(volume$SubjID, subjData$SubjID, nomatch=0),]

############################
#### SET BAD DATA TO NA ####
############################

#If Sophia said ICV was bad in the old ICV variable, mark it bad in the new ICV variable.
#Note: we are using a new ICV variable because re-extracting the data during replication resulted in slightly different ICV values. 
subjData$ICV <- ifelse(subjData$ICV_old >= 0, subjData$ICV, NA)

###################
#### SAVE FILE ####
###################

#Remove ICV_old
subjData$ICV_old <- NULL

#Save .csv
write.csv(subjData,"/data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd_QA/LandRvolumes.csv",row.names=FALSE, quote=F)
