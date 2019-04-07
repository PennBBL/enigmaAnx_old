##This script will extract subcortical volume values for ENIGMA Anxiety (this will be used to create the LandRvolumes.csv and to create histograms for QA)

#############################
#### Load and merge data ####
#############################

#Read in data
subjData <- readRDS("/data/jux/BBL/projects/enigmaAnxiety/subjectData/n1120_AllAnxTd_subjData.rds")
volume <- read.csv("/data/jux/BBL/projects/pncReproc2015/freesurfer/stats5_3/aseg.stats/n2416_aseg.stats.volume.csv", header=TRUE)

#Merge (this will only keep the 1120 anxiety and TD subjects from the n1601 sample)
mergeData <- merge(subjData,volume,by=c("bblid","scanid"))


#############################################
#### Extract the bblids and datexscanids ####
#############################################

#Pull the bblids and datexscanids for the script "MoveDirUp.sh"
IDnames <- c("bblid", "datexscanid")
IDs <- mergeData[IDnames]

#Remove header
names(IDs) <- NULL

#Save list
write.csv(IDs, file="/data/jux/BBL/studies/enigmaAnxiety/subjectData/n1110_enigmaAnx_bblids_datexscanids.csv", row.names=FALSE, quote=FALSE)


#####################################
#### Pull the relevant variables ####
#####################################

#Subset to only subcortical regions needed for the LandRvolumes.csv; also keep bblid and anxiety group variables for subsetting the data
subjData_short <- mergeData[c(grep("bblid|Left.Lateral.Ventricle|Right.Lateral.Ventricle|Left.Thalamus.Proper|Right.Thalamus.Proper|Left.Caudate|Right.Caudate|Left.Putamen|Right.Putamen|Left.Pallidum|Right.Pallidum|Left.Hippocampus|Right.Hippocampus|Left.Amygdala|Right.Amygdala|Left.Accumbens.area|Right.Accumbens.area|EstimatedTotalIntraCranialVol|^Agr$|^Gad$|^Ocd$|^Pan$|^Ptd$|^Sep$|^Soc$|^Sph$|^Td$",names(mergeData)))]

#Change names to match the enigma scripts
library(data.table)

setnames(subjData_short, old = c('bblid','Left.Lateral.Ventricle','Right.Lateral.Ventricle','Left.Thalamus.Proper','Right.Thalamus.Proper','Left.Caudate','Right.Caudate','Left.Putamen','Right.Putamen','Left.Pallidum','Right.Pallidum','Left.Hippocampus','Right.Hippocampus','Left.Amygdala','Right.Amygdala','Left.Accumbens.area','Right.Accumbens.area','EstimatedTotalIntraCranialVol'), new = c('SubjID','LLatVent','RLatVent','Lthal','Rthal','Lcaud','Rcaud','Lput','Rput','Lpal','Rpal','Lhippo','Rhippo','Lamyg','Ramyg','Laccumb','Raccumb','ICV'))

#Reorder the columns (columns need to be in a specific order for the enigma scripts)
subjData_ordered<-subjData_short[c(1,11,19,12,20,13,21,14,22,15,23,16,24,17,25,18,26,27,2:10)]


#################################
#### Save full n=1110 sample ####
#################################

#Remove the anxiety group variables in the master LandRvolumes.csv file
LandRvolumes <- within(subjData_ordered, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save the file (n=1110)
write.csv(LandRvolumes,"/data/jux/BBL/studies/enigmaAnxiety/figures/all/LandRvolumes.csv",row.names=FALSE, quote=F)


######################
#### Subset Panic ####
######################

#Subset to the Panic patients only
panic <- subjData_ordered[which(subjData_ordered$Pan == 1), ]

#Remove the anxiety group variables
LandRvolumes_pan <- within(panic, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=14)
write.csv(LandRvolumes_pan,"/data/jux/BBL/studies/enigmaAnxiety/figures/pan/LandRvolumes.csv",row.names=FALSE, quote=F)


###############################
#### Subset Social Anxiety ####
###############################

#Subset to the Social Anxiety patients only
socialAnx <- subjData_ordered[which(subjData_ordered$Soc == 1), ]

#Remove the anxiety group variables
LandRvolumes_soc <- within(socialAnx, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=325)
write.csv(LandRvolumes_soc,"/data/jux/BBL/studies/enigmaAnxiety/figures/soc/LandRvolumes.csv",row.names=FALSE, quote=F)


####################
#### Subset GAD ####
####################

#Subset to the GAD patients only
gad <- subjData_ordered[which(subjData_ordered$Gad == 1), ]

#Remove the anxiety group variables
LandRvolumes_gad <- within(gad, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=27)
write.csv(LandRvolumes_gad,"/data/jux/BBL/studies/enigmaAnxiety/figures/gad/LandRvolumes.csv",row.names=FALSE, quote=F)


################################
#### Subset Specific Phobia ####
################################

#Subset to the Specific Phobia patients only
specificPhobia <- subjData_ordered[which(subjData_ordered$Sph == 1), ]

#Remove the anxiety group variables
LandRvolumes_sph <- within(specificPhobia, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=424)
write.csv(LandRvolumes_sph,"/data/jux/BBL/studies/enigmaAnxiety/figures/sph/LandRvolumes.csv",row.names=FALSE, quote=F)


###################
#### Subset TD ####
###################

#Subset to the Typically Developing youth only
td <- subjData_ordered[which(subjData_ordered$Td == 1), ]

#Remove the anxiety group variables
LandRvolumes_td <- within(td, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=420)
write.csv(LandRvolumes_td,"/data/jux/BBL/studies/enigmaAnxiety/figures/td/LandRvolumes.csv",row.names=FALSE, quote=F)

