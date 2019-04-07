##This script will create subsets of the LandRvolumes.csv for each anxiety diagnosis, which will be used to create histograms for QA.

#############################
#### Load and merge data ####
#############################

#Read in data
subjData <- readRDS("/data/jux/BBL/projects/enigmaAnxiety/subjectData/n1120_AllAnxTd_subjData.rds")
volume <- read.csv("/data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd/LandRvolumes.csv", header=TRUE)

#Merge data
mergeData <- merge(subjData,volume,by="SubjID")


#####################################
#### Pull the relevant variables ####
#####################################

#Subset to only SubjID, the subcortical regions, and the anxiety group variables for subsetting the data
subjData_short <- mergeData[c(grep("SubjID|LLatVent|RLatVent|Lthal|Rthal|Lcaud|Rcaud|Lput|Rput|Lpal|Rpal|Lhippo|Rhippo|Lamyg|Ramyg|Laccumb|Raccumb|ICV|^Agr$|^Gad$|^Ocd$|^Pan$|^Ptd$|^Sep$|^Soc$|^Sph$|^Td$",names(mergeData)))]


######################
#### Subset Panic ####
######################

#Subset to the Panic patients only
panic <- subjData_short[which(subjData_short$Pan == 1), ]

#Remove the anxiety group variables
LandRvolumes_pan <- within(panic, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=14)
write.csv(LandRvolumes_pan,"/data/jux/BBL/projects/enigmaAnxiety/figures/Pan/LandRvolumes.csv",row.names=FALSE, quote=F)


###############################
#### Subset Social Anxiety ####
###############################

#Subset to the Social Anxiety patients only
socialAnx <- subjData_short[which(subjData_short$Soc == 1), ]

#Remove the anxiety group variables
LandRvolumes_soc <- within(socialAnx, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=328)
write.csv(LandRvolumes_soc,"/data/jux/BBL/projects/enigmaAnxiety/figures/Soc/LandRvolumes.csv",row.names=FALSE, quote=F)


####################
#### Subset GAD ####
####################

#Subset to the GAD patients only
gad <- subjData_short[which(subjData_short$Gad == 1), ]

#Remove the anxiety group variables
LandRvolumes_gad <- within(gad, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=27)
write.csv(LandRvolumes_gad,"/data/jux/BBL/projects/enigmaAnxiety/figures/Gad/LandRvolumes.csv",row.names=FALSE, quote=F)


################################
#### Subset Specific Phobia ####
################################

#Subset to the Specific Phobia patients only
specificPhobia <- subjData_short[which(subjData_short$Sph == 1), ]

#Remove the anxiety group variables
LandRvolumes_sph <- within(specificPhobia, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=426)
write.csv(LandRvolumes_sph,"/data/jux/BBL/projects/enigmaAnxiety/figures/Sph/LandRvolumes.csv",row.names=FALSE, quote=F)


###################
#### Subset TD ####
###################

#Subset to the Typically Developing youth only
td <- subjData_short[which(subjData_short$Td == 1), ]

#Remove the anxiety group variables
LandRvolumes_td <- within(td, rm(Agr,Gad,Ocd,Pan,Ptd,Sep,Soc,Sph,Td))

#Save file (n=428)
write.csv(LandRvolumes_td,"/data/jux/BBL/projects/enigmaAnxiety/figures/Td/LandRvolumes.csv",row.names=FALSE, quote=F)

