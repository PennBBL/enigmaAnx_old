#This script will extract the subcortical values for each subject from the Freesurfer output. 
#We do this for all anxiety and TD subjects initially and then subset to individual diagnoses later for specific projects. 

#!/bin/bash

cd /data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd

echo "SubjID,LLatVent,RLatVent,Lthal,Rthal,Lcaud,Rcaud,Lput,Rput,Lpal,Rpal,Lhippo,Rhippo,Lamyg,Ramyg,Laccumb,Raccumb,ICV" > LandRvolumes.csv

cat /data/jux/BBL/projects/enigmaAnxiety/subjectData/AllAnxTd_bblids_scanids.csv | while IFS="," read -r a b ;
do

for subj_id in $a; do
printf "%s,"  "${subj_id}" >> LandRvolumes.csv

for x in Left-Lateral-Ventricle Right-Lateral-Ventricle Left-Thalamus-Proper Right-Thalamus-Proper Left-Caudate Right-Caudate Left-Putamen Right-Putamen Left-Pallidum Right-Pallidum Left-Hippocampus Right-Hippocampus Left-Amygdala Right-Amygdala Left-Accumbens-area Right-Accumbens-area; do 
printf "%g," `grep  ${x} /data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/${subj_id}/*x${b}/stats/aseg.stats | awk '{print $4}'` >> LandRvolumes.csv
done 

printf "%g" `cat /data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/${subj_id}/*x${b}/stats/aseg.stats | grep IntraCranialVol | awk -F, '{print $4}'` >> LandRvolumes.csv
echo "" >> LandRvolumes.csv 
done


done

