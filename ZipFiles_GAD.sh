#Zip only select files (TD and GAD separately) for the ENIGMA GAD project.

#!/bin/bash

cat /data/jux/BBL/projects/enigmaAnxiety/subjectData/GAD_bblids_scanids.csv | while IFS="," read -r a b ;
do
                                                                                                  
dir=`ls -d /data/joy/BBL/studies/pnc/rawData/${a}/*x${b}/mprage/${a}_*x${b}_t1.nii.gz`;

for i in $dir; do

        echo $dir >> /data/jux/BBL/projects/enigmaAnxiety/subjectData/GADpaths.csv

done;

done

tar -cvzf /data/jux/BBL/projects/enigmaAnxiety/raw/PNC_GAD.tar.gz -T /data/jux/BBL/projects/enigmaAnxiety/subjectData/GADpaths.csv

