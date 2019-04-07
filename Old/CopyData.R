#Copy the FS output to the enigmaAnxiety project folder. This is done because we need to change the directory levels to get the matlab script to work for creating png files for the quality check. 

#!/bin/bash

cat /data/jux/BBL/studies/enigmaAnxiety/subjectData/n1110_enigmaAnx_bblids_scanids.csv | while IFS="," read -r a b ;
do

if [ -d /data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/${a} ]; then

    echo ${a}

    cp -R /data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/${a} /data/jux/BBL/studies/enigmaAnxiety/SUBJECTDATA/freesurfer53

fi

done
