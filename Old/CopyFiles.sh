#Copy the FS output to the enigmaAnxiety project folder.

#!/bin/bash

cat /data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_enigmaAnx_bblids_scanids.csv | while IFS="," read -r a b ;
do

if [ -d /data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/${b} ]; then

    ln -s /data/joy/BBL/studies/pnc/processedData/structural/freesurfer53/${b} /data/joy/BBL/projects/enigmaAnxiety/subjects
   
fi

done
