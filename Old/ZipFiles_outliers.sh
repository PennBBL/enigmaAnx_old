#Zip the png files for the bblids flagged as outliers.

#!/bin/bash

cat /data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd/outlier_bblids.csv | while IFS="" read -r a ;
do
                                                                                                  
dir=`ls -d /data/jux/BBL/projects/enigmaAnxiety/QC/${a}`;

for i in $dir; do

        echo $dir >> /data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd/outlier_paths.csv

done;

done

tar -cvzf /data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd/outlierPngs.tar.gz -T /data/jux/BBL/projects/enigmaAnxiety/figures/AllAnxTd/outlier_paths.csv

