#Move the "mri" folder up one level so the matlab script for creating the pngs will run.

#Input list of bblids and datexscanids
cat /data/jux/BBL/studies/enigmaAnxiety/subjectData/n1110_enigmaAnx_bblids_datexscanids.csv | while IFS="," read -r a b ;

do

#Define the original directory path
dir=`ls -d /data/jux/BBL/studies/enigmaAnxiety/SUBJECTDATA/freesurfer53/TD/${a}/${b}`;

for i in $dir; do

#Get new directory path: first echo the path (echo $i), then cut the path up by delimiter "/" (-d'/'), then take all fields (-f) except the last one which is the datexscanid (which we don't want).

        newPath=$(echo $i | cut -d'/' -f 1,2,3,4,5,6,7,8,9,10 )
        echo "New path is $newPath"
	
#Move the mri folder up one level

	mv $i/mri $newPath

done; 

done
