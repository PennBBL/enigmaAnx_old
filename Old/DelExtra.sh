cat /data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_enigmaAnx_bblids.csv
paste -s -d : /data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_enigmaAnx_bblids.csv
GLOBIGNORE=$(paste -s -d : /data/joy/BBL/projects/enigmaAnxiety/subjectData/n690_enigmaAnx_bblids.csv)
rm -r -- *

