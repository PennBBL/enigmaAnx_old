%Create .png files for each subject for quality checking

%Specify the output directory where the png files will be saved
QC_output_directory='/data/jux/BBL/projects/enigmaAnxiety/QC';

%Specify the directory where the freesurfer output is located
FS_directory='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53';

%List of bblids
%subject = [100031 100050];
subject = readmatrix('/data/jux/BBL/projects/enigmaAnxiety/subjectData/GAD_bblids_scanids.csv')


for i = 1:(length(subject))

    disp(subject(i));
    
    
end
