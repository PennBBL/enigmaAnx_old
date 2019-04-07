%Create .png files for each subject for quality checking

%Addpaths
addpath /data/jux/BBL/projects/enigmaAnxiety/scripts/enigmaAnxiety/ENIGMA_QC

%Specify the output directory where the png files will be saved
QC_output_directory='/data/jux/BBL/projects/enigmaAnxiety/QC';

%Specify the directory where the freesurfer output is located
FS_directory='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53';

%Read in the list of bblids and scanids
ID = csvread('/data/jux/BBL/projects/enigmaAnxiety/subjectData/AllAnxTd_bblids_scanids.csv');

%Define a vector of bblids
bblid = ID(:,1);

%Define a vector of scanids
scanid = ID(:,2);

%Run the function for each subject
for i = 1:length(bblid)
    try
       imageF = g_ls([FS_directory,'/',num2str(bblid(i)),'/','*x',num2str(scanid(i)),'/mri/orig.mgz']);
       overlay = g_ls([FS_directory,'/',num2str(bblid(i)),'/','*x',num2str(scanid(i)),'/mri/aparc+aseg.mgz']); 
       func_make_subcorticalFS_ENIGMA_QC(QC_output_directory, num2str(bblid(i)), imageF{1}, overlay{1});
    end
    display(['Done with subject: ', num2str(bblid(i))]);
end

