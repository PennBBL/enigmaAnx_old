%Create .png files for each subject for quality checking

%Specify the output directory where the png files will be saved
QC_output_directory='/data/jux/BBL/projects/enigmaAnxiety/QC';

%Specify the directory where the freesurfer output is located
FS_directory='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53';

%Select only folders that contain FS output
a=dir(char(strcat(FS_directory,'/*')));
   for x = 1:size(a,1)
        [c,b,d]=fileparts(a(x,1).name); %b becomes the subject_name
        try
        display([FS_directory,'/', b,'/','*','/mri/orig.mgz']);
        end
   %display(['Done with subject: ', b,': ',num2str(x-2), ' of ', num2str(size(a,1)-2)]);
   end
