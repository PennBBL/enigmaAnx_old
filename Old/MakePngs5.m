%Create .png files for each subject for quality checking

%Specify the output directory where the png files will be saved
QC_output_directory='/data/jux/BBL/projects/enigmaAnxiety/QC';

%Specify the directory where the freesurfer output is located
FS_directory='/data/joy/BBL/studies/pnc/processedData/structural/freesurfer53';

%Read in the list of bblids and scanids
ID = csvread('/data/jux/BBL/projects/enigmaAnxiety/subjectData/AllAnxTd_bblids_scanids.csv');
%filename = '/data/jux/BBL/projects/enigmaAnxiety/subjectData/AllAnxTd_bblids_scanids.csv';

%Define a vector of bblids
bblid = ID(:,1,:);

%Define a vector of scanids
scanid = ID(:,end);

%for x = 1:size(a,1)
%	  [c,b,d]=fileparts(a(x,1).name); %b becomes the subject_name
%        try
%	  func_make_subcorticalFS_ENIGMA_QC(QC_output_directory, b, [FS_directory,'/', b, '/*/mri/orig.mgz'], [FS_directory,'/',b, '/*/mri/aparc+aseg.mgz']);
%        end
%	display(['Done with subject: ', b,': ',num2str(x-2), ' of ', num2str(size(a,1)-2)]);
%   end

%for i = 1:size(bblid,1)
    
%          [c,b,d]=fileparts(bblid(i,1).name); 
%	  display(['Done with subject: ', b,': ',num2str(i-2), ' of ', num2str(size(bblid)-2)]);

%end


%for i = 1:length(bblid)

%	  fprintf('%i |', bblid(i));
          
%end


for ii = 1:numel(bblid)
           b = bblid(ii)
	   display([FS_directory,'/', b, '/mri/orig.mgz']);
           
end

%fid = fopen(filename);
%x = textscan(fid,'%s %s')
%  st = x{1,1};
%[stunq,idx,idy] = unique(x{1,1});
%dmg = x{1,2};
%for i = 1:length(idy)%
%	  display(['Done with subject: ', st]);
%  end

%Select only folders that contain FS output
%a=dir(char(strcat(FS_directory,'/*')));
%   for x = 1:size(a,1)
%        [c,b,d]=fileparts(a(x,1).name); %b becomes the subject_name
%        try
%        func_make_subcorticalFS_ENIGMA_QC(QC_output_directory, b, [FS_directory,'/', b, '/*/mri/orig.mgz'], [FS_directory,'/',b, '/*/mri/aparc+aseg.mgz']);
%        end
%   display(['Done with subject: ', b,': ',num2str(x-2), ' of ', num2str(size(a,1)-2)]);
%   end
