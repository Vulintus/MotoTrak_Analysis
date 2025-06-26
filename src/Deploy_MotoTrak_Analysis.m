function Deploy_MotoTrak_Analysis

%
% Deploy_MotoTrak_Analysis.m
% 
%   copyright 2015, Vulintus, Inc.
%
%   DEPLOY_MOTOTRAK_ANALYSIS collates all of the *.m file dependencies for
%   the MotoTrak Analysis program into a single *.m file.
%
%   UPDATE LOG:
%   2016-01-13 - Drew Sloan - Ensured backup location was always in the
%                             same directory as the 'MotoTrak_Startup.m' 
%                             initialization script.
%   2016-04-28 - Drew Sloan - Renamed MotoTrak 2.0 to MotoTrak 1.1 to limit
%                             confusion with new C-code 2.0 version.
%   2016-08-08 - Drew Sloan - Transfered most functionality to the new
%                             generalized Vulintus_Collate_Functions.m
%                             script.
%   2023-03-22 - Drew Sloan - Renamed from "Deploy_MotoTrak_V1p1.m" to
%                             "Deploy_MotoTrak_Analysis.m" to reflect 
%                             version control through GitHub. Removed 
%                             automatic backup zip file creation.
%


start_script = 'MotoTrak_Analysis_Startup.m';                               %Set the expected name of the initialization script.
collated_filename = 'MotoTrak_Analysis.m';                                  %Set the name for the collated script.

temp = which(start_script);                                                 %Find the location of the start script.
[path, ~, ~] = fileparts(temp);                                             %Grab the parts of the collated file.
path(strfind(path,'\src'):end) = [];                                        %Find the parent directory of the "src" folder.
collated_filename = fullfile(path,collated_filename);                       %Add the parent direction to the collated script.

[collated_file, ~] = Vulintus_Collate_Functions(start_script,...
    collated_filename,...
    'DepFunFolder','on',...
    'RemoveOrphans','on');                                                  %Call the generalized function-collating script.

open(collated_file);                                                        %Open the newly collated *.m file.