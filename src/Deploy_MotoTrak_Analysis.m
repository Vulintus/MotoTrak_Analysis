function Deploy_MotoTrak_Analysis

%
%Deploy_MotoTrak_Analysis.m - Vulintus, Inc., 2015
%
%   DEPLOY_MOTOTRAK_ANALYSIS collates all of the *.m file dependencies for
%   the MotoTrak Analysis program into a single *.m file.
%
%   UPDATE LOG:
%   2016-01-13 - Drew Sloan - Ensured backup location was always in the
%       same directory as the 'MotoTrak_Startup.m' initialization script.
%   2016-04-28 - Drew Sloan - Renamed MotoTrak 2.0 to MotoTrak 1.1 to limit
%       confusion with new C-code 2.0 version.
%   2016-08-08 - Drew Sloan - Transfered most functionality to the new
%       generalized Vulintus_Collate_Functions.m script.
%   2023-03-22 - Drew Sloan - Renamed from "Deploy_MotoTrak_V1p1.m" to
%       "Deploy_MotoTrak_Analysis.m" to reflect version control through
%       GitHub. Removed automatic backup zip file creation.


start_script = 'MotoTrak_Analysis_Startup.m';                               %Set the expected name of the initialization script.
collated_filename = 'MotoTrak_Analysis.m';                                  %Set the name for the collated script.

release_path = which('Deploy_MotoTrak_Analysis.m');                         %Find the current path.
[release_path, cur_dir, ~] = fileparts(release_path);                       %Strip out the filename from the path to the collated file.
while ~strcmpi(cur_dir,'MotoTrak Analysis') && ~isempty(cur_dir)            %Loop until we get to the "\MotoTrak Analysis" folder.
    [release_path, cur_dir, ~] = fileparts(release_path);                   %Strip out the filename from the path.
end
release_path = fullfile(release_path,'MotoTrak Analysis');                  %Add the "\MotoTrak Analysis" directory back to the path.

collated_filename = fullfile(release_path,collated_filename);               %Add the release path to the collated filename.

Vulintus_Collate_Functions(start_script,collated_filename,...
    'depfunfolder','on');                                                   %Call the generalized function-collating script.

open(collated_filename);                                                    %Open the collated script.