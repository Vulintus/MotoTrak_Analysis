function Deploy_MotoTrak_Analysis

%
%Deploy_MotoTrak_V1p1.m - Vulintus, Inc., 2015
%
%   Deploy_MotoTrak_V1p1 collates all of the *.m file dependencies for the
%   MotoTrak program into a single *.m file and creates time-stamped
%   back-up copies of each file when a file modification is detected. It
%   will offer to compile an executable of the program, and will then
%   bundle the executable with associated deployment files and will
%   automatically upload that zip file to the Vulintus download page.
%
%   UPDATE LOG:
%   01/13/2016 - Drew Sloan - Ensured backup location was always in the
%       same directory as the 'MotoTrak_Startup.m' initialization script.
%   04/28/2016 - Drew Sloan - Renamed MotoTrak 2.0 to MotoTrak 1.1 to limit
%       confusion with new C-code 2.0 version.
%   08/08/2016 - Drew Sloan - Transfered most functionality to the new
%       generalized Vulintus_Collate_Functions.m script.

cur_ver = 1.19;                                                             %Specify the current program version.

start_script = 'MotoTrak_Analysis_Startup.m';                               %Set the expected name of the initialization script.
ver_str = num2str(cur_ver,'v%1.2f');                                        %Convert the version number to a string.
ver_str(ver_str == '.') = 'p';                                              %Replace the period in the version string with a lower-case "p".
collated_filename = sprintf('MotoTrak_Analysis_%s.m',ver_str);              %Set the name for the collated script.
update_url = ['https://docs.google.com/document/d/1m9hmhpy5GpY1U0eX8GM7'...
    'Y3WiOQgyypnaLRQzMCayzdo/pub'];                                         %Specify the URL where program updates will be described.
web_file = 'mototrak_analysis_updates.html';                                %Specify the name of the updates HTML page.

[collated_file, zip_file] = ...
    Vulintus_Collate_Functions(start_script, collated_filename);            %Call the generalized function-collating script.

% Vulintus_Upload_File(collated_file, 'public_html/downloads/');              %Upload the collated file to the Vulintus downloads page.
% Vulintus_Upload_File(zip_file, 'public_html/downloads/');                   %Upload the zipped functions file to the Vulintus downloads page.

temp = which(start_script);                                                 %Grab the full path of the initialization script.
[installer_dir, ~, ~] = fileparts(temp);                                    %Keep only the path.
installer_dir = fullfile(installer_dir,'Web Installer');                    %Add the "Web Installer" subfolder to the path.
compile_time = 'na';                                                        %Assume the web installer compile time can't be determined by default.
if exist(installer_dir,'dir')                                               %If the web installer directory exists...
    file = fullfile(installer_dir,['MotoTrak_Analysis_Installer_' ...
        ver_str '_win64.exe']);                                             %Set the expected file name of the web isntaller.
    if exist(file,'file')                                                   %If the web installer exists...        
        info = dir(file);                                                   %Grab the file information.
        compile_time = info.date;                                           %Grab the last modified date for the web installer.
        [path, stamped_file, ~] = fileparts(file);                          %Grab the path and root filename.
        stamped_file = [stamped_file '_' datestr(compile_time,'yyyymmdd')]; %Add the compile time to the filename.
        stamped_file = fullfile(path, [stamped_file '.exe']);               %Create the full time-stamped filename.
        if ~exist(stamped_file,'file')                                      %If the timestamped copy doesn't yet exist...
            copyfile(file, stamped_file, 'f');                              %Copy the web installer to the timestamped copy.
        end
%         Vulintus_Upload_File(file, 'public_html/downloads/');             %Upload the MotoTrak Analysis Web Installer to the Vulintus downloads page.        
    else                                                                    %Otherwise...
        warning(['WARNING: Could not find the MotoTrak Analysis Web '...
            'Installer executable.']);                                      %Show a warning.
    end
else                                                                        %Otherwise...
    warning(['WARNING: Could not find the MotoTrak Analysis Web '...
        'Installer directory.']);                                           %Show a warning.
end

% web_file = [tempdir web_file];                                              %Create the updates HTML file in the temporary folder.
% fid = fopen(web_file,'wt');                                                 %Open the updates HTML file for writing as text.
% fprintf(fid,'<HTML><p>MOTOTRAK ANALYSIS</p><p>CURRENT VERSION: ');          %Write the HTML start tag to the file.
% fprintf(fid,'%1.2f</p><p>COMPILE TIME: ',cur_ver);                          %Write the the current program version to the file.
% fprintf(fid,'%s</p><p>UPDATE URL: ',compile_time);                          %Write the the installer compile time to the file.
% fprintf(fid,'%s</p></HTML>',update_url);                                    %Write the the update URL to the file.
% fclose(fid);                                                                %Close the HTML file.
% Vulintus_Upload_File(web_file, 'public_html/updates/');                     %Upload the updates HTML file to the Vulintus updates page.
% delete(web_file);                                                           %Delete the temporary HTML file.