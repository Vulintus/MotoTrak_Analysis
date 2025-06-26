function appdata_path = Vulintus_Set_AppData_Path(program)

%
%Vulintus_Set_AppData_Path.m - Vulintus, Inc.
%
%   This function finds and/or creates the local application data folder
%   for Vulintus functions specified by "program".
%   
%   UPDATE LOG:
%   2016-06-05 - Drew Sloan - Function created to replace within-function
%                             calls in multiple programs.
%   2025-02-03 - Drew Sloan - Moved the bulk of the code to a more
%                             generalized "Set_AppData_Local_Path" script.
%


appdata_path = Set_AppData_Local_Path('Vulintus',program);                  %Call the more general AppData path generator.

if strcmpi(program,'mototrak')                                              %If the specified function is MotoTrak.
    oldpath = Set_AppData_Local_Path('MotoTrak');                           %Create the expected name of the previous version appdata directory.
    if exist(oldpath,'dir')                                                 %If the previous version directory exists...
        files = dir(oldpath);                                               %Grab the list of items contained within the previous directory.
        for f = 1:length(files)                                             %Step through each item.
            if ~files(f).isdir                                             	%If the item isn't a directory...
                copyfile([oldpath, files(f).name],appdata_path,'f');        %Copy the file to the new directory.
            end
        end
        [status, msg] = rmdir(oldpath,'s');                                 %Delete the previous version appdata directory.
        if status ~= 1                                                      %If the directory couldn't be deleted...
            warning(['Unable to delete application data'...
                ' directory\n\n%s\n\nDetails:\n\n%s'],oldpath,msg);         %Show an warning.
        end
    end
end