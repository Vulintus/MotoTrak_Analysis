function appdata_path = Set_AppData_Local_Path(varargin)

%
% Set_AppData_Local_Path.m
% 
%   copyright 2025, Vulintus, Inc.
%
%   SET_APPDATA_LOCAL_PATH finds and/or creates the the local application 
%   data folder and any subfolders specified in the variable input
%   arguments.
%   
%   UPDATE LOG:
%   2025-02-03 - Drew Sloan - Function first created, adapted from 
%                             "Vulintus_Set_AppData_Path.m".
%


appdata_path = winqueryreg('HKEY_CURRENT_USER',...
        ['Software\Microsoft\Windows\CurrentVersion\' ...
        'Explorer\Shell Folders'],'Local AppData');                         %Grab the local application data directory.    

if nargin == 0                                                              %If there were no input arguments...
    return                                                                  %Skip the rest of the function and return just the basic appdata path.
end

for i = 1:length(varargin)                                                  %Step through each variable input argument.
    if ~ischar(varargin{i})                                                 %If the input argument isn't a character array.
        error('ERROR IN %s: Inputs must be character arrays!',...
            upper(mfilename));                                              %Show an error.
    end
    appdata_path = fullfile(appdata_path,varargin{i});                      %Append each new subfolder to the directory name.
    if ~exist(appdata_path,'dir')                                           %If the directory doesn't already exist...
        [status, msg, ~] = mkdir(appdata_path);                             %Create the directory.
        if status ~= 1                                                      %If the directory couldn't be created...
            error(['ERROR IN %s: Unable to create application data'...
                ' directory:\n\t%s\n\t%s'], upper(mfilename),...
                appdata_path,msg);                                          %Show an error.
        end
    end
end
appdata_path = fullfile(appdata_path,'\');                                  %Add a forward slash to the path.

