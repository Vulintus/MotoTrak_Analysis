function MotoTrak_Analysis_Edit_Config(varargin)

%
%MotoTrak_Analysis_Edit_Config.m - Vulintus, Inc.
%
%   MotoTrak_Analysis_Edit_Config opens the MotoTrak Analysis configuration
%   file so that the user can make any desired edits.
%   
%   UPDATE LOG:
%   10/03/2017 - Drew Sloan - Function first created.
%

handles = varargin{end};                                                    %Assume a handles structure is the last input argument

winopen(handles.config_file);                                               %Open the configuration file with the default editor.