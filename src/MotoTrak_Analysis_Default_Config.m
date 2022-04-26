function handles = MotoTrak_Analysis_Default_Config(varargin)

%
%MotoTrak_Analysis_Default_Config.m - Vulintus, Inc.
%
%   MotoTrak_Analysis_Default_Config sets the default values of all program
%   parameters when the MotoTrak Analysis suite is launched.
%   
%   UPDATE LOG:
%   10/03/2017 - Drew Sloan - Function first created, adapted from
%       MotoTrak_Default_Config.m.
%

if nargin > 0                                                               %If there's at least one optional input argument...
    handles = varargin{1};                                                  %An existing handles structure is the first expected argument.
else                                                                        %Otherwise...
    handles = [];                                                           %Create a new, empty handles structure.
end

handles.err_rcpt = 'software.error@vulintus.com';                           %Set the default recipient for software error reports.