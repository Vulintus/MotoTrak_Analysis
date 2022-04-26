function [time, signal] = MotoTrak_Session_Trace(varargin)

%
%MOTOTRAK_SESSION_TRACE.m - Vulintus, Inc., 2020.
%   MOTOTRAK_SESSION_TRACE takes all of the force values, with associated
%   timestamps, from a MotoTrak session and combines them into a
%   semi-continuous trace. Note that since by default MotoTrak only records
%   snippets of the force signal for each trial, resulting traces will have
%   gaps unless the training/testing stage is set up for continuous
%   recording.
%
%   Created April 16, 2020, by Drew Sloan.
%
%   UPDATE LOG:
%
%   04/16/2020 - Drew Sloan - Function first created.
%

if nargin > 1 && ishandle(varargin{1})                                      %If the first input argument is a uicontrol handle...
    varargin(1:2) = [];                                                     %Kick out the first two input arguments.
    temp_nargin = nargin - 2;                                               %Subtract two from the number of input argumnets.
else                                                                        %Otherwise...
    temp_nargin = nargin;                                                   %Copy the number of input arguments to a temporary variable.
end
    
if temp_nargin > 1                                                          %If the user entered too many input arguments...
    error(['ERROR IN MOTOTRAK_SESSION_TRACE: Too many inputs! '...
        'Input should be either an *.ArdyMotor or *.MotoTrak filename '...
        'string or cell array of filename strings']);                       %Show an error.
end
if temp_nargin > 0                                                          %If the user specified file names.
    temp = varargin{1};                                                     %Grab the first input argument.
    if ischar(temp)                                                         %If the argument is a string...
        files = {temp};                                                     %Save the filename as a string.
    elseif iscell(temp)                                                     %If the argument is a cell array...
        files = temp;                                                       %Save the filenames as a cell array.
    else                                                                    %If the input isn't a string or cell array..., string, or number, show an error.
        error(['ERROR IN MOTOTRAK_SESSION_TRACE: Input argument '...
            'must be either an *.ArdyMotor or *.MotoTrak filename '...
            'string or cell array of filenames!']);                         %Show an error.
    end
else                                                                        %Otherwise, if the user hasn't specified an input file...
    [files, path] = ...
        uigetfile({'*.ArdyMotor;*.MotoTrak', 'MotoTrak Data'},...
        'multiselect','on');                                                %Have the user pick an input *.ArdyMotor file or files.
    if ~iscell(files) && files(1) == 0                                      %If no file was selected...
        return                                                              %Exit the function.
    end
    cd(path);                                                               %Change the current directory to the specified folder.
    if ischar(files)                                                        %If only one file was selected...
        files = {files};                                                    %Convert the string to a cell array.
    end
end

%Now step through each file and export a companion *.tsv file for each.
time = cell(length(files),1);                                               %Create a cell array to hold the timestamps.
signal = cell(length(files),1);                                             %Create a cell array to hold the primary signal values.
waitbar = big_waitbar('title','Loading MotoTrak Files...');                 %Create a waitbar figure.
for f = 1:length(files)                                                     %Step through each file.
    
    [~,filename,ext] = fileparts(files{f});                                 %Grab the filename and extension from the trial.
    if waitbar.isclosed()                                                   %If the user closed the waitbar figure...
        errordlg(['The MotoTrak session trace function was cancelled '...
            'by the user!'],'Function Cancelled');                          %Show an error.
        return                                                              %Skip execution of the rest of the function.
    else                                                                    %Otherwise...
        waitbar.string([sprintf('Loading (%1.0f/%1.0f): ',...
            [f,length(files)]) filename]);                                  %Update the waitbar text.
        waitbar.value(f/length(files));                                     %Update the waitbar value.
    end
    
    try                                                                     %Try to read in the data file...
        switch ext                                                          %Switch between the possible file extensions.
            case '.ArdyMotor'                                               %If the data file is an *.ArdyMotor file.
                data = ArdyMotorFileRead(files{f});                         %Read in the data from each file.
            case '.MotoTrak'                                                %If the data file is a *.MotoTrak file.
                data = MotoTrakFileRead(files{f});                          %Read in the data from each file.  
                data = MotoTrak_to_ArdyMotor(data);                         %Convert the *.MotoTrak format data to *ArdyMotor format data.
        end
    catch err                                                               %If an error occurs...
        warning(['ERROR READING: ' files{f}]);                              %Show which file had a read problem...
        warning(err.message);                                               %Show the actual error message.
        continue                                                            %Skip to the next file.
    end    
    
    if ~isfield(data,'trial') || isempty(data.trial)                        %If there's no trials in this data file...
        warning('MOTOTRAK_SESSION_TRACE:NoTrials',['WARNING FROM '...
            'MOTOTRAK_SESSION_TRACE: The file "' files{f} '" has zero '...
            'trials and will be skipped.']);                                %Show a warning.
        continue                                                            %Skip to the next file.
    end
 
    for t = 1:length(data.trial)                                            %Step through each trial.
        data.trial(t).timestamps = ...
            double(data.trial(t).sample_times)/86400000 + ...
            data.trial(t).starttime;                                        %Convert the sample times to serial date numbers.
    end
    time{f} = vertcat(data.trial.timestamps);                               %Concatenate all of the timestamps.
    signal{f} = vertcat(data.trial.signal);                                 %Concatenate all of the signal values.
    [time{f}, i] = unique(time{f});                                         %Find and sort all of the unique timestamps.
    signal{f} = signal{f}(i,:);                                             %Sort the signal using the same indices.
end
waitbar.close();                                                            %Close the waitbar.

if length(files) == 1                                                       %If there's only one file.
    time = time{1};                                                         %Return the timestamps as a matrix.
    signal = signal{1};                                                     %Return the signal values as a matrix.
end