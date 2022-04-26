function MotoTrak_Analysis

%Compiled: 12/15/2016, 13:49:50

MotoTrak_Analysis_Startup;                                                 %Call the startup function.


function MotoTrak_Analysis_Startup


%% Define program-wide constants.
if isdeployed                                                               %If the code is running as a compiled standalone...
    path = Vulintus_Set_AppData_Path('MotoTrak Analysis');                  %Use the MotoTrak Analysis AppData folder for configuration files.
else                                                                        %Otherwise...
    path = pwd;                                                             %Use the current directory for configuration files.
end

%% Specify which analysis functions to display for selection.
fcn_list = {'Graphical Analysis',@MotoTrak_Graphical_Analysis;...
    'Population Data to TSV',{@MotoTrak_PopData_to_TSV,path};...
    'Single Session Data to TSV',@MotoTrak_Session_to_TSV;...
    'Isometric Pull Trial Viewer',@MotoTrak_Pull_Viewer;...
    'Supination Trial Viewer',@MotoTrak_Knob_Viewer;...
    'Lever Trial Viewer',@MotoTrak_Lever_Viewer;...
    'Session Info Editor',@MotoTrak_File_Editor};                           %Create a cell array listing the included functions.


%% Create the selection GUI.
uih = 1.5;                                                                  %Set the height for all buttons.
w = 10;                                                                     %Set the width of the function selection figure.
h = size(fcn_list,1)*(uih + 0.1) + 0.5 - 0.25*uih;                          %Set the height of the function selection figure.
set(0,'units','centimeters');                                               %Set the screensize units to centimeters.
pos = get(0,'ScreenSize');                                                  %Grab the screensize.
pos = [pos(3)/2-w/2, pos(4)/2-h/2, w, h];                                   %Scale a figure position relative to the screensize.
fig = figure('units','centimeters',...
    'Position',pos,...
    'resize','off',...
    'MenuBar','none',...
    'name','Select a MotoTrak Analysis Function',...
    'numbertitle','off');                                                   %Set the properties of the figure.
for i = 1:size(fcn_list,1)                                                  %Step through each available analysis function   
    uicontrol(fig,'style','pushbutton',...
        'string',fcn_list{i,1},...
        'units','centimeters',...
        'position',[0.1 h-i*(uih+0.1) 9.8 uih],...
        'fontweight','bold',...
        'fontsize',14,...
        'callback',fcn_list{i,2});                                          %Make a button for the port showing that it is busy.
end


function [data, varargout] = ArdyMotorFileRead(file)
%
%ARDYMOTORFILEREAD.m - Vulintus, Inc., 2015.
%
%   ARDYMOTORFILEREAD reads in the sensor recordings from motor behavioral
%   tasks controlled by an Arduino board.  The data is organized into a
%   MATLAB structure for easy analysis
%
%   data = ARDYMOTORFILEREAD(file) reads in the behavioral record from the
%   *.ARDYMOTOR files specified by the string variable "file" into the
%   output "data" structure.
%
%   UPDATE LOG:
%   12/14/2016 - Drew Sloan - Added more commenting for deployment to
%       customers. Change the "rat" field name to "subject".
%

data = [];                                                                  %Start a data structure to receive the recordings.
fid = fopen(file,'r');                                                      %Open the file for read access.
fseek(fid,0,-1);                                                            %Rewind to the beginning of the file.

version = fread(fid,1,'int8');                                              %Read the file format version from the first byte as a signed integer.

data.version = version;                                                     %Save the file version to the data structure.

if version < 0                                                              %If the file format version indicates ArdyMotor V2.0...
    
    if version == -1 || version <= -3                                       %If the file format version is -1 or -2...
        data.daycode = fread(fid,1,'uint16');                               %Read in the daycode.
    end
    data.booth = fread(fid,1,'uint8');                                      %Read in the booth number.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the rat's name.
    data.subject = fread(fid,N,'*char')';                                   %Read in the characters of the rat's name.
    data.position = fread(fid,1,'float32');                                 %Read in the device position, in centimeters.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the stage description.
    data.stage = fread(fid,N,'*char')';                                     %Read in the characters of the stage description.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the device name.
    data.device = fread(fid,N,'*char')';                                    %Read in the characters of the device name.
    fileinfo = dir(file);                                                   %Grab the input file information.
    
    data.cal = [1,0];                                                       %Assume by default that no calibration conversion was applied.
    if fileinfo.datenum < 735088                                            %If this file was written between 8/3/2012 and 8/6/2012...
        data.device = 'Wheel';                                              %Set the device name to 'Wheel'.
        data.cal(1) = 0.5;                                                  %Set the degrees/tick calibration constant to 0.5.
    end
      
    if (version <= -3)                                                      %If the file format version is -3 or less...
        if any(strcmpi(data.device,{'pull', 'knob', 'lever'}))              %If the device is a pull, knob, or wheel...
            data.cal(:) = fread(fid,2,'float32');                           %Read in the grams/tick and baseline grams calibration coefficients.
        elseif any(strcmpi(data.device,{'wheel'}))                          %If the device was a wheel...
            data.cal(1) = fread(fid,1,'float32');                           %Read in the degrees/tick calibraton coefficient.
        end
    else                                                                    %Otherwise, for all other file format versions...
        if any(strcmpi(data.device,{'pull'}))                               %If the device was a pull...
            data.cal(:) = fread(fid,2,'float32');                           %Read in the grams/tick and baseline grams calibration coefficients.
        elseif any(strcmpi(data.device,{'wheel','knob'})) && ...
                fileinfo.datenum > 735088                                   %If the device was a wheel or a knob...
            data.cal(1) = fread(fid,1,'float32');                           %Read in the degrees/tick calibration coefficient.
        end
    end
    
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the constraint description.
    data.constraint = fread(fid,N,'*char')';                                %Read in the characters of the constraint description.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the threshold type description.
    data.threshtype = fread(fid,N,'*char')';                                %Read in the characters of the threshold type  description.
    if version == -1 || version <= -3                                       %If the file format version is -1...
        data.pre_trial_sampling_dur = 1000;                                 %Indicate 1000 milliseconds of pre-trial sampling.
    else                                                                    %Otherwise, for later versions...
        data.pre_trial_sampling_dur = fread(fid,1,'float32');               %Read in the pre-trial sampling duration (in milliseconds).
    end
    data.pauses = [];                                                       %Create a field in the data structure to hold pause times.
    data.manual_feeds = [];                                                 %Create a field in the data structure to hold manual feed times.
    while ~feof(fid)                                                        %Loop until the end of the file.
        trial = fread(fid,1,'uint32');                                      %Read in the trial number.
        if isempty(trial)                                                   %If no trial number was read...
            continue                                                        %Skip execution of the rest of the loop.
        end
        starttime = fread(fid,1,'float64');                                 %Read in the trial start time.
        outcome = fread(fid,1,'uint8');                                     %Read in the trial outcome.
        if feof(fid)                                                        %If we've reached the end of the file.
            continue                                                        %Skip execution of the rest of the loop.
        end
        if outcome == 'P'                                                   %If the outcome was a pause...
            temp = fread(fid,1,'float64');                                  %Read in the end time of the pause.
            data.pauses(end+1,1:2) = [starttime, temp];                     %Save the start and end time of the pause.
        elseif outcome == 'F'                                               %If the outcome was a manual feeding.
            data.manual_feeds(end+1,1) = starttime;                         %Save the timing of the manual feeding.
        else                                                                %Otherwise, if the outcome was a hit or a miss...
            if fileinfo.datenum < 735122.5980                               %If a file was created before 9/10/2012...
                fseek(fid,-1,'cof');                                        %Rewind the file one byte.
            end
            data.trial(trial).starttime = starttime;                        %Save the trial start time.
            data.trial(trial).hitwin = fread(fid,1,'float32');              %Read in the hit window.
            data.trial(trial).init = fread(fid,1,'float32');                %Read in the initiation threshold.
            data.trial(trial).thresh = fread(fid,1,'float32');              %Read in the hit threshold.
            if version == -4                                                %If the file version is -4...
                data.trial(trial).ceiling = fread(fid,1,'float32');         %Read in the force ceiling.
            end
            data.trial(trial).hittime = [];                                 %List no hit times for this trial by default.
            N = fread(fid,1,'uint8');                                       %Read in the number of hits for this trial.
            data.trial(trial).hittime = fread(fid,N,'float64');             %Read in the hit times for this trial.
            if fileinfo.datenum <  735122.5980                              %If a file was created before 9/10/2012...
                if N > 1                                                    %If there was at least one hit time...
                    outcome = 'H';                                          %Label the trial as a hit.
                else                                                        %Otherwise...
                    outcome = 'M';                                          %Label the trial as a miss.
                end
            end
            data.trial(trial).outcome = outcome;                            %Save the trial outcome.
            N = fread(fid,1,'uint8');                                       %Read in the number of VNS events for this trial.
            data.trial(trial).stimtime = fread(fid,N,'float64');            %Read in the stimulation trigger event times for this trial.
            buffsize = fread(fid,1,'uint32');                               %Read in the number of samples for this trial.
            data.trial(trial).sample_times = ...
                fread(fid,buffsize,'uint16');                               %Read in the sample times, in milliseconds.
            data.trial(trial).signal = fread(fid,buffsize,'float32');       %Read in the device signal.
            data.trial(trial).ir = fread(fid,buffsize,'int16');             %Read in the IR signal.
            if all(data.trial(trial).sample_times == 0)                     %If all of the sample times equal zero...
                if trial ~= 1                                               %If this isn't the first trial...
                    data.trial(trial).sample_times = ...
                        data.trial(trial-1).sample_times;                   %Use the previous trial's sample times.
                else                                                        %Otherwise...
                    data.trial(trial).sample_times = ...
                        int16(10*(1:length(data.trial(trial).signal)) - ...
                        data.pre_trial_sampling_dur)';                      %Subtract the pre-trial sampling duration from the sample times.
                end
            else                                                            %Otherwise...
                data.trial(trial).sample_times = ...
                    int16(data.trial(trial).sample_times) - ...
                    data.pre_trial_sampling_dur;                            %Subtract the pre-trial sampling duration from the sample times.
            end
        end
    end
    if version < -1 && isfield(data,'trial') && ...
            isfield(data.trial,'starttime')                                 %If the file format version is newer than version -1 and the daycode function exists...
        data.daycode = daycode(data.trial(1).starttime);                    %Find the daycode for this file.
    end
    
else                                                                        %Otherwise, for all other versions...
    
    data.session_start = fread(fid,1,'float64');                            %Read in the daycode.
    data.booth = fread(fid,1,'uint8');                                      %Read in the booth number.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the rat's name.
    data.subject = fread(fid,N,'*char')';                                   %Read in the characters of the rat's name.
    data.position = fread(fid,1,'float32');                                 %Read in the device position, in centimeters.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the stage description.
    data.stage = fread(fid,N,'*char')';                                     %Read in the characters of the stage description.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the device name.
    data.device = fread(fid,N,'*char')';                                    %Read in the characters of the device name.
    fileinfo = dir(file);                                                   %Grab the input file information.
    
    data.cal = [1,0];                                                       %Assume by default that no calibration conversion was applied.
    if fileinfo.datenum < 735088                                            %If this file was written between 8/3/2012 and 8/6/2012
        data.device = 'Wheel';                                              %Set the device name to 'Wheel'.
        data.cal(1) = 0.5;                                                  %Set the degrees/tick calibration constant to 0.5.
    end
      
    if (version <= -3)                                                      %If the file format version is -3 or less...
        if any(strcmpi(data.device,{'pull', 'knob', 'lever'}))              %If the device is a pull, knob, or wheel...
            data.cal(:) = fread(fid,2,'float32');                           %Read in the grams/tick and baseline grams calibration coefficients.
        elseif any(strcmpi(data.device,{'wheel'}))                          %If the device was a wheel...
            data.cal(1) = fread(fid,1,'float32');                           %Read in the degrees/tick calibraton coefficient.
        end
    else                                                                    %Otherwise, for all other file format versions...
        if any(strcmpi(data.device,{'pull'}))                               %If the device was a pull...
            data.cal(:) = fread(fid,2,'float32');                           %Read in the grams/tick and baseline grams calibration coefficients.
        elseif any(strcmpi(data.device,{'wheel','knob'})) && ...
                fileinfo.datenum > 735088                                   %If the device was a wheel or a knob...
            data.cal(1) = fread(fid,1,'float32');                           %Read in the degrees/tick calibration coefficient.
        end
    end
        
    fseek(fid,0,-1);                                                        %Rewind to the beginning of the file.
    data.version = 1;                                                       %Version of the struct
    data.daycode = fread(fid,1,'uint16');                                   %DayCode.
    data.booth = fread(fid,1,'uint8');                                      %Booth number.
    N = fread(fid,1,'uint8');                                               %Number of characters in the rat's name.
    data.subject = fread(fid,N,'*char')';                                   %Characters of the rat's name.
    data.position = fread(fid,1,'uint8');                                   %Position of the input device (0-3 inches).
    data.responsewindow = fread(fid,1,'uint8');                             %Response window.
    data.stage(1) = fread(fid,1,'uchar');                                   %First character of the stage title stage number.
    temp = fread(fid,1,'uchar');                                            %Read in the next character.
    while temp(end) ~= data.stage(1)                                        %Loop until we get to the first letter of the device description.
        temp = [temp, fread(fid,1,'uchar')];                                %Read in the next character.
    end
    data.stage = char(horzcat(data.stage,temp(1:end-1)));                   %Concatenate the stage name together.
    if data.stage(1) == 'P'                                                 %If the stage is a pull stage...
        data.device = horzcat(temp(end),fread(fid,3,'*char')');             %Read in the selected device (Pull).
    else                                                                    %Otherwise, if the stage is a lever or wheel stage...
        data.device = horzcat(temp(end),fread(fid,4,'*char')');             %Read in the selected device (Lever or Wheel).
    end
    data.bin = fread(fid,1,'uint8');                                        %Bin Size.
    numparams = fread(fid,1,'uint8');                                       %Number of stimulus parameters.
    for i = 1:numparams                                                     %Step through each stimulus parameter.
        N = fread(fid,1,'uint16');                                          %Number of characters in a parameter name.
        data.param(i).name = fread(fid,N,'*char')';                         %Parameter name.
    end
    trial = 0;                                                              %Start a trial counter.
    while ~feof(fid)                                                        %Loop until the end of the file.
        trial = trial + 1;                                                  %Increment the trial counter.
        try                                                                 %Try to read in a trial, abort if the trial is corrupted.
            data.trial(trial).threshold = fread(fid,1,'uint16');            %Read Threshold.
            data.trial(trial).starttime = fread(fid,1,'float64');           %Read trial start time.
            data.trial(trial).hittime = fread(fid,1,'float64');             %Read hit/reward time.
            data.trial(trial).outcome = fread(fid,1,'float64');             %Read Trial Outcome.
            for i = 1:3                                                     %Step through the three IR inputs.
                numIR = fread(fid,1,'uint32');                              %Read the number of breaks on the IR input.
                data.trial(trial).IR1(i).times = ...
                    fread(fid,numIR,'float64');                             %Read in the timestamp for the IR break.
            end
            numbins = fread(fid,1,'uint32');                                %Read in the number of signal datapoints.
            data.trial(trial).signal = fread(fid,numbins,'float64');        %Read in the sensory signal.
        catch err                                                           %If an error occurs...
            lastwarn(['MOTOTRAK_FILE_READ: ' err.message],...
                ['MOTOTRAK_FILE_READ:' err.identifier]);                    %Save the warning ID.
        end
    end
    if isempty(data.trial(end).starttime)                                   %If the last trial had no data...
        data.trial(end) =[];                                                %Remove the empty trial.
    end
end

if isfield(data,'trial') && ~isempty(data.trial)                            %If there's at least one trial...
    data.daycode = fix(data.trial(1).starttime);                            %Set the daycode to the fixed first trial timestamp.
    temp = zeros(length(data.trial),1);                                     %Pre-allocation a matrix to mark incomplete trials for exclusion.
    for t = 1:length(data.trial)                                            %Step through each trial.
        if isempty(data.trial(t).signal)                                    %If a trial has no signal...
            temp(t) = 1;                                                    %Mark the trial for exclusion.
        end
    end
    data.trial(temp == 1) = [];                                             %Kick out all trials marked for exclusion.
end

fclose(fid);                                                                %Close the input file.
varargout{1} = version;                                                     %Output the format version number if the user asked for it.


%% This subfunction returns the daycode (1-365) for a given date.
function d = daycode(date)
date = datevec(date);                                                       %Convert the serial date number to a date vector.
year = date(1);                                                             %Pull the year out of the date vector.
month = date(2);                                                            %Pull out the month.
day = date(3);                                                              %Pull out the day.
if year/4 == fix(year/4);                                                   %If the year is a leap year...
    numDays = [31 29 31 30 31 30 31 31 30 31 30 31];                        %Include 29 days in February.
else                                                                        %Otherwise...
	numDays = [31 28 31 30 31 30 31 31 30 31 30 31];                        %Include 28 days in February.
end
date = sum(numDays(1:(month-1)));                                           %Sum the days in the preceding months...
d = date + day;                                                             %...and add the day of the specified month.


function MotoTrak_Analysis_Startup


%% Define program-wide constants.
if isdeployed                                                               %If the code is running as a compiled standalone...
    path = Vulintus_Set_AppData_Path('MotoTrak Analysis');                  %Use the MotoTrak Analysis AppData folder for configuration files.
else                                                                        %Otherwise...
    path = pwd;                                                             %Use the current directory for configuration files.
end

%% Specify which analysis functions to display for selection.
fcn_list = {'Graphical Analysis',@MotoTrak_Graphical_Analysis;...
    'Population Data to TSV',{@MotoTrak_PopData_to_TSV,path};...
    'Single Session Data to TSV',@MotoTrak_Session_to_TSV;...
    'Isometric Pull Trial Viewer',@MotoTrak_Pull_Viewer;...
    'Supination Trial Viewer',@MotoTrak_Knob_Viewer;...
    'Lever Trial Viewer',@MotoTrak_Lever_Viewer;...
    'Session Info Editor',@MotoTrak_File_Editor};                           %Create a cell array listing the included functions.


%% Create the selection GUI.
uih = 1.5;                                                                  %Set the height for all buttons.
w = 10;                                                                     %Set the width of the function selection figure.
h = size(fcn_list,1)*(uih + 0.1) + 0.5 - 0.25*uih;                          %Set the height of the function selection figure.
set(0,'units','centimeters');                                               %Set the screensize units to centimeters.
pos = get(0,'ScreenSize');                                                  %Grab the screensize.
pos = [pos(3)/2-w/2, pos(4)/2-h/2, w, h];                                   %Scale a figure position relative to the screensize.
fig = figure('units','centimeters',...
    'Position',pos,...
    'resize','off',...
    'MenuBar','none',...
    'name','Select a MotoTrak Analysis Function',...
    'numbertitle','off');                                                   %Set the properties of the figure.
for i = 1:size(fcn_list,1)                                                  %Step through each available analysis function   
    uicontrol(fig,'style','pushbutton',...
        'string',fcn_list{i,1},...
        'units','centimeters',...
        'position',[0.1 h-i*(uih+0.1) 9.8 uih],...
        'fontweight','bold',...
        'fontsize',14,...
        'callback',fcn_list{i,2});                                          %Make a button for the port showing that it is busy.
end


function MotoTrak_File_Editor(varargin)

handles = Make_GUI;                                                         %Create the main GUI.

set(handles.loadbutton,'callback',@LoadFile);                               %Set the callback for the file load button.
set(handles.savebutton,'callback',@SaveEdits);                              %Set the callback for the edit-save button and disable it.
set(handles.editrat(2),'callback',@EditRat);                                %Set the callback for the rat name editbox.
set(handles.editbooth(2),'callback',@EditBooth);                            %Set the callback for the booth number editbox.
set(handles.editstage(2),'callback',@EditStage);                            %Set the callback for the stage name editbox.

handles.path = 'C:\MotoTrak\';                                              %Set the expected primary local data path for saving data files.
if ~exist(handles.path,'dir')                                               %If the primary local data path doesn't exist...
    handles.path = pwd;                                                     %Set the default path to the current directory.
end

guidata(handles.mainfig,handles);                                           %Pin the handles structure to the main figure.


%% This function has the user select a file to edit and then loads the header from the file.
function LoadFile(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
[file, path] = uigetfile(handles.path,'*.ArdyMotor');                       %Have the user select an *.ArdyMotor file.
if file(1) == 0                                                             %If the user clicked "cancel"...
    return                                                                  %Skip execution of the rest of the function.
end
handles.file = file;                                                        %Save the original file name.
handles.path = path;                                                        %Save the original file's path.
handles.oldfile = file;                                                     %Keep track of the previous file name.
handles.oldpath = path;                                                     %Keep track of the previous path name.
fid = fopen([path file],'r');                                               %Open the *.ArdyMotor file for read access.
handles.version = fread(fid,1,'int8');                                      %Read the file format version from the first byte as a signed integer.
if handles.version ~= -3                                                    %If the file format is an older version...
    fclose(fid);                                                            %Close the file.
    errordlg('Sorry, this program cannot edit Version 1.0 files.',...
        'Old File Format');                                                 %Show an error dialog box.
end
handles.daycode = fread(fid,1,'uint16');                                    %Read in the daycode.
handles.booth = fread(fid,1,'uint8');                                       %Read in the booth number.
N = fread(fid,1,'uint8');                                                   %Read in the number of characters in the rat's name.
handles.rat = fread(fid,N,'*char')';                                        %Read in the characters of the rat's name.
handles.oldrat = handles.rat;                                               %Save the previous rat name.
handles.position = fread(fid,1,'float32');                                  %Read in the device position, in centimeters.
N = fread(fid,1,'uint8');                                                   %Read in the number of characters in the stage description.
handles.stage = fread(fid,N,'*char')';                                      %Read in the characters of the stage description.
i = find(handles.stage == ':',1,'first');                                   %Look for a colon in the stage name.
if isempty(i)                                                               %If no colon was found in the stage name...
    handles.stage_number = [];                                              %Set the stage number to empty brackets by default.
    i = strfind(handles.file,'Stage');                                      %Find the word stage in the filename.
    if ~isempty(i)                                                          %If the word stage was found...
        j = (handles.file == '_' & 1:length(handles.file) > i);             %Find all underscores after the stage number.
        if any(j)                                                           %If any underscores were found.
            j = find(j,1,'first');                                          %Find the first underscore after the stage number.
            handles.stage_number = handles.file(i+5:j-1);                   %Grab the stage number from the filename.
        end
    end
else                                                                        %Otherwise...
    handles.stage_number = handles.stage(1:i-1);                            %Grab the stage number from the stage name.
    handles.stage(1:i) = [];                                                %Kick the stage number out of the stage name.
    handles.stage(1:find(handles.stage ~= ' ',1,'first')-1) = [];           %Kick out any leading spaces from the stage name.
end
N = fread(fid,1,'uint8');                                                   %Read in the number of characters in the device name.
handles.device = fread(fid,N,'*char')';                                     %Read in the characters of the device name.
checker = zeros(1,length(handles.file));                                    %Create a matrix to find the timestamp in the original filename.
for i = 1:length(handles.file) - 14                                         %Step through the characters of the original filename.
    if all(handles.file([i:i+7,i+9:i+14]) >= 48) && ...
            all(handles.file([i:i+7,i+9:i+14]) <= 57) && ...
            handles.file(i+8) == 'T'                                        %If a valid timestamp is found in the filename...
        checker(i) = 1;                                                     %Set the checker for that character to 1.
    end
end
if any(checker)                                                             %If any valid timestamp was found in the filename...
    i = find(checker == 1,1,'first');                                       %Grab the start index for the ifrst timestamp found in the filename.
    handles.daycode = datenum(handles.file(i:i+14),'yyyymmddTHHMMSS');      %Grab the session time from the filename.
else                                                                        %Otherwise, if no valid timestamp was found...
    if any(strcmpi({'pull', 'knob', 'lever'},handles.device))               %If the device was the pull, knob, or lever.
        fseek(fid,8,'cof');                                                 %Skip over two float32 values.
    elseif any(strcmpi(handles.device,{'wheel'}))                           %Otherwise, if the device was the wheel...
        fseek(fid,4,'cof');                                                 %Skip over one float32 value.
    end
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the constraint description.
    fseek(fid,N,'cof');                                                     %Skip over the characters of the constraint description.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the threshold type description.
    fseek(fid,N,'cof');                                                     %Skip over the characters of the threshold type description.
    temp = fread(fid,1,'uint32');                                           %Read in the trial number.
    if ~isempty(temp)                                                       %If a trial number was read...
        handles.daycode = fread(fid,1,'float64');                           %Grab the first trial start time as the session start time.
    else                                                                    %If there was no trial number to read...
        temp = dir([handles.path, handles.file]);                           %Grab the file info for the data file.
        handles.daycode = temp.datenum;                                     %Use the last date modified as the session start time for this file.
    end
end
fclose(fid);                                                                %Close the data file.
set(handles.editrat,'string',handles.rat);                                  %Set the string for both subject editboxes.
set(handles.editbooth,'string',num2str(handles.booth,'%1.0f'));             %Set the string for both booth number editboxes.
if ~isempty(handles.stage_number)                                           %If a stage number was found...
    set(handles.editstage,...
        'string',[handles.stage_number ': ' handles.stage]);                %Set the string for both subject editboxes.
else                                                                        %Otherwise...
    set(handles.editstage,'string',handles.stage);                          %Set the string for both subject editboxes.
end
set(handles.editfile,'string',handles.file,'horizontalalignment','left');   %Set the string for both file editboxes.
set(handles.editpath,'string',handles.path,'horizontalalignment','left');   %Set the string for both path editboxes.
set([handles.editrat(2), handles.editbooth(2), handles.editstage(2)],...
    'enable','on');                                                         %Enable the various editboxes.
set(handles.editfile(2),'ButtonDownFcn',@EditFile);                         %Set the buttondown function for the filename editbox.
set(handles.editpath(2),'ButtonDownFcn',@EditPath);                         %Set the buttondown function for the path editbox.
set(handles.savebutton,'enable','on');                                      %Enable the edit-saving pushbutton.
handles.autoset_file = 1;                                                   %Create a field to control auto-setting of the path name.
handles.autoset_path = 1;                                                   %Create a field to control auto-setting of the path name.
guidata(handles.mainfig,handles);                                           %Pin the handles structure to the main figure.


%% This function saves the file edits, replacing the old file or creating a new one.
function SaveEdits(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
set([handles.editrat(2), handles.editbooth(2), handles.editstage(2),...
    handles.loadbutton, handles.savebutton],'enable','off');                %Disable the various editboxes and pushbuttons.
set(handles.editfile(2),'ButtonDownFcn',[]);                                %Remove the buttondown function for the filename editbox.
set(handles.editpath(2),'ButtonDownFcn',[]);                                %Remove the buttondown function for the path editbox.
if exist([handles.path, handles.file],'file')                               %If a file already exists with the specified filename...
    if strcmpi([handles.path, handles.file],...
            [handles.oldpath handles.oldfile])                              %If the filename hasn't changed...
        temp = questdlg(['The filename hasn''t changed. Do you want to '...
            'replace the original file?'],'Replace Original?','Yes',...
            'No','Yes');                                                    %Ask the user if they want to replace the original file.
        if ~strcmpi(temp,'Yes')                                             %If the user didn't select "Yes"...
            set([handles.editrat(2), handles.editbooth(2),...
                handles.editstage(2),handles.savebutton,...
                handles.loadbutton],'enable','on');                         %Re-enable the various editboxes and pushbuttons.
            set(handles.editfile(2),'ButtonDownFcn',@EditFile);             %Reset the buttondown function for the filename editbox.
            set(handles.editpath(2),'ButtonDownFcn',@EditPath);             %Reset the buttondown function for the path editbox.
            return                                                          %Skip execution of the rest of the function.
        end
    else                                                                    %Otherwise...
        temp = questdlg(['Another file already exists with this '...
            'filename. Do you want to replace it?'],...
            'Replace Other File?','Yes','No','Yes');                        %Ask the user if they want to replace the other file.
        if ~strcmpi(temp,'Yes')                                             %If the user didn't select "Yes"...
            set([handles.editrat(2), handles.editbooth(2),...
                handles.editstage(2),handles.savebutton,...
                handles.loadbutton],'enable','on');                         %Re-enable the various editboxes and pushbuttons.
            set(handles.editfile(2),'ButtonDownFcn',@EditFile);             %Reset the buttondown function for the filename editbox.
            set(handles.editpath(2),'ButtonDownFcn',@EditPath);             %Reset the buttondown function for the path editbox.
            return                                                          %Skip execution of the rest of the function.
        end
    end
end
delete_original = 0;                                                        %Set the program to not delete the original file by default.
if ~strcmpi([handles.path, handles.file],[handles.oldpath handles.oldfile]) %If the filename changed from the original...
    temp = questdlg('Do you want to delete the original file?',...
            'Delete Original?','Yes','No','No');                            %Ask the user if they want to replace the other file.
    if strcmpi(temp,'Yes')                                                  %If the user selected "Yes"...
        delete_original = 1;                                                %Set the program to delete the original file after copying.
    elseif isempty(temp)                                                    %If the user closed the dialog box without clicking a button...
        set([handles.editrat(2), handles.editbooth(2),...
            handles.editstage(2),handles.savebutton,handles.loadbutton],...
            'enable','on');                                                 %Re-enable the various editboxes and pushbuttons.
        set(handles.editfile(2),'ButtonDownFcn',@EditFile);                 %Reset the buttondown function for the filename editbox.
        set(handles.editpath(2),'ButtonDownFcn',@EditPath);                 %Reset the buttondown function for the path editbox.
        return                                                              %Skip execution of the rest of the function.
    end
else                                                                        %Otherwise, if the filename hasn't changed...
    copyfile([handles.oldpath, handles.oldfile],...
        [handles.oldpath, 'temp.ArdyMotor'],'f');                           %Copy the original file to a temporary filename.   
    handles.oldfile = 'temp.ArdyMotor';                                     %Use the new temporary file as the old filename.
end

waitbar = big_waitbar('title','Saving File Edits...',...
    'string',['Saving to: ' handles.file],'color','m');                     %Create a waitbar figure.
waitbar.value(0);                                                           %Set the waitbar value to zero.

a = find(handles.path == '\');                                              %Find all forward slashes in the new path.
for i = a                                                                   %Step through all forward slashes.
    if ~exist(handles.path(1:i),'dir')                                      %If the directory doesn't already exist...
        mkdir(handles.path(1:i));                                           %Create the directory.
    end
end
oldfid = fopen([handles.oldpath, handles.oldfile],'r');                     %Open the original *.ArdyMotor file for read access.
newfid = fopen([handles.path, handles.file],'w');                           %Open a new *.ArdyMotor file for overwrite access.

fwrite(newfid,fread(oldfid,1,'int8'),'int8');                               %Write the data file version number.

fseek(oldfid,3,'cof');                                                      %Skip over the daycode and booth number in the original file.
fwrite(newfid,fix(handles.daycode),'uint16');                               %Write the DayCode.
fwrite(newfid,handles.booth,'uint8');                                       %Write the booth number.

N = fread(oldfid,1,'uint8');                                                %Read in the number of characters in the original rat name.
fseek(oldfid,N,'cof');                                                      %Skip the characters of the original rat name.
fwrite(newfid,length(handles.rat),'uint8');                                 %Write the number of characters in the new rat name.
fwrite(newfid,handles.rat,'uchar');                                         %Write the characters of the new rat name.

fwrite(newfid,fread(oldfid,1,'float32'),'float32');                         %Write the position of the input device (in centimeters).

if ~isempty(handles.stage_number)                                           %If a stage number was set...
    temp = [handles.stage_number ': ' handles.stage];                       %Set the string for the stage description to include the stage number.
else                                                                        %Otherwise...
    temp = handles.stage;                                                   %Set the string to just the stage description.
end

N = fread(oldfid,1,'uint8');                                                %Read in the number of characters in the original stage description.
fseek(oldfid,N,'cof');                                                      %Skip the characters of the original stage description.
fwrite(newfid,length(temp),'uint8');                                        %Write the number of characters in the stage description.
fwrite(newfid,temp,'uchar');                                                %Write the characters of the stage description.
counter = 0;                                                                %Create a counter variable.
temp = fread(oldfid,1,'int8');                                              %Grab one byte from the original file.
while ~isempty(temp)                                                        %Loop until there's no more bytes in the original file.
    counter = counter + 0.0001;                                             %Add 0.0001 to the counter.
    if rem(counter,0.01) == 0                                               %If the counter is at an even hundredth...
        waitbar.value(counter);                                             %Update the waitbar value.
    end
    if counter >= 1                                                         %If the counter value is greater than or equal to 1...
        counter = 0;                                                        %Reset the counter to zero.
    end
    fwrite(newfid,temp,'int8');                                             %Write the byte to the new file.
    temp = fread(oldfid,1,'int8');                                          %Grab another byte from the original file.
end
while counter < 1;                                                          %Loop until the counter is greater than 1...
    counter = counter + 0.01;                                               %Add 0.01 to the counter.
    waitbar.value(counter);                                                 %Update the waitbar value.
end
waitbar.close();                                                            %Close the waitbar.
fclose(oldfid);                                                             %Close the original file.
fclose(newfid);                                                             %Close the new file.
if delete_original == 1                                                     %If the user selected to delete the original file...
    delete([handles.oldpath, handles.oldfile]);                             %Delete the original file.
end
if exist([handles.oldpath,'temp.ArdyMotor'],'file')                         %If a temporary file was created...
    delete([handles.oldpath,'temp.ArdyMotor']);                             %Delete the temporary file.
end
set(handles.editrat,'string',handles.rat);                                  %Set the string for both subject editboxes.
set(handles.editbooth,'string',num2str(handles.booth,'%1.0f'));             %Set the string for both booth number editboxes.
if ~isempty(handles.stage_number)                                           %If a stage number was found...
    set(handles.editstage,...
        'string',[handles.stage_number ': ' handles.stage]);                %Set the string for both subject editboxes.
else                                                                        %Otherwise...
    set(handles.editstage,'string',handles.stage);                          %Set the string for both subject editboxes.
end
set(handles.editfile,'string',handles.file,'horizontalalignment','left');   %Set the string for both file editboxes.
set(handles.editpath,'string',handles.path,'horizontalalignment','left');   %Set the string for both path editboxes.
handles.oldfile = handles.file;                                             %Set the current file name to be the new original file.
handles.oldpath = handles.path;                                             %Set the current path to be the new original path.
set([handles.editrat(2), handles.editbooth(2), handles.editstage(2),...
    handles.savebutton,handles.loadbutton],'enable','on');                  %Re-enable the various editboxes and pushbuttons
set(handles.editfile(2),'ButtonDownFcn',@EditFile);                         %Reset the buttondown function for the filename editbox.
set(handles.editpath(2),'ButtonDownFcn',@EditPath);                         %Reset the buttondown function for the path editbox.
guidata(handles.mainfig,handles);                                           %Pin the handles structure to the main figure.



%% This function executes when the user enters a rat's name in the editbox
function EditRat(hObject,~)           
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
temp = get(hObject,'string');                                               %Grab the string from the rat name editbox.
for c = '/\?%*:|"<>. '                                                      %Step through all reserved characters.
    temp(temp == c) = [];                                                   %Kick out any reserved characters from the rat name.
end
if ~strcmpi(temp,handles.rat)                                               %If the rat's name was changed.
    handles.rat = upper(temp);                                              %Save the new rat name in the handles structure.
end
set(handles.editrat(2),'string',handles.rat);                               %Reset the rat name in the rat name editbox.
if handles.autoset_file == 1                                                %If the user hasn't yet overridden the auto-set filename...
    handles.file = UpdateFilename(handles);                                 %Call the function to update the filename.
end
if handles.autoset_path == 1                                                %If the user hasn't yet overridden the auto-set path name...
    handles.path = UpdatePath(handles);                                     %Call the function to update the path.
end
guidata(handles.mainfig,handles);                                           %Pin the handles structure to the main figure.


%% This function executes when the user changes the booth number in the editbox.
function EditBooth(hObject,~)           
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
temp = get(hObject,'string');                                               %Grab the string from the booth number editbox.
temp = str2double(temp);                                                    %Convert the string to a number.
if temp > 0 && mod(temp,1) == 0 && temp < 65535                             %If the entered booth number is positive and a whole number...
    handles.booth = temp;                                                   %Save the booth number in the handles structure.
end
set(handles.editbooth(2),'string',num2str(handles.booth,'%1.0f'));          %Reset the string in the booth number editbox to the current booth number.
if handles.autoset_file == 1                                                %If the user hasn't yet overridden the auto-set filename...
    handles.file = UpdateFilename(handles);                                 %Call the function to update the filename.
end
if handles.autoset_path == 1                                                %If the user hasn't yet overridden the auto-set path name...
    handles.path = UpdatePath(handles);                                     %Call the function to update the path.
end
guidata(handles.mainfig,handles);                                           %Pin the handles structure to the main figure.


%% This function executes when the user enters a rat's name in the editbox
function EditStage(hObject,~)           
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
temp = get(hObject,'string');                                               %Grab the string from the rat name editbox.
temp(temp < 32) = [];                                                       %Kick out any special characters.
i = find(temp == ':',1,'first');                                            %Look for a colon in the entered stage name.
if isempty(i)                                                               %If no colon was found in the stage name...
    handles.stage_number = [];                                              %Set the stage number to empty brackets.
    handles.stage = temp;                                                   %Set the stage name to the entered text.
else                                                                        %Otherwise...
    handles.stage_number = temp(1:i-1);                                     %Grab the stage number from the stage name.
    for c = '/\?%*:|"<>. '                                                  %Step through all reserved characters.
        handles.stage_number(handles.stage_number == c) = [];               %Kick out any reserved characters from the stage number.
    end
    temp(1:i) = [];                                                         %Kick the stage number out of the stage name.
    temp(1:find(temp ~= ' ',1,'first')-1) = [];                             %Kick out any leading spaces from the stage name.
    handles.stage = temp;                                                   %Save the stage name.
end
if ~isempty(handles.stage_number)                                           %If a stage number was found...
    set(handles.editstage(2),...
        'string',[handles.stage_number ': ' handles.stage]);                %Set the string for both subject editboxes.
else                                                                        %Otherwise...
    set(handles.editstage(2),'string',handles.stage);                       %Set the string for both subject editboxes.
end
if handles.autoset_file == 1                                                %If the user hasn't yet overridden the auto-set filename...
    handles.file = UpdateFilename(handles);                                 %Call the function to update the filename.
end
if handles.autoset_path == 1                                                %If the user hasn't yet overridden the auto-set path name...
    handles.path = UpdatePath(handles);                                     %Call the function to update the path.
end
guidata(handles.mainfig,handles);                                           %Pin the handles structure to the main figure.


%% This function auto-sets the filename and path for the editted data file.
function file = UpdateFilename(handles)
i = find(handles.oldfile == '_',1,'last');                                  %Find the last underscore in the filename.
if ~isempty(i)                                                              %If an underscore was found...
    suffix = handles.oldfile(i:end-10);                                     %Grab the existing suffix from the previous file.
else                                                                        %Otherwise...
    suffix = [];                                                            %Don't include a suffix in the expected filename.
end
temp = datestr(handles.daycode,30);                                         %Grab a timestamp accurate to the second.
if ~isempty(handles.stage_number)                                           %If there's a valid stage number...
    file = [handles.rat '_' temp '_Stage' handles.stage_number...          
        '_' handles.device suffix '.ArdyMotor' ];                           %Create the expected filename.
else                                                                        %Otherwise...
    file = [handles.rat '_' temp '_' handles.device suffix '.ArdyMotor' ];  %Create the expected filename, minus the stage number
end
set(handles.editfile(2),'string',file);                                     %Show the new filename in the editted filename editbox.


%% This function auto-sets the filename and path for the editted data file.
function path = UpdatePath(handles)
path = handles.oldpath;                                                     %Set the default path to the original path name.
i = strfind(handles.oldpath,handles.oldrat);                                %Find the original rat name in the search path.
if ~isempty(i)                                                              %If the original rat's name was found in the path...
    i = strfind(handles.oldpath,handles.rat);                               %Find the new rat name in the search path.
end
if ~isempty(i)                                                              %If a match was found to either the original or new rat name. 
    path = handles.oldpath(1:i(1)-1);                                       %Grab the path up to the rat name.
    i = find(path == '\');                                                  %Find the last forward slash in the path name.
    if ~isempty(i)                                                          %If a forward slash was found...
        path(i:end) = [];                                                   %Kick out everything from the forward slash and after.
    end
    path = [path '\' handles.rat '\'];                                      %Add the rat name to the expected path.
    if ~isempty(handles.stage_number)                                       %If there's a valid stage number...
        path = [path handles.rat '-Stage' handles.stage_number '\'];        %Make a subfolder name for the current stage in the rat's folder.
    end
end
set(handles.editpath(2),'string',path);                                     %Show the new path in the editted path editbox.


%% This function sets the filename when the user specifies a specific filename in the editbox.
function EditFile(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
if exist(handles.path,'dir')                                                %If the current new path exists...
    path = handles.path;                                                    %Set the default path to the specified path.
else                                                                        %Otherwise...
    path = handles.oldpath;                                                 %Set the default path to the original path.
end
[file, path] = uiputfile([path, handles.file],...
    'Specific a New Filename');                                             %Have the user explicitly specified a filename.
if file(1) == 0                                                             %If the user clicked cancel...
    return                                                                  %Skip execution of the rest of the function.
end
if ~strcmpi(file,handles.file)                                              %If the filename was changed...
    handles.file = file;                                                    %Set the new filename to that specified.
    handles.autoset_file = 0;                                               %Turn off auto-setting of the filename from here on.
    set(handles.editfile,'string',handles.file);                            %Show the new filename in the editbox.
end
if ~strcmpi(path,handles.path)                                              %If the path was changed...
    handles.path = path;                                                    %Set the new path to that specified.
    handles.autoset_path = 0;                                               %Turn off auto-setting of the path from here on.
    set(handles.editpath,'string',handles.path);                         %Show the new path in the editbox.
end
guidata(handles.mainfig,handles);                                           %Pin the handles structure to the main figure.


%% This function sets the path when the user specifies a specific path in the editbox.
function EditPath(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
if exist(handles.path,'dir')                                                %If the current new path exists...
    path = handles.path;                                                    %Set the default path to the specified path.
else                                                                        %Otherwise...
    path = handles.oldpath;                                                 %Set the default path to the original path.
end
path = uigetdir(path,'Select a Destination Directory');                     %Have the user select a path with a dialog box.
if path(1) == 0                                                             %If the user clicked "cancel"...
    return                                                                  %Skip execution of the rest of the function.
end
if ~strcmpi(path,handles.path)                                              %If the path was changed...
    handles.path = path;                                                    %Set the new path to that specified.
    handles.autoset_path = 0;                                               %Turn off auto-setting of the path from here on.
    set(handles.editpath,'string',handles.path);                            %Show the new path in the editbox.
end
guidata(handles.mainfig,handles);                                           %Pin the handles structure to the main figure.


%% This function creates the main figure and populates it with the required uicontrols.
function handles = Make_GUI
set(0,'units','centimeters');                                               %Set the system units to centimeters.
pos = get(0,'ScreenSize');                                                  %Grab the system screen size.
w = 0.9*pos(3);                                                             %Set the figure width relative to the screensize.
h = 0.3*w;                                                                  %Set the height relative to the width.
sp = 0.01*h;                                                                %Set the spacing for all uicontrols.
fontsize = 1.2*h;                                                           %Set the fontsize relative to the figure height.
ui_h = 0.069*fontsize;                                                      %Set the height of all uicontrols relative to the fontsize.
lbl_w = 0.2*fontsize;                                                       %Set the width for editbox labels relative to the fontsize.

%Create the main figure.
pos = [pos(3)/2-w/2, pos(4)/2-h/2, w, h];                                   %Set the figure position.
handles.mainfig = figure('units','centimeters',...
    'Position',pos,...
    'MenuBar','none',...
    'numbertitle','off',...
    'resize','off',...
    'name','MotoTrak Session Info Editor',...
    'color',[0.8 0.8 0.8]);                                                 %Create the main figure.

%Create a panel showing the header info from the original file.
p_h = 3.5*ui_h + (3+1)*sp;                                                  %Set the height of the figure.
pos = [sp, h-p_h-2*sp, w/2-2*sp, p_h];                                      %Set the position of the upper-left panel.
for j = 1:2                                                                 %Step through two panels.
    if j == 2                                                               %If this is the second panel.
        pos(1) = w/2 + 0.1;                                                 %Set the position of the upper-right panel.
    end
    p = uipanel(handles.mainfig,'units','centimeters',...
        'position',pos,...
        'title','Original File',...
        'fontweight','bold',...
        'fontsize',fontsize,...
        'backgroundcolor',[0.7 0.7 0.8]);                                   %Create the panel to hold the session information uicontrols.
    h = fliplr({'editrat','editbooth','editstage'});                        %Create the uicontrol handle names for editabble header parameter.
    l = fliplr({'Subject:','Booth:','Stage:'});                             %Create the labels for the uicontrols' string property.
    for i = 1:length(h)                                                     %Step through the uicontrols.
        handles.label(i+3*(j-1)) = uicontrol(p,'style','edit',...
            'enable','inactive',...
            'string',l{i},...
            'units','centimeters',...
            'position',[sp/2, sp*i/2+ui_h*(i-1), lbl_w, ui_h],...
            'fontweight','bold',...
            'fontsize',fontsize,...
            'horizontalalignment','right',...
            'backgroundcolor',get(p,'backgroundcolor'));                    %Make a static text label for each uicontrol.
        temp = uicontrol(p,'style','edit',...
            'enable','inactive',...
            'units','centimeters',...
            'string','-',...
            'position',[lbl_w+sp/2, sp*i/2+ui_h*(i-1), pos(3)-lbl_w-3*sp/2, ui_h],...
            'fontweight','bold',...
            'fontsize',fontsize,...
            'horizontalalignment','center',...
            'backgroundcolor','w');                                         %Create an editbox for entering in each parameter.
        handles.(h{i})(j) = temp;                                           %Save the uicontrol handle to the specified field in the handles structure.
    end
end
set(p,'title','Editted File','backgroundcolor',[0.7 0.8 0.7]);              %Change the second panel title and backgroundcolor.
set(handles.label(4:6),'backgroundcolor',[0.7 0.8 0.7]);                    %Change the background color of the text labels in the right-hand panel.
set([handles.editrat(2), handles.editbooth(2), handles.editstage(2)],...
    'enable','off');                                                        %Create an editbox for entering in each parameter.

%Create a panel showing the original file name and path.
p_h = 2.5*ui_h + (2+1)*sp;                                                  %Set the height of the figure.
pos = [sp, pos(2)-p_h-3*sp, w - 2*sp, p_h];                                 %Set the position of the middle panel.
for j = 1:2                                                                 %Step through two panels.
    if j == 2                                                               %If this is the second panel.
        pos(2) = pos(2) - p_h - 0.3;                                        %Set the position of the bottom panel.
    end
    p = uipanel(handles.mainfig,'units','centimeters',...
        'position',pos,...
        'title','Original File ',...
        'fontweight','bold',...
        'fontsize',fontsize,...
        'backgroundcolor',[0.7 0.7 0.8]);                                   %Create the panel to hold the session information uicontrols.
    h = fliplr({'editfile','editpath'});                                    %Create the uicontrol handle names for editabble header parameter.
    l = fliplr({'File:','Path:'});                                          %Create the labels for the uicontrols' string property.
    for i = 1:length(h)                                                     %Step through the uicontrols.
        handles.label(6+i+2*(j-1)) = uicontrol(p,'style','edit',...
            'enable','inactive',...
            'string',l{i},...
            'units','centimeters',...
            'position',[sp/2, sp*i/2+ui_h*(i-1), lbl_w, ui_h],...
            'fontweight','bold',...
            'fontsize',fontsize,...
            'horizontalalignment','right',...
            'backgroundcolor',[0.7 0.7 0.8]);                               %Make a static text label for each uicontrol.
        temp = uicontrol(p,'style','edit',...
            'enable','inactive',...
            'units','centimeters',...
            'string','-',...
            'position',[lbl_w+sp/2, sp*i/2+ui_h*(i-1), pos(3)-lbl_w-3*sp/2, ui_h],...
            'fontweight','bold',...
            'fontsize',fontsize,...
            'horizontalalignment','center',...
            'backgroundcolor','w');                                         %Create an editbox for entering in each parameter.
        handles.(h{i})(j) = temp;                                           %Save the uicontrol handle to the specified field in the handles structure.
    end
end
set(p,'title','Editted File','backgroundcolor',[0.7 0.8 0.7]);              %Change the second panel title and backgroundcolor.
set(handles.label(9:10),'backgroundcolor',[0.7 0.8 0.7]);                   %Change the background color of the text labels in the right-hand panel.

%Create buttons for loading data files and for saving the file edits.
pos = [sp, pos(2)-ui_h-2*sp, w/2 - 2*sp, ui_h];                             %Set the position of the middle panel.
handles.loadbutton = uicontrol(handles.mainfig,'style','pushbutton',...
    'string','Load File',...
    'units','centimeters',...
    'position',pos,...
    'fontweight','bold',...
    'fontsize',fontsize,...
    'foregroundcolor','k',...
    'backgroundcolor',[0.8 0.8 0.8]);                                       %Make a file load pushbutton.
pos(1) = w/2 + sp;                                                          %Set the position of the upper-right panel.
handles.savebutton = uicontrol(handles.mainfig,'style','pushbutton',...
    'string','Save Edits',...
    'enable','off',...
    'units','centimeters',...
    'position',pos,...
    'fontweight','bold',...
    'fontsize',fontsize,...
    'foregroundcolor','k',...
    'backgroundcolor',[0.8 0.8 0.8]);                                       %Make an edit-saving pushbutton.


function MotoTrak_Graphical_Analysis(varargin)

%% Find the path containing the MotoTrak Analysis program.
if isdeployed                                                               %If this is deployed code...
    progpath = [pwd '\'];                                                   %The default data path will begin with the current directory...
else                                                                        %Otherwise, if we're evaluating code or running an *.m file...
    temp = mfilename('fullpath');                                           %Grab the current *.m filename.
    if isempty(temp)                                                        %If we're evaluating code...
        progpath = which('MotoTrak_Graphical_Analysis.m');                  %Find the path containing the current folder.
        progpath(find(progpath == '\',1,'last')+1:end) = [];                %Kick out everything after the path.
    else
        progpath = temp(1:find(temp == '\',1,'last'));                      %Pull the path out of the m-file name.
    end
end
   
%% Have the user choose a path containing data files to analyze.
datapath = 'C:\MotoTrak\';                                                  %Set the expected primary local data path for saving data files.
if ~exist(datapath,'dir')                                                   %If the primary local data path doesn't exist...
    datapath = pwd;                                                         %Set the default path to the current directory.
end
datapath = uigetdir(datapath,'Where is your MotoTrak data located?');       %Ask the user where their data is located.
if datapath(1) == 0                                                         %If the user pressed "cancel"...
    return                                                                  %Skip execution of the rest of the function.
end

%% Find all of the MotoTrak data files in the data path.
files = file_miner(datapath,'*.ArdyMotor');                                 %Find all LPS *.ArdyMotor files in the LPS folders.
pause(0.01);                                                                %Pause for 10 milliseconds.
if isempty(files)                                                           %If no files were found...
    errordlg('No MotoTrak data files were found in the that directory!');   %Show an error dialog box.
end

%% Have the user select rats to include or exclude in the analysis.
rats = files;                                                               %Copy the filenames to another cell array.
for r = 1:length(rats)                                                      %Step through each file.
    rats{r}(1:find(rats{r} == '\' | rats{r} == '/',1,'last')) = [];         %Kick out the path from the filename.
    i = strfind(rats{r},'_20');                                             %Find the start of the timestamp.
    if isempty(i) || length(i) > 1                                          %If no timestamp was found in the filename, or multiple timestamps were found...
        rats{r} = [];                                                       %Set the rat name to empty brackets.
    else                                                                    %Otherwise...
        rats{r}(i:end) = [];                                                %Kick out all characters of the filename except the rat name.
    end
end
rat_list = unique(rats);                                                    %Make a list of all the unique rat names.
i = listdlg('PromptString','Which rats would you like to include?',...
    'name','MotoTrak Analysis',...
    'SelectionMode','multiple',...
    'listsize',[300 400],...
    'initialvalue',1:length(rat_list),...
    'uh',25,...
    'ListString',rat_list);                                                 %Have the user pick rats to include.
if isempty(i)                                                               %If the user clicked "cancel" or closed the dialog...
    return                                                                  %Skip execution of the rest of the function.
else                                                                        %Otherwise...
    rat_list = rat_list(i);                                                 %Pare down the rat list to those that the user selected.
end
keepers = ones(length(rats),1);                                             %Create a matrix to check which files match the selected rat names.
for r = 1:length(rats)                                                      %Step through each file's rat name.
    if ~any(strcmpi(rat_list,rats{r})) && ~isempty(rats{r})                 %If this file's rat name wasn't selected and a rat name was found in the filename...
        keepers(r) = 0;                                                     %Mark the file for exclusion.
    end
end
files(keepers == 0) = [];                                                   %Kick out all files the user doesn't want to include.

%% Step through all of the data files and load them into a structure.
set(0,'units','centimeters');                                               %Set the system units to centimeters.
pos = get(0,'Screensize');                                                  %Grab the screensize.
h = 2;                                                                      %Set the height of the figure.
w = 15;                                                                     %Set the width of the figure.
fig = figure('numbertitle','off','name','Loading MotoTrak Files...',...
    'units','centimeters','Position',[pos(3)/2-w/2, pos(4)/2-h/2, w, h],...
    'menubar','none','resize','off');                                       %Create a figure to show the progress of reading in the files.
ax = axes('units','centimeters','position',[0.25,0.25,w-0.5,h/2-0.3],...
    'parent',fig);                                                          %Create axes for showing loading progress.
obj = fill([0 1 1 0 0],[0 0 1 1 0],'b','edgecolor','k');                    %Create a fill object to show loading progress.
set(ax,'xtick',[],'ytick',[],'box','on','xlim',[0,length(files)],...
    'ylim',[0,1]);                                                          %Set the axis limits and ticks.
txt = uicontrol(fig,'style','text','units','centimeters',...
    'position',[0.25,h/2+0.05,w-0.5,h/2-0.3],'fontsize',8,...
    'horizontalalignment','left','backgroundcolor',get(fig,'color'));       %Create a text object to show which file is being loaded.
data = [];                                                                  %Create a structure to receive data.
for f = 1:length(files)                                                     %Step through the data files.
    a = find(files{f} == '\',1,'last');                                     %Find the last forward slash in the filename.
    temp = files{f}(a+1:end);                                               %Grab the filename minus the path.
    if ishandle(fig)                                                        %If the user hasn't closed the waitbar figure...
        set(txt,'string',...
            [sprintf('Loading (%1.0f/%1.0f): ',[f,length(files)]) temp]);   %Update the waitbar figure.
        set(obj,'xdata',f*[0 1 1 0 0]);                                     %Update the x-coordinates for the fill object.
        drawnow;                                                            %Update the plot immediately.
    else                                                                    %Otherwise, if the user has closed the waitbar...
        return                                                              %Skip execution of the rest of the function.
    end
    try                                                                     %Try to read in the data file...
        temp = ArdyMotorFileRead(files{f});                                 %Read in the data from each file.
    catch err                                                               %If an error occurs...
        warning(['ERROR READING: ' files{f}]);                              %Show which file had a read problem...
        warning(err.message);                                               %Show the actual error message.
    end
    if isfield(temp,'trial') && length(temp.trial) >= 5 && ...
            any(strcmpi(rat_list,temp.rat))                                 %If there were at least 5 trials...        
        s = length(data) + 1;                                               %Create a new field index.
        for field = {'rat','device','stage'}                               %Step through the fields we want to save from the data file...
            data(s).(field{1}) = temp.(field{1});                           %Grab each field from the data file and save it.
        end
        data(s).outcome = char([temp.trial.outcome]');                      %Grab the outcome of each trial.
        data(s).thresh = [temp.trial.thresh]';                              %Grab the threshold for each trial.
        data(s).starttime = [temp.trial.starttime]';                        %Grab the start time for each trial.
        data(s).peak = nan(length(temp.trial),1);                           %Create a matrix to hold the peak force.
        for t = 1:length(temp.trial)                                        %Step through every trial.
            i = (temp.trial(t).sample_times >= 0 & ...
                temp.trial(t).sample_times < 1000*temp.trial(t).hitwin);    %Find the indices for samples in the hit window.
            if any(i ~= 0)                                                  %If there's any samples...
                data(s).peak(t) = max(temp.trial(t).signal(i));             %Find the maximum force in each hit window.
                data(s).impulse(t) = max(diff(temp.trial(t).signal(i)));    %Find the maximum impulse in each hit window.
            end
        end
        data(s).timestamp = data(s).starttime(1);                           %Grab the timestamp from the start of the first trial.
    end
end
if ishandle(fig)                                                            %If the user hasn't closed the waitbar figure...
    close(fig);                                                             %Close the waitbar figure.
    drawnow;                                                                %Immediately update the figure to allow it to close.
end
if isempty(data)                                                            %If no data files were found...
    errordlg(['There were no MotoTrak data files with 5 or more trials '...
        'for the selected rats!']);                                         %Show an error dialog box.
end
[~,i] = sort([data.timestamp]);                                             %Find the indices to sort all files chronologically.
data = data(i);                                                             %Sort all files chronologically.
devices = unique({data.device});                                            %Grab the unique device names across all sessions.

%% Create interactive figures for each of the device types.
for d = 1:length(devices)                                                   %Step through the devices.
    s = strcmpi({data.device},devices{d});                                  %Find all sessions with each device.
    rats = unique({data(s).rat});                                           %Find all of the unique rat names that have used this device.    
    plotdata = struct([]);                                                  %Create a structure to hold data just for the plot.    
    for r = 1:length(rats)                                                  %Step through each rat.
        plotdata(r).rat = rats{r};                                          %Save the rat's name to the plotdata structure.
        plotdata(r).device = devices{d};                                    %Save the device to the plotdata structure.
        i = find(strcmpi({data.rat},rats{r}) & ...
            strcmpi({data.device},devices{d}));                             %Find all the session for this rat on this device.
        plotdata(r).times = [data(i).timestamp];                            %Grab the timestamps for all sessions.
        plotdata(r).peak = nan(1,length(i));                                %Pre-allocate a matrix to hold the average peak signal for each session.
        plotdata(r).hitrate = nan(1,length(i));                             %Pre-allocate a matrix to hold the hit rate for each session.
        plotdata(r).numtrials = nan(1,length(i));                           %Pre-allocate a matrix to hold the number of trials for each session.
        plotdata(r).peak = nan(1,length(i));                                %Pre-allocate a matrix to hold the average peak signal for each session.
        plotdata(r).first_hit_five = nan(1,length(i));                      %Pre-allocate a matrix to hold the number of hits in the first 5 minutes.
        plotdata(r).first_trial_five = nan(1,length(i));                    %Pre-allocate a matrix to hold the number of trials in the first 5 minutes.
        plotdata(r).any_hit_five = nan(1,length(i));                        %Pre-allocate a matrix to hold the maximum number of hits in any 5 minutes.
        plotdata(r).any_trial_five = nan(1,length(i));                      %Pre-allocate a matrix to hold the maximum number of trials in any 5 minutes.
        plotdata(r).any_hitrate_five = nan(1,length(i));                    %Pre-allocate a matrix to hold the maximum hit rate in any 5 minutes.
        plotdata(r).min_iti = nan(1,length(i));                             %Pre-allocate a matrix to hold the minimum inter-trial interval.
        plotdata(r).impulse = nan(1,length(i));                             %Pre-allocate a matrix to hold the average peak impulse for each session.
        plotdata(r).stage = cell(1,length(i));                              %Pre-allocate a cell array to hold the stage name for each session..
        for s = 1:length(i)                                                 %Step through each session.
            plotdata(r).peak(s) = mean(data(i(s)).peak);                    %Save the mean signal peak for each session.
            plotdata(r).impulse(s) = mean(data(i(s)).impulse);              %Save the mean signal impulse peak for each session.
            plotdata(r).hitrate(s) = mean(data(i(s)).outcome == 'H');       %Save the hit rate for each session.
            plotdata(r).numtrials(s) = length(data(i(s)).outcome);          %Save the total number of trials for each session.
            plotdata(r).stage{s} = data(i(s)).stage;                        %Save the stage for each session.
            times = data(i(s)).starttime;                                   %Grab the trial start times.
            times = 86400*(times - data(i(s)).timestamp);                   %Convert the trial start times to seconds relative to the first trial.            
            if any(times >= 300)                                            %If the session lasted for at least 5 minutes...
                plotdata(r).first_hit_five(s) = ...
                    sum(data(i(s)).outcome(times <= 300) == 'H');           %Count the number of hits in the first 5 minutes.
                plotdata(r).first_trial_five(s) = sum(times <= 300);        %Count the number of trials in the first 5 minutes.
                a = zeros(length(times)-1,2);                               %Create a matrix to hold hit counts in any 5 minutes.                
                for j = 1:length(times) - 1                                 %Step through each trial.
                    a(j,2) = sum(times(j:end) - times(j) <= 300);           %Count the number of trials within 5 minutes of each trial.
                    a(j,1) = sum(times(j:end) - times(j) <= 300 & ...
                        data(i(s)).outcome(j:end) == 'H');                  %Count the number of hits within 5 minutes of each trial.
                end
                plotdata(r).any_hit_five(s) = nanmax(a(:,1));               %Find the maximum number of hits in any 5 minutes.
                plotdata(r).any_trial_five(s) = nanmax(a(:,2));             %Find the maximum number of trials in any 5 minutes.
                a(a(:,2) < 10,:) = NaN;                                     %Kick out any epochs with fewer than 10 trials.
                a = a(:,1)./a(:,2);                                         %Calculate the hit rate within each 5 minute epoch.
                plotdata(r).any_hitrate_five(s) = nanmax(a);                %Find the maximum hit rate of trials in any 5 minutes.
            end
            if length(times) > 1                                            %If there's more than one trial...
                times = diff(times);                                        %Calculate the inter-trial intervals.
                times = boxsmooth(times,10);                                %Box-smooth the inter-trial intervals over 10 trials.
                if length(times) > 11                                       %If there's mor than 11 trials...
                    plotdata(r).min_iti(s) = nanmin(times(6:end-5));        %Find the minimum inter-trial interval over full groups of 10 trials.
                else                                                        %Otherwise...
                    plotdata(r).min_iti(s) = times(round(length(times)/2)); %Set the minimum inter-trial interval to the middle-most value.
                end
            end
        end
    end    
    pos = get(0,'Screensize');                                              %Grab the screensize.
    h = 10;                                                                 %Set the height of the figure, in centimeters.    
    w = 15;                                                                 %Set the width of the figure, in centimeters.    
    fig = figure('numbertitle','off','units','centimeters',...
        'name',['MotoTrak Analysis: ' devices{d}],'menubar','none',...
        'position',[pos(3)/2-w/2, pos(4)/2-h/2, w, h]);                     %Create a figure.
    ui_h = 0.07*h;                                                          %Set the heigh of the uicontrols.
    fontsize = 0.6*28.34*ui_h;                                              %Set the fontsize for all uicontrols.
    sp1 = 0.02*h;                                                           %Set the vertical spacing between axes and uicontrols.
    sp2 = 0.01*w;                                                           %Set the horizontal spacing between axes and uicontrols.
    pos = [7*sp2,3*sp1,w-8*sp2,h-ui_h-5*sp1];                               %Set the position of the axes.
    ax = axes('units','centimeters','position',pos,'box','on',...
        'linewidth',2);                                                     %Create axes for showing the log events histogram.
    obj = zeros(1,5);                                                       %Create a matrix to hold timescale uicontrol handles.
    str = {'Overall Hit Rate',...
        'Total Trial Count',...
        'Mean Peak Force',...
        'Median Peak Force',...
        'Trial Count',...
        'Hits in First 5 Minutes',...
        'Trials in First 5 Minutes',...
        'Max. Hits in Any 5 Minutes',...
        'Max. Trials in Any 5 Minutes',...
        'Max. Hit Rate in Any 5 Minutes',...
        'Min. Inter-Trial Interval (Smoothed)',...
        'Mean Peak Impulse',...
        'Median Peak Impulse'};                                             %List the available plots for the pull data.
    if any(strcmpi(devices{d},{'knob','lever'}))                            %If we're plotting knob data...
        str(2:3) = {'Mean Peak Angle','Median Peak Angle'};                 %Set the plots to show "angle" instead of "force".
    elseif ~any(strcmpi(devices{d},{'knob','lever','pull'}))                %Otherwise, if this isn't pull, knob, or lever data...
        str(2:3) = {'Mean Signal Peak','Median Signal Peak'};               %Set the plots to show "signal" instead of "peak force".
    end    
    pos = [sp2, h-sp1-ui_h, 2*(w-6*sp2)/6, ui_h];                           %Set the position for the pop-up menu.
    obj(1) = uicontrol(fig,'style','popup','string',str,...
            'units','centimeters','position',pos,'fontsize',fontsize);      %Create pushbuttons for selecting the timescale.
    str = {'Session','Daily','Weekly','Export'};                            %List the timescale labels.
    for i = 2:5                                                             %Step through the 3 timescales.        
        pos = [i*sp2+i*(w-6*sp2)/6, h-sp1-ui_h, (w-6*sp2)/6, ui_h];         %Set the position for each pushbutton.
        obj(i) = uicontrol(fig,'style','pushbutton','string',str{i-1},...
            'units','centimeters','position',pos,'fontsize',fontsize);      %Create pushbuttons for selecting the timescale.
    end
    set(obj(1),'callback',{@Set_Plot_Type,obj});                            %Set the callback for the pop-up menu.
    set(obj(2:4),'callback',{@Plot_Timeline,obj,[]});                       %Set the callback for the timescale buttons.
    set(obj(5),'callback',{@Export_Data,ax,obj});                           %Set the callback for the export button.
    set(fig,'userdata',plotdata);                                           %Save the plot data to the figure's 'UserData' property.
    Plot_Timeline(obj(2),[],obj,[]);                                        %Call the function to plot the session data in the figure.
    set(fig,'ResizeFcn',{@Resize,ax,obj});                                  %Set the Resize function for the figure.
end


%% This function is called when the user selects a plot type in the pop-up menu.
function Set_Plot_Type(~,~,obj)
i = strcmpi(get(obj,'fontweight'),'bold');                                  %Find the pushbutton with the bold fontweight.
Plot_Timeline(obj(i),[],obj,[]);                                            %Call the subfunction to plot the data by the appropriate timeline.


%% This subfunction sorts the data into single-session values and sends it to the plot function.
function Plot_Timeline(hObject,~,obj,fid)
set(hObject,'fontweight','bold','foregroundcolor',[0 0.5 0]);               %Make this pushbutton's text bold.
set(setdiff(obj(2:4),hObject),'fontweight','normal','foregroundcolor','k'); %Make the other pushbutton's text normal and black.
fig = get(hObject,'parent');                                                %Grab the parent figure of the pushbutton.
data = get(fig,'userdata');                                                 %Grab the plot data from the figure's 'UserData' property.
i = find(hObject == obj);                                                   %Find the index of the button that called the function.
t = unique(horzcat(data.times));                                            %Horizontally concatenate all of the timestamps.
if i == 2                                                                   %If the user wants to plot by session...
    t = [t; t + 0.00001]';                                                  %Set the time bounds to enclose only a single session.
elseif i == 3                                                               %If the user wants to plot by day...
    t = unique(fix(t));                                                     %Find the unique truncated serial date numbers.
    t = [t; t + 1]';                                                        %Set the time bounds to go from the start of the day to the end of the day.
else                                                                        %Otherwise, if the user wants to plot by week...
    t = [min(fix(t)), max(fix(t))];                                         %Find the first and last timestamp.
    i = find(strcmpi({'sun','mon','tue','wed','thu','fri','sat'},...
        datestr(t(1),'ddd')));                                              %Find the index for the day of the week of the first timestamp.
    t(1) = t(1) - i + 1;                                                    %Round down the timestamp to the nearest Sunday.
    t = t(1):7:t(2);                                                        %Find the timestamps for weekly spacing.
    t = [t; t + 7]';                                                        %Set the time bounds to go from the start of the week to the end of the week.
end
str = get(obj(1),'string');                                                 %Grab the strings from the pop-up menu.
i = get(obj(1),'value');                                                    %Grab the value of the pop-up menu.
str = str{i};                                                               %Grab the selected plot type.
plotdata = struct([]);                                                      %Create a structure to hold plot data.
for r = 1:length(data)                                                      %Step through each rat in the data structure.
    plotdata(r).rat = data(r).rat;                                          %Copy the rat name to the plot data structure.
    y = nan(1,size(t,1));                                                   %Pre-allocate a matrix to hold the data y-coordinates.
    s = cell(1,size(t,1));                                                  %Pre-allocate a cell array to hold the last stage of each time frame.
    n = cell(1,size(t,1));                                                  %Pre-allocate a cell array to hold the hit rate and trial count text.
    for i = 1:size(t,1)                                                     %Step through the specified time frames.
        j = data(r).times >= t(i,1) & data(r).times < t(i,2);               %Find all sessions within the time frame.
        if any(j)                                                           %If any sessions are found.
            if strcmpi(str,'overall hit rate')                              %If we're plotting overall hit rate...
                y(i) = nanmean(data(r).hitrate(j));                         %Grab the mean hit rate over this time frame.
            elseif strcmpi(str,'total trial count')                         %If we're plotting trial count...
                y(i) = nanmean(data(r).numtrials(j));                       %Grab the mean number of trials over this time frame.
            elseif any(strcmpi(str,{'median peak force',...
                    'median peak angle','median signal peak'}))             %If we're plotting the median signal peak...
                y(i) = nanmedian(data(r).peak(j));                          %Grab the mean signal peak over this time frame.
            elseif any(strcmpi(str,{'mean peak force',...
                    'mean peak angle','mean signal peak'}))                 %If we're plotting the mean signal peak...
                y(i) = nanmean(data(r).peak(j));                            %Grab the mean signal peak over this time frame.
            elseif strcmpi(str,'trial count')                               %If we're plotting number of trials....
                y(i) = nanmean(data(r).numtrials(j));                       %Grab the mean number of trials over this time frame.
            elseif strcmpi(str,'hits in first 5 minutes')                   %If we're plotting the hit count within the first 5 minutes.
                y(i) = nanmean(data(r).first_hit_five(j));                  %Grab the mean number of hits within the first 5 minutes over this time frame.
            elseif strcmpi(str,'trials in first 5 minutes')                 %If we're plotting the trial count within the first 5 minutes.
                y(i) = nanmean(data(r).first_trial_five(j));                %Grab the mean number of hits within the first 5 minutes over this time frame.
            elseif strcmpi(str,'max. hits in any 5 minutes')                %If we're plotting the maximum hit count within any 5 minutes.
                y(i) = nanmean(data(r).any_hit_five(j));                    %Grab the mean maximum number of hits within any 5 minutes over this time frame.
            elseif strcmpi(str,'max. trials in any 5 minutes')              %If we're plotting the maximum trial count within any 5 minutes.
                y(i) = nanmean(data(r).any_trial_five(j));                  %Grab the mean maximum number of trials within any 5 minutes over this time frame.
            elseif strcmpi(str,'max. hit rate in any 5 minutes')            %If we're plotting the maximum hit rate within any 5 minutes.
                y(i) = nanmean(data(r).any_hitrate_five(j));                %Grab the mean maximum hit rate within any 5 minutes over this time frame.
            elseif strcmpi(str,'min. inter-trial interval (smoothed)')      %If we're plotting the minimum inter-trial interval.
                y(i) = nanmean(data(r).min_iti(j));                         %Grab the mean minimum inter-trial interval over this time frame.
            elseif strcmpi(str,'median peak impulse')                       %If we're plotting the median signal impulse...
                y(i) = nanmedian(data(r).impulse(j));                       %Grab the mean signal impulse over this time frame.
            elseif strcmpi(str,'mean peak impulse')                         %If we're plotting the mean signal impulse...
                y(i) = nanmean(data(r).impulse(j));                         %Grab the mean signal impulse over this time frame.
            end
            temp = [nanmean(data(r).hitrate(j)),...
                nansum(data(r).numtrials(j))];                              %Grab the mean hit rate and total number of trials over this time frame.
            temp(1) = temp(1)*temp(2);                                      %Calculate the number of hits in the total number of trials.
            n{i} = sprintf('%1.0f hits/%1.0f trials',temp);                 %Create a string showing the number of hits and trials.
            j = find(j,1,'last');                                           %Find the last matching session.
            s{i} = data(r).stage{j};                                        %Save the last stage the rat ran on for this trime frame.            
        end
    end
    plotdata(r).x = t(~isnan(y),:);                                         %Grab the daycodes at the start of each time frame.
    plotdata(r).y = y(~isnan(y))';                                          %Save only the non-NaN y-coordinates.
    plotdata(r).s = s(~isnan(y))';                                          %Save the stage information for each time frame.
    plotdata(r).n = n(~isnan(y))';                                          %Save the hit rate and trial information for each time frame.
end
ax = get(fig,'children');                                                   %Grab all children of the figure.
ax(~strcmpi(get(ax,'type'),'axes')) = [];                                   %Kick out all non-axes objects.
if isempty(fid)                                                             %If no text file handle was passed to this function...
    Make_Plot(plotdata,ax,str);                                             %Call the subfunction to make the plot.
else                                                                        %Otherwise...
    t = vertcat(plotdata.x);                                                %Vertically concatenate all time-points.
    t = unique(t,'rows');                                                   %Find all unique rows of the timepoints.
    fprintf(fid,'%s,\t','DATE/TIME');                                       %Print a date column header.
    for r = 1:length(plotdata)                                              %Step through the rats.
        if r == length(plotdata)                                            %If this is the last rat...
            fprintf(fid,'%s,\n',plotdata(r).rat);                           %Print the rat name followed by a carraige return.
        else                                                                %Otherwise...
            fprintf(fid,'%s,\t',plotdata(r).rat);                           %Print the rat name followed by a tab.
        end
    end
    for i = 1:size(t,1)                                                     %Step through each time-point.
        if rem(t(i,1),1) ~= 0                                               %If the timestamp is a fractional number of days...
            fprintf(fid,'%s,\t',datestr(t(i,1),'mm/dd/yyyy - HH:MM'));      %Show the date and the time.
        elseif t(i,2) - t(i,1) == 1                                         %If the timestamps only cover one day...
            fprintf(fid,'%s,\t',datestr(t(i,1),'mm/dd/yyyy'));              %Show only the date.
        else                                                                %Otherwise...
            fprintf(fid,'%s,\t',[datestr(t(i,1),'mm/dd/yyyy') '-' ...
                datestr(t(i,2)-1,'mm/dd/yyyy')]);                           %Show the date range.
        end
        for r = 1:length(plotdata)                                          %Step through the rats.
            j = plotdata(r).x(:,1) == t(i,1) & ...
                plotdata(r).x(:,1) == t(i,1);                               %Find any matching timepoint for this rat.
            if any(j)                                                       %If any matching timepoint was found...
                fprintf(fid,'%1.3f,',plotdata(r).y(j));                     %Print the value for this date range.
            else                                                            %Otherwise...
                fprintf(fid,'-,');                                          %Print a hyphen.
            end
            if r == length(plotdata)                                        %If this is the last rat...
                fprintf(fid,'\n');                                          %Print a carraige return.
            else                                                            %Otherwise...
                fprintf(fid,'\t');                                          %Print a tab.
            end
        end
    end
end


%% This subfunction sorts the data into daily values and sends it to the plot function.
function Export_Data(hObject,~,ax,obj)
output = questdlg(['Would you like to export the data as a spreadsheet '...
    'or figure image?'],'Data Type?','Spreadsheet','Image','Both','Both');  %Ask the user if they'd like to export the data as a spreadsheet or image.
fig = get(hObject,'parent');                                                %Grab the parent figure of the export button.
if any(strcmpi({'image','both'},output))                                    %If the user wants to save an image...
    [file, path] = uiputfile({'*.png';'*.jpg';'*.pdf'},...
        'Save Figure Image');                                               %Ask the user for a filename.
    if file(1) == 0                                                         %If the user clicked cancel...
        return                                                              %Skip execution of the rest of the function.
    end
    set(fig,'units','centimeters','resizefcn',[]);                          %Set the figure units to centimeters.
    pos = get(fig,'position');                                              %Get the figure position.
    temp = get(fig,'color');                                                %Grab the curret figure color.
    set(fig,'paperunits','centimeters','papersize',pos(3:4),...
        'inverthardcopy','off','paperposition',[0 0 pos(3:4)],'color','w'); %Set the paper size and paper position, in centimeters.
    w = pos(3);                                                             %Grab the width of the figure.
    h = pos(4);                                                             %Grab the height of the figure.
    ui_h = 0.07*h;                                                          %Set the height of all uicontrols.
    sp1 = 0.02*h;                                                           %Set the vertical spacing between axes and uicontrols.
    sp2 = 0.01*w;                                                           %Set the horizontal spacing between axes and uicontrols.
    pos = [7*sp2,3*sp1,w-8*sp2,h-4*sp1];                                    %Create an axes position matrix.
    set(ax,'units','centimeters','position',pos);                           %Expand the axes to fill the figure.
    set(obj,'visible','off');                                               %Make all of the other figures invisible.
    drawnow;                                                                %Immediately update the figure.    
    a = find(file == '.',1,'last');                                         %Find the start of the file extension.
    ext = file(a:end);                                                      %Grab the file extension.
    if strcmpi(ext,'.png')                                                  %If the user chose to save as a PNG...
        print(fig,[path file(1:a-1)],'-dpng');                              %Save the figure as a PNG file.
    elseif strcmpi(ext,'.jpg')                                              %If the user chose to save as a JPEG...
        print(fig,[path file(1:a-1)],'-djpeg');                             %Save the figure as a JPEG file.
    elseif strcmpi(ext,'.pdf')                                              %If the user chose to save as a PDF...
        print(fig,[path file(1:a-1)],'-dpdf');                              %Save the figure as a PDF file.
    end
    pos = [7*sp2,3*sp1,w-8*sp2,h-ui_h-5*sp1];                               %Create an axes position matrix.
    set(ax,'units','centimeters','position',pos);                           %Reset the position of the axes.
    set(obj,'visible','on');                                                %Make all of the other figures visible again.
    i = strcmpi(get(obj,'fontweight'),'bold');                              %Find the pushbutton with the bold fontweight.
    Plot_Timeline(obj(i),[],obj,[]);                                        %Call the subfunction to plot the data by the appropriate timeline.
    set(fig,'color',temp,'ResizeFcn',{@Resize,ax,obj});                     %Set the Resize function for the figure.
    drawnow;                                                                %Immediately update the figure.    
end
if any(strcmpi({'spreadsheet','both'},output))                              %If the user wants to save a spreadsheet...
    temp = get(ax,'ylabel');                                                %Grab the handle for the axes y-label.
    file = lower(get(temp,'string'));                                       %Grab the axes y-axis label.
    file(file == ' ') = '_';                                                %Replace any spaces with underscores.
    for i = '<>:"/\|?*().'                                                  %Step through all reserved characters.
        file(file == i) = [];                                               %Kick out any reserved characters.
    end
    file = [file '_' datestr(now,'yyyymmdd')];                              %Add today's date to the default filename.
    temp = lower(get(fig,'name'));                                          %Grab the figure name.
    file = [temp(20:end) '_' file];                                         %Add the device name to the default filename.
    [file, path] = uiputfile('*.csv','Save Spreadsheet',file);              %Ask the user for a filename.
    if file(1) == 0                                                         %If the user clicked cancel...
        return                                                              %Skip execution of the rest of the function.
    end
    fid = fopen([path file],'wt');                                          %Open a new text file to write the data to.   
    i = strcmpi(get(obj,'fontweight'),'bold');                              %Find the pushbutton with the bold fontweight.
    Plot_Timeline(obj(i),[],obj,fid);                                       %Call the subfunction to write the data by the appropriate timeline.
    fclose(fid);                                                            %Close the figure.
    winopen([path file]);                                                   %Open the CSV file.
end


%% This section plots session/daily/weekly data in the specified axes.
function Make_Plot(plotdata,ax,str)
fig = get(ax,'parent');                                                     %Grab the figure handle for the axes' parent.
set(fig,'units','centimeters');                                             %Set the figure's units to centimeters.
temp = get(fig,'position');                                                 %Grab the figure position.
h = temp(4);                                                                %Grab the figure height, in centimeters.
linewidth = 0.1*h;                                                          %Set the linewidth for the plots.
markersize = 0.75*h;                                                        %Set the marker size for the plots.
fontsize = 0.6*h;                                                           %Set the fontsize for all text objects.
cla(ax);                                                                    %Clear the axes.
colors = hsv(length(plotdata));                                             %Grab unique colors for all the rats.
hoverdata = struct([]);                                                     %Create an empty structure to hold data for the MouseHover function.
for r = 1:length(plotdata)                                                  %Step through each rat.
    line(mean(plotdata(r).x,2),plotdata(r).y,'color',colors(r,:),...
        'linewidth',linewidth,'userdata',1,'parent',ax);                    %Show the rat's performance as a thick line.
    for i = 1:size(plotdata(r).x,1)                                         %Step through each timepoint.
        l = line(mean(plotdata(r).x(i,:)),plotdata(r).y(i),...
            'markeredgecolor',colors(r,:),'linestyle','none',...
            'markerfacecolor',colors(r,:),'marker','.',...
            'linewidth',linewidth,'markersize',markersize,'userdata',2,...
            'parent',ax);                                                   %Mark each session with a unique marker.        
        hoverdata(end+1).xy = [mean(plotdata(r).x(i,:)),plotdata(r).y(i)];  %Save the x- and y-coordinates.
        if rem(plotdata(r).x(i,1),1) ~= 0                                   %If the timestamp is a fractional number of days...
            temp = datestr(plotdata(r).x(i,1),'mm/dd/yyyy, HH:MM');         %Show the date and the time.
        elseif plotdata(r).x(i,2) - plotdata(r).x(i,1) == 1                 %If the timestamps only cover one day...
            temp = datestr(plotdata(r).x(i,1),'mm/dd/yyyy');                %Show only the date.
        else                                                                %Otherwise...
            temp = [datestr(plotdata(r).x(i,1),'mm/dd/yyyy') '-' ...
                datestr(plotdata(r).x(i,2)-1,'mm/dd/yyyy')];                %Show the date range.
        end
        hoverdata(end).txt = {plotdata(r).rat,plotdata(r).s{i},...
            temp,plotdata(r).n{i}};                                         %Save the rat's name, stage, date, and hit rate/num trials.
        hoverdata(end).handles = l;                                         %Save the line handle for the point.
    end
end
temp = vertcat(hoverdata.xy);                                               %Grab all of the datapoint coordinates.
x = [min(temp(:,1)), max(temp(:,1))];                                       %Find the minimim and maximum x-values.
x = x + [-0.05,0.05]*(x(2) - x(1));                                         %Add some padding to the x-axis limits.
if length(x) < 2 || any(isnan(x))                                           %If there are any missing x values.
    x = now + [-1,1];                                                       %Set arbitrary x-axis limits.
end
if x(1) == x(2)                                                             %If the x-axis limits are the same...
    x = x + [-1,1];                                                         %Add a day to each side of the single point.
end
xlim(ax,x);                                                                 %Set the x-axis limits.
y = [min(temp(:,2)), max(temp(:,2))];                                       %Find the minimim and maximum x-values.
y = y + [-0.05,0.05]*(y(2) - y(1));                                         %Add some padding to the y-axis limits.
if length(y) < 2 || any(isnan(y))                                           %If there are any missing y values.
    y = [0,1];                                                              %Set arbitrary y-axis limits.
end
if y(1) == y(2)                                                             %If the y-axis limits are the same...
    y = y + [-0.1, 0.1];                                                    %Add 0.1 to each side of the single point.
end
ylim(ax,y);                                                                 %Set the y-axis limits.
set(ax,'xticklabel',datestr(get(ax,'xtick'),'mm/dd'),...
    'fontsize',fontsize,'fontweight','bold','linewidth',linewidth);         %Show the date for each x-axis tick.
ylabel(ax,str,'fontweight','bold','fontsize',1.1*fontsize);                 %Label the x-axis.
temp = get(ax,'ytick');                                                     %Grab the y-axis ticks.
for i = 1:length(temp)                                                      %Step through the y-axis ticks.
    temp(i) = line(xlim(ax),temp(i)*[1,1],'color',[0.75 0.75 0.75],...
        'linestyle','--','linewidth',0.5*linewidth,'userdata',3,...
        'parent',ax);                                                       %Draw a gridline at each y-tick.
end
uistack(temp,'bottom');                                                     %Send all grid lines to the bottom.
txt = text(x(1),y(1),' ','fontsize',fontsize,'margin',2,...
    'backgroundcolor','w','edgecolor','k','visible','off',...
    'verticalalignment','bottom','horizontalalignment','center',...
    'userdata',4);                                                          %Create a text object for labeling points.
set(fig,'WindowButtonMotionFcn',{@MouseHover,ax,hoverdata,txt});            %Set the mouse hover function for the figure.


%% This function executes while the mouse hovers over the axes of an interactive figure.
function MouseHover(~,~,ax,data,txt)
xy = get(ax,'CurrentPoint');                                                %Grab the current mouse position in the axes.
xy = xy(1,1:2);                                                             %Pare down the x-y coordinates matrix.
a = [get(ax,'xlim'), get(ax,'ylim')];                                       %Grab the x- and y-axis limits.
if xy(1) >= a(1) && xy(1) <= a(2) && xy(2) >= a(3) && xy(2) <= a(4)         %If the mouse was clicked inside the axes...
    fig = get(ax,'parent');                                                 %Grab the parent figure of the axes.
    set(fig,'units','centimeters');                                         %Set the figure units to centimeters
    pos = get(fig,'position');                                              %Grab the current figure size, in centimeters.
    markersize = 0.75*pos(4);                                               %Set the marker size for the plots.
    xy = (xy - a([1,3]))./[a(2) - a(1), a(4) - a(3)];                       %Normalize the mouse x-y coordinates.
    temp = vertcat(data.xy);                                                %Vertically concatenate the point x-y coordinates.
    temp(:,1) = (temp(:,1) - a(1))/(a(2) - a(1));                           %Normalize the point x coordinates.
    temp(:,2) = (temp(:,2) - a(3))/(a(4) - a(3));                           %Normalize the point 3 coordinates.
    set(ax,'units','centimeters');                                          %Set the axes position units to centimeters.
    a = get(ax,'position');                                                 %Get the axes position.
    xy = xy.*a(3:4);                                                        %Calculate the axes position in centimeters.
    temp(:,1) = a(3)*temp(:,1);                                             %Calculate the point x coordinates in centimeters
    temp(:,2) = a(4)*temp(:,2);                                             %Calculate the point y coordinates in centimeters
    d = [xy(1) - temp(:,1), xy(2) - temp(:,2)];                             %Calculate the x-y distances from the mouse to the points.
    d = sqrt(sum(d.^2,2));                                                  %Find the straight-line distance to each point.
    if any(d <= 0.5)                                                        %If we're within half a centimeter of any data point...
        i = find(d == min(d),1,'first');                                    %Find the closest point.
        xy = data(i).xy;                                                    %Grab the data point x-y coordinate.
        temp = xlim(ax);                                                    %Grab the x-axis limits.
        if xy(1) < temp(1) + 0.25*(temp(2) - temp(1))                       %If the data point is in the left-most quartile...
            set(txt,'horizontalalignment','left');                          %Set the horizontal alignment to left-hand.
        elseif xy(1) > temp(1) + 0.75*(temp(2) - temp(1))                   %If the data point is in the right-most quartile...
            set(txt,'horizontalalignment','right');                         %Set the horizontal alignment to right-hand.
        else                                                                %Otherwise...
            set(txt,'horizontalalignment','center');                        %Set the horizontal alignment to centered.
        end
        if xy(2) > mean(ylim(ax))                                           %If the data point is in the top half...
            xy(2) = xy(2) - 0.05*range(ylim);                               %Adjust the y coordinate to place the text below the data point.
            set(txt,'verticalalignment','top');                             %Set the vertical alignment to top.
        else                                                                %Otherwise...
            xy(2) = xy(2) + 0.05*range(ylim);                               %Adjust the y coordinate to place the text above the data point.
            set(txt,'verticalalignment','bottom');                          %Set the vertical alignment to bottom.
        end
        temp = get(txt,'position');                                         %Grab the current text object position.
        str = get(txt,'string');                                            %Grab the rat's name.
        if ~isequal(xy,temp) || ~strcmpi(str,data(i).txt)                   %If the current label is incorrect...
            set(txt,'position',xy,'string',data(i).txt,'visible','on');     %Update the position, string, and visibility of the text object.
            set(data(i).handles,'markersize',2*markersize);                 %Make the selected marker larger.
            set(setdiff([data.handles],data(i).handles),...
                'markersize',markersize);                                   %Make the other markers smaller.
        end
    else                                                                    %Otherwise...
        set([data.handles],'markersize',markersize);                        %Make all markers smaller.
        set(txt,'visible','off');                                           %Make the text object invisible.
    end
end


%% This function is called whenever the main figure is resized.
function Resize(hObject,~,ax,obj)
set(hObject,'units','centimeters');                                         %Set the figure units to centimeters
pos = get(hObject,'position');                                              %Grab the current figure size, in centimeters.
w = pos(3);                                                                 %Grab the width of the figure.
h = pos(4);                                                                 %Grab the height of the figure.
ui_h = 0.07*h;                                                              %Set the heigh of all uicontrols.
sp1 = 0.02*h;                                                               %Set the vertical spacing between axes and uicontrols.
sp2 = 0.01*w;                                                               %Set the horizontal spacing between axes and uicontrols.
fontsize = 0.6*28.34*ui_h;                                                  %Set the fontsize for all uicontrols.
pos = [7*sp2,3*sp1,w-8*sp2,h-ui_h-5*sp1];                                   %Create an axes position matrix.
set(ax,'units','centimeters','position', pos);                              %Reset the position of the axes.
pos = [sp2, h-sp1-ui_h, 2*(w-6*sp2)/6, ui_h];                               %Create the position matrix for the pop-up menu.
set(obj(1),'units','centimeters','position',pos,'fontsize',fontsize);       %Set the position of the pop-up menu.
for i = 2:5                                                                 %Step through the timescale and export pushbuttons.      
    pos = [i*sp2+i*(w-6*sp2)/6, h-sp1-ui_h, (w-6*sp2)/6, ui_h];             %Create the position matrix for each pushbutton.
    set(obj(i),'units','centimeters','position',pos,'fontsize',fontsize);   %Set the pushbutton position and fontsize.
end
linewidth = 0.1*h;                                                          %Set the linewidth for the plots.
markersize = 0.75*h;                                                        %Set the marker size for the plots.
fontsize = 0.6*h;                                                           %Set the fontsize for all text objects.
obj = get(ax,'children');                                                   %Grab all children of the axes.
temp = vertcat(get(obj,'userdata'));                                        %Vertically concatenate the 'UserData' from each objects.
temp = vertcat(temp{:});                                                    %Change the 'UserData' from a cell array to matrix.
i = strcmpi(get(obj,'type'),'line') & temp == 1;                            %Find all plot line objects.
set(obj(i),'linewidth',linewidth);                                          %Set the linewidth for all plot line objects.
i = strcmpi(get(obj,'type'),'line') & temp == 2;                            %Find all plot marker objects.
set(obj(i),'markersize',markersize);                                        %Set the markersize for all plot line objects.
i = strcmpi(get(obj,'type'),'line') & temp == 3;                            %Find all grid line objects.
set(obj(i),'linewidth',0.5*linewidth);                                      %Set the linewidth for all grid line objects.
i = strcmpi(get(obj,'type'),'text');                                        %Find all text objects.
set(obj(i),'fontsize',fontsize);                                            %Set the font size for all text objects.
set(ax,'fontsize',fontsize,'linewidth',linewidth);                          %Set the axes linewidth and fontsize.
temp = get(ax,'ylabel');                                                    %Grab the y-axis label handle for the axes.
set(temp,'fontsize',1.1*fontsize);                                          %Set the font size for y-axis label.


function MotoTrak_Knob_Viewer(varargin)

[files, path] = uigetfile('*.ArdyMotor','Select MotoTrak Files',...
    'multiselect','on');                                                    %Have the user pick an input *.ArdyMotor file or files.
if ~iscell(files) && files(1) == 0                                          %If no file was selected...
    return                                                                  %Exit the function.
end
cd(path);                                                                   %Change the current directory to the specified folder.
if ischar(files)                                                            %If only one file was selected...
    files = {files};                                                        %Convert the string to a cell array.
end

for f = 1:length(files)                                                     %Step through each file.
    handles = ArdyMotorFileRead(files{f});                                  %Read in the data from the *.ArdyMotor file.
    if ~isfield(handles,'trial') || isempty(handles.trial)                  %If there's no trials in this data file...
        warning('ARDYMOTOR2TEXT:NoTrials',['WARNING FROM '...
            'ARDYMOTOR2TEXT: The file "' files{f} '" has zero trials '...
            'and will be skipped.']);                                       %Show a warning.
        continue                                                            %Skip to the next file.
    end
    handles.file = files{f};                                                %Save the filename.
    handles.ir_thresh = [1023, 0];                                          %Create a matrix to hold the IR signal bounds.
    for t = 1:length(handles.trial)                                         %Step through each trial.
        handles.ir_thresh(1) = ...
            min([handles.trial(t).ir; handles.ir_thresh(1)]);               %Find the new minimum for each trial.
        handles.ir_thresh(2) = ...
            max([handles.trial(t).ir; handles.ir_thresh(2)]);               %Find the new maximum for each trial.
        s = median(double(diff(handles.trial(t).sample_times)));            %Find the median inter-sample interval for each trial.
        if s == 0                                                           %If all the inter-sample intervals are the same...
            handles.trial(t).sample_times = ...
                (10*(1:length(handles.trial(t).signal)) - 1010)';           %Use the sample times from a different trial in place of the bad times on the curren trial.
        end
    end
    handles.cur_trial = 1;                                                  %Set the current trial to 1.
    handles.num_trials = length(handles.trial);                             %Grab the number of trials.
    handles = Make_GUI(handles);                                            %Create the GUI.
    ShowTrial(handles,handles.cur_trial);                                   %Show the first trial.
    set(handles.slider,'callback',@SliderClick);                            %Set the callback for action on the slider.
    set(handles.savebutton,'callback',@SavePlot);                           %Set the callback for the save plot pushbutton.
    guidata(handles.fig,handles);                                           %Pin the handles structure to the GUI.
end


%% This function displays the force and IR traces from the selected trial.
function ShowTrial(handles,t)
pos = get(handles.fig,'position');                                          %Grab the main figure position.
area(handles.trial(t).sample_times,handles.trial(t).ir,...
    'linewidth',2,'facecolor',[1 0.5 0.5],'parent',handles.ir_axes,...
    'basevalue',handles.ir_thresh(2));                                      %Show the IR signal as an area plot.
set(handles.ir_axes,'ylim',handles.ir_thresh,'ydir','reverse',...
    'xlim',handles.trial(t).sample_times([1,end]),'xticklabel',[],...
    'ytick',[]);                                                            %Set the IR axes properties.
ylabel('IR Signal','parent',handles.ir_axes,'fontsize',0.75*pos(4),...
    'rotation',0,'verticalalignment','middle',...
    'horizontalalignment','right');                                         %Label the IR signal.
set(handles.label,'string',['Subject: ' handles.rat ', Trial ' ...
    num2str(t) '/' num2str(handles.num_trials) ', ' ...
    datestr(handles.trial(t).starttime,'HH:MM:SS, mm/dd/yy')],...
    'fontsize',0.75*pos(4));                                                %Update the trial label.
area(handles.trial(t).sample_times,handles.trial(t).signal,...
    'linewidth',2,'facecolor',[0.5 0.5 1],'parent',handles.force_axes);     %Show the force signal as an area plot.

% [pks, sig] = Knob_Peak_Finder(handles.trial(t).signal);                     %find peaks
% set(sig, pks, '*r', 'parent', handles.force_axes);

min_max = [min(handles.trial(t).signal), max(handles.trial(t).signal)];     %Grab the minimum and maximum of the signal.
set(handles.force_axes,'xlim',handles.trial(t).sample_times([1,end]),...
    'ylim',min_max + [-0.05,0.1]*range(min_max),'fontsize',0.5*pos(4));     %Set the force axes properties.
line([0,0],min_max(2)+[0.02,0.08]*range(min_max),'color','k',...
    'parent',handles.force_axes,'linewidth',2);                             %Draw a line to show the start of the hit window.
line(1000*[1,1]*handles.trial(t).hitwin,...
    min_max(2)+[0.02,0.08]*range(min_max),'color','k',...
    'parent',handles.force_axes,'linewidth',2);                             %Draw a line to show the end of the hit window.
line([0,1000*handles.trial(t).hitwin],...
    min_max(2)+0.05*range(min_max)*[1,1],'color','k',...
    'parent',handles.force_axes,'linestyle','--','linewidth',2);            %Draw a line to show the length of the hit window.
text(500*handles.trial(t).hitwin,min_max(2)+0.05*range(min_max),...
    'Hit Window','margin',2,'edgecolor','w','backgroundcolor','w',...
    'fontsize',0.5*pos(4),'fontweight','bold',...
    'parent',handles.force_axes,'horizontalalignment','center',...
    'verticalalignment','middle');                                          %Label the hit window.
a = line([0,0],get(handles.force_axes,'ylim'),'color',[0.5 0.5 0.5],...
    'parent',handles.force_axes,'linewidth',2,'linestyle','--');            %Draw a gray dotted line to show the start of the hit window.
uistack(a,'bottom');                                                        %Move the dotted line to the bottom of the stack.
a = line(1000*[1,1]*handles.trial(t).hitwin,...
    get(handles.force_axes,'ylim'),'color',[0.5 0.5 0.5],...
    'parent',handles.force_axes,'linewidth',2,'linestyle','--');            %Draw a gray dotted line to show the end of the hit window.
uistack(a,'bottom');                                                        %Move the dotted line to the bottom of the stack.
if max(get(handles.force_axes,'ylim')) > handles.trial(t).init              %If the y-axis scale is large enough to show the initiation threshold...
    line([-100,max(get(handles.force_axes,'xlim'))],...
        handles.trial(t).init*[1,1],'color',[0 0.5 0],...
        'parent',handles.force_axes,'linewidth',2,'linestyle','--');        %Draw a line showing the initiation threshold.
    text(-100,handles.trial(t).init,'Initiation ','color',[0 0.5 0],...
        'fontsize',0.5*pos(4),'fontweight','bold',...
        'parent',handles.force_axes,'horizontalalignment','right',...
        'verticalalignment','middle');                                      %Label the initiation threshold.
end
if max(get(handles.force_axes,'ylim')) > handles.trial(t).thresh            %If the y-axis scale is large enough to show the hit threshold...
    line([-100,max(get(handles.force_axes,'xlim'))],...
        handles.trial(t).thresh*[1,1],'color',[0.5 0 0],...
        'parent',handles.force_axes,'linewidth',2,'linestyle','--');        %Draw a line showing the hit threshold.
    text(-100,handles.trial(t).thresh,'Hit Threshold ',...
        'color',[0.5 0 0],'fontsize',0.5*pos(4),'fontweight','bold',...
        'parent',handles.force_axes,'horizontalalignment','right',...
        'verticalalignment','middle');                                      %Label the hit threshold.
end
ylabel('Angle (degrees)','parent',handles.force_axes,'fontsize',0.75*pos(4));%Label the force signal.
xlabel('Time (ms)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the time axis.


%% This function executes when the user interacts with the slider.
function SliderClick(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
handles.cur_trial = round(get(hObject,'value'));                            %Set the current trial to the value of the slider.
if handles.cur_trial < 1                                                    %If the current trial is less than 1...
    handles.cur_trial = 1;                                                  %Set the current trial to 1.
elseif handles.cur_trial > handles.num_trials                               %Otherwise, if the current trials is greater than the total number of trials.
    handles.cur_trial = handles.num_trials;                                 %Set the current trial to the last trial.
end
set(hObject,'value',handles.cur_trial);                                     %Update the value of the slider.
ShowTrial(handles,handles.cur_trial);                                       %Show the current trial.
guidata(hObject,handles);                                                   %Pin the handles structure back to the GUI.


%% This function executes when the user interacts with the slider.
function SavePlot(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
filename = [handles.file(1:end-10) '_TRIAL' ...
    num2str(handles.cur_trial,'%03.0f') '.png'];                            %Create a default filename for the PNG file.
[filename, path] = uiputfile('*.png','Name Image File',filename);           %Ask the user for a new filename.
if filename(1) == 0                                                         %If the user selected cancel...
    return                                                                  %Exit the function.
end
set([handles.slider,handles.savebutton],'visible','off','enable','off');    %Make the uicontrols invisible and disable them.
% fix_dotted_line_export(handles.force_axes);                                 %Fix the dotted lines in the force axes.
pos = get(handles.fig,'position');                                          %Grab the figure position.
temp = get(handles.fig,'color');                                            %Grab the starting color of the figure.
set(handles.fig,'paperpositionmode','auto',...
    'inverthardcopy','off',...
    'paperunits',get(handles.fig,'units'),...
    'papersize',pos(3:4),...
    'color','w');                                                           %Set the figure properties for printing.
set(handles.label,'backgroundcolor','w');                                   %Set the label background color to white.
drawnow;                                                                    %Immediately update the figure.
print(gcf,[path, filename],'-dpng','-r300');                                %Save the current image as a PNG file.
set([handles.slider,handles.savebutton],'visible','on','enable','on');      %Make the uicontrols visible and enabled again.
set(handles.fig,'color',temp);                                              %Reset the figure color to the original color.
set(handles.label,'backgroundcolor',temp);                                  %Set the label background color to the original color.


%% This subfunction creates the GUI.
function handles = Make_GUI(handles)
set(0,'units','centimeters');                                               %Set the system units to centimeters.
pos = get(0,'screensize');                                                  %Grab the screen size.
h = 0.8*pos(4);                                                             %Calculate the height of the figure.
w = 4*h/3;                                                                  %Scale the width of the figure to the height.
handles.fig = figure('MenuBar','none',...
    'numbertitle','off',...
    'name',['Pull Viewer: ' handles.file],...
    'units','centimeters',...
    'resize','on',...
    'ResizeFcn',@Resize,...
    'Position',[pos(3)/2-w/2, pos(4)/2-h/2, w, h]);                         %Create a figure.
handles.label =  uicontrol(handles.fig,'style','text',...
    'units','normalized',...
    'position',[0.01,0.95,0.98,0.04],...
    'string',[],...
    'fontsize',0.5*h,...
    'backgroundcolor',get(handles.fig,'color'),...
    'horizontalalignment','left',...
    'fontweight','bold');                                                   %Create a text label for showing the trial number and time.
handles.ir_axes = axes('units','normalized',...
    'position',[0.1,0.85,0.89,0.09],...
    'box','on',...
    'linewidth',2);                                                         %Create axes for showing the IR signal.
handles.force_axes = axes('units','normalized',...
    'position',[0.1,0.12,0.89,0.72],...
    'box','on',...
    'linewidth',2);                                                         %Create axes for showing the IR signal.
handles.slider = uicontrol(handles.fig,'style','slider',...
    'units','normalized',...
    'position',[0.01,0.01,0.78,0.04],...
    'value',1,...
    'min',1,...
    'max',handles.num_trials,...
    'SliderStep',[1/handles.num_trials, 0.1]);                              %Create a trial slider.
handles.savebutton = uicontrol(handles.fig,'style','pushbutton',...
    'units','normalized',...
    'position',[0.80,0.01,0.19,0.04],...
    'string','Save Plot (PNG)',...
    'fontsize',0.75*h);                                                     %Create a button for saving a plot image.


%% This function is called whenever the main figure is resized.
function Resize(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
pos = get(handles.fig,'position');                                          %Grab the main figure position.
ylabel('IR Signal','parent',handles.ir_axes,'fontsize',0.75*pos(4),...
    'rotation',0,'verticalalignment','middle',...
    'horizontalalignment','right');                                         %Label the IR signal with the new fontsize.
set([handles.label,handles.savebutton],'fontsize',0.75*pos(4));             %Update the trial label and savebutton fontsize.
objs = get(handles.force_axes,'children');                                  %Grab all children of the force axes.
objs(~strcmpi('text',get(objs,'type'))) = [];                               %Kick out all non-text objects.
set(objs,'fontsize',0.5*pos(4));                                            %Update the fontsize of all text objects.
ylabel('Angle (degrees)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the force signal.
xlabel('Time (ms)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the time axis.

%% This function finds peaks in the signal, accounting for equality of contiguous samples.
function [pks, sig] = Knob_Peak_Finder(signal)
    %This code finds and kicks out peaks that have a std dev between 
    %them less than 1
    
    smoothed_signal = boxsmooth(signal);                                        %smooth out the trial signal
    [pks, sig] = findpeaks(smoothed_signal, 'MINPEAKHEIGHT', 5, ...
        'MINPEAKDISTANCE', 10);                                            %Find local maximma
    n = length(pks);
    j = 1;
    if n>1
        while j <= n-1
            if (abs(pks(j)-pks(j+1)) <= 5)                                 % if the diff between 2 peaks is less than or equal to 5
                start_sig = sig(j);
                end_sig = sig(j+1);

                signal_interest = smoothed_signal(start_sig:end_sig);
                deviation_signal = std(signal_interest);

                if deviation_signal < 1
                    pks(j+1) = [];
                    sig(j+1) = [];
                    j = j-1;
                end

            end
            n = length(pks);
            j = j+1;
        end
    end


function MotoTrak_Analysis_Lever_Viewer(varargin)

[files, path] = uigetfile('*.ArdyMotor','Select MotoTrak Files',...
    'multiselect','on');                                                    %Have the user pick an input *.ArdyMotor file or files.
if ~iscell(files) && files(1) == 0                                          %If no file was selected...
    return                                                                  %Exit the function.
end
cd(path);                                                                   %Change the current directory to the specified folder.
if ischar(files)                                                            %If only one file was selected...
    files = {files};                                                        %Convert the string to a cell array.
end

for f = 1:length(files)                                                     %Step through each file.
    handles = ArdyMotorFileRead(files{f});                                  %Read in the data from the *.ArdyMotor file.
    if ~isfield(handles,'trial') || isempty(handles.trial)                  %If there's no trials in this data file...
        warning('ARDYMOTOR2TEXT:NoTrials',['WARNING FROM '...
            'ARDYMOTOR2TEXT: The file "' files{f} '" has zero trials '...
            'and will be skipped.']);                                       %Show a warning.
        continue                                                            %Skip to the next file.
    end
    handles.file = files{f};                                                %Save the filename.
    handles.ir_thresh = [1023, 0];                                          %Create a matrix to hold the IR signal bounds.
    for t = 1:length(handles.trial)                                         %Step through each trial.
        handles.ir_thresh(1) = ...
            min([handles.trial(t).ir; handles.ir_thresh(1)]);               %Find the new minimum for each trial.
        handles.ir_thresh(2) = ...
            max([handles.trial(t).ir; handles.ir_thresh(2)]);               %Find the new maximum for each trial.
        s = median(double(diff(handles.trial(t).sample_times)));            %Find the median inter-sample interval for each trial.
        if s == 0                                                           %If all the inter-sample intervals are the same...
            handles.trial(t).sample_times = ...
                (10*(1:length(handles.trial(t).signal)) - 1010)';           %Use the sample times from a different trial in place of the bad times on the curren trial.
        end
    end
    if handles.ir_thresh(1) == handles.ir_thresh(2)                         %If there is no variation in the IR signal...
        handles.ir_thresh(1) = handles.ir_thresh(2) - 1;                    %Subtract one from the IR threshold minimum to calculate a dummy maximum.
    end
    handles.cur_trial = 1;                                                  %Set the current trial to 1.
    handles.num_trials = length(handles.trial);                             %Grab the number of trials.
    handles = Make_GUI(handles);                                            %Create the GUI.
    ShowTrial(handles,handles.cur_trial);                                   %Show the first trial.
    set(handles.slider,'callback',@SliderClick);                            %Set the callback for action on the slider.
    set(handles.savebutton,'callback',@SavePlot);                           %Set the callback for the save plot pushbutton.
    guidata(handles.fig,handles);                                           %Pin the handles structure to the GUI.
end


%% This function displays the force and IR traces from the selected trial.
function ShowTrial(handles,t)
pos = get(handles.fig,'position');                                          %Grab the main figure position.
area(handles.trial(t).sample_times,handles.trial(t).ir,...
    'linewidth',2,'facecolor',[1 0.5 0.5],'parent',handles.ir_axes,...
    'basevalue',handles.ir_thresh(2));                                      %Show the IR signal as an area plot.
set(handles.ir_axes,'ylim',handles.ir_thresh,'ydir','reverse',...
    'xlim',handles.trial(t).sample_times([1,end]),'xticklabel',[],...
    'ytick',[]);                                                            %Set the IR axes properties.
ylabel('IR Signal','parent',handles.ir_axes,'fontsize',0.75*pos(4),...
    'rotation',0,'verticalalignment','middle',...
    'horizontalalignment','right');                                         %Label the IR signal.
set(handles.label,'string',['Subject: ' handles.rat ', Trial ' ...
    num2str(t) '/' num2str(handles.num_trials) ', ' ...
    datestr(handles.trial(t).starttime,'HH:MM:SS, mm/dd/yy')],...
    'fontsize',0.75*pos(4));                                                %Update the trial label.
area(handles.trial(t).sample_times,handles.trial(t).signal,...
    'linewidth',2,'facecolor',[0.5 0.5 1],'parent',handles.force_axes);     %Show the force signal as an area plot.
min_max = [min(handles.trial(t).signal), max(handles.trial(t).signal)];     %Grab the minimum and maximum of the signal.
set(handles.force_axes,'xlim',handles.trial(t).sample_times([1,end]),...
    'ylim',min_max + [-0.05,0.1]*range(min_max),'fontsize',0.5*pos(4));     %Set the force axes properties.
line([0,0],min_max(2)+[0.02,0.08]*range(min_max),'color','k',...
    'parent',handles.force_axes,'linewidth',2);                             %Draw a line to show the start of the hit window.
line(1000*[1,1]*handles.trial(t).hitwin,...
    min_max(2)+[0.02,0.08]*range(min_max),'color','k',...
    'parent',handles.force_axes,'linewidth',2);                             %Draw a line to show the end of the hit window.
line([0,1000*handles.trial(t).hitwin],...
    min_max(2)+0.05*range(min_max)*[1,1],'color','k',...
    'parent',handles.force_axes,'linestyle','--','linewidth',2);            %Draw a line to show the length of the hit window.
text(500*handles.trial(t).hitwin,min_max(2)+0.05*range(min_max),...
    'Hit Window','margin',2,'edgecolor','w','backgroundcolor','w',...
    'fontsize',0.5*pos(4),'fontweight','bold',...
    'parent',handles.force_axes,'horizontalalignment','center',...
    'verticalalignment','middle');                                          %Label the hit window.
a = line([0,0],get(handles.force_axes,'ylim'),'color','k',...
    'parent',handles.force_axes,'linewidth',2,'linestyle','--');            %Draw a gray dotted line to show the start of the hit window.
uistack(a,'top');                                                        %Move the dotted line to the bottom of the stack.
a = line(1000*[1,1]*handles.trial(t).hitwin,...
    get(handles.force_axes,'ylim'),'color','k',...
    'parent',handles.force_axes,'linewidth',2,'linestyle','--');            %Draw a gray dotted line to show the end of the hit window.
uistack(a,'top');                                                        %Move the dotted line to the bottom of the stack.
ylabel('Angle (degrees)','parent',handles.force_axes,...
    'fontsize',0.75*pos(4));                                                %Label the force signal.
a = line(xlim,10*[1,1],'color',[0 0.5 0],...
    'parent',handles.force_axes,'linewidth',2,'linestyle','--');            %Draw a gray dotted line to show the end of the hit window.
uistack(a,'top');                                                        %Move the dotted line to the bottom of the stack.
a = line(xlim,10*[1,1],'color',[0 0.5 0],...
    'parent',handles.force_axes,'linewidth',2,'linestyle','--');            %Draw a gray dotted line to show the end of the hit window.
uistack(a,'top');                                                        %Move the dotted line to the bottom of the stack.
text(-900,10.1,'Press Threshold',...
    'fontsize',0.5*pos(4),...
    'color',[0 0.5 0],...
    'fontweight','bold',...
    'parent',handles.force_axes,...
    'horizontalalignment','left',...
    'verticalalignment','bottom');                                          %Label the hit window.
xlabel('Time (ms)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the time axis.


%% This function executes when the user interacts with the slider.
function SliderClick(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
handles.cur_trial = round(get(hObject,'value'));                            %Set the current trial to the value of the slider.
if handles.cur_trial < 1                                                    %If the current trial is less than 1...
    handles.cur_trial = 1;                                                  %Set the current trial to 1.
elseif handles.cur_trial > handles.num_trials                               %Otherwise, if the current trials is greater than the total number of trials.
    handles.cur_trial = handles.num_trials;                                 %Set the current trial to the last trial.
end
set(hObject,'value',handles.cur_trial);                                     %Update the value of the slider.
ShowTrial(handles,handles.cur_trial);                                       %Show the current trial.
guidata(hObject,handles);                                                   %Pin the handles structure back to the GUI.


%% This function executes when the user interacts with the slider.
function SavePlot(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
filename = [handles.file(1:end-10) '_TRIAL' ...
    num2str(handles.cur_trial,'%03.0f') '.png'];                            %Create a default filename for the PNG file.
[filename, path] = uiputfile('*.png','Name Image File',filename);           %Ask the user for a new filename.
if filename(1) == 0                                                         %If the user selected cancel...
    return                                                                  %Exit the function.
end
set([handles.slider,handles.savebutton],'visible','off','enable','off');    %Make the uicontrols invisible and disable them.
% fix_dotted_line_export(handles.force_axes);                                 %Fix the dotted lines in the force axes.
pos = get(handles.fig,'position');                                          %Grab the figure position.
temp = get(handles.fig,'color');                                            %Grab the starting color of the figure.
set(handles.fig,'paperpositionmode','auto',...
    'inverthardcopy','off',...
    'paperunits',get(handles.fig,'units'),...
    'papersize',pos(3:4),...
    'color','w');                                                           %Set the figure properties for printing.
set(handles.label,'backgroundcolor','w');                                   %Set the label background color to white.
drawnow;                                                                    %Immediately update the figure.
print(gcf,[path, filename],'-dpng','-r300');                                %Save the current image as a PNG file.
set([handles.slider,handles.savebutton],'visible','on','enable','on');      %Make the uicontrols visible and enabled again.
set(handles.fig,'color',temp);                                              %Reset the figure color to the original color.
set(handles.label,'backgroundcolor',temp);                                  %Set the label background color to the original color.


%% This subfunction creates the GUI.
function handles = Make_GUI(handles)
set(0,'units','centimeters');                                               %Set the system units to centimeters.
pos = get(0,'screensize');                                                  %Grab the screen size.
h = 0.8*pos(4);                                                             %Calculate the height of the figure.
w = 4*h/3;                                                                  %Scale the width of the figure to the height.
handles.fig = figure('MenuBar','none',...
    'numbertitle','off',...
    'name',['Pull Viewer: ' handles.file],...
    'units','centimeters',...
    'resize','on',...
    'ResizeFcn',@Resize,...
    'Position',[pos(3)/2-w/2, pos(4)/2-h/2, w, h]);                         %Create a figure.
handles.label =  uicontrol(handles.fig,'style','text',...
    'units','normalized',...
    'position',[0.01,0.95,0.98,0.04],...
    'string',[],...
    'fontsize',0.5*h,...
    'backgroundcolor',get(handles.fig,'color'),...
    'horizontalalignment','left',...
    'fontweight','bold');                                                   %Create a text label for showing the trial number and time.
handles.ir_axes = axes('units','normalized',...
    'position',[0.1,0.85,0.89,0.09],...
    'box','on',...
    'linewidth',2);                                                         %Create axes for showing the IR signal.
handles.force_axes = axes('units','normalized',...
    'position',[0.1,0.12,0.89,0.72],...
    'box','on',...
    'linewidth',2);                                                         %Create axes for showing the IR signal.
handles.slider = uicontrol(handles.fig,'style','slider',...
    'units','normalized',...
    'position',[0.01,0.01,0.78,0.04],...
    'value',1,...
    'min',1,...
    'max',handles.num_trials,...
    'SliderStep',[1/handles.num_trials, 0.1]);                              %Create a trial slider.
handles.savebutton = uicontrol(handles.fig,'style','pushbutton',...
    'units','normalized',...
    'position',[0.80,0.01,0.19,0.04],...
    'string','Save Plot (PNG)',...
    'fontsize',0.75*h);                                                     %Create a button for saving a plot image.


%% This function is called whenever the main figure is resized.
function Resize(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
pos = get(handles.fig,'position');                                          %Grab the main figure position.
ylabel('IR Signal','parent',handles.ir_axes,'fontsize',0.75*pos(4),...
    'rotation',0,'verticalalignment','middle',...
    'horizontalalignment','right');                                         %Label the IR signal with the new fontsize.
set([handles.label,handles.savebutton],'fontsize',0.75*pos(4));             %Update the trial label and savebutton fontsize.
objs = get(handles.force_axes,'children');                                  %Grab all children of the force axes.
objs(~strcmpi('text',get(objs,'type'))) = [];                               %Kick out all non-text objects.
set(objs,'fontsize',0.5*pos(4));                                            %Update the fontsize of all text objects.
ylabel('Angle (degrees)','parent',handles.force_axes,...
    'fontsize',0.75*pos(4));                                                %Label the angle signal.
xlabel('Time (ms)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the time axis.


function [data, varargout] = ArdyMotorFileRead(file)
%
%ARDYMOTORFILEREAD.m - Rennaker Neural Engineering Lab, 2011
%
%   ArdyMotorFileRead reads in the sensor recordings from motor behavioral
%   tasks controlled by an Arduino board.  The data is organized into a
%   MATLAB structure for easy analysis
%
%   data = ArdyMotorFileRead(file) reads in the behavioral record from the
%   *.ARDYMOTOR files specified by the string variable "file" into the
%   output "data" structure.
%
%   Last modified February 18, 2013, by Drew Sloan.

data = [];                                                                  %Start a data structure to receive the recordings.
fid = fopen(file,'r');                                                      %Open the file for read access.
fseek(fid,0,-1);                                                            %Rewind to the beginning of the file.

temp = dir(file);                                                           %Grab the file information.
% if temp.datenum >= 735264                                                   %If the file was created after January 29, 2013...
%     version = fread(fid,1,'int16');                                         %Read the version from the first two bytes as an signed integer.
%     if version > 0                                                          %If the integer is greater than zero...
%         version = 1;                                                        %Set the file format version to 1.
%     end
% else
    version = fread(fid,1,'int8');                                          %Read the file format version from the first byte as a signed integer.
    if (version ~= -1) || (version == -1 && daycode(temp.datenum) == 255)     %If the version isn't -1...
        if (version ~= -3)
            version = 1;                                                        %Set the file format version to 1.
        end
    end
% end

data.version = version;

if version < 0                                                              %If the file format version indicates ArdyMotor V2.0...
    
    if version == -1 || version == -3
        data.daycode = fread(fid,1,'uint16');                               %Read in the daycode.
    end
    data.booth = fread(fid,1,'uint8');                                      %Read in the booth number.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the rat's name.
    data.rat = fread(fid,N,'*char')';                                       %Read in the characters of the rat's name.
    data.position = fread(fid,1,'float32');                                 %Read in the device position, in centimeters.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the stage description.
    data.stage = fread(fid,N,'*char')';                                     %Read in the characters of the stage description.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the device name.
    data.device = fread(fid,N,'*char')';                                    %Read in the characters of the device name.
    fileinfo = dir(file);                                                   %Grab the input file information.
    
    data.cal = [1,0];                                                       %Assume by default that no calibration conversion was applied.
    if fileinfo.datenum < 735088                                            %If this file was written between 8/3/2012 and 8/6/2012
        data.device = 'Wheel';                                              %Set the device name to 'Wheel'.
        data.cal(1) = 0.5;                                                  %Set the degrees/tick calibration constant to 0.5.
    end
      
    if (version == -3)
        if any(strcmpi(data.device,{'pull', 'knob', 'lever'}))
            data.cal(:) = fread(fid,2,'float32');
        elseif any(strcmpi(data.device,{'wheel'}))
            data.cal(1) = fread(fid,1,'float32');                              
        end
    else      
        if any(strcmpi(data.device,{'pull'}))                                   %If the device was a pull...
            data.cal(:) = fread(fid,2,'float32');                               %Read in the grams/tick and baseline grams calibration coefficients.
        elseif any(strcmpi(data.device,{'wheel','knob'})) && ...
                fileinfo.datenum > 735088                                       %If the device was a wheel or a knob...
            data.cal(1) = fread(fid,1,'float32');                               %Read in the degrees/tick calibration coefficient.
        end
    end
    
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the constraint description.
    data.constraint = fread(fid,N,'*char')';                                %Read in the characters of the constraint description.
    N = fread(fid,1,'uint8');                                               %Read in the number of characters in the threshold type description.
    data.threshtype = fread(fid,N,'*char')';                                %Read in the characters of the threshold type  description.
    if version == -1 || version == -3                                       %If the file format version is -1...
        data.pre_trial_sampling_dur = 1000;                                 %Indicate 1000 milliseconds of pre-trial sampling.
    else                                                                    %Otherwise, for later versions...
        data.pre_trial_sampling_dur = fread(fid,1,'float32');               %Read in the pre-trial sampling duration (in milliseconds).
    end
    data.pauses = [];                                                       %Create a field in the data structure to hold pause times.
    data.manual_feeds = [];                                                 %Create a field in the data structure to hold manual feed times.
    while ~feof(fid)                                                        %Loop until the end of the file.
        trial = fread(fid,1,'uint32');                                      %Read in the trial number.
        if isempty(trial)                                                   %If no trial number was read...
            continue                                                        %Skip execution of the rest of the loop.
        end
        starttime = fread(fid,1,'float64');                                 %Read in the trial start time.
        outcome = fread(fid,1,'uint8');                                     %Read in the trial outcome.
        if outcome == 'P'                                                   %If the outcome was a pause...
            temp = fread(fid,1,'float64');                                  %Read in the end time of the pause.
            data.pauses(end+1,1:2) = [starttime, temp];                     %Save the start and end time of the pause.
        elseif outcome == 'F'                                               %If the outcome was a manual feeding.
            data.manual_feeds(end+1,1) = starttime;                         %Save the timing of the manual feeding.
        else                                                                %Otherwise, if the outcome was a hit or a miss...
            if fileinfo.datenum < 735122.5980                               %If a file was created before 9/10/2012...
                fseek(fid,-1,'cof');                                        %Rewind the file one byte.
            end

            data.trial(trial).starttime = starttime;                        %Save the trial start time.
            data.trial(trial).hitwin = fread(fid,1,'float32');              %Read in the hit window.
            data.trial(trial).init = fread(fid,1,'float32');                %Read in the initiation threshold.
            data.trial(trial).thresh = fread(fid,1,'float32');              %Read in the hit threshold.
            data.trial(trial).hittime = [];                                 %List no hit times for this trial by default.
            N = fread(fid,1,'uint8');                                       %Read in the number of hits for this trial.
            data.trial(trial).hittime = fread(fid,N,'float64');             %Read in the hit times for this trial.
            if fileinfo.datenum <  735122.5980                              %If a file was created before 9/10/2012...
                if N > 1                                                    %If there was at least one hit time...
                    outcome = 'H';                                          %Label the trial as a hit.
                else                                                        %Otherwise...
                    outcome = 'M';                                          %Label the trial as a miss.
                end
            end
            data.trial(trial).outcome = outcome;                            %Save the trial outcome.
            N = fread(fid,1,'uint8');                                       %Read in the number of VNS events for this trial.
            data.trial(trial).vnstime = fread(fid,N,'float64');             %Read in the VNS event times for this trial.
            buffsize = fread(fid,1,'uint32');                               %Read in the number of samples for this trial.
            data.trial(trial).sample_times = ...
                fread(fid,buffsize,'uint16');                               %Read in the sample times, in milliseconds.
            data.trial(trial).signal = fread(fid,buffsize,'float32');       %Read in the device signal.
            data.trial(trial).ir = fread(fid,buffsize,'int16');             %Read in the IR signal.
            if all(data.trial(trial).sample_times == 0)                     %If all of the sample times equal zero...
                if trial ~= 1                                               %If this isn't the first trial...
                    data.trial(trial).sample_times = ...
                        data.trial(trial-1).sample_times;                   %Use the previous trial's sample times.
                else                                                        %Otherwise...
                    data.trial(trial).sample_times = ...
                        int16(10*(1:length(data.trial(trial).signal)) - ...
                        data.pre_trial_sampling_dur)';                      %Subtract the pre-trial sampling duration from the sample times.
                end
            else                                                            %Otherwise...
                data.trial(trial).sample_times = ...
                    int16(data.trial(trial).sample_times) - ...
                    data.pre_trial_sampling_dur;                            %Subtract the pre-trial sampling duration from the sample times.
            end
        end
    end
    if version < -1 && isfield(data,'trial') && ...
            isfield(data.trial,'starttime')                                 %If the file format version is newer than version -1 and the daycode function exists...
        data.daycode = daycode(data.trial(1).starttime);                    %Find the daycode for this file.
    end
elseif version == 1                                                         %If the file format version is 1...
    fseek(fid,0,-1);                                                        %Rewind to the beginning of the file.
    data.version = 1;                                                       %Version of the struct
    data.daycode = fread(fid,1,'uint16');                                   %DayCode.
    data.booth = fread(fid,1,'uint8');                                      %Booth number.
    N = fread(fid,1,'uint8');                                               %Number of characters in the rat's name.
    data.rat = fread(fid,N,'*char')';                                       %Characters of the rat's name.
    data.position = fread(fid,1,'uint8');                                   %Position of the input device (0-3 inches).
    data.responsewindow = fread(fid,1,'uint8');                             %Response window.
    data.stage(1) = fread(fid,1,'uchar');                                   %First character of the stage title stage number.
    temp = fread(fid,1,'uchar');                                            %Read in the next character.
    while temp(end) ~= data.stage(1)                                        %Loop until we get to the first letter of the device description.
        temp = [temp, fread(fid,1,'uchar')];                                %Read in the next character.
    end
    data.stage = char(horzcat(data.stage,temp(1:end-1)));                   %Concatenate the stage name together.
    if data.stage(1) == 'P'                                                 %If the stage is a pull stage...
        data.device = horzcat(temp(end),fread(fid,3,'*char')');             %Read in the selected device (Pull).
    else                                                                    %Otherwise, if the stage is a lever or wheel stage...
        data.device = horzcat(temp(end),fread(fid,4,'*char')');             %Read in the selected device (Lever or Wheel).
    end
    data.bin = fread(fid,1,'uint8');                                        %Bin Size.
    numparams = fread(fid,1,'uint8');                                       %Number of stimulus parameters.
    for i = 1:numparams                                                     %Step through each stimulus parameter.
        N = fread(fid,1,'uint16');                                          %Number of characters in a parameter name.
        data.param(i).name = fread(fid,N,'*char')';                         %Parameter name.
    end
    trial = 0;                                                              %Start a trial counter.
    while ~feof(fid)                                                        %Loop until the end of the file.
        trial=trial+1;                                                      %Increment the trial counter.
        try                                                                 %Try to read in a trial, abort if the trial is corrupted.
            data.trial(trial).threshold = fread(fid,1,'uint16');            %Read Threshold.
            data.trial(trial).starttime = fread(fid,1,'float64');           %Read trial start time.
            data.trial(trial).hittime = fread(fid,1,'float64');             %Read hit/reward time.
            data.trial(trial).outcome = fread(fid,1,'float64');             %Read Trial Outcome.
            for i = 1:3                                                     %Step through the three IR inputs.
                numIR = fread(fid,1,'uint32');                              %Read the number of breaks on the IR input.
                data.trial(trial).IR1(i).times = ...
                    fread(fid,numIR,'float64');                             %Read in the timestamp for the IR break.
            end
            numbins = fread(fid,1,'uint32');                                %Read in the number of signal datapoints.
            data.trial(trial).signal = fread(fid,numbins,'float64');        %Read in the sensory signal.
        catch ME                                                            %If an error occurs...
            lastwarn(['ARDYMOTORFILEREAD: ' ME.message],...
                ['ARDYMOTORFILEREAD:' ME.identifier]);                      %Save the warning ID.
        end
    end
    if isempty(data.trial(end).starttime)                                   %If the last trial had no data...
        data.trial(end) =[];                                                %Remove the empty trial.
    end
end
if isfield(data,'trial') && ~isempty(data.trial)                            %If there's at least one trial...
    data.daycode = fix(data.trial(1).starttime);                            %Set the daycode to the fixed first trial timestamp.
end
fclose(fid);                                                                %Close the input file.
varargout{1} = version;                                                     %Output the format version number if the user asked for it.


%% This subfunction returns the daycode (1-365) for a given date.
function d = daycode(date)
date = datevec(date);                                                       %Convert the serial date number to a date vector.
year = date(1);                                                             %Pull the year out of the date vector.
month = date(2);                                                            %Pull out the month.
day = date(3);                                                              %Pull out the day.
if year/4 == fix(year/4);                                                   %If the year is a leap year...
    numDays = [31 29 31 30 31 30 31 31 30 31 30 31];                        %Include 29 days in February.
else                                                                        %Otherwise...
	numDays = [31 28 31 30 31 30 31 31 30 31 30 31];                        %Include 28 days in February.
end
date = sum(numDays(1:(month-1)));                                           %Sum the days in the preceding months...
d = date + day;                                                             %...and add the day of the specified month.


function y = range(x,dim)
%RANGE  Sample range.
%   Y = RANGE(X) returns the range of the values in X.  For a vector input,
%   Y is the difference between the maximum and minimum values.  For a
%   matrix input, Y is a vector containing the range for each column.  For
%   N-D arrays, RANGE operates along the first non-singleton dimension.
%
%   RANGE treats NaNs as missing values, and ignores them.
%
%   Y = RANGE(X,DIM) operates along the dimension DIM.
%
%   See also IQR, MAD, MAX, MIN, STD.

%   Copyright 1993-2004 The MathWorks, Inc.
%   $Revision: 1.1.8.1 $  $Date: 2010/03/16 00:17:06 $

if nargin < 2
    y = max(x) - min(x);
else
    y = max(x,[],dim) - min(x,[],dim);
end


function MotoTrak_PopData_to_TSV(varargin)
%
%MOTOTRAK_POPDATA_TO_TSV.m - Vulintus, Inc., 2016.
%
%   MOTOTRAK_POPDATA_TO_TSV reads data from batches of MotoTrak session
%   files (with either *.ArdyMotor or *.MotoTrak file extensions), computes
%   session averages for user-selected task parameters, and then outputs
%   the averages into an Excel-readable TSV (tab-separated values)
%   spreadsheet, with each line corresponding to one behavioral session.
%
%   UPDATE LOG:
%   12/14/2016 - Drew Sloan - Added support for MotoTrak 2.0 (*.MotoTrak)
%       data files.
%

fields = [];                                                                %Create a structure to hold all possible output fields.                 
fields.mandatory = {    'DATE (DD/MM/YYYY)';
                        'START TIME (HH:MM)';
                        'SUBJECT';
                        'STAGE'};                                           %List the mandatory columns that will be displayed in every TSV file.
                    
fields.optional =   {   'BOOTH';
                        'POSITION';
                        'TOTAL FEEDS';
                        'NUMBER OF TRIALS';
                        'HITS';
                        'MISSES';
                        'MANUAL FEEDS';
                        'INITATION TO HIT LATENCY (s)';
                        'HITS IN FIRST 5 MINUTES';
                        'TRIALS IN FIRST 5 MINUTES';
                        'MAX HITS IN ANY 5 MINUTES';
                        'MAX TRIALS IN ANY 5 MINUTES';                        
                        'MAX HIT RATE IN ANY 5 MINUTES';
                        'MIN. INTER-TRIAL INTERVAL (s)'};                   %List the optional columns that apply to every task type.
                        
fields.pull =       {   'MEAN PEAK FORCE (gm)';
                        'MAX PEAK FORCE (gm)';
                        'MAX HIT THRESHOLD (gm)';
                        'LATENCY TO PEAK FORCE (s)';
                        'PULL ATTEMPTS PER TRIAL';
                        'MEAN ATTEMPT FORCE (gm)';
                        'FATIGUE RATIO (LAST 10/FIRST 10)';
                        'MEAN PULL DURATION (s)';
                        'MEAN PEAK IMPULSE (gm/s)'};                        %List the optional columns that apply to only the isometric pull tasks.
                        
fields.knob =       {   'MEAN PEAK ANGLE (degrees)';
                        'MAX PEAK ANGLE (degrees)';
                        'MAX HIT THRESHOLD (degrees)';
                        'LATENCY TO PEAK ANGLE (s)';
                        'TURN ATTEMPTS PER TRIAL';
                        'MEAN ATTEMPT ANGLE (degrees)';
                        'FATIGUE RATIO (LAST 10/FIRST 10)';
                        'MEAN TURN DURATION (s)';
                        'MEAN PEAK ROTATIONAL VELOCITY (degrees/s)'};       %List the optional columns that apply to only the supination tasks. 
                    
fields.lever =      {   'MEAN PEAK ANGLE (degrees)';
                        'MAX PEAK ANGLE (degrees)';
                        'MAX HIT THRESHOLD (degrees)';
                        'LATENCY TO PEAK ANGLE (s)';
                        'TURN ATTEMPTS PER TRIAL';
                        'MEAN ATTEMPT ANGLE (degrees)';
                        'FATIGUE RATIO (LAST 10/FIRST 10)';
                        'MEAN TURN DURATION (s)';
                        'MEAN PEAK ROTATIONAL VELOCITY (degrees/s)'};       %List the optional columns that apply to only the lever press tasks. 

                    
%Set the expected path for configuration files.
if nargin > 0                                                               %If there's more than one optional input argument.
    configpath = varargin{end};                                             %Assume the last input is the AppData path.
else                                                                        %Otherwise, if no AppData path was passed to the function...
    if isdeployed                                                           %If the function is running as compiled code...
        configpath = pwd;                                                   %Use the current folder as the AppData path.
    else                                                                    %Otherwise, if the function is running in MATLAB...
        [configpath,~,~] = fileparts(which(mfilename));                     %Use the folder containing the function as the AppData path.        
    end
end
if isempty(configpath)                                                      %If the AppData path couldn't be set by any of the previous methods...
    configpath = pwd;                                                       %Use the current folder as the AppData path.
end


%Have the user choose a path containing data files to analyze.
datapath = 'C:\MotoTrak\';                                                  %Set the expected primary local data path for saving data files.
if ~exist(datapath,'dir')                                                   %If the primary local data path doesn't exist...
    datapath = pwd;                                                         %Set the default path to the current directory.
end
datapath = uigetdir(datapath,'Where is your MotoTrak data located?');       %Ask the user where their data is located.
if datapath(1) == 0                                                         %If the user pressed "cancel"...
    return                                                                  %Skip execution of the rest of the function.
end


%Find all of the MotoTrak data files in the data path.
files = file_miner(datapath,{'*.ArdyMotor','*.MotoTrak'});                  %Find all LPS *.ArdyMotor files in the LPS folders.
pause(0.01);                                                                %Pause for 10 milliseconds.
if isempty(files)                                                           %If no files were found...
    errordlg('No MotoTrak data files were found in the that directory!');   %Show an error dialog box.
end


%Have the user select subjects to include or exclude in the analysis.
subjects = files;                                                           %Copy the filenames to another cell array.
for r = 1:length(subjects)                                                  %Step through each file.
    [~,filename,~] = fileparts(files{r});                                   %Grab the basic filename from the full filename.
    i = strfind(filename,'_20');                                            %Find the start of the timestamp.
    if isempty(i) || length(i) > 1                                          %If no timestamp was found in the filename, or multiple timestamps were found...
        subjects{r} = [];                                                   %Set the rat name to empty brackets.
    else                                                                    %Otherwise...
        subjects{r} = filename(1:i-1);                                      %Kick out all characters of the filename except the rat name.
    end
end
subject_list = unique(subjects);                                            %Make a list of all the unique rat names.
i = listdlg('PromptString','Which subjects would you like to include?',...
    'name','MotoTrak Analysis',...
    'SelectionMode','multiple',...
    'listsize',[300 400],...
    'initialvalue',1:length(subject_list),...
    'uh',25,...
    'ListString',subject_list);                                             %Have the user pick subjects to include.
if isempty(i)                                                               %If the user clicked "cancel" or closed the dialog...
    return                                                                  %Skip execution of the rest of the function.
else                                                                        %Otherwise...
    subject_list = subject_list(i);                                         %Pare down the rat list to those that the user selected.
end
keepers = ones(length(subjects),1);                                         %Create a matrix to check which files match the selected rat names.
for r = 1:length(subjects)                                                  %Step through each file's rat name.
    if ~any(strcmpi(subject_list,subjects{r})) && ~isempty(subjects{r})     %If this file's rat name wasn't selected and a rat name was found in the filename...
        keepers(r) = 0;                                                     %Mark the file for exclusion.
    end
end
files(keepers == 0) = [];                                                   %Kick out all files the user doesn't want to include.


%Step through all of the data files and load them into a structure.
waitbar = big_waitbar('title','Loading MotoTrak Files...');                 %Create a waitbar figure.
data = [];                                                                  %Create a structure to receive data.
for f = 1:length(files)                                                     %Step through the data files.
    [~,filename,ext] = fileparts(files{f});                                 %Grab the filename and extension from the trial.
    if waitbar.isclosed()                                                   %If the user closed the waitbar figure...
        errordlg(['The MotoTrak export population data to TSV file was '...
            'cancelled by the user!'],'Export Cancelled');                  %Show an error.
        return                                                              %Skip execution of the rest of the function.
    else                                                                    %Otherwise...
        waitbar.string(sprintf('Loading (%1.0f/%1.0f): %s%s',...
            f, length(files), filename, ext));                              %Update the waitbar text.
        waitbar.value(f/length(files));                                     %Update the waitbar value.
    end
    try                                                                     %Try to read in the data file...
        switch ext                                                          %Switch between the possible file extensions.
            case '.ArdyMotor'                                               %If the data file is an *.ArdyMotor file.
                session = ArdyMotorFileRead(files{f});                      %Read in the data from each file.
            case '.MotoTrak'                                                %If the data file is a *.MotoTrak file.
                session = MotoTrakFileRead(files{f});                       %Read in the data from each file.                
        end
    catch err                                                               %If an error occurs...
        warning(['ERROR READING: ' files{f}]);                              %Show which file had a read problem...
        warning(err.message);                                               %Show the actual error message.
        continue                                                            %Skip to the next file.
    end    
    if any(strcmpi(subject_list,session.subject))                           %If this rat is one of the selected subjects.
        s = length(data) + 1;                                               %Create a new field index.
        if strcmpi(ext,'.MotoTrak')                                         %If the file is in the *.MotoTrak format...
            session = MotoTrak_to_ArdyMotor(session);                       %Convert the *.MotoTrak format data to *ArdyMotor format data.
        end
        for field = {'subject','device','stage','manual_feeds','booth',...
                'position'}                                                 %Step through the fields we want to save from the data file...
            data(s).(field{1}) = session.(field{1});                        %Grab each field from the data file and save it.
        end
        data(s).device = lower(data(s).device);                             %Make the device name all lowercase.
        if isfield(session,'trial') && length(session.trial) >= 1           %If there was at least 1 trial...            
            data(s).outcome = vertcat(session.trial.outcome);               %Grab the outcome of each trial.
            data(s).thresh = vertcat(session.trial.thresh);                 %Grab the threshold for each trial.
            data(s).starttime = vertcat(session.trial.starttime);           %Grab the start time for each trial.
            data(s).peak = nan(length(session.trial),1);                    %Create a matrix to hold the peak force.
            data(s).impulse = nan(length(session.trial),1);                 %Create a matrix to hold the peak impulse.
            data(s).hittime = 86400*(vertcat(session.trial.hittime) - ...
                vertcat(session.trial.starttime));                          %Create a matrix to hold the time to hit.
            data(s).hittime(data(s).hittime <= 0) = NaN;                    %Replace all miss trial hit times with NaNs.
            all_peak_width = [];                                            %Create a matrix to hold all peak widths.
            data(s).hit_peak_width = nan(length(session.trial),1);          %Create a matrix to hold hit peak widths.
            attempt_force = [];                                             %Create a matrix to hold attempt force.
            data(s).time_to_peak = nan(length(session.trial),1);            %Create a matrix to hold the time to peak force.
            data(s).pre_hit_attempts = nan(length(session.trial),1);        %Create a matrix to hold the time to peak force.
            for t = 1:length(session.trial)                                 %Step through every trial.
                signal = session.trial(t).signal;                           %Grab the signal for the trial.
                times = double(session.trial(t).sample_times);              %Grab the sampling times for the trial.
                if any(signal > 2500)                                       %If any part of the signal is greater than 2500...
                    data(s).outcome(t) = NaN;                               %Exclude the trial outcome.
                    data(s).hittime(t) = NaN;                               %Exclude the trial hit time.
                    continue                                                %Exclude this trial from the analysis.
                end
                hitwin = 1000*session.trial(t).hitwin;                      %Find the hit window for this trial, in milliseconds.
                init = session.trial(t).init;                               %Grab the initiation threshold.
                thresh = session.trial(t).thresh;                           %Grab the hit threshold.
                hit_i = find(session.trial(t).sample_times >= 0 & ...
                    session.trial(t).sample_times < hitwin);                %Find the indices for samples in the hit window.
                if isempty(hit_i)                                           %If there's no samples in the hit window...
                    continue                                                %Exclude this trial from the analysis.
                end
                data(s).peak(t) = max(signal(hit_i));                       %Find the maximum force in each hit window.
                data(s).impulse(t) = max(diff(signal(hit_i)));              %Find the maximum impulse in each hit window.
                i = find(signal(1:end-1) <= session.trial(t).init...
                    & signal(2:end) > session.trial(t).init) + 1;           %Find all positive-going initiation threshold crossings.
                j = find(signal(1:end-1) > session.trial(t).init...
                    & signal(2:end) <= session.trial(t).init) + 1;          %Find all negative-going initiation threshold crossings.
                if isempty(i)                                               %If the signal never rose through the threshold...
                    continue                                                %Exclude this trial from the analysis.
                end
                j(j < i(1)) = [];                                           %Kick out any negative-going initiation threshold crossings before the first positive-going crossing.
                if isempty(j)                                               %If the rat never released the device...
                    continue                                                %Exclude this trial from the analysis.
                end
                num_attempts = 0;                                           %Start an attempt counter.
                for k = 1:length(j)                                         %Step through all peaks in the signal.
                    num_attempts = num_attempts + 1;                        %Add to the number of attempts.
                    s1 = times(i(k)-1) + (times(i(k)) - ...
                        times(i(k)-1))*(init-signal(i(k)-1)) / ...
                        (signal(i(k))-signal(i(k)-1));                      %Interpolate the start time of the peak.
                    s2 = times(j(k)-1) + (times(j(k)) - ...
                        times(j(k)-1))*(init - signal(j(k)-1)) / ...
                        (signal(j(k))-signal(j(k)-1));                      %Interpolate the stop time of the peak.
                    [p, s3] = max(signal(i(k):j(k)));                       %Find the peak height.
                    s3 = times(i(k)+s3-1);                                  %Find the peak time.
                    snippet = signal(i(k)-1:j(k));                          %Grab the signal for the peak.
                    if any(snippet >= thresh) && ...
                            isnan(data(s).hit_peak_width(t)) && ...
                            any(s3 == hit_i)                                %If this is the first supra-threshold peak for this trial...                        
                        data(s).hit_peak_width(t) = (s2-s1)/1000;           %Save the hit peak width for this trial.
                        data(s).time_to_peak(t) = (s3-s1)/1000;             %Save the hit time-to-peak for this trial.
                        data(s).pre_hit_attempts(t) = num_attempts - 1;     %Save the number of attempts preceding the hit.
                    end
                    all_peak_width(end+1) = s2 - s1;                        %Add this peak's width to the all peaks width list.
                    attempt_force(end+1) = p;                               %Save the peak force for all peaks.
                end
            end
            data(s).all_peak_width = nanmean(all_peak_width)/1000;          %Save the mean peak width for all peaks.
            data(s).attempt_force = nanmean(attempt_force);                 %Save the mean attempt force.
            if length(data(s).peak) >= 20                                   %If there's at least 20 samples...
                data(s).fatigue = nanmean(data(s).peak(end-9:end))/...
                    nanmean(data(s).peak(1:10));                            %Calculate the ratio of the first 10 trial's peak force to the last 10 trials peak force.
            end
            data(s).timestamp = data(s).starttime(1);                       %Grab the timestamp from the start of the first trial.            
        else                                                                %Otherwise, if there's not trials.
            for str = {'outcome','thresh','starttime','peak','impulse',...
                    'hittime','hit_peak_width','time_to_peak',...
                    'pre_hit_attempts','all_peak_width','attempt_force',...
                    'fatigue','timestamp','file'};                          %Step through each field of the data structure.
                data(s).(str{1}) = NaN;                                     %Set each value to NaN.
            end
            if strcmpi(ext,'.MotoTrak')                                     %If the file is in the *.MotoTrak format...
                data(s).timestamp = session.start_time;                     %Use the start-time as the session timestamp.
            else                                                            %Otherwise, if the file is in the *.ArdyMotor format...                
                info = dir(files{f});                                       %Grab the file information.
                if ((info.datenum - session.daycode) < 1 && ...
                        (info.datenum - session.daycode) >= 0) || ...
                        session.daycode < 734139                            %If the file hasn't been editted since the specified day code...
                    data(s).timestamp = info.datenum;                       %Set the timestampes to the last file modification date.
                else                                                        %Otherwise...
                    data(s).timestamp = session.daycode;                    %Set the timestampes to the recorded daycode.
                end
            end
        end
        a = find(files{f} == '\',1,'last');                                 %Find the last forward slash in the filename.
        session = files{f}(a+1:end);                                        %Grab the filename minus the path.
        data(s).file = session;                                             %Save the filename.
    end
end
if isempty(data)                                                            %If no data files were found...
    errordlg(['There were no MotoTrak data files with 5 or more trials '...
        'for the selected subjects!']);                                         %Show an error dialog box.
end
[~,i] = sort([data.timestamp]);                                             %Find the indices to sort all files chronologically.
data = data(i);                                                             %Sort all files chronologically.
waitbar.close();                                                            %Close the waitbar.


%Find all devices used in the data and load any existing configuration files.
output = [];                                                                %Create a structure to hold the output fields for each device.
devices = unique({data.device});                                            %Grab the unique device names across all sessions.
for d = 1:length(devices)                                                   %Step through each device...
    filename = fullfile(configpath, [devices{d} '_popdata_tsv.config']);    %Create the expected configuration filename.
    if exist(filename,'file')                                               %If a configuration file already exists for this device type...
        fid = fopen(filename,'rt');                                         %Open the configuration file for reading.
        txt = fread(fid,'*char');                                           %Read in the data as characters.
        fclose(fid);                                                        %Close the configuration file.
        a = [0; find(txt == 10); length(txt) + 1];                          %Find all carriage returns in the text data.        
        for j = 1:length(a) - 1                                             %Step through all lines in the data.
            ln = txt(a(j)+1:a(j+1)-1)';                                     %Grab the line of text.
            ln(ln == 0) = [];                                               %Kick out all null characters.
            if ~isempty(ln)                                                 %If the area any non-null characters in the line...
                output.(devices{d}){j,1} = ln;                              %Each line will be a field name.
            end
        end
    else                                                                    %Otherwise...
        output.(devices{d}) = fields.mandatory;                             %Pre-populate the output with only the mandatory fields.
    end
end


%Have the user select which fields they'd like printed to the TSV files.
for d = 1:length(devices)                                                   %Step through each device...
    selected_fields = output.(devices{d});                                  %Grab the currently-selected fields.
    all_fields = vertcat(fields.mandatory, fields.optional,...
        fields.(devices{d}));                                               %Create a list of all available fields for this device.
    fig = Selection_GUI(selected_fields,fields.mandatory,all_fields,...
        devices{d});                                                        %Call the subfunction to create the selection GUI.
    uiwait(fig);                                                            %Wait for the user to make a selection.
    if ishandle(fig)                                                        %If the user didn't close the figure without choosing a port...
        objs = get(fig,'children');                                         %Grab all children of the figure.
        j = strcmpi(get(objs,'style'),'popupmenu');                         %Find all pop-up menu objects.
        objs(j == 0) = [];                                                  %Kick out all non-pop-up objects.
        str = get(objs(1),'string');                                        %Grab the string from the first object.
        j = cell2mat(vertcat(get(objs,'userdata')));                        %Grab the UserData property of all pop-up menu objects.
        k = cell2mat(vertcat(get(objs,'value')));                           %Grab the value of all pop-up menu objects.
        selected_fields = str(k(j));                                        %Grab the list of selected columns.        
        delete(fig);                                                        %Close the figure.
    else                                                                    %Otherwise, if the user closed the figure without choosing a port...
        delete(fig);                                                         %Close the figure.
        return                                                              %Skip execution of the rest of the function.
    end
    filename = fullfile(configpath, [devices{d} '_popdata_tsv.config']);    %Create the expected configuration filename.
    fid = fopen(filename,'wt');                                             %Open the configuration file for writing.
    for j = 1:length(selected_fields)                                       %Step through each selected field.
        fprintf(fid,'%s\n',selected_fields{j});                             %Print each column name to the configuration file.
    end
    fclose(fid);                                                            %Close the configuration file.
    output.(devices{d}) = selected_fields;                                  %Save the selected fields to the structure.
end


%Create individual spreadsheets for each of the device types.
files = cell(length(devices),1);                                            %Create a cell array to hold file names.
fid = nan(length(devices),1);                                               %Create a matrix to hold file identifiers.
path = [getenv('USERPROFILE') '\'];                                         %User the user profile root documents folder as the defaul path.
clc;                                                                        %Clear the command window.
for d = 1:length(devices)                                                   %Step through the devices.
    session = upper(['MotoTrak_' devices{d} '_Session_Data']);              %Create a default file name.
    [session, path] = uiputfile('*.tsv',...
        ['Save ' devices{d} ' Spreadsheet'],[path session]);                %Ask the user for a filename.
    if session(1) == 0                                                      %If the user clicked cancel...        
        fclose all;                                                         %Close all open binary files.
        for f = 1:(d-1)                                                     %Step through any preceding device's files...            
            delete(files{f});                                               %Delete the files.
        end
        errordlg(['The MotoTrak export to TSV file was cancelled by '...
            'the user!'],'Export Cancelled');                               %Show an error.
        return                                                              %Skip execution of the rest of the function.
    end
    files{d} = [path session];                                              %Save the filename with the specified path.
    fid(d) = fopen([path session],'wt');                                    %Open a new text file to write the data to.    
    if fid(d) == -1                                                         %If the file couldn't be opened...
        fclose all;                                                         %Close all open binary files.
        for f = 1:(d-1)                                                     %Step through any preceding device's files...
            delete(files{f});                                               %Delete the files.
        end
        errordlg(['Could not create the specified file! Make sure the '...
            'file isn''t currently open and retry.'],'File Write Error');   %Show an error.
        return                                                              %Skip execution of the rest of the function.
    end
    N = length(output.(devices{d}));                                        %Grab the total number of output columns.
    for i = 1:length(output.(devices{d}))                                   %Step through all of the output columns.
        fprintf(fid(d),'%s',output.(devices{d}){i});                        %Print each column heading.
        if i < N                                                            %If this isn't the last column...
            fprintf(fid(d),'\t');                                           %Print a tab separator.
        else                                                                %Otherwise, if this is the last column...
            fprintf(fid(d),'\n');                                           %Print a carriage return.
        end
    end
end
waitbar = big_waitbar;                                                      %Create a waitbar figure.
for d = 1:length(devices)                                                   %Step through the devices.
    waitbar.title(['Exporting ' devices{d} ' Data...']);                    %Change the title on the waitbar...
    s = strcmpi({data.device},devices{d});                                  %Find all sessions with each device.
    num_files = sum(s);                                                     %Keep track of the total number of files.
    counter = 0;                                                            %Create a counter to count through the files.
    subjects = unique({data(s).subject});                                   %Find all of the unique rat names that have used this device.  
    for r = 1:length(subjects)                                              %Step through each rat.
        i = find(strcmpi({data.subject},subjects{r}) & ...
            strcmpi({data.device},devices{d}));                             %Find all the session for this rat on this device
        for s = i                                                           %Step through each session.
            counter = counter + 1;                                          %Increment the session counter.
            if waitbar.isclosed()                                           %If the user closed the waitbar figure...
            	fclose all;                                                 %Close all open output files.
                delete(files{d});                                           %Delete the current file.
                errordlg(['The MotoTrak export to CSV file was '...
                    'cancelled by the user!'],'Export Cancelled');          %Show an error.
                return                                                      %Skip execution of the rest of the function.
            else                                                            %Otherwise...
                waitbar.string([sprintf('Exporting (%1.0f/%1.0f): ',...
                    [counter,num_files]) data(s).file]);                    %Update the waitbar text.
                waitbar.value(counter/num_files);                           %Update the waitbar value.
            end
            N = length(output.(devices{d}));                                %Grab the total number of output columns.
            times = data(s).starttime;                                      %Grab the trial start times.
            times = 86400*(times - data(s).timestamp);                      %Convert the trial start times to seconds relative to the first trial.  
            for i = 1:length(output.(devices{d}))                           %Step through all of the output columns.                     
                switch output.(devices{d}){i}                               %Switch between the possible column values.
                    case 'BOOTH'                                            
                        fprintf(fid(d),'%1.0f',data(s).booth);              %Print the booth number.
                    case 'DATE (DD/MM/YYYY)'
                        fprintf(fid(d),'%s',...
                            datestr(data(s).timestamp,'mm/dd/yyyy'));       %Print the start date.
                    case 'FATIGUE RATIO (LAST 10/FIRST 10)'
                        fprintf(fid(d),'%1.3f',data(s).fatigue);            %Print the fatigue ratio for the session.
                    case 'HITS'
                        fprintf(fid(d),'%1.0f',...
                            sum(data(s).outcome == 'H'));                   %Print the number of hits.
                    case 'HITS IN FIRST 5 MINUTES'
                        if any(times >= 300)                                %If the session lasted for at least 5 minutes...
                            j = (times <= 300);                             %Find all trials in the first 5 minutes.
                            fprintf(fid(d),'%1.0f',...
                                sum(data(s).outcome(j) == 'H'));            %Print the number of hits in the first 5 minutes.
                        else                                                %Otherwise, if the trial didn't last 5 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'INITATION TO HIT LATENCY (s)'
                        fprintf(fid(d),'%1.3f',nanmean(data(s).hittime));   %Print the mean initiation-to-hit latency.
                    case 'LATENCY TO PEAK ANGLE (s)'
                        fprintf(fid(d),'%1.3f',...
                            nanmean(data(s).time_to_peak));                 %Print the mean initiation-to-hit-peak latency.
                    case 'LATENCY TO PEAK FORCE (s)'
                        fprintf(fid(d),'%1.3f',...
                            nanmean(data(s).time_to_peak));                 %Print the mean initiation-to-hit-peak latency.
                    case 'MANUAL FEEDS'
                        fprintf(fid(d),'%1.0f',...
                            length(data(s).manual_feeds));                  %Print the number of manual feedings.    
                    case 'MAX HIT RATE IN ANY 5 MINUTES'
                        if any(times >= 300)                                %If the session lasted for at least 5 minutes...
                            a = zeros(length(times)-1,2);                   %Create a matrix to hold hit counts in any 5 minutes.                
                            for j = 1:length(times) - 1                     %Step through each trial.
                                a(j,2) = ...
                                    sum(times(j:end) - times(j) <= 300);    %Count the number of trials within 5 minutes of each trial.
                                a(j,1) = ...
                                    sum(times(j:end) - times(j) <= 300 & ...
                                    data(s).outcome(j:end) == 'H');         %Count the number of hits within 5 minutes of each trial.
                            end
                            fprintf(fid(d),'%1.0f',nanmax(a(:,1)));         %Print the maximum number of hits in any 5 minutes.
                            fprintf(fid(d),'%1.0f',nanmax(a(:,2)));         %Print the maximum number of trials in any 5 minutes.
                            a(a(:,2) < 10,:) = NaN;                         %Kick out any epochs with fewer than 10 trials.
                            a = a(:,1)./a(:,2);                             %Calculate the hit rate within each 5 minute epoch.
                            fprintf(fid(d),'%1.3f',nanmax(a(:,1)));         %Print the maximum hit rate within any 5 minutes.
                        else                                                %Otherwise, if the trial didn't last 5 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'MAX HIT THRESHOLD (degrees)'
                        fprintf(fid(d),'%1.3f',nanmax(data(s).thresh));     %Print the maximum hit threshold.     
                    case 'MAX HIT THRESHOLD (gm)'
                        fprintf(fid(d),'%1.3f',nanmax(data(s).thresh));     %Print the maximum hit threshold.     
                    case 'MAX HITS IN ANY 5 MINUTES'
                    case 'MAX PEAK ANGLE (degrees)'
                        fprintf(fid(d),'%1.3f',nanmax(data(s).peak));       %Print the maximum signal peaks for each session.
                    case 'MAX PEAK FORCE (gm)'
                        fprintf(fid(d),'%1.3f',nanmax(data(s).peak));       %Print the maximum signal peaks for each session.
                    case 'MAX TRIALS IN ANY 5 MINUTES'
                        if any(times >= 300)                                %If the session lasted for at least 5 minutes...
                            a = zeros(length(times)-1,1);                   %Create a matrix to hold hit counts in any 5 minutes.                
                            for j = 1:length(times) - 1                     %Step through each trial.
                                a(j) = sum(times(j:end) - times(j) <= 300); %Count the number of trials within 5 minutes of each trial.
                            end
                            fprintf(fid(d),'%1.0f',nanmax(a(:,2)));         %Print the maximum number of trials in any 5 minutes.
                        else                                                %Otherwise, if the trial didn't last 5 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'MEAN ATTEMPT ANGLE (degrees)'
                        fprintf(fid(d),'%1.3f',data(s).attempt_force);      %Print the mean peak force per attempt.    
                    case 'MEAN ATTEMPT FORCE (gm)'
                        fprintf(fid(d),'%1.3f',data(s).attempt_force);      %Print the mean peak force per attempt.    
                    case 'MEAN PEAK ANGLE (degrees)'
                        fprintf(fid(d),'%1.3f',mean(data(s).peak));         %Print the mean signal peaks for each session.
                    case 'MEAN PEAK FORCE (gm)'
                        fprintf(fid(d),'%1.3f',mean(data(s).peak));         %Print the mean signal peaks for each session.
                    case 'MEAN PEAK IMPULSE (gm/s)'
                        fprintf(fid(d),'%1.3f,\n',mean(data(s).impulse));   %Print the mean signal impulse for each session.
                    case 'MEAN PEAK ROTATIONAL VELOCITY (degrees/s)'
                        fprintf(fid(d),'%1.3f,\n',mean(data(s).impulse));   %Print the mean signal impulse for each session.
                    case 'MEAN PULL DURATION (s)'
                        fprintf(fid(d),'%1.3f',data(s).all_peak_width);     %Print the mean peak duration for all peaks.
                    case 'MEAN TURN DURATION (s)'
                        fprintf(fid(d),'%1.3f',data(s).all_peak_width);     %Print the mean peak duration for all peaks.
                    case 'MIN. INTER-TRIAL INTERVAL (s)'
                        if length(times) > 1                                %If there's more than one trial...
                            ints = diff(times);                             %Calculate the inter-trial intervals.
                            ints = boxsmooth(ints,10);                      %Box-smooth the inter-trial intervals over 10 trials.
                            if length(ints) > 11                            %If there's mor than 11 trials...
                                fprintf(fid(d),'%1.3f',...
                                    nanmin(ints(6:end-5)));                 %Print the minimum inter-trial interval over full groups of 10 trials.
                            else                                            %Otherwise...
                                fprintf(fid(d),'%1.3f',...
                                    ints(round(length(ints)/2)));           %Print the minimum inter-trial interval from the middle-most value.
                            end
                        else                                                %Otherwise, if there's only one trial...
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end
                    case 'MISSES'
                        fprintf(fid(d),'%1.0f',...
                            sum(data(s).outcome == 'M'));                   %Print the number of misses.
                    case 'NUMBER OF TRIALS'
                        fprintf(fid(d),'%1.0f',...
                            sum(~isnan(data(s).outcome)));                  %Print the total number of trials.
                    case 'POSITION'
                        fprintf(fid(d),'%1.2f',data(s).position);           %Print the device position.  
                    case 'PULL ATTEMPTS PER TRIAL'
                        fprintf(fid(d),'%1.3f',...
                            nanmean(data(s).pre_hit_attempts));             %Print the mean number of attempts before a hit.
                    case 'STAGE'
                        str = data(s).stage;                                %Grab the stage name.
                        str(str == ',') = [];                               %Kick out any commas in the stage name.
                        fprintf(fid(d),'%s',str);                           %Print the stage name.  
                    case 'START TIME (HH:MM)'
                        fprintf(fid(d),'%s',...
                            datestr(data(s).timestamp,'HH:MM'));            %Print the start time.
                    case 'SUBJECT'
                        fprintf(fid(d),'%s',data(s).subject);               %Print the subject name.
                    case 'TOTAL FEEDS'
                        temp = sum(data(s).outcome == 'H') + ...
                            length(data(s).manual_feeds);                   %Grab the total number of feedings.
                        fprintf(fid(d),'%1.0f',temp);                       %Print the total number of feedings.
                    case 'TRIALS IN FIRST 5 MINUTES'
                        if any(times >= 300)                                %If the session lasted for at least 5 minutes...
                            fprintf(fid(d),'%1.0f',sum(times <= 300));      %Print the number of trials in the first 5 minutes.
                        else                                                %Otherwise, if the trial didn't last 5 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'TURN ATTEMPTS PER TRIAL'
                        fprintf(fid(d),'%1.3f',...
                            nanmean(data(s).pre_hit_attempts));             %Print the mean number of attempts before a hit.

                        %fprintf(fid(d),'%1.3f',nanmean(data(s).hit_peak_width));     %Print the mean peak duration for hit peaks.
                end
                if i < N                                                    %If this isn't the last column...
                    fprintf(fid(d),'\t');                                   %Print a tab separator.
                else                                                        %Otherwise, if this is the last column...
                    fprintf(fid(d),'\n');                                   %Print a carriage return.
                end
            end
        end
    end    
end
fclose all;                                                                 %Close all open files.
for d = 1:length(devices)                                                   %Step through the devices.
    winopen(files{d});                                                      %Open each CSV file.
end
waitbar.close();                                                            %Close the waitbar.


%% This subfunction creates the GUI for selecting output columns.
function fig = Selection_GUI(selected_fields,mandatory_fields,all_fields,device)
uih = 0.75;                                                                 %Set the height for all dropdown menus.
w = 20;                                                                     %Set the width of the field selection figure.
sp = 0.05;                                                                  %Set the space between elements, in centimeters.
N = length(selected_fields);                                                %Grab the number of currently-selected fields.
h = (N + 2.5)*(uih + sp) + sp;                                              %Set the height of the function selection figure.
set(0,'units','centimeters');                                               %Set the screensize units to centimeters.
pos = get(0,'ScreenSize');                                                  %Grab the screensize.
pos = [pos(3)/2-w/2, pos(4)/2-h/2, w, h];                                   %Scale a figure position relative to the screensize.
device(1) = upper(device(1));                                               %Capitalize the first letter of the device.
fig = figure('units','centimeters',...
    'Position',pos,...
    'resize','off',...
    'MenuBar','none',...
    'name',sprintf('Select Output Columns for %s Data',device),...
    'numbertitle','off',...
    'closerequestfcn','uiresume(gcbf);');                                   %Set the properties of the figure.
obj = uicontrol(fig,'style','edit',...
        'string',sprintf('%s Data Output Columns:',device),...
        'units','centimeters',...
        'position',[sp h-1.5*(uih+sp) w-2*sp 1.5*uih],...
        'fontweight','bold',...
        'fontsize',16,...
        'horizontalalignment','left',...
        'userdata',0,...
        'enable','inactive');                                               %Create a label at the top.
switch device                                                               %Switch between the device types.
    case 'knob'                                                             %If the current input device is a knob...
        set(obj,'backgroundcolor',[0.9 0.7 0.9]);                           %Set the label color to a light red.
    case 'pull'                                                             %If the current input device is a pull...
        set(obj,'backgroundcolor',[0.7 0.9 0.7]);                           %Set the label color to a light green.
    case 'lever'                                                            %If the current input device is a lever...
        set(obj,'backgroundcolor',[0.7 0.7 0.9]);                           %Set the label color to a light red.
    case 'wheel'                                                            %If the current input device is a wheel...
        set(obj,'backgroundcolor',[0.9 0.9 0.7]);                           %Set the label color to a light yellow.
    case 'touch'                                                            %If the current input device is a capacitive touch sensor...
        set(obj,'backgroundcolor',[0.9 0.7 0.9]);                           %Set the label color to a light magenta.
    case 'both'                                                             %If the current input device is a capacitive touch sensor...
        set(obj,'backgroundcolor',[0.7 0.9 0.9]);                           %Set the label color to a light cyan.
    otherwise                                                               %Otherwise, for any unrecognized device...
        set(obj,'backgroundcolor',[0.7 0.7 0.7]);                           %Set the label color to a neutral gray.
end
for j = 1:size(selected_fields,1)                                           %Step through each currently-selected field.
    uicontrol(fig,'style','edit',...
        'string',num2str(j),...
        'units','centimeters',...
        'position',[sp h-(j+1.5)*(uih+sp) uih uih],...
        'fontsize',12,...
        'fontweight','bold',...
        'foregroundcolor','k',...
        'enable','inactive',...
        'userdata',j);                                                      %Create text for labeling the column number.
    btn = uicontrol(fig,'style','pushbutton',...
        'string','X',...
        'units','centimeters',...
        'position',[2*sp+uih h-(j+1.5)*(uih+sp) uih uih],...
        'fontsize',12,...
        'fontweight','bold',...
        'foregroundcolor',[0.5 0 0],...
        'userdata',j,...
        'callback',@Delete_Menu);                                           %Create a button for deleting the column.
    pop = uicontrol(fig,'style','popupmenu',...
        'string',all_fields,...
        'value',find(strcmpi(selected_fields{j},all_fields)),...
        'units','centimeters',...
        'position',[3*sp+2*uih h-(j+1.5)*(uih+sp) w-4*sp-2*uih uih],...
        'userdata',j,...
        'fontsize',12);                                                     %Make a pop-up menu for each field.
    if any(strcmpi(all_fields{j},mandatory_fields))                         %If this field is mandatory...
        set(btn,'string','-',...
            'foregroundcolor','k',...
            'enable','inactive');                                           %Disable the delete menu button.
        set(pop,'enable','inactive');                                       %Disable the popup menu.
    end    
end
uicontrol(fig,'style','pushbutton',...
    'string','+',...
    'units','centimeters',...
    'position',[2*sp+uih h-(j+2.5)*(uih+sp) uih uih],...
    'fontsize',12,...
    'fontweight','bold',...
    'foregroundcolor','k',...
    'callback',@Create_Menu);                                               %Create a button for adding a new column.
uicontrol(fig,'style','pushbutton',...
    'string','Set Outputs',...
    'units','centimeters',...
    'position',[3*sp+2*uih sp w-4*sp-2*uih uih],...
    'fontsize',12,...
    'fontweight','bold',...
    'callback','uiresume(gcbf);');                                          %Create a button for finalizing the column outputs.


%% This subfunction is called when the user presses the button to add a column.
function Create_Menu(hObject,~)
pos = get(hObject,'position');                                              %Grab the calling pushbutton's position.
uih = pos(4);                                                               %Match the height for all dropdown menus.
sp = pos(2);                                                                %Set the space between elements.
pos = get(gcbf,'position');                                                 %Grab the figure's position.
w = pos(3);                                                                 %Set the width of the field selection figure.
objs = get(gcbf,'children');                                                %Grab all children of the parent figure.
N = sum(strcmpi(get(objs,'style'),'popupmenu')) + 1;                        %Increment the current column count.
h = (N + 2.5)*(uih + sp) + sp;                                              %Set the height of the function selection figure.
pos(4) = h;                                                                 %Reset the figure height.
set(gcbf,'position',pos);                                                   %Update the figure height.
for i = 1:length(objs)                                                      %Step through each object.
    pos = get(objs(i),'position');                                          %Step through each object.
    pos(2) = pos(2) + uih + sp;                                             %Scoot each object up one step.
    set(objs(i),'position',pos);                                            %Update each object's position.
end
i = find(strcmpi(get(objs,'style'),'pushbutton'));                          %Find all pushbutton objects.
j = i(strcmpi(get(objs(i),'string'),'+'));                                  %Find the add column button.
pos = get(objs(j),'position');                                              %Grab the button's position.
pos(2) = sp;                                                                %Reset the bottom edge to the very bottom of the figure.
set(objs(j),'position',pos);                                                %Update the button position.
j = i(strcmpi(get(objs(i),'string'),'Set Outputs'));                        %Find the set button.
pos = get(objs(j),'position');                                              %Grab the button's position.
pos(2) = sp;                                                                %Reset the bottom edge to the very bottom of the figure.
set(objs(j),'position',pos);                                                %Update the button position.
uicontrol(gcbf,'style','edit',...
    'string',num2str(N),...
    'units','centimeters',...
    'position',[sp h-(N+1.5)*(uih+sp) uih uih],...
    'fontsize',12,...
    'fontweight','bold',...
    'foregroundcolor','k',...
    'enable','inactive',...
    'userdata',N);                                                          %Create text for labeling the column number.
uicontrol(gcbf,'style','pushbutton',...
    'string','X',...
    'units','centimeters',...
    'position',[2*sp+uih h-(N+1.5)*(uih+sp) uih uih],...
    'fontsize',12,...
    'fontweight','bold',...
    'foregroundcolor',[0.5 0 0],...
    'userdata',N,...
    'callback',@Delete_Menu);                                               %Create a button for deleting the new column.
i = find(strcmpi(get(objs,'style'),'popupmenu'));                           %Find all popupmenu objects.
str = get(objs(i(1)),'string');                                             %Grab the string from an existing popupmenu.
vals = get(objs(i),'value');                                                %Grab the current value of all popup menus.
vals = [vals{:}];                                                           %Convert the values to a matrix.
i = 1:length(str);                                                          %Create an index for every field option.
i = setdiff(i,vals);                                                        %Find all indices that aren't already selected.
uicontrol(gcbf,'style','popupmenu',...
    'string',str,...
    'value',i(1),...
    'units','centimeters',...
    'position',[3*sp+2*uih h-(N+1.5)*(uih+sp) w-4*sp-2*uih uih],...
    'userdata',N,...
    'fontsize',12);                                                         %Make a pop-up menu for the new column.


%% This subfunction is called when the user presses the button to add a column.
function Delete_Menu(hObject,~)
d = get(hObject,'userdata');                                                %Grab the button index from the calling pushbutton's UserData property.
objs = get(gcbf,'children');                                                %Grab all children of the parent figure.
i = find(strcmpi(get(objs,'style'),'pushbutton'));                          %Find all pushbutton objects.
j = i(strcmpi(get(objs(i),'string'),'+'));                                  %Find the add column button.
pos = get(objs(j),'position');                                              %Grab the button's position.
sp = pos(2);                                                                %Set the space between elements.
uih = pos(4);                                                               %Match the height for all dropdown menus.
N = sum(strcmpi(get(objs,'style'),'popupmenu')) - 1;                        %Decrement the current column count.
h = (N + 2.5)*(uih + sp) + sp;                                              %Set the height of the function selection figure.
pos = get(gcbf,'position');                                                 %Grab the figure's position.
w = pos(3);                                                                 %Set the width of the field selection figure.
pos(4) = h;                                                                 %Reset the figure height.
set(gcbf,'position',pos);                                                   %Update the figure height.
for i = 1:length(objs)                                                      %Step through each object.
    style = get(objs(i),'style');                                           %Grab each object's style.
    index = get(objs(i),'userdata');                                        %Grab each object's index.
    if ~isempty(index)                                                      %If there's an index...
        if index < d                                                        %If the object is above the to-be-deleted column...
            pos = get(objs(i),'position');                                  %Grab the button's position.
            pos(2) = pos(2) - uih - sp;                                     %Move the object downward.
            set(objs(i),'position',pos);                                    %Update the object's position.
        elseif index == d                                                   %If the object is in the to-be-deleted column...
            delete(objs(i));                                                %Delete the object.
        else                                                                %Otherwise, if the object is below the to-be-deleted column...
            set(objs(i),'userdata',index - 1);                              %Decrement the object index.            
            if strcmpi(style,'edit')                                        %If the object is an editbox...
                set(objs(i),'string',num2str(index-1));                     %Update the number label.
            end
        end
    end
end


function MotoTrak_Pull_Viewer(varargin)

[files, path] = uigetfile('*.ArdyMotor','Select MotoTrak Files',...
    'multiselect','on');                                                    %Have the user pick an input *.ArdyMotor file or files.
if ~iscell(files) && files(1) == 0                                          %If no file was selected...
    return                                                                  %Exit the function.
end
cd(path);                                                                   %Change the current directory to the specified folder.
if ischar(files)                                                            %If only one file was selected...
    files = {files};                                                        %Convert the string to a cell array.
end

for f = 1:length(files)                                                     %Step through each file.
    handles = ArdyMotorFileRead(files{f});                                  %Read in the data from the *.ArdyMotor file.
    if ~isfield(handles,'trial') || isempty(handles.trial)                  %If there's no trials in this data file...
        warning('ARDYMOTOR2TEXT:NoTrials',['WARNING FROM '...
            'ARDYMOTOR2TEXT: The file "' files{f} '" has zero trials '...
            'and will be skipped.']);                                       %Show a warning.
        continue                                                            %Skip to the next file.
    end
    handles.file = files{f};                                                %Save the filename.
    handles.ir_thresh = [1023, 0];                                          %Create a matrix to hold the IR signal bounds.
    for t = 1:length(handles.trial)                                         %Step through each trial.
        handles.ir_thresh(1) = ...
            min([handles.trial(t).ir; handles.ir_thresh(1)]);               %Find the new minimum for each trial.
        handles.ir_thresh(2) = ...
            max([handles.trial(t).ir; handles.ir_thresh(2)]);               %Find the new maximum for each trial.
        s = median(double(diff(handles.trial(t).sample_times)));            %Find the median inter-sample interval for each trial.
        if s == 0                                                           %If all the inter-sample intervals are the same...
            handles.trial(t).sample_times = ...
                (10*(1:length(handles.trial(t).signal)) - 1010)';           %Use the sample times from a different trial in place of the bad times on the curren trial.
        end
    end
    handles.cur_trial = 1;                                                  %Set the current trial to 1.
    handles.num_trials = length(handles.trial);                             %Grab the number of trials.
    handles = Make_GUI(handles);                                            %Create the GUI.
    ShowTrial(handles,handles.cur_trial);                                   %Show the first trial.
    set(handles.slider,'callback',@SliderClick);                            %Set the callback for action on the slider.
    set(handles.savebutton,'callback',@SavePlot);                           %Set the callback for the save plot pushbutton.
    guidata(handles.fig,handles);                                           %Pin the handles structure to the GUI.
end


%% This function displays the force and IR traces from the selected trial.
function ShowTrial(handles,t)
pos = get(handles.fig,'position');                                          %Grab the main figure position.
area(handles.trial(t).sample_times,handles.trial(t).ir,...
    'linewidth',2,'facecolor',[1 0.5 0.5],'parent',handles.ir_axes,...
    'basevalue',handles.ir_thresh(2));                                      %Show the IR signal as an area plot.
% set(handles.ir_axes,'ylim',handles.ir_thresh,'ydir','reverse',...
%     'xlim',handles.trial(t).sample_times([1,end]),'xticklabel',[],...
%     'ytick',[]);                                                            %Set the IR axes properties.
ylabel('IR Signal','parent',handles.ir_axes,'fontsize',0.75*pos(4),...
    'rotation',0,'verticalalignment','middle',...
    'horizontalalignment','right');                                         %Label the IR signal.
set(handles.label,'string',['Subject: ' handles.rat ', Trial ' ...
    num2str(t) '/' num2str(handles.num_trials) ', ' ...
    datestr(handles.trial(t).starttime,'HH:MM:SS, mm/dd/yy')],...
    'fontsize',0.75*pos(4));                                                %Update the trial label.
area(handles.trial(t).sample_times,handles.trial(t).signal,...
    'linewidth',2,'facecolor',[0.5 0.5 1],'parent',handles.force_axes);     %Show the force signal as an area plot.
min_max = [min(handles.trial(t).signal), max(handles.trial(t).signal)];     %Grab the minimum and maximum of the signal.
set(handles.force_axes,'xlim',handles.trial(t).sample_times([1,end]),...
    'ylim',min_max + [-0.05,0.1]*range(min_max),'fontsize',0.5*pos(4));     %Set the force axes properties.
line([0,0],min_max(2)+[0.02,0.08]*range(min_max),'color','k',...
    'parent',handles.force_axes,'linewidth',2);                             %Draw a line to show the start of the hit window.
line(1000*[1,1]*handles.trial(t).hitwin,...
    min_max(2)+[0.02,0.08]*range(min_max),'color','k',...
    'parent',handles.force_axes,'linewidth',2);                             %Draw a line to show the end of the hit window.
line([0,1000*handles.trial(t).hitwin],...
    min_max(2)+0.05*range(min_max)*[1,1],'color','k',...
    'parent',handles.force_axes,'linestyle','--','linewidth',2);            %Draw a line to show the length of the hit window.
text(500*handles.trial(t).hitwin,min_max(2)+0.05*range(min_max),...
    'Hit Window','margin',2,'edgecolor','w','backgroundcolor','w',...
    'fontsize',0.5*pos(4),'fontweight','bold',...
    'parent',handles.force_axes,'horizontalalignment','center',...
    'verticalalignment','middle');                                          %Label the hit window.
a = line([0,0],get(handles.force_axes,'ylim'),'color',[0.5 0.5 0.5],...
    'parent',handles.force_axes,'linewidth',2,'linestyle','--');            %Draw a gray dotted line to show the start of the hit window.
uistack(a,'bottom');                                                        %Move the dotted line to the bottom of the stack.
a = line(1000*[1,1]*handles.trial(t).hitwin,...
    get(handles.force_axes,'ylim'),'color',[0.5 0.5 0.5],...
    'parent',handles.force_axes,'linewidth',2,'linestyle','--');            %Draw a gray dotted line to show the end of the hit window.
uistack(a,'bottom');                                                        %Move the dotted line to the bottom of the stack.
if max(get(handles.force_axes,'ylim')) > handles.trial(t).init              %If the y-axis scale is large enough to show the initiation threshold...
    line([-100,max(get(handles.force_axes,'xlim'))],...
        handles.trial(t).init*[1,1],'color',[0 0.5 0],...
        'parent',handles.force_axes,'linewidth',2,'linestyle','--');        %Draw a line showing the initiation threshold.
    text(-100,handles.trial(t).init,'Initiation ','color',[0 0.5 0],...
        'fontsize',0.5*pos(4),'fontweight','bold',...
        'parent',handles.force_axes,'horizontalalignment','right',...
        'verticalalignment','middle');                                      %Label the initiation threshold.
end
if max(get(handles.force_axes,'ylim')) > handles.trial(t).thresh            %If the y-axis scale is large enough to show the hit threshold...
    line([-100,max(get(handles.force_axes,'xlim'))],...
        handles.trial(t).thresh*[1,1],'color',[0.5 0 0],...
        'parent',handles.force_axes,'linewidth',2,'linestyle','--');        %Draw a line showing the hit threshold.
    text(-100,handles.trial(t).thresh,'Hit Threshold ',...
        'color',[0.5 0 0],'fontsize',0.5*pos(4),'fontweight','bold',...
        'parent',handles.force_axes,'horizontalalignment','right',...
        'verticalalignment','middle');                                      %Label the hit threshold.
end
ylabel('Force (g)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the force signal.
xlabel('Time (ms)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the time axis.


%% This function executes when the user interacts with the slider.
function SliderClick(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
handles.cur_trial = round(get(hObject,'value'));                            %Set the current trial to the value of the slider.
if handles.cur_trial < 1                                                    %If the current trial is less than 1...
    handles.cur_trial = 1;                                                  %Set the current trial to 1.
elseif handles.cur_trial > handles.num_trials                               %Otherwise, if the current trials is greater than the total number of trials.
    handles.cur_trial = handles.num_trials;                                 %Set the current trial to the last trial.
end
set(hObject,'value',handles.cur_trial);                                     %Update the value of the slider.
ShowTrial(handles,handles.cur_trial);                                       %Show the current trial.
guidata(hObject,handles);                                                   %Pin the handles structure back to the GUI.


%% This function executes when the user interacts with the slider.
function SavePlot(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
filename = [handles.file(1:end-10) '_TRIAL' ...
    num2str(handles.cur_trial,'%03.0f') '.png'];                            %Create a default filename for the PNG file.
[filename, path] = uiputfile('*.png','Name Image File',filename);           %Ask the user for a new filename.
if filename(1) == 0                                                         %If the user selected cancel...
    return                                                                  %Exit the function.
end
set([handles.slider,handles.savebutton],'visible','off','enable','off');    %Make the uicontrols invisible and disable them.
% fix_dotted_line_export(handles.force_axes);                                 %Fix the dotted lines in the force axes.
pos = get(handles.fig,'position');                                          %Grab the figure position.
temp = get(handles.fig,'color');                                            %Grab the starting color of the figure.
set(handles.fig,'paperpositionmode','auto',...
    'inverthardcopy','off',...
    'paperunits',get(handles.fig,'units'),...
    'papersize',pos(3:4),...
    'color','w');                                                           %Set the figure properties for printing.
set(handles.label,'backgroundcolor','w');                                   %Set the label background color to white.
drawnow;                                                                    %Immediately update the figure.
print(gcf,[path, filename],'-dpng','-r300');                                %Save the current image as a PNG file.
set([handles.slider,handles.savebutton],'visible','on','enable','on');      %Make the uicontrols visible and enabled again.
set(handles.fig,'color',temp);                                              %Reset the figure color to the original color.
set(handles.label,'backgroundcolor',temp);                                  %Set the label background color to the original color.


%% This subfunction creates the GUI.
function handles = Make_GUI(handles)
set(0,'units','centimeters');                                               %Set the system units to centimeters.
pos = get(0,'screensize');                                                  %Grab the screen size.
h = 0.8*pos(4);                                                             %Calculate the height of the figure.
w = 4*h/3;                                                                  %Scale the width of the figure to the height.
handles.fig = figure('MenuBar','none',...
    'numbertitle','off',...
    'name',['Force Viewer: ' handles.file],...
    'units','centimeters',...
    'resize','on',...
    'ResizeFcn',@Resize,...
    'Position',[pos(3)/2-w/2, pos(4)/2-h/2, w, h]);                         %Create a figure.
handles.label =  uicontrol(handles.fig,'style','text',...
    'units','normalized',...
    'position',[0.01,0.95,0.98,0.04],...
    'string',[],...
    'fontsize',0.5*h,...
    'backgroundcolor',get(handles.fig,'color'),...
    'horizontalalignment','left',...
    'fontweight','bold');                                                   %Create a text label for showing the trial number and time.
handles.ir_axes = axes('units','normalized',...
    'position',[0.1,0.85,0.89,0.09],...
    'box','on',...
    'linewidth',2);                                                         %Create axes for showing the IR signal.
handles.force_axes = axes('units','normalized',...
    'position',[0.1,0.12,0.89,0.72],...
    'box','on',...
    'linewidth',2);                                                         %Create axes for showing the IR signal.
handles.slider = uicontrol(handles.fig,'style','slider',...
    'units','normalized',...
    'position',[0.01,0.01,0.78,0.04],...
    'value',1,...
    'min',1,...
    'max',handles.num_trials,...
    'SliderStep',[1/handles.num_trials, 0.1]);                              %Create a trial slider.
handles.savebutton = uicontrol(handles.fig,'style','pushbutton',...
    'units','normalized',...
    'position',[0.80,0.01,0.19,0.04],...
    'string','Save Plot (PNG)',...
    'fontsize',0.75*h);                                                     %Create a button for saving a plot image.


%% This function is called whenever the main figure is resized.
function Resize(hObject,~)
handles = guidata(hObject);                                                 %Grab the handles structure from the GUI.
pos = get(handles.fig,'position');                                          %Grab the main figure position.
ylabel('IR Signal','parent',handles.ir_axes,'fontsize',0.75*pos(4),...
    'rotation',0,'verticalalignment','middle',...
    'horizontalalignment','right');                                         %Label the IR signal with the new fontsize.
set([handles.label,handles.savebutton],'fontsize',0.75*pos(4));             %Update the trial label and savebutton fontsize.
objs = get(handles.force_axes,'children');                                  %Grab all children of the force axes.
objs(~strcmpi('text',get(objs,'type'))) = [];                               %Kick out all non-text objects.
set(objs,'fontsize',0.5*pos(4));                                            %Update the fontsize of all text objects.
ylabel('Force (g)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the force signal.
xlabel('Time (ms)','parent',handles.force_axes,'fontsize',0.75*pos(4));     %Label the time axis.


function data = MotoTrak_to_ArdyMotor(data)
%
%MOTOTRAK_TO_ARDYMOTOR.m - Vulintus, Inc., 2016.
%
%   MOTOTRAK_TO_ARDYMOTOR takes the data structure returned by the
%   MotoTrakFileRead function (which reads MotoTrak files with the
%   *.MotoTrak extension) and converts it into a data structure matching
%   the style returned by the ArdyMotorFileRead function (which reads
%   MotoTrak files with the *.ArdyMotor extension).
%
%   UPDATE LOG:
%   12/14/2016 - Drew Sloan - Function first created.
%

header_fields = {   'calibration_coefficients', 'cal'};                     %List the fields in the header (column 1) with their corresponding renamings (column 2).

trial_fields = {    'start_time',               'starttime';
                    'result',                   'outcome';
                    'hit_window_duration',      'hitwin';
                    'hit_times',                'hittime';
                    'output_trigger_times',     'stimtime';};               %List the trial fields (column 1) with their corresponding renamings (column 2).
                
for i = 1:size(header_fields,1)                                             %Step through each header field to rename.
    data.(header_fields{i,2}) = data.(header_fields{i,1});                  %Copy each field to the new field name.
end
data = rmfield(data,header_fields(:,1));                                    %Kick out the now-redundant fields.

data.cal = double(data.cal)';                                               %Convert the calibration coefficients to double precision and transpose the matrix.
data.booth = str2double(data.booth);                                        %Convert the booth number from a string to a value.

init_index = strcmpi(data.parameters,'Initiation Threshold');               %Find the parameter index for the initiation threshold.
thresh_index = strcmpi(data.parameters,'Hit Threshold');                    %Find the parameter index for the hit threshold.

if isfield(data,'trial') && ~isempty(data.trial)                            %If there are any trials in the data structure...
    
    for t = 1:length(data.trial)                                            %Step through each trial in the data structure.
        for i = 1:size(trial_fields,1)                                      %Step through each trial field to rename.
            data.trial(t).(trial_fields{i,2}) = ...
                data.trial(t).(trial_fields{i,1});                          %Copy each field to the new field name.
        end
        if isempty(data.trial(t).hittime)                                   %If there's no hit time for this trial...
            data.trial(t).hittime = NaN;                                    %Set the hit time to NaN.
        end
    end    
    data.trial = rmfield(data.trial,trial_fields(:,1));                     %Kick out the now-redundant fields.
    
    data.pre_trial_sampling_dur = data.trial(1).pre_trial_duration;         %Copy the pre trial sampling duration from the first trial to the header.
    data.position = data.trial(1).position;                                 %Copy the device position from the first trial to the header.
    
    for t = 1:length(data.trial)                                            %Step through each trial in the data structure.
        data.trial(t).signal(1,:) = data.trial(t).signal(1,:)/1000;         %Convert the sample times to milliseconds.        
        data.trial(t).signal(1,:) = data.trial(t).signal(1,:) - ...
            data.trial(t).signal(1,1) - ...
            1000*data.trial(t).pre_trial_duration;                          %Make the sample times relative to the trial initiation.
        data.trial(t).sample_times = int16(data.trial(t).signal(1,:)');     %Copy the sample times to a field called "sample times".
        data.trial(t).ir = data.trial(t).signal(3,:)';                      %Copy the IR signal to a field called "ir".
        data.trial(t).signal = data.trial(t).signal(2,:)';                 	%Trim the "signal" field to just those values coming from the primary device.
        data.trial(t).init = data.trial(t).parameters(init_index);          %Copy the the initiation threshold to a field called "init".
        data.trial(t).thresh = data.trial(t).parameters(thresh_index);      %Copy the the hit threshold to a field called "thresh".
    end
else                                                                        %Otherwise...
    data.pre_trial_sampling_dur = NaN;                                      %Set the pre trial sampling duration to NaN.
    data.position = NaN;                                                    %Set the device position to NaN.
end


function data = MotoTrakFileRead ( file )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MotoTrakFileRead.m
% Date Created: 8/16/2016
% Last date modified: 11/8/2016
% Author: David Pruitt
% Description: This is a first pass at some code to load in MotoTrak 2.0
%   data files into Matlab.  These data files are generated by the C# 
%   MotoTrak program using file version -5.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Create an empty structure for the data
data = [];

%Open the MotoTrak file
fid = fopen(file, 'r');

%Rewind to the beginning of the file
fseek(fid, 0, -1);

%Get the file version
version = fread(fid, 1, 'int8');

if (version == -5)
    
    %If the file version is -5, then go ahead and attempt to read it
    data.version = version;
    
    %Read the session start time
    data.start_time = fread(fid, 1, 'float64');
    
    %Read the number of characters in the rat name
    N = fread(fid, 1, 'uint8');
    
    %Read the rat name
    data.subject = fread(fid, N, '*char')';
    
    %Read the number of characters in the booth name
    N = fread(fid, 1, 'uint8');
    
    %Read the booth name
    data.booth = fread(fid, N, '*char')';
    
    %Read the number of characters in the stage title
    N = fread(fid, 1, 'uint8');
    
    %Read the stage title
    data.stage = fread(fid, N, '*char')';
    
    %Read the number of characters in the device name
    N = fread(fid, 1, 'uint8');
    
    %Read the device name
    data.device = fread(fid, N, '*char')';
    
    %Read the number of calibration coefficients that exist
    N = fread(fid, 1, 'uint8');
    
    %Read in each calibration coefficient
    data.calibration_coefficients = fread(fid, N, '*float32');
    
    %Read in the number of streams that exist in this data file
    N = fread(fid, 1, 'uint8');
    
    %Read in the metadata for each data stream
    data.data_streams = struct('stream_description', {}, 'stream_units', {});
    for i=1:N
        %Load in the description of each stream
        n_descr = fread(fid, 1, 'uint8');
        descr = fread(fid, n_descr, '*char')';
        
        %Load in the units for each stream
        n_units = fread(fid, 1, 'uint8');
        units = fread(fid, n_units, '*char')';
        
        %Save the data stream metadata to our structure
        new_stream.stream_description = descr;
        new_stream.stream_units = units;
        data.data_streams(end+1) = new_stream;
    end
    
    number_of_streams = length(data.data_streams);
    
    %Read in the number of stage parameters that exist for this session
    N = fread(fid, 1, 'uint32');
    
    %Read in the stage parameters for this stage
    data.parameters = {};
    for i=1:N
        n_param_name = fread(fid, 1, 'uint8');
        param_name = fread(fid, n_param_name, '*char')';
        data.parameters{end+1} = param_name;
    end
    
    %Define block id types:
    BlockID_Trial = 0;
    BlockID_ManualFeed = 1;
    BlockID_PauseStart = 2;
    BlockID_PauseFinish = 3;
    BlockID_TimestampedNote = 4;
    BlockID_GeneralNote = 5;
    BlockID_SessionEnd = 6;
    
    data.trial = [];
    data.manual_feeds = [];
    data.pause_start_times = [];
    data.pause_end_times = [];
    data.session_notes = '';
    data.timestamped_notes = struct('timestamp', {}, 'text', {});
    
    %Read in all of the trials
    while (~feof(fid))
        
        %Read in the block identifier
        block_id = fread(fid, 1, 'int32');
        
        if (block_id == BlockID_Trial)
            
            %Read in the trial from the file
            [new_trial, ~] = mototrak_read_trial(fid, number_of_streams);
            data.trial = [data.trial new_trial];
            
        elseif (block_id == BlockID_ManualFeed)
            
            %Read a manual feed from the file
            manual_feed_timestamp = fread(fid, 1, 'float64');
            data.manual_feeds(end+1) = manual_feed_timestamp;
            
        elseif (block_id == BlockID_PauseStart)
            
            %Read a pause start from the file
            pause_start_timestamp = fread(fid, 1, 'float64');
            data.pause_start_times(end+1) = pause_start_timestamp;
            
        elseif (block_id == BlockID_PauseFinish)
            
            %Read the pause end from the file
            pause_end_timestamp = fread(fid, 1, 'float64');
            data.pause_end_times(end+1) = pause_end_timestamp;
            
        elseif (block_id == BlockID_TimestampedNote)
            
            %Read in the timestamped note
            note_timestamp = fread(fid, 1, 'float64');
            note_length = fread(fid, 1, 'uint16');
            note_content = fread(fid, note_length, '*char');
            
            data.timestamped_notes(end+1) = struct('timestamp', note_timestamp, 'text', note_content);
            
        elseif (block_id == BlockID_GeneralNote)
            
            %Read the session notes from the file
            note_length = fread(fid, 1, 'uint16');
            note_content = fread(fid, note_length, '*char');
            data.session_notes = note_content;
            
        elseif (block_id == BlockID_SessionEnd)
            
            %Read in the session end time
            session_end_timestamp = fread(fid, 1, 'float64');
            data.end_time = session_end_timestamp;
            
        end
        
    end
    
else
    %If the version doesn't equal -5, print an error message and exit this
    %function
    disp('Incorrect file version.  We cannot read this file.');
    return;
end

    %Close the data file.
    fclose(fid);
    
end

%% mototrak_read_trial - a subfunction that reads in individual trials from the MotoTrak session file

function [trial, trial_number] = mototrak_read_trial ( fid, num_streams )

    %Read in the trial number
    trial_number = fread(fid, 1, 'uint32');

    %Read in the start time of the trial
    trial.start_time = fread(fid, 1, 'float64');

    %Read in the outcome of the trial
    result = fread(fid, 1, 'uint8');
    trial.result = result;

    %If the trial was a pause, then read the end time of the trial
    if (result == 'P')
        trial.end_time = fread(fid, 1, 'float64');
    else
        trial.end_time = NaN;
    end

    %Read in the hit window duration
    trial.hit_window_duration = fread(fid, 1, 'float32');

    %Read in the pre-trial duration
    trial.pre_trial_duration = fread(fid, 1, 'float32');

    %Read in the post-trial duration
    trial.post_trial_duration = fread(fid, 1, 'float32');

    %Read in the post-trial time-out period
    trial.post_trial_timeout = fread(fid, 1, 'float32');

    %Read in the manipulandum position
    trial.position = fread(fid, 1, 'float32');

    %Read in the number of variable parameters that exist for the trial
    N = fread(fid, 1, 'uint8');

    %Read in each variable parameter
    variable_params = [];
    for i=1:N
        %Read in the parameter value
        param_value = fread(fid, 1, 'float32');
        variable_params(end+1) = param_value;
    end
    trial.parameters = variable_params;

    %Read in the number of hits that occurred during this trial
    N = fread(fid, 1, 'uint8');

    %Read in the timestamp of each hit
    trial.hit_times = fread(fid, N, '*float64');

    %Read in the number of output triggers that occurred during this
    %trial
    N = fread(fid, 1, 'uint8');

    %Read in the output triggers for the trial
    trial.output_trigger_times = fread(fid, N, '*float64');

    %Read in the number of samples in each data stream for this trial
    N = fread(fid, 1, 'uint32');

    %Read in each data stream
    trial.signal = nan(num_streams, N);
    for i=1:num_streams
        trial.signal(i, :) = fread(fid, N, '*float32');
    end
    
end


function path = Vulintus_Set_AppData_Path(program)

%
%Vulintus_Set_AppData_Path.m - Vulintus, Inc.
%
%   This function finds and/or creates the local application data folder
%   for Vulintus functions specified by "program".
%   
%   UPDATE LOG:
%   08/05/2016 - Drew Sloan - Function created to replace within-function
%       calls in multiple programs.
%

local = winqueryreg('HKEY_CURRENT_USER',...
        ['Software\Microsoft\Windows\CurrentVersion\' ...
        'Explorer\Shell Folders'],'Local AppData');                         %Grab the local application data directory.    
path = fullfile(local,'Vulintus','\');                                      %Create the expected directory name for Vulintus data.
if ~exist(path,'dir')                                                       %If the directory doesn't already exist...
    [status, msg, ~] = mkdir(path);                                         %Create the directory.
    if status ~= 1                                                          %If the directory couldn't be created...
        errordlg(sprintf(['Unable to create application data'...
            ' directory\n\n%s\n\nDetails:\n\n%s'],path,msg),...
            'Vulintus Directory Error');                                    %Show an error.
    end
end
path = fullfile(path,program,'\');                                          %Create the expected directory name for MotoTrak data.
if ~exist(path,'dir')                                                       %If the directory doesn't already exist...
    [status, msg, ~] = mkdir(path);                                         %Create the directory.
    if status ~= 1                                                          %If the directory couldn't be created...
        errordlg(sprintf(['Unable to create application data'...
            ' directory\n\n%s\n\nDetails:\n\n%s'],path,msg),...
            [program ' Directory Error']);                                  %Show an error.
    end
end

if strcmpi(program,'mototrak')                                              %If the specified function is MotoTrak.
    oldpath = fullfile(local,'MotoTrak','\');                               %Create the expected name of the previous version appdata directory.
    if exist(oldpath,'dir')                                                 %If the previous version directory exists...
        files = dir(oldpath);                                               %Grab the list of items contained within the previous directory.
        for f = 1:length(files)                                             %Step through each item.
            if ~files(f).isdir                                             	%If the item isn't a directory...
                copyfile([oldpath, files(f).name],path,'f');                %Copy the file to the new directory.
            end
        end
        [status, msg] = rmdir(oldpath,'s');                                 %Delete the previous version appdata directory.
        if status ~= 1                                                      %If the directory couldn't be deleted...
            warning(['Unable to delete application data'...
                ' directory\n\n%s\n\nDetails:\n\n%s'],oldpath,msg);         %Show an warning.
        end
    end
end


function waitbar = big_waitbar(varargin)

figsize = [2,16];                                                           %Set the default figure size, in centimeters.
barcolor = 'b';                                                             %Set the default waitbar color.
titlestr = 'Waiting...';                                                    %Set the default waitbar title.
txtstr = 'Waiting...';                                                      %Set the default waitbar string.
val = 0;                                                                    %Set the default value of the waitbar to zero.

str = {'FigureSize','Color','Title','String','Value'};                      %List the allowable parameter names.
for i = 1:2:length(varargin)                                                %Step through any optional input arguments.
    if ~ischar(varargin{i}) || ~any(strcmpi(varargin{i},str))               %If the first optional input argument isn't one of the expected property names...
        beep;                                                               %Play the Matlab warning noise.
        cprintf('red','%s\n',['ERROR IN BIG_WAITBAR: Property '...
            'name not recognized! Optional input properties are:']);        %Show an error.
        for j = 1:length(str)                                               %Step through each allowable parameter name.
            cprintf('red','\t%s\n',str{j});                                 %List each parameter name in the command window, in red.
        end
        return                                                              %Skip execution of the rest of the function.
    else                                                                    %Otherwise...
        if strcmpi(varargin{i},'FigureSize')                                %If the optional input property is "FigureSize"...
            figsize = varargin{i+1};                                        %Set the figure size to that specified, in centimeters.            
        elseif strcmpi(varargin{i},'Color')                                 %If the optional input property is "Color"...
            barcolor = varargin{i+1};                                       %Set the waitbar color the specified color.
        elseif strcmpi(varargin{i},'Title')                                 %If the optional input property is "Title"...
            titlestr = varargin{i+1};                                       %Set the waitbar figure title to the specified string.
        elseif strcmpi(varargin{i},'String')                                %If the optional input property is "String"...
            txtstr = varargin{i+1};                                         %Set the waitbar text to the specified string.
        elseif strcmpi(varargin{i},'Value')                                 %If the optional input property is "Value"...
            val = varargin{i+1};                                            %Set the waitbar value to the specified value.
        end
    end    
end

orig_units = get(0,'units');                                                %Grab the current system units.
set(0,'units','centimeters');                                               %Set the system units to centimeters.
pos = get(0,'Screensize');                                                  %Grab the screensize.
h = figsize(1);                                                             %Set the height of the figure.
w = figsize(2);                                                             %Set the width of the figure.
fig = figure('numbertitle','off',...
    'name',titlestr,...
    'units','centimeters',...
    'Position',[pos(3)/2-w/2, pos(4)/2-h/2, w, h],...
    'menubar','none',...
    'resize','off');                                                        %Create a figure centered in the screen.
ax = axes('units','centimeters',...
    'position',[0.25,0.25,w-0.5,h/2-0.3],...
    'parent',fig);                                                          %Create axes for showing loading progress.
if val > 1                                                                  %If the specified value is greater than 1...
    val = 1;                                                                %Set the value to 1.
elseif val < 0                                                              %If the specified value is less than 0...
    val = 0;                                                                %Set the value to 0.
end    
obj = fill(val*[0 1 1 0 0],[0 0 1 1 0],barcolor,'edgecolor','k');           %Create a fill object to show loading progress.
set(ax,'xtick',[],'ytick',[],'box','on','xlim',[0,1],'ylim',[0,1]);         %Set the axis limits and ticks.
txt = uicontrol(fig,'style','text','units','centimeters',...
    'position',[0.25,h/2+0.05,w-0.5,h/2-0.3],'fontsize',10,...
    'horizontalalignment','left','backgroundcolor',get(fig,'color'),...
    'string',txtstr);                                                       %Create a text object to show the current point in the wait process.  
set(0,'units',orig_units);                                                  %Set the system units back to the original units.

waitbar.title = @(str)SetTitle(fig,str);                                    %Set the function for changing the waitbar title.
waitbar.string = @(str)SetString(fig,txt,str);                              %Set the function for changing the waitbar string.
waitbar.value = @(val)SetVal(fig,obj,val);                                  %Set the function for changing waitbar value.
waitbar.color = @(val)SetColor(fig,obj,val);                                %Set the function for changing waitbar color.
waitbar.close = @()CloseWaitbar(fig);                                       %Set the function for closing the waitbar.
waitbar.isclosed = @()WaitbarIsClosed(fig);                                 %Set the function for checking whether the waitbar figure is closed.

drawnow;                                                                    %Immediately show the waitbar.


%% This function sets the name/title of the waitbar figure.
function SetTitle(fig,str)
if ishandle(fig)                                                            %If the waitbar figure is still open...
    set(fig,'name',str);                                                    %Set the figure name to the specified string.
    drawnow;                                                                %Immediately update the figure.
else                                                                        %Otherwise...
    warning('Cannot update the waitbar figure. It has been closed.');       %Show a warning.
end


%% This function sets the string on the waitbar figure.
function SetString(fig,txt,str)
if ishandle(fig)                                                            %If the waitbar figure is still open...
    set(txt,'string',str);                                                  %Set the string in the text object to the specified string.
    drawnow;                                                                %Immediately update the figure.
else                                                                        %Otherwise...
    warning('Cannot update the waitbar figure. It has been closed.');       %Show a warning.
end


%% This function sets the current value of the waitbar.
function SetVal(fig,obj,val)
if ishandle(fig)                                                            %If the waitbar figure is still open...
    if val > 1                                                              %If the specified value is greater than 1...
        val = 1;                                                            %Set the value to 1.
    elseif val < 0                                                          %If the specified value is less than 0...
        val = 0;                                                            %Set the value to 0.
    end
    set(obj,'xdata',val*[0 1 1 0 0]);                                       %Set the patch object to extend to the specified value.
    drawnow;                                                                %Immediately update the figure.
else                                                                        %Otherwise...
    warning('Cannot update the waitbar figure. It has been closed.');       %Show a warning.
end


%% This function sets the color of the waitbar.
function SetColor(fig,obj,val)
if ishandle(fig)                                                            %If the waitbar figure is still open...
    set(obj,'facecolor',val);                                               %Set the patch object to have the specified facecolor.
    drawnow;                                                                %Immediately update the figure.
else                                                                        %Otherwise...
    warning('Cannot update the waitbar figure. It has been closed.');       %Show a warning.
end


%% This function closes the waitbar figure.
function CloseWaitbar(fig)
if ishandle(fig)                                                            %If the waitbar figure is still open...
    close(fig);                                                             %Close the waitbar figure.
    drawnow;                                                                %Immediately update the figure to allow it to close.
end


%% This function returns a logical value indicate whether the waitbar figure has been closed.
function isclosed = WaitbarIsClosed(fig)
isclosed = ~ishandle(fig);                                                  %Check to see if the figure handle is still a valid handle.


function X = boxsmooth(X,wsize)
%Box smoothing function for 2-D matrices.

%X = BOXSMOOTH(X,WSIZE) performs a box-type smoothing function on 2-D
%matrices with window width and height equal to WSIZE.  If WSIZE isn't
%given, the function uses a default value of 5.

if (nargin < 2)                                                             %If the use didn't specify a box size...
    wsize = 5;                                                              %Set the default box size to a 5x5 square.
end     
if (nargin < 1)                                                             %If the user entered no input arguments...
   error('BoxSmooth requires 2-D matrix input.');                           %Show an error.
end

if length(wsize) == 1                                                       %If the user only inputted one dimension...
    rb = round(wsize);                                                      %Round the number of row bins to the nearest integer.
    cb = rb;                                                                %Set the number of column bins equal to the number of row bins.
elseif length(wsize) == 2                                                   %If the user inputted two dimensions...
    rb = round(wsize(1));                                                   %Round the number of row bins to the nearest integer.
    cb = round(wsize(2));                                                   %Round the number of column bins to the nearest integer.
else                                                                        %Otherwise, if the 
    error('The input box size for the boxsmooth can only be a one- or two-element matrix.');
end

w = ones(rb,cb);                                                            %Make a matrix to hold bin weights.
if rem(rb,2) == 0                                                           %If the number of row bins is an even number.
    rb = rb + 1;                                                            %Add an extra bin to the number of row bins.
    w([1,end+1],:) = 0.5;                                                   %Set the tail bins to have half-weight.
end
if rem(cb,2) == 0                                                           %If the number of column bins is an even number.
    cb = cb + 1;                                                            %Add an extra bin to the number of row bins.
    w(:,end+1) = w(:,1);                                                    %Make a new column of weights with the weight of the first column.
    w(:,[1,end]) = 0.5*w(:,[1,end]);                                        %Set the tail bins to have half-weight.
end

[r,c] = size(X);                                                            %Find the number of rows and columns in the input matrix.
S = nan(r+rb-1,c+cb-1);                                                     %Pre-allocate an over-sized matrix to hold the original data.
S((1:r)+(rb-1)/2,(1:c)+(cb-1)/2) = X;                                       %Copy the original matrix to the center of the over-sized matrix.

temp = zeros(size(w));                                                      %Pre-allocate a temporary matrix to hold the box values.
for i = 1:r                                                                 %Step through each row of the original matrix.
    for j = 1:c                                                             %Step through each column of the original matrix.
        temp(:) = S(i:(i+rb-1),j:(j+cb-1));                                 %Pull all of the bin values into a temporary matrix.
        k = ~isnan(temp(:));                                                %Find all the non-NaN bins.
        X(i,j) = sum(w(k).*temp(k))/sum(w(k));                              %Find the weighted mean of the box and save it to the original matrix.
    end
end


function count = cprintf(style,format,varargin)
% CPRINTF displays styled formatted text in the Command Window
%
% Syntax:
%    count = cprintf(style,format,...)
%
% Description:
%    CPRINTF processes the specified text using the exact same FORMAT
%    arguments accepted by the built-in SPRINTF and FPRINTF functions.
%
%    CPRINTF then displays the text in the Command Window using the
%    specified STYLE argument. The accepted styles are those used for
%    Matlab's syntax highlighting (see: File / Preferences / Colors / 
%    M-file Syntax Highlighting Colors), and also user-defined colors.
%
%    The possible pre-defined STYLE names are:
%
%       'Text'                 - default: black
%       'Keywords'             - default: blue
%       'Comments'             - default: green
%       'Strings'              - default: purple
%       'UnterminatedStrings'  - default: dark red
%       'SystemCommands'       - default: orange
%       'Errors'               - default: light red
%       'Hyperlinks'           - default: underlined blue
%
%       'Black','Cyan','Magenta','Blue','Green','Red','Yellow','White'
%
%    STYLE beginning with '-' or '_' will be underlined. For example:
%          '-Blue' is underlined blue, like 'Hyperlinks';
%          '_Comments' is underlined green etc.
%
%    STYLE beginning with '*' will be bold (R2011b+ only). For example:
%          '*Blue' is bold blue;
%          '*Comments' is bold green etc.
%    Note: Matlab does not currently support both bold and underline,
%          only one of them can be used in a single cprintf command. But of
%          course bold and underline can be mixed by using separate commands.
%
%    STYLE also accepts a regular Matlab RGB vector, that can be underlined
%    and bolded: -[0,1,1] means underlined cyan, '*[1,0,0]' is bold red.
%
%    STYLE is case-insensitive and accepts unique partial strings just
%    like handle property names.
%
%    CPRINTF by itself, without any input parameters, displays a demo
%
% Example:
%    cprintf;   % displays the demo
%    cprintf('text',   'regular black text');
%    cprintf('hyper',  'followed %s','by');
%    cprintf('key',    '%d colored', 4);
%    cprintf('-comment','& underlined');
%    cprintf('err',    'elements\n');
%    cprintf('cyan',   'cyan');
%    cprintf('_green', 'underlined green');
%    cprintf(-[1,0,1], 'underlined magenta');
%    cprintf([1,0.5,0],'and multi-\nline orange\n');
%    cprintf('*blue',  'and *bold* (R2011b+ only)\n');
%    cprintf('string');  % same as fprintf('string') and cprintf('text','string')
%
% Bugs and suggestions:
%    Please send to Yair Altman (altmany at gmail dot com)
%
% Warning:
%    This code heavily relies on undocumented and unsupported Matlab
%    functionality. It works on Matlab 7+, but use at your own risk!
%
%    A technical description of the implementation can be found at:
%    <a href="http://undocumentedmatlab.com/blog/cprintf/">http://UndocumentedMatlab.com/blog/cprintf/</a>
%
% Limitations:
%    1. In R2011a and earlier, a single space char is inserted at the
%       beginning of each CPRINTF text segment (this is ok in R2011b+).
%
%    2. In R2011a and earlier, consecutive differently-colored multi-line
%       CPRINTFs sometimes display incorrectly on the bottom line.
%       As far as I could tell this is due to a Matlab bug. Examples:
%         >> cprintf('-str','under\nline'); cprintf('err','red\n'); % hidden 'red', unhidden '_'
%         >> cprintf('str','regu\nlar'); cprintf('err','red\n'); % underline red (not purple) 'lar'
%
%    3. Sometimes, non newline ('\n')-terminated segments display unstyled
%       (black) when the command prompt chevron ('>>') regains focus on the
%       continuation of that line (I can't pinpoint when this happens). 
%       To fix this, simply newline-terminate all command-prompt messages.
%
%    4. In R2011b and later, the above errors appear to be fixed. However,
%       the last character of an underlined segment is not underlined for
%       some unknown reason (add an extra space character to make it look better)
%
%    5. In old Matlab versions (e.g., Matlab 7.1 R14), multi-line styles
%       only affect the first line. Single-line styles work as expected.
%       R14 also appends a single space after underlined segments.
%
%    6. Bold style is only supported on R2011b+, and cannot also be underlined.
%
% Change log:
%    2012-08-09: Graceful degradation support for deployed (compiled) and non-desktop applications; minor bug fixes
%    2012-08-06: Fixes for R2012b; added bold style; accept RGB string (non-numeric) style
%    2011-11-27: Fixes for R2011b
%    2011-08-29: Fix by Danilo (FEX comment) for non-default text colors
%    2011-03-04: Performance improvement
%    2010-06-27: Fix for R2010a/b; fixed edge case reported by Sharron; CPRINTF with no args runs the demo
%    2009-09-28: Fixed edge-case problem reported by Swagat K
%    2009-05-28: corrected nargout behavior sugegsted by Andreas G�b
%    2009-05-13: First version posted on <a href="http://www.mathworks.com/matlabcentral/fileexchange/authors/27420">MathWorks File Exchange</a>
%
% See also:
%    sprintf, fprintf

% License to use and modify this code is granted freely to all interested, as long as the original author is
% referenced and attributed as such. The original author maintains the right to be solely associated with this work.

% Programmed and Copyright by Yair M. Altman: altmany(at)gmail.com
% $Revision: 1.08 $  $Date: 2012/10/17 21:41:09 $

  persistent majorVersion minorVersion
  if isempty(majorVersion)
      %v = version; if str2double(v(1:3)) <= 7.1
      %majorVersion = str2double(regexprep(version,'^(\d+).*','$1'));
      %minorVersion = str2double(regexprep(version,'^\d+\.(\d+).*','$1'));
      %[a,b,c,d,versionIdStrs]=regexp(version,'^(\d+)\.(\d+).*');  %#ok unused
      v = sscanf(version, '%d.', 2);
      majorVersion = v(1); %str2double(versionIdStrs{1}{1});
      minorVersion = v(2); %str2double(versionIdStrs{1}{2});
  end

  % The following is for debug use only:
  %global docElement txt el
  if ~exist('el','var') || isempty(el),  el=handle([]);  end  %#ok mlint short-circuit error ("used before defined")
  if nargin<1, showDemo(majorVersion,minorVersion); return;  end
  if isempty(style),  return;  end
  if all(ishandle(style)) && length(style)~=3
      dumpElement(style);
      return;
  end

  % Process the text string
  if nargin<2, format = style; style='text';  end
  %error(nargchk(2, inf, nargin, 'struct'));
  %str = sprintf(format,varargin{:});

  % In compiled mode
  try useDesktop = usejava('desktop'); catch, useDesktop = false; end
  if isdeployed | ~useDesktop %#ok<OR2> - for Matlab 6 compatibility
      % do not display any formatting - use simple fprintf()
      % See: http://undocumentedmatlab.com/blog/bold-color-text-in-the-command-window/#comment-103035
      % Also see: https://mail.google.com/mail/u/0/?ui=2&shva=1#all/1390a26e7ef4aa4d
      % Also see: https://mail.google.com/mail/u/0/?ui=2&shva=1#all/13a6ed3223333b21
      count1 = fprintf(format,varargin{:});
  else
      % Else (Matlab desktop mode)
      % Get the normalized style name and underlining flag
      [underlineFlag, boldFlag, style] = processStyleInfo(style);

      % Set hyperlinking, if so requested
      if underlineFlag
          format = ['<a href="">' format '</a>'];

          % Matlab 7.1 R14 (possibly a few newer versions as well?)
          % have a bug in rendering consecutive hyperlinks
          % This is fixed by appending a single non-linked space
          if majorVersion < 7 || (majorVersion==7 && minorVersion <= 1)
              format(end+1) = ' ';
          end
      end

      % Set bold, if requested and supported (R2011b+)
      if boldFlag
          if (majorVersion > 7 || minorVersion >= 13)
              format = ['<strong>' format '</strong>'];
          else
              boldFlag = 0;
          end
      end

      % Get the current CW position
      cmdWinDoc = com.mathworks.mde.cmdwin.CmdWinDocument.getInstance;
      lastPos = cmdWinDoc.getLength;

      % If not beginning of line
      bolFlag = 0;  %#ok
      %if docElement.getEndOffset - docElement.getStartOffset > 1
          % Display a hyperlink element in order to force element separation
          % (otherwise adjacent elements on the same line will be merged)
          if majorVersion<7 || (majorVersion==7 && minorVersion<13)
              if ~underlineFlag
                  fprintf('<a href=""> </a>');  %fprintf('<a href=""> </a>\b');
              elseif format(end)~=10  % if no newline at end
                  fprintf(' ');  %fprintf(' \b');
              end
          end
          %drawnow;
          bolFlag = 1;
      %end

      % Get a handle to the Command Window component
      mde = com.mathworks.mde.desk.MLDesktop.getInstance;
      cw = mde.getClient('Command Window');
      xCmdWndView = cw.getComponent(0).getViewport.getComponent(0);

      % Store the CW background color as a special color pref
      % This way, if the CW bg color changes (via File/Preferences), 
      % it will also affect existing rendered strs
      com.mathworks.services.Prefs.setColorPref('CW_BG_Color',xCmdWndView.getBackground);

      % Display the text in the Command Window
      count1 = fprintf(2,format,varargin{:});

      %awtinvoke(cmdWinDoc,'remove',lastPos,1);   % TODO: find out how to remove the extra '_'
      drawnow;  % this is necessary for the following to work properly (refer to Evgeny Pr in FEX comment 16/1/2011)
      docElement = cmdWinDoc.getParagraphElement(lastPos+1);
      if majorVersion<7 || (majorVersion==7 && minorVersion<13)
          if bolFlag && ~underlineFlag
              % Set the leading hyperlink space character ('_') to the bg color, effectively hiding it
              % Note: old Matlab versions have a bug in hyperlinks that need to be accounted for...
              %disp(' '); dumpElement(docElement)
              setElementStyle(docElement,'CW_BG_Color',1+underlineFlag,majorVersion,minorVersion); %+getUrlsFix(docElement));
              %disp(' '); dumpElement(docElement)
              el(end+1) = handle(docElement);  %#ok used in debug only
          end

          % Fix a problem with some hidden hyperlinks becoming unhidden...
          fixHyperlink(docElement);
          %dumpElement(docElement);
      end

      % Get the Document Element(s) corresponding to the latest fprintf operation
      while docElement.getStartOffset < cmdWinDoc.getLength
          % Set the element style according to the current style
          %disp(' '); dumpElement(docElement)
          specialFlag = underlineFlag | boldFlag;
          setElementStyle(docElement,style,specialFlag,majorVersion,minorVersion);
          %disp(' '); dumpElement(docElement)
          docElement2 = cmdWinDoc.getParagraphElement(docElement.getEndOffset+1);
          if isequal(docElement,docElement2),  break;  end
          docElement = docElement2;
          %disp(' '); dumpElement(docElement)
      end

      % Force a Command-Window repaint
      % Note: this is important in case the rendered str was not '\n'-terminated
      xCmdWndView.repaint;

      % The following is for debug use only:
      el(end+1) = handle(docElement);  %#ok used in debug only
      %elementStart  = docElement.getStartOffset;
      %elementLength = docElement.getEndOffset - elementStart;
      %txt = cmdWinDoc.getText(elementStart,elementLength);
  end

  if nargout
      count = count1;
  end
  return;  % debug breakpoint

% Process the requested style information
function [underlineFlag,boldFlag,style] = processStyleInfo(style)
  underlineFlag = 0;
  boldFlag = 0;

  % First, strip out the underline/bold markers
  if ischar(style)
      % Styles containing '-' or '_' should be underlined (using a no-target hyperlink hack)
      %if style(1)=='-'
      underlineIdx = (style=='-') | (style=='_');
      if any(underlineIdx)
          underlineFlag = 1;
          %style = style(2:end);
          style = style(~underlineIdx);
      end

      % Check for bold style (only if not underlined)
      boldIdx = (style=='*');
      if any(boldIdx)
          boldFlag = 1;
          style = style(~boldIdx);
      end
      if underlineFlag && boldFlag
          warning('YMA:cprintf:BoldUnderline','Matlab does not support both bold & underline')
      end

      % Check if the remaining style sting is a numeric vector
      %styleNum = str2num(style); %#ok<ST2NM>  % not good because style='text' is evaled!
      %if ~isempty(styleNum)
      if any(style==' ' | style==',' | style==';')
          style = str2num(style); %#ok<ST2NM>
      end
  end

  % Style = valid matlab RGB vector
  if isnumeric(style) && length(style)==3 && all(style<=1) && all(abs(style)>=0)
      if any(style<0)
          underlineFlag = 1;
          style = abs(style);
      end
      style = getColorStyle(style);

  elseif ~ischar(style)
      error('YMA:cprintf:InvalidStyle','Invalid style - see help section for a list of valid style values')

  % Style name
  else
      % Try case-insensitive partial/full match with the accepted style names
      validStyles = {'Text','Keywords','Comments','Strings','UnterminatedStrings','SystemCommands','Errors', ...
                     'Black','Cyan','Magenta','Blue','Green','Red','Yellow','White', ...
                     'Hyperlinks'};
      matches = find(strncmpi(style,validStyles,length(style)));

      % No match - error
      if isempty(matches)
          error('YMA:cprintf:InvalidStyle','Invalid style - see help section for a list of valid style values')

      % Too many matches (ambiguous) - error
      elseif length(matches) > 1
          error('YMA:cprintf:AmbigStyle','Ambiguous style name - supply extra characters for uniqueness')

      % Regular text
      elseif matches == 1
          style = 'ColorsText';  % fixed by Danilo, 29/8/2011

      % Highlight preference style name
      elseif matches < 8
          style = ['Colors_M_' validStyles{matches}];

      % Color name
      elseif matches < length(validStyles)
          colors = [0,0,0; 0,1,1; 1,0,1; 0,0,1; 0,1,0; 1,0,0; 1,1,0; 1,1,1];
          requestedColor = colors(matches-7,:);
          style = getColorStyle(requestedColor);

      % Hyperlink
      else
          style = 'Colors_HTML_HTMLLinks';  % CWLink
          underlineFlag = 1;
      end
  end

% Convert a Matlab RGB vector into a known style name (e.g., '[255,37,0]')
function styleName = getColorStyle(rgb)
  intColor = int32(rgb*255);
  javaColor = java.awt.Color(intColor(1), intColor(2), intColor(3));
  styleName = sprintf('[%d,%d,%d]',intColor);
  com.mathworks.services.Prefs.setColorPref(styleName,javaColor);

% Fix a bug in some Matlab versions, where the number of URL segments
% is larger than the number of style segments in a doc element
function delta = getUrlsFix(docElement)  %#ok currently unused
  tokens = docElement.getAttribute('SyntaxTokens');
  links  = docElement.getAttribute('LinkStartTokens');
  if length(links) > length(tokens(1))
      delta = length(links) > length(tokens(1));
  else
      delta = 0;
  end

% fprintf(2,str) causes all previous '_'s in the line to become red - fix this
function fixHyperlink(docElement)
  try
      tokens = docElement.getAttribute('SyntaxTokens');
      urls   = docElement.getAttribute('HtmlLink');
      urls   = urls(2);
      links  = docElement.getAttribute('LinkStartTokens');
      offsets = tokens(1);
      styles  = tokens(2);
      doc = docElement.getDocument;

      % Loop over all segments in this docElement
      for idx = 1 : length(offsets)-1
          % If this is a hyperlink with no URL target and starts with ' ' and is collored as an error (red)...
          if strcmp(styles(idx).char,'Colors_M_Errors')
              character = char(doc.getText(offsets(idx)+docElement.getStartOffset,1));
              if strcmp(character,' ')
                  if isempty(urls(idx)) && links(idx)==0
                      % Revert the style color to the CW background color (i.e., hide it!)
                      styles(idx) = java.lang.String('CW_BG_Color');
                  end
              end
          end
      end
  catch
      % never mind...
  end

% Set an element to a particular style (color)
function setElementStyle(docElement,style,specialFlag, majorVersion,minorVersion)
  %global tokens links urls urlTargets  % for debug only
  global oldStyles
  if nargin<3,  specialFlag=0;  end
  % Set the last Element token to the requested style:
  % Colors:
  tokens = docElement.getAttribute('SyntaxTokens');
  try
      styles = tokens(2);
      oldStyles{end+1} = styles.cell;

      % Correct edge case problem
      extraInd = double(majorVersion>7 || (majorVersion==7 && minorVersion>=13));  % =0 for R2011a-, =1 for R2011b+
      %{
      if ~strcmp('CWLink',char(styles(end-hyperlinkFlag))) && ...
          strcmp('CWLink',char(styles(end-hyperlinkFlag-1)))
         extraInd = 0;%1;
      end
      hyperlinkFlag = ~isempty(strmatch('CWLink',tokens(2)));
      hyperlinkFlag = 0 + any(cellfun(@(c)(~isempty(c)&&strcmp(c,'CWLink')),tokens(2).cell));
      %}

      styles(end-extraInd) = java.lang.String('');
      styles(end-extraInd-specialFlag) = java.lang.String(style);  %#ok apparently unused but in reality used by Java
      if extraInd
          styles(end-specialFlag) = java.lang.String(style);
      end

      oldStyles{end} = [oldStyles{end} styles.cell];
  catch
      % never mind for now
  end
  
  % Underlines (hyperlinks):
  %{
  links = docElement.getAttribute('LinkStartTokens');
  if isempty(links)
      %docElement.addAttribute('LinkStartTokens',repmat(int32(-1),length(tokens(2)),1));
  else
      %TODO: remove hyperlink by setting the value to -1
  end
  %}

  % Correct empty URLs to be un-hyperlinkable (only underlined)
  urls = docElement.getAttribute('HtmlLink');
  if ~isempty(urls)
      urlTargets = urls(2);
      for urlIdx = 1 : length(urlTargets)
          try
              if urlTargets(urlIdx).length < 1
                  urlTargets(urlIdx) = [];  % '' => []
              end
          catch
              % never mind...
              a=1;  %#ok used for debug breakpoint...
          end
      end
  end
  
  % Bold: (currently unused because we cannot modify this immutable int32 numeric array)
  %{
  try
      %hasBold = docElement.isDefined('BoldStartTokens');
      bolds = docElement.getAttribute('BoldStartTokens');
      if ~isempty(bolds)
          %docElement.addAttribute('BoldStartTokens',repmat(int32(1),length(bolds),1));
      end
  catch
      % never mind - ignore...
      a=1;  %#ok used for debug breakpoint...
  end
  %}
  
  return;  % debug breakpoint

% Display information about element(s)
function dumpElement(docElements)
  %return;
  numElements = length(docElements);
  cmdWinDoc = docElements(1).getDocument;
  for elementIdx = 1 : numElements
      if numElements > 1,  fprintf('Element #%d:\n',elementIdx);  end
      docElement = docElements(elementIdx);
      if ~isjava(docElement),  docElement = docElement.java;  end
      %docElement.dump(java.lang.System.out,1)
      disp(' ');
      disp(docElement)
      tokens = docElement.getAttribute('SyntaxTokens');
      if isempty(tokens),  continue;  end
      links = docElement.getAttribute('LinkStartTokens');
      urls  = docElement.getAttribute('HtmlLink');
      try bolds = docElement.getAttribute('BoldStartTokens'); catch, bolds = []; end
      txt = {};
      tokenLengths = tokens(1);
      for tokenIdx = 1 : length(tokenLengths)-1
          tokenLength = diff(tokenLengths(tokenIdx+[0,1]));
          if (tokenLength < 0)
              tokenLength = docElement.getEndOffset - docElement.getStartOffset - tokenLengths(tokenIdx);
          end
          txt{tokenIdx} = cmdWinDoc.getText(docElement.getStartOffset+tokenLengths(tokenIdx),tokenLength).char;  %#ok
      end
      lastTokenStartOffset = docElement.getStartOffset + tokenLengths(end);
      txt{end+1} = cmdWinDoc.getText(lastTokenStartOffset, docElement.getEndOffset-lastTokenStartOffset).char;  %#ok
      %cmdWinDoc.uiinspect
      %docElement.uiinspect
      txt = strrep(txt',sprintf('\n'),'\n');
      try
          data = [tokens(2).cell m2c(tokens(1)) m2c(links) m2c(urls(1)) cell(urls(2)) m2c(bolds) txt];
          if elementIdx==1
              disp('    SyntaxTokens(2,1) - LinkStartTokens - HtmlLink(1,2) - BoldStartTokens - txt');
              disp('    ==============================================================================');
          end
      catch
          try
              data = [tokens(2).cell m2c(tokens(1)) m2c(links) txt];
          catch
              disp([tokens(2).cell m2c(tokens(1)) txt]);
              try
                  data = [m2c(links) m2c(urls(1)) cell(urls(2))];
              catch
                  % Mtlab 7.1 only has urls(1)...
                  data = [m2c(links) urls.cell];
              end
          end
      end
      disp(data)
  end

% Utility function to convert matrix => cell
function cells = m2c(data)
  %datasize = size(data);  cells = mat2cell(data,ones(1,datasize(1)),ones(1,datasize(2)));
  cells = num2cell(data);

% Display the help and demo
function showDemo(majorVersion,minorVersion)
  fprintf('cprintf displays formatted text in the Command Window.\n\n');
  fprintf('Syntax: count = cprintf(style,format,...);  click <a href="matlab:help cprintf">here</a> for details.\n\n');
  url = 'http://UndocumentedMatlab.com/blog/cprintf/';
  fprintf(['Technical description: <a href="' url '">' url '</a>\n\n']);
  fprintf('Demo:\n\n');
  boldFlag = majorVersion>7 || (majorVersion==7 && minorVersion>=13);
  s = ['cprintf(''text'',    ''regular black text'');' 10 ...
       'cprintf(''hyper'',   ''followed %s'',''by'');' 10 ...
       'cprintf(''key'',     ''%d colored'',' num2str(4+boldFlag) ');' 10 ...
       'cprintf(''-comment'',''& underlined'');' 10 ...
       'cprintf(''err'',     ''elements:\n'');' 10 ...
       'cprintf(''cyan'',    ''cyan'');' 10 ...
       'cprintf(''_green'',  ''underlined green'');' 10 ...
       'cprintf(-[1,0,1],  ''underlined magenta'');' 10 ...
       'cprintf([1,0.5,0], ''and multi-\nline orange\n'');' 10];
   if boldFlag
       % In R2011b+ the internal bug that causes the need for an extra space
       % is apparently fixed, so we must insert the sparator spaces manually...
       % On the other hand, 2011b enables *bold* format
       s = [s 'cprintf(''*blue'',   ''and *bold* (R2011b+ only)\n'');' 10];
       s = strrep(s, ''')',' '')');
       s = strrep(s, ''',5)',' '',5)');
       s = strrep(s, '\n ','\n');
   end
   disp(s);
   eval(s);


%%%%%%%%%%%%%%%%%%%%%%%%%% TODO %%%%%%%%%%%%%%%%%%%%%%%%%
% - Fix: Remove leading space char (hidden underline '_')
% - Fix: Find workaround for multi-line quirks/limitations
% - Fix: Non-\n-terminated segments are displayed as black
% - Fix: Check whether the hyperlink fix for 7.1 is also needed on 7.2 etc.
% - Enh: Add font support


function [files, varargout] = file_miner(folders,str)

if nargin == 1                                                              %If the user didn't specify a search directory.
    str = folders;                                                          %Set the string variable to what the user entered for the folder.
    folders = uigetdir(cd,['Select macro directory to search for "'...
        str '"']);                                                          %Have the user choose a starting directory.
    if folders(1) == 0                                                      %If the user clicked "cancel"...
        return                                                              %Skip execution of the rest of the function.
    end
end
if ~iscell(folders)                                                         %If the directories input is not yet a cell array...
    folders = {folders};                                                    %Convert the directories input to a cell array.
end
if ~iscell(str)                                                             %If the search string isn't a cell array...
    str = {str};                                                            %Convert the search string to a cell array.
end
str = str(:);                                                               %Make sure the search string cell array has a singleton dimension.
for f = 1:length(folders)                                                   %Step through each specified directory...
    if folders{f}(end)  ~= '\'                                              %If a directory doesn't end in a forward slash...
        folders{f}(end+1) = '\';                                            %Add a forward slash to the end of the main path.
    end
end

varargout = {[],[]};                                                        %Set the optional output arguments to empty brackets by default.

waitbar = big_waitbar('title','Finding all subfolders...','color','r');     %Create a waitbar figure.
checker = zeros(1,length(folders));                                         %Create a checking matrix to see if we've looked in all the subfolders.
while any(checker == 0)                                                     %Keep looking until all subfolders have been checked for *.xls files.
    a = find(checker == 0,1,'first');                                       %Find the next folder that hasn't been checked for subfolders.
    temp = dir(folders{a});                                                 %Grab all the files and folders in the current folder.
    if waitbar.isclosed()                                                   %If the user closed the waitbar figure...
        files = [];                                                         %Return an empty matrix.
        return                                                              %Skip execution of the rest of the function.
    else                                                                    %Otherwise...
        b = find(folders{a}(1:end-2) == '\',1,'last');                      %Find the last forward slash.
        if isempty(b)                                                       %If no forward slash was found...
            b = 1;                                                          %Set the index to 1.
        end
        waitbar.string(sprintf('Checking: %s',folders{a}(b:end)));          %Update the waitbar text.
        waitbar.value(sum(checker)/length(checker));                        %Update the waitbar value.
    end
    for f = 1:length(temp)                                                  %Step through all of the returned contents.
        if ~any(temp(f).name == '.') && temp(f).isdir == 1                  %If an item is a folder, but not a system folder...
            subfolder = [folders{a} temp(f).name '\'];                      %Concatenate the full subfolder name...
            if ~any(strcmpi(subfolder,folders))                             %If the subfolder is not yet in the list of subfolders...                
                folders{end+1} = [folders{a} temp(f).name '\'];             %Add the subfolder to the list of subfolders.
                checker(end+1) = 0;                                         %Add an entry to the checker matrix to check this subfolder for more subfolders.
            end
        end
    end
    checker(a) = 1;                                                         %Mark the last folder as having been checked.        
end

N = 0;                                                                      %Create a file counter.
for i = 1:length(str)                                                       %Step through each search string.
    waitbar.title(['Counting ' str{i} ' files...']);                        %Update the title on the waitbar.
    for f = 1:length(folders)                                               %Step through every subfolder.
        temp = dir([folders{f} str{i}]);                                    %Grab all the matching filenames in the subfolder.
        N = N + length(temp);                                               %Add the number of files to the file counter.
        if waitbar.isclosed()                                               %If the user closed the waitbar figure...
            files = [];                                                     %Return an empty matrix.
            return                                                          %Skip execution of the rest of the function.
        else                                                                %Otherwise...
            b = find(folders{f}(1:end-2) == '\',1,'last');                  %Find the last forward slash.
            if isempty(b)                                                   %If no forward slash was found...
                b = 1;                                                      %Set the index to 1.
            end
            waitbar.string(sprintf('Checking: %s',folders{f}(b:end)));      %Update the waitbar text.
            waitbar.value(f/length(folders));                               %Update the waitbar value.
        end
    end
end

files = cell(N,1);                                                          %Create an empty cell array to hold filenames.
file_times = zeros(N,1);                                                    %Create a matrix to hold file modification dates.
wait_step = ceil(length(files)/100);                                        %Find an even stepsize for displaying progress on the waitbar.
N = 0;                                                                      %Reset the file counter.
for i = 1:length(str)                                                       %Step through each search string.
    waitbar.title(['Saving ' str{i} ' filenames...']);                      %Update the title on the waitbar.
    for f = 1:length(folders)                                               %Step through every subfolder.
        temp = dir([folders{f} str{i}]);                                    %Grab all the matching filenames in the subfolder.
        for j = 1:length(temp)                                              %Step through every matching file.
            if ~all(temp(j).name == '.')                                    %If the filename isn't a hidden folder.
                N = N + 1;                                                  %Increment the file counter.
                files{N} = [folders{f} temp(j).name];                       %Save the filename with it's full path.
                file_times(N) = temp(j).datenum;                            %Save the last file modification date.
                if waitbar.isclosed()                                       %If the user closed the waitbar figure...
                    files = [];                                             %Return an empty matrix.
                    return                                                  %Skip execution of the rest of the function.
                elseif rem(N,wait_step) == 0                                %Otherwise, if it's time to update the waitbar...
                    waitbar.string(sprintf('Indexing: %s',temp(j).name));   %Update the waitbar text.
                    waitbar.value(N/length(files));                         %Update the waitbar value.
                    drawnow;                                                %Update the plot immediately.
                end
            end
        end
    end
end

waitbar.close();                                                            %Close the waitbar.

varargout{1} = file_times;                                                  %Return the file modification times as an optional output argument.
varargout{2} = folders;                                                     %Return the folders cell array as an optional output argument.
drawnow;                                                                    %Update all figures to close the waitbar.


function [varargout] = nanmax(varargin)
%NANMAX Maximum value, ignoring NaNs.
%   M = NANMAX(A) returns the maximum of A with NaNs treated as missing. 
%   For vectors, M is the largest non-NaN element in A.  For matrices, M is
%   a row vector containing the maximum non-NaN element from each column.
%   For N-D arrays, NANMAX operates along the first non-singleton
%   dimension.
%
%   [M,NDX] = NANMAX(A) returns the indices of the maximum values in A.  If
%   the values along the first non-singleton dimension contain more than
%   one maximal element, the index of the first one is returned.
%  
%   M = NANMAX(A,B) returns an array the same size as A and B with the
%   largest elements taken from A or B.  Either one can be a scalar.
%
%   [M,NDX] = NANMAX(A,[],DIM) operates along the dimension DIM.
%
%   See also MAX, NANMIN, NANMEAN, NANMEDIAN, NANMIN, NANVAR, NANSTD.

%   Copyright 1993-2004 The MathWorks, Inc. 


% Call [m,ndx] = max(a,b) with as many inputs and outputs as needed
[varargout{1:nargout}] = max(varargin{:});


function m = nanmean(x,dim)
%NANMEAN Mean value, ignoring NaNs.
%   M = NANMEAN(X) returns the sample mean of X, treating NaNs as missing
%   values.  For vector input, M is the mean value of the non-NaN elements
%   in X.  For matrix input, M is a row vector containing the mean value of
%   non-NaN elements in each column.  For N-D arrays, NANMEAN operates
%   along the first non-singleton dimension.
%
%   NANMEAN(X,DIM) takes the mean along dimension DIM of X.
%
%   See also MEAN, NANMEDIAN, NANSTD, NANVAR, NANMIN, NANMAX, NANSUM.

%   Copyright 1993-2004 The MathWorks, Inc.
%   $Revision: 2.13.4.3 $  $Date: 2004/07/28 04:38:41 $

% Find NaNs and set them to zero
nans = isnan(x);
x(nans) = 0;

if nargin == 1 % let sum deal with figuring out which dimension to use
    % Count up non-NaNs.
    n = sum(~nans);
    n(n==0) = NaN; % prevent divideByZero warnings
    % Sum up non-NaNs, and divide by the number of non-NaNs.
    m = sum(x) ./ n;
else
    % Count up non-NaNs.
    n = sum(~nans,dim);
    n(n==0) = NaN; % prevent divideByZero warnings
    % Sum up non-NaNs, and divide by the number of non-NaNs.
    m = sum(x,dim) ./ n;
end


function y = nanmedian(x,dim)
%NANMEDIAN Median value, ignoring NaNs.
%   M = NANMEDIAN(X) returns the sample median of X, treating NaNs as
%   missing values.  For vector input, M is the median value of the non-NaN
%   elements in X.  For matrix input, M is a row vector containing the
%   median value of non-NaN elements in each column.  For N-D arrays,
%   NANMEDIAN operates along the first non-singleton dimension.
%
%   NANMEDIAN(X,DIM) takes the median along the dimension DIM of X.
%
%   See also MEDIAN, NANMEAN, NANSTD, NANVAR, NANMIN, NANMAX, NANSUM.

%   Copyright 1993-2004 The MathWorks, Inc.


if nargin == 1
    y = prctile(x, 50);
else
    y = prctile(x, 50,dim);
end


function [varargout] = nanmin(varargin)
%NANMIN Minimum value, ignoring NaNs.
%   M = NANMIN(A) returns the minimum of A with NaNs treated as missing. 
%   For vectors, M is the smallest non-NaN element in A.  For matrices, M
%   is a row vector containing the minimum non-NaN element from each
%   column.  For N-D arrays, NANMIN operates along the first non-singleton
%   dimension.
%
%   [M,NDX] = NANMIN(A) returns the indices of the minimum values in A.  If
%   the values along the first non-singleton dimension contain more than
%   one minimal element, the index of the first one is returned.
%  
%   M = NANMIN(A,B) returns an array the same size as A and B with the
%   smallest elements taken from A or B.  Either one can be a scalar.
%
%   [M,NDX] = NANMIN(A,[],DIM) operates along the dimension DIM.
%
%   See also MIN, NANMAX, NANMEAN, NANMEDIAN, NANVAR, NANSTD.

%   Copyright 1993-2004 The MathWorks, Inc. 


% Call [m,ndx] = min(a,b) with as many inputs and outputs as needed
[varargout{1:nargout}] = min(varargin{:});


function y = nansum(x,dim)
%NANSUM Sum, ignoring NaNs.
%   Y = NANSUM(X) returns the sum of X, treating NaNs as missing values.
%   For vector input, Y is the sum of the non-NaN elements in X.  For
%   matrix input, Y is a row vector containing the sum of non-NaN elements
%   in each column.  For N-D arrays, NANSUM operates along the first
%   non-singleton dimension.
%
%   Y = NANSUM(X,DIM) takes the sum along dimension DIM of X.
%
%   See also SUM, NANMEAN, NANVAR, NANSTD, NANMIN, NANMAX, NANMEDIAN.

%   Copyright 1993-2004 The MathWorks, Inc.


% Find NaNs and set them to zero.  Then sum up non-NaNs.  Cols of all NaNs
% will return zero.
x(isnan(x)) = 0;
if nargin == 1 % let sum figure out which dimension to work along
    y = sum(x);
else           % work along the explicitly given dimension
    y = sum(x,dim);
end


function y = prctile(x,p,dim)
%PRCTILE Percentiles of a sample.
%   Y = PRCTILE(X,P) returns percentiles of the values in X.  P is a scalar
%   or a vector of percent values.  When X is a vector, Y is the same size
%   as P, and Y(i) contains the P(i)-th percentile.  When X is a matrix,
%   the i-th row of Y contains the P(i)-th percentiles of each column of X.
%   For N-D arrays, PRCTILE operates along the first non-singleton
%   dimension.
%
%   Y = PRCTILE(X,P,DIM) calculates percentiles along dimension DIM.  The
%   DIM'th dimension of Y has length LENGTH(P).
%
%   Percentiles are specified using percentages, from 0 to 100.  For an N
%   element vector X, PRCTILE computes percentiles as follows:
%      1) The sorted values in X are taken as the 100*(0.5/N), 100*(1.5/N),
%         ..., 100*((N-0.5)/N) percentiles.
%      2) Linear interpolation is used to compute percentiles for percent
%         values between 100*(0.5/N) and 100*((N-0.5)/N)
%      3) The minimum or maximum values in X are assigned to percentiles
%         for percent values outside that range.
%
%   PRCTILE treats NaNs as missing values, and removes them.
%
%   Examples:
%      y = prctile(x,50); % the median of x
%      y = prctile(x,[2.5 25 50 75 97.5]); % a useful summary of x
%
%   See also IQR, MEDIAN, NANMEDIAN, QUANTILE.

%   Copyright 1993-2004 The MathWorks, Inc.


if ~isvector(p) || numel(p) == 0 || any(p < 0 | p > 100) || ~isreal(p)
    error(message('stats:prctile:BadPercents'));
end

% Figure out which dimension prctile will work along.
sz = size(x);
if nargin < 3 
    dim = find(sz ~= 1,1);
    if isempty(dim)
        dim = 1; 
    end
    dimArgGiven = false;
else
    % Permute the array so that the requested dimension is the first dim.
    nDimsX = ndims(x);
    perm = [dim:max(nDimsX,dim) 1:dim-1];
    x = permute(x,perm);
    % Pad with ones if dim > ndims.
    if dim > nDimsX
        sz = [sz ones(1,dim-nDimsX)];
    end
    sz = sz(perm);
    dim = 1;
    dimArgGiven = true;
end

% If X is empty, return all NaNs.
if isempty(x)
    if isequal(x,[]) && ~dimArgGiven
        y = nan(size(p),class(x));
    else
        szout = sz; szout(dim) = numel(p);
        y = nan(szout,class(x));
    end

else
    % Drop X's leading singleton dims, and combine its trailing dims.  This
    % leaves a matrix, and we can work along columns.
    nrows = sz(dim);
    ncols = prod(sz) ./ nrows;
    x = reshape(x, nrows, ncols);

    x = sort(x,1);
    nonnans = ~isnan(x);

    % If there are no NaNs, do all cols at once.
    if all(nonnans(:))
        n = sz(dim);
        if isequal(p,50) % make the median fast
            if rem(n,2) % n is odd
                y = x((n+1)/2,:);
            else        % n is even
                y = (x(n/2,:) + x(n/2+1,:))/2;
            end
        else
            q = [0 100*(0.5:(n-0.5))./n 100]';
            xx = [x(1,:); x(1:n,:); x(n,:)];
            y = zeros(numel(p), ncols, class(x));
            y(:,:) = interp1q(q,xx,p(:));
        end

    % If there are NaNs, work on each column separately.
    else
        % Get percentiles of the non-NaN values in each column.
        y = nan(numel(p), ncols, class(x));
        for j = 1:ncols
            nj = find(nonnans(:,j),1,'last');
            if nj > 0
                if isequal(p,50) % make the median fast
                    if rem(nj,2) % nj is odd
                        y(:,j) = x((nj+1)/2,j);
                    else         % nj is even
                        y(:,j) = (x(nj/2,j) + x(nj/2+1,j))/2;
                    end
                else
                    q = [0 100*(0.5:(nj-0.5))./nj 100]';
                    xx = [x(1,j); x(1:nj,j); x(nj,j)];
                    y(:,j) = interp1q(q,xx,p(:));
                end
            end
        end
    end

    % Reshape Y to conform to X's original shape and size.
    szout = sz; szout(dim) = numel(p);
    y = reshape(y,szout);
end
% undo the DIM permutation
if dimArgGiven
     y = ipermute(y,perm);  
end

% If X is a vector, the shape of Y should follow that of P, unless an
% explicit DIM arg was given.
if ~dimArgGiven && isvector(x)
    y = reshape(y,size(p)); 
end


function y = range(x,dim)
%RANGE  Sample range.
%   Y = RANGE(X) returns the range of the values in X.  For a vector input,
%   Y is the difference between the maximum and minimum values.  For a
%   matrix input, Y is a vector containing the range for each column.  For
%   N-D arrays, RANGE operates along the first non-singleton dimension.
%
%   RANGE treats NaNs as missing values, and ignores them.
%
%   Y = RANGE(X,DIM) operates along the dimension DIM.
%
%   See also IQR, MAD, MAX, MIN, STD.

%   Copyright 1993-2004 The MathWorks, Inc.
%   $Revision: 1.1.8.1 $  $Date: 2010/03/16 00:17:06 $

if nargin < 2
    y = max(x) - min(x);
else
    y = max(x,[],dim) - min(x,[],dim);
end


