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
%   02/20/2017 - Drew Sloan - Corrected for a bug in MotoTrak 2.0
%       (*.MotoTrak) data files in which the sample times would roll over
%       to zero after reaching the float32 maximum.
%   03/03/2017 - Drew Sloan - Added a column to the pull analysis options
%       to output the mean number of pull attempts between hits.
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
                        'HIT RATE (%)';
                        'MISSES';
                        'MANUAL FEEDS';
                        'INITATION TO HIT LATENCY (s)';
                        'HITS IN FIRST 5 MINUTES';
                        'TRIALS IN FIRST 5 MINUTES';
                        'HIT RATE (%) IN FIRST 5 MINUTES';
                        'HIT RATE (%) IN FIRST 15 MINUTES';
                        'HITS IN FIRST 10 MINUTES';
                        'TRIALS IN FIRST 10 MINUTES';
                        'HIT RATE (%) IN FIRST 10 MINUTES';
                        'HITS IN FIRST 15 MINUTES';
                        'TRIALS IN FIRST 15 MINUTES';
                        'HIT RATE (%) IN FIRST 10 TRIALS';
                        'HIT RATE (%) IN FIRST 50 TRIALS';
                        'HIT RATE (%) IN LAST 10 TRIALS';
                        'HIT RATE (%) IN LAST 50 TRIALS';
                        'MAX HITS IN ANY 5 MINUTES';
                        'MAX TRIALS IN ANY 5 MINUTES';                        
                        'MAX HIT RATE IN ANY 5 MINUTES';
                        'MIN. INTER-TRIAL INTERVAL (s)'};                   %List the optional columns that apply to every task type.
fields.optional = sort(fields.optional);                                    %Sort the optional fields alphabetically.
                        
fields.pull =       {   'MEAN PEAK FORCE (gm)';
                        'MEDIAN PEAK FORCE (gm)';
                        'MAX PEAK FORCE (gm)';
                        'MAX HIT THRESHOLD (gm)';
                        'MEDIAN HIT THRESHOLD (gm)';
                        'PERCENTAGE OF TRIALS EXCEEDING MAX HIT THRESHOLD (%)'
                        'LATENCY TO PEAK FORCE (s)';
                        'PULL ATTEMPTS PER TRIAL';
                        'MEAN PULL ATTEMPTS BETWEEN HITS'
                        'MEAN ATTEMPT FORCE (gm)';
                        'FATIGUE RATIO (LAST 10/FIRST 10)';
                        'MEAN PULL DURATION (s)';
                        'MEAN PEAK IMPULSE (gm/s)';
                        'NUMBER OF ATTEMPTS';
                        'MEAN SUB-CEILING ATTEMPT FORCE (gm)';
                        'MEAN SUB-CEILING PEAK FORCE (gm)';
                        'MEAN SUCCESSFUL ATTEMPT PEAK FORCE (gm)';
                        'SUB-CEILING PULL ATTEMPTS PER TRIAL';
                        'MEAN PEAK FORCE (gm) IN FIRST 10 TRIALS';
                        'MEDIAN PEAK FORCE (gm) IN FIRST 10 TRIALS'                        
                        'MEAN PEAK FORCE (gm) IN FIRST 50 TRIALS';
                        'MEDIAN PEAK FORCE (gm) IN FIRST 50 TRIALS';
                        'MEAN PEAK FORCE (gm) IN LAST 10 TRIALS';
                        'MEDIAN PEAK FORCE (gm) IN LAST 10 TRIALS'                        
                        'MEAN PEAK FORCE (gm) IN LAST 50 TRIALS';
                        'MEDIAN PEAK FORCE (gm) IN LAST 50 TRIALS';
                    };                                                      %List the optional columns that apply to only the isometric pull tasks.
fields.pull = sort(fields.pull);                                            %Sort the pull task fields alphabetically.
                        
fields.knob =       {   'MEAN PEAK ANGLE (degrees)';
                        'MAX PEAK ANGLE (degrees)';
                        'MAX HIT THRESHOLD (degrees)';
                        'LATENCY TO PEAK ANGLE (s)';
                        'TURN ATTEMPTS PER TRIAL';
                        'MEAN ATTEMPT ANGLE (degrees)';
                        'FATIGUE RATIO (LAST 10/FIRST 10)';
                        'MEAN TURN DURATION (s)';
                        'MEAN PEAK ROTATIONAL VELOCITY (degrees/s)'};       %List the optional columns that apply to only the supination tasks. 
fields.knob = sort(fields.knob);                                            %Sort the knob task fields alphabetically.
                    
fields.lever =      {   'MEAN PEAK ANGLE (degrees)';
                        'MAX PEAK ANGLE (degrees)';
                        'MAX HIT THRESHOLD (degrees)';
                        'LATENCY TO PEAK ANGLE (s)';
                        'TURN ATTEMPTS PER TRIAL';
                        'MEAN ATTEMPT ANGLE (degrees)';
                        'FATIGUE RATIO (LAST 10/FIRST 10)';
                        'MEAN TURN DURATION (s)';
                        'LEVER PRESS TIMESTAMPS';
                        'MEAN PEAK ROTATIONAL VELOCITY (degrees/s)'};       %List the optional columns that apply to only the lever press tasks.
fields.lever = sort(fields.lever);                                          %Sort the lever task fields alphabetically.


%List the recognized handles field names for custom analysis situations.
custom_fields = {   'popdata_to_tsv_min_position',              'Minimum Position',                 'cm';
                    'popdata_to_tsv_pull_min_hit_thresh',       'Minimum Pull Hit Threshold',       'g';
                    'popdata_to_tsv_knob_min_hit_thresh',       'Minimum Supination Hit Threshold',	'degrees'};
min_pos = NaN;                                                              %Create a variable to hold a minimum position.
pull_min_hit_thresh = NaN;                                                  %Create a variable to hold a minimum pull hit threshold.
knob_min_hit_thresh = NaN;                                                  %Create a variable to hold a minimum knob hit threshold.

                    
%Set the expected path for configuration files.
if nargin > 0                                                               %If there's more than one optional input argument.
    handles = varargin{end};                                                %Assume a handles structure is the last input argument
else                                                                        %Otherwise...
    handles = [];                                                           %Create an empty handles structure.
end
if ~isstruct(handles) || ~isfield(handles,'config_path')                    %If no AppData path was passed to the function...
    if isdeployed                                                           %If the function is running as compiled code...
        handles.config_path = pwd;                                          %Use the current folder as the AppData path.
    else                                                                    %Otherwise, if the function is running in MATLAB...
        [temp,~,~] = fileparts(which(mfilename));                           %Use the folder containing the function as the AppData path.
        handles.config_path = temp;                                         %Save the folder containing the function as the configuration path.
    end
elseif ~exist(handles.config_path,'dir')                                    %If the specifieid AppData path doesn't exist...
    error(['ERROR IN ' upper(mfilename) ': The specified configuration '...
        'directory doesn''t exist!']);                                      %Trhow an error.
end
if isempty(handles.config_path)                                             %If the AppData path couldn't be set by any of the previous methods...
    handles.config_path = pwd;                                              %Use the current folder as the AppData path.
end

%Check for custom analysis tweaks in the configuration fields.
if isfield(handles,'custom_popdata_to_tsv') && ...
        strcmpi(handles.custom_popdata_to_tsv,'on')                         %If the user has a custom function profile turned on...
    enabled_fields = intersect(fieldnames(handles),custom_fields(:,1));     %Find all custom field names specified by the user.
    uih = 1.5;                                                              %Set the height for all buttons.
    w = 15;                                                                 %Set the width of the function selection figure.
    h = (numel(enabled_fields)+1)*(uih + 0.1) + 0.5 - 0.25*uih;             %Set the height of the function selection figure.
    set(0,'units','centimeters');                                           %Set the screensize units to centimeters.
    pos = get(0,'ScreenSize');                                              %Grab the screensize.
    pos = [pos(3)/2-w/2, pos(4)/2-h/2, w, h];                               %Scale a figure position relative to the screensize.
    fig = figure('units','centimeters',...
        'Position',pos,...
        'resize','off',...
        'MenuBar','none',...
        'name','Use Custom Analysis Options?',...
        'numbertitle','off',...
        'userdata',0);                                                      %Set the properties of the figure.
    for i = 1:size(enabled_fields,1)                                        %Step through each recognize custom analysis field name.
        j = strcmpi(enabled_fields{i},custom_fields(:,1));                  %Find the index for the description of the custom field.
        uicontrol(fig,'style','edit',...
            'enable','inactive',...
            'string',[custom_fields{j,2} ': '],...
            'units','centimeters',...
            'position',[0.1 h-i*(uih+0.1) (w-0.3)/2 uih],...
            'fontsize',14,...
            'backgroundcolor',[0.8 0.8 0.9],...
            'horizontalalignment','right');                                 %Make an text box for each parameter.
        str = sprintf(' %1.2f %s',handles.(custom_fields{j,1}),...
            custom_fields{j,3});                                            %Create the textbox string.
        uicontrol(fig,'style','edit',...
            'enable','inactive',...
            'string',str,...
            'units','centimeters',...
            'position',[0.1+(w-0.3)/2 h-i*(uih+0.1) (w-0.3)/2 uih],...
            'fontsize',14,...
            'horizontalalignment','left');                                  %Make an text box for each parameter.
    end
    uicontrol(fig,'style','pushbutton',...
            'string','YES',...
            'units','centimeters',...
            'position',[0.1 0.1 (w-0.3)/2 uih],...
            'fontweight','bold',...
            'fontsize',14,...
            'callback','set(gcbf,''userdata'',1); uiresume(gcbf);');        %Make pushbutton for "YES'.
    uicontrol(fig,'style','pushbutton',...
            'string','NO',...
            'units','centimeters',...
            'position',[0.1+(w-0.3)/2 0.1 (w-0.3)/2 uih],...
            'fontweight','bold',...
            'fontsize',14,...
            'callback','set(gcbf,''userdata'',2); uiresume(gcbf)');         %Make pushbutton for "YES'.
    uiwait(fig);                                                            %Wait for the user to make a selection.
    temp = get(fig,'userdata');                                             %Grab the value in the figure's UserData property.
    if ishandle(fig)                                                        %If the figure still exists...
        delete(fig);                                                        %Close the figure.
    end
    if temp == 0                                                            %If the user closed the figure without choosing a port...
        return                                                              %Skip execution of the rest of the function.
    end    
    if temp == 1                                                            %If the user selected to apply the custom values...
        for str = enabled_fields'                                           %Step through the custom options.
            switch str{1}                                                   %Switch between the custom options.
                case 'popdata_to_tsv_pull_min_hit_thresh'                   %If a minimum pull hit threshold is specified...
                    pull_min_hit_thresh = ...
                        handles.popdata_to_tsv_pull_min_hit_thresh;         %Save the minimum hit threshold.
                case 'popdata_to_tsv_knob_min_hit_thresh'                   %If a minimum knob hit threshold is specified...
                    knob_min_hit_thresh = ...
                        handles.popdata_to_tsv_knob_min_hit_thresh;         %Save the minimum hit threshold.
                case 'popdata_to_tsv_min_position'                          %If a minimum position is specified...
                	min_pos = handles.popdata_to_tsv_min_position;          %Save the minim position.
            end
        end
    end              
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
waitbar = big_waitbar('title','Identifying subjects from files...');        %Create a waitbar figure.
subjects = files;                                                           %Copy the filenames to another cell array.
for f = 1:length(subjects)                                                  %Step through each file.
    [~,filename,ext] = fileparts(files{f});                                 %Grab the filename and extension from the trial.
    if waitbar.isclosed()                                                   %If the user closed the waitbar figure...
        errordlg(['The MotoTrak export population data to TSV file was '...
            'cancelled by the user!'],'Export Cancelled');                  %Show an error.
        return                                                              %Skip execution of the rest of the function.
    else                                                                    %Otherwise...
        waitbar.string(sprintf('Checking (%1.0f/%1.0f): %s%s',...
            f, length(files), filename, ext));                              %Update the waitbar text.
        waitbar.value(f/length(files));                                     %Update the waitbar value.
    end
    try                                                                     %Try to read in the data file...
        switch ext                                                          %Switch between the possible file extensions.
            case '.ArdyMotor'                                               %If the data file is an *.ArdyMotor file.
                session = ArdyMotorHeaderRead(files{f});                    %Read in the data from each file.
            case '.MotoTrak'                                                %If the data file is a *.MotoTrak file.
                session = MotoTrakHeaderRead(files{f});                     %Read in the data from each file.                
        end
        subjects{f} = session.subject;                                      %Save the subject name.
    catch err                                                               %If an error occurs...
        warning(['ERROR READING: ' files{f}]);                              %Show which file had a read problem...
        warning(err.message);                                               %Show the actual error message.
        continue                                                            %Skip to the next file.
    end    
%     [~,filename,~] = fileparts(files{r});                                   %Grab the basic filename from the full filename.
%     i = strfind(filename,'_20');                                            %Find the start of the timestamp.
%     if isempty(i) || length(i) > 1                                          %If no timestamp was found in the filename, or multiple timestamps were found...
%         subjects{r} = [];                                                   %Set the rat name to empty brackets.
%     else                                                                    %Otherwise...
%         subjects{r} = filename(1:i-1);                                      %Kick out all characters of the filename except the rat name.
%     end
end
waitbar.close();                                                            %Close the waitbar.
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
        data(s).file = [filename ext];                                      %Save the filename.
        if ~isfield(session,'trial') || isempty(session.trial)              %If there's no trials in the session...
           	trials = [];                                                    %Create an empty trials matrix.
        else                                                                %Otherwise...
            trials = 1:length(session.trial);                               %Create indices for the trials.
        end
        if ~isempty(trials) && ~isnan(min_pos)                              %If a minimum position was set...
            switch ext                                                      %Switch between the possible file extensions.
                case '.ArdyMotor'                                           %If the data file is an *.ArdyMotor file.
                    if session.position < min_pos                           %If the position was less than the minimum position...
                        trials = [];                                        %Set the trials matrix to empty brackets.
                    end
                case '.MotoTrak'                                            %If the data file is a *.MotoTrak file.
                    temp = horzcat(session.trial.position);                 %Grab the device position for all trials.
                    temp = find(temp >= min_pos);                           %Find all trials at or above the minimum position.        
                    trials = intersect(trials,temp);                        %Find all valid trials.
            end
        end
        if ~isempty(trials) && ...
                (~isnan(pull_min_hit_thresh) && ...
                strcmpi(session.device,'pull')) || ...
                (~isnan(knob_min_hit_thresh) && ...
                strcmpi(session.device,'knob'))                             %If there's a minimum hit threshold for this session's device...
            temp = horzcat(session.trial.thresh);                           %Grab the hit threshold for all trials.
            if strcmpi(session.device,'pull')                               %If this was a pull session...
                temp = find(temp >= pull_min_hit_thresh);                   %Find all trials with thresholds at or above the minimum.
            elseif strcmpi(session.device,'knob')                           %If this was a knob session...
                temp = find(temp >= knob_min_hit_thresh);                   %Find all trials with thresholds at or above the minimum.
            end
            trials = intersect(trials,temp);                                %Find all valid trials.
        end
        if isempty(trials)                                                  %If we're skipping this session...
            for str = {'outcome','thresh','starttime','peak','impulse',...
                    'hittime','hit_peak_width','time_to_peak',...
                    'pre_hit_attempts','all_peak_width','attempt_force',...
                    'fatigue','timestamp','file','long_attempts'}           %Step through each field of the data structure.
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
            continue                                                        %Skip to the next file.
        end
        exclude_trials = setdiff(1:length(session.trial),trials);           %Find all excluded trials.
        data(s).outcome = vertcat(session.trial.outcome);                   %Grab the outcome of each trial.
        data(s).outcome(exclude_trials) = NaN;                              %Kick out all excluded trials.
        data(s).thresh = vertcat(session.trial.thresh);                     %Grab the threshold for each trial.
        data(s).starttime = vertcat(session.trial.starttime);               %Grab the start time for each trial.
        data(s).timestamp = data(s).starttime(1);                           %Grab the timestamp from the start of the first trial.  
        data(s).peak = nan(length(session.trial),1);                        %Create a matrix to hold the peak force.
        data(s).subceiling_peak = nan(length(session.trial),1);             %Create a matrix to hold the peak force.
        data(s).success_peak = nan(length(session.trial),1);                %Create a matrix to hold the peak force.
        data(s).impulse = nan(length(session.trial),1);                     %Create a matrix to hold the peak impulse.
        data(s).hittime = 86400*(vertcat(session.trial.hittime)...
            - vertcat(session.trial.starttime));                            %Create a matrix to hold the time to hit.
        data(s).hittime(data(s).hittime <= 0) = NaN;                        %Replace all miss trial hit times with NaNs.
        data(s).hittime(exclude_trials) = NaN;                              %Kick out all excluded trials.
        all_peak_width = [];                                                %Create a matrix to hold all peak widths.
        data(s).hit_peak_width = nan(length(session.trial),1);              %Create a matrix to hold hit peak widths.
        attempt_force = [];                                                 %Create a matrix to hold attempt force.
        data(s).time_to_peak = nan(length(session.trial),1);                %Create a matrix to hold the time to peak force.
        data(s).pre_hit_attempts = nan(length(session.trial),1);            %Create a matrix to hold the time to peak force.
        data(s).long_attempts = nan(length(session.trial),1);               %Create a matrix to hold the time to peak force.
        data(s).subceiling_pre_hit_attempts = ...
            nan(length(session.trial),1);                                   %Create a matrix to hold the number of subceiling pre-hit attempts.
        subceiling_attempt_force = [];                                      %Create a matrix to hold sub-ceiling attempt force.
        total_attempts = 0;                                                 %Start a counter of the total number of attempts.
        long_attempt_counter = 0;                                           %Start a counter to count attempts across trials.
        if isfield(session,'parameters') && ...
                any(strcmpi(session.parameters,...
                'upper bound force threshold'))                             %If the file is a MotoTrak 2.0 version for a stage with a force ceiling....
            i = strcmpi(session.parameters,...
                'upper bound force threshold');                             %Find the parameter index for the ceiling.
            temp = vertcat(session.trial.parameters);                       %Grab the parameters for all trials.
            data(s).ceiling = temp(:,i);                                    %Save the trial-by-trial ceiling values to the structure.
        else                                                                %Otherwise...
            data(s).ceiling = NaN;                                          %Set the force ceiling to NaN.
        end            
        for t = trials                                                      %Step through every valid trial.
            signal = session.trial(t).signal;                               %Grab the signal for the trial.
            times = double(session.trial(t).sample_times);                  %Grab the sampling times for the trial.
            if any(signal > 2500)                                           %If any part of the signal is greater than 2500...
                data(s).outcome(t) = NaN;                                   %Exclude the trial outcome.
                data(s).hittime(t) = NaN;                                   %Exclude the trial hit time.
                continue                                                    %Exclude this trial from the analysis.
            end
            hitwin = 1000*session.trial(t).hitwin;                          %Find the hit window for this trial, in milliseconds.
            init = session.trial(t).init;                                   %Grab the initiation threshold.
            if isempty(session.trial(t).thresh)                             %If there's no threshold...
                thresh = session.trial(t).parameters(1);                    %Use the first parameter as the threshold.
            else                                                            %Otherwise...
                thresh = session.trial(t).thresh;                           %Grab the hit threshold.
            end
            hit_i = find(session.trial(t).sample_times >= 0 & ...
                session.trial(t).sample_times < hitwin);                    %Find the indices for samples in the hit window.
            if isempty(hit_i)                                               %If there's no samples in the hit window...
                continue                                                    %Exclude this trial from the analysis.
            end
            data(s).peak(t) = max(signal(hit_i));                           %Find the maximum force in each hit window.
            try                                                             %Try to find the maximum impulse.
                data(s).impulse(t) = max(diff(signal(hit_i)));              %Find the maximum impulse in each hit window.
            catch err                                                       %If an error occured...
                warning(err.message);                                       %Show the error message.
            end
            i = find(signal(1:end-1) <= session.trial(t).init...
                & signal(2:end) > session.trial(t).init) + 1;               %Find all positive-going initiation threshold crossings.
            j = find(signal(1:end-1) > session.trial(t).init...
                & signal(2:end) <= session.trial(t).init) + 1;              %Find all negative-going initiation threshold crossings.
            if isempty(i)                                                   %If the signal never rose through the threshold...
                continue                                                    %Exclude this trial from the analysis.
            end
            j(j < i(1)) = [];                                               %Kick out any negative-going initiation threshold crossings before the first positive-going crossing.
            if isempty(j)                                                   %If the rat never released the device...
                continue                                                    %Exclude this trial from the analysis.
            end
            num_attempts = 0;                                               %Start an attempt counter.
            num_subceiling_attempts = 0;                                    %Start a sub-ceiling attempt counter.
            for k = 1:length(j)                                             %Step through all peaks in the signal.
                num_attempts = num_attempts + 1;                            %Add to the number of attempts.
                long_attempt_counter = long_attempt_counter + 1;            %Increment the longer attempt counter.
                s1 = times(i(k)-1) + (times(i(k)) - ...
                    times(i(k)-1))*(init-signal(i(k)-1)) / ...
                    (signal(i(k))-signal(i(k)-1));                          %Interpolate the start time of the peak.
                s2 = times(j(k)-1) + (times(j(k)) - ...
                    times(j(k)-1))*(init - signal(j(k)-1)) / ...
                    (signal(j(k))-signal(j(k)-1));                          %Interpolate the stop time of the peak.
                [p, s3] = max(signal(i(k):j(k)));                           %Find the peak height.  
                if ~isnan(data(s).ceiling(1)) && p < data(s).ceiling(t)     %If the peak is below the ceiling...
                    num_subceiling_attempts = ...
                        num_subceiling_attempts + 1;                        %Increment the subceiling attempt counter.
                    subceiling_attempt_force(end+1) = p;                    %Save the peak for for all subceiling peaks.
                    if isnan(data(s).subceiling_peak(t)) || ...
                            p > data(s).subceiling_peak(t)                  %If the peak value is greater than the current sub-ceiling max peak.
                        data(s).subceiling_peak(t) = p;                     %Save the peak value as the new sub-ceiling max peak.
                    end
                end
                s4 = times(i(k)+s3-1);                                      %Find the peak time.
                s3 = i(k)+s3-1;                                             %Find the index for the peak time.
                snippet = signal(i(k)-1:j(k));                              %Grab the signal for the peak.
                if any(snippet >= thresh) && ...
                        isnan(data(s).hit_peak_width(t)) && ...
                        any(s3 == hit_i)                                    %If this is the first supra-threshold peak for this trial...
                    if isnan(data(s).ceiling(1)) || ...
                            (p < data(s).ceiling(t))                        %If this trial doesn't have a ceiling or the peak was less than the ceiling...
                        data(s).hit_peak_width(t) = (s2-s1)/1000;           %Save the hit peak width for this trial.
                        data(s).time_to_peak(t) = (s4-s1)/1000;             %Save the hit time-to-peak for this trial.
                        data(s).pre_hit_attempts(t) = num_attempts - 1;     %Save the number of attempts preceding the hit.
                        data(s).subceiling_pre_hit_attempts(t) = ...
                            num_subceiling_attempts - 1;                    %Save the number of subceiling attempts preceding the hit.
                        data(s).success_peak(t) = p;                        %Save the successful peak value.
                        data(s).long_attempts(t) = ...
                            long_attempt_counter - 1;                       %Save the long attempt count.
                        long_attempt_counter = 0;                           %Reset the long attempt count.                 
                    end
                end
                all_peak_width(end+1) = s2 - s1;                            %Add this peak's width to the all peaks width list.
                attempt_force(end+1) = p;                                   %Save the peak force for all peaks.                    
            end
            total_attempts = total_attempts + num_attempts;                 %Add the attempts to the total attempts count.
        end
        data(s).total_attempts = total_attempts;                            %Save the total number of attempts.
        data(s).all_peak_width = nanmean(all_peak_width)/1000;              %Save the mean peak width for all peaks.
        data(s).attempt_force = nanmean(attempt_force);                     %Save the mean attempt force.
        data(s).subceiling_attempt_force = ...
            nanmean(subceiling_attempt_force);                              %Save the mean sub-ceiling attempt force.
        if length(data(s).peak) >= 20                                       %If there's at least 20 samples...
            data(s).fatigue = nanmean(data(s).peak(end-9:end))/...
                nanmean(data(s).peak(1:10));                                %Calculate the ratio of the first 10 trial's peak force to the last 10 trials peak force.
        end        
    end
end
if isempty(data)                                                            %If no data files were found...
    errordlg(['There were no MotoTrak data files with 5 or more trials '...
        'for the selected subjects!']);                                     %Show an error dialog box.
end
[~,i] = sort([data.timestamp]);                                             %Find the indices to sort all files chronologically.
data = data(i);                                                             %Sort all files chronologically.
waitbar.close();                                                            %Close the waitbar.

%Find all devices used in the data and load any existing configuration files.
output = [];                                                                %Create a structure to hold the output fields for each device.
devices = unique({data.device});                                            %Grab the unique device names across all sessions.
for d = 1:length(devices)                                                   %Step through each device...
    filename = fullfile(handles.config_path, [devices{d} '_popdata_tsv.config']);    %Create the expected configuration filename.
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
    filename = fullfile(handles.config_path, [devices{d} '_popdata_tsv.config']);    %Create the expected configuration filename.
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
            for j = 1:length(output.(devices{d}))                           %Step through all of the output columns.                     
                switch output.(devices{d}){j}                               %Switch between the possible column values.
                    case 'BOOTH'                                            
                        fprintf(fid(d),'%1.0f',data(s).booth);              %Print the booth number.
                    case 'DATE (DD/MM/YYYY)'
                        fprintf(fid(d),'%s',...
                            datestr(data(s).timestamp,'mm/dd/yyyy'));       %Print the start date.
                    case 'FATIGUE RATIO (LAST 10/FIRST 10)'
                        if ~isempty(data(s).fatigue)                        %If a fatigue ratio exists for this rat...
                            fprintf(fid(d),'%1.3f',data(s).fatigue);        %Print the fatigue ratio for the session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end
                    case 'HITS'
                        fprintf(fid(d),'%1.0f',...
                            sum(data(s).outcome == 'H'));                   %Print the number of hits.
                    case 'HITS IN FIRST 5 MINUTES'
                        if any(times >= 300)                                %If the session lasted for at least 5 minutes...
                            k = (times <= 300);                             %Find all trials in the first 5 minutes.
                            fprintf(fid(d),'%1.0f',...
                                sum(data(s).outcome(k) == 'H'));            %Print the number of hits in the first 5 minutes.
                        else                                                %Otherwise, if the trial didn't last 5 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end
                    case 'HITS IN FIRST 10 MINUTES'
                        if any(times >= 600)                                %If the session lasted for at least 10 minutes...
                            k = (times <= 600);                             %Find all trials in the first 10 minutes.
                            fprintf(fid(d),'%1.0f',...
                                sum(data(s).outcome(k) == 'H'));            %Print the number of hits in the first 10 minutes.
                        else                                                %Otherwise, if the trial didn't last 10 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'HITS IN FIRST 15 MINUTES'
                        if any(times >= 900)                                %If the session lasted for at least 15 minutes...
                            k = (times <= 900);                             %Find all trials in the first 15 minutes.
                            fprintf(fid(d),'%1.0f',...
                                sum(data(s).outcome(k) == 'H'));            %Print the number of hits in the first 15 minutes.
                        else                                                %Otherwise, if the trial didn't last 15 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'HIT RATE (%)'                                     
                        fprintf(fid(d),'%1.1f%%',...
                            100*nanmean(data(s).outcome == 'H'));           %Print the hit rate.
                    case 'HIT RATE (%) IN FIRST 5 MINUTES'
                        if any(times >= 300)                                %If the session lasted for at least 5 minutes...
                            k = (times <= 300);                             %Find all trials in the first 5 minutes.
                            fprintf(fid(d),'%1.1f%%',...
                                100*sum(data(s).outcome(k) == 'H')/sum(k)); %Print the number of hits in the first 5 minutes.
                        else                                                %Otherwise, if the trial didn't last 5 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end
                    case 'HIT RATE (%) IN FIRST 10 MINUTES'
                        if any(times >= 600)                                %If the session lasted for at least 10 minutes...
                            k = (times <= 600);                             %Find all trials in the first 10 minutes.
                            fprintf(fid(d),'%1.1f%%',...
                                100*sum(data(s).outcome(k) == 'H')/sum(k)); %Print the number of hits in the first 10 minutes.
                        else                                                %Otherwise, if the trial didn't last 10 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end  
                    case 'HIT RATE (%) IN FIRST 15 MINUTES'
                        if any(times >= 900)                                %If the session lasted for at least 15 minutes...
                            k = (times <= 900);                             %Find all trials in the first 15 minutes.
                            fprintf(fid(d),'%1.1f%%',...
                                100*sum(data(s).outcome(k) == 'H')/sum(k)); %Print the number of hits in the first 15 minutes.
                        else                                                %Otherwise, if the trial didn't last 15 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end
                    case 'HIT RATE (%) IN FIRST 10 TRIALS'
                        if numel(data(s).outcome) > 10                      %If there's more than 10 trials...
                            fprintf(fid(d),'%1.1f%%',...
                                100*nanmean(data(s).outcome(1:10) == 'H')); %Print the hit rate for the first 10 trials.
                        else                                                %Otherwise, if there's 10 trials or fewer...
                            fprintf(fid(d),'%1.1f%%',...
                                100*nanmean(data(s).outcome == 'H'));       %Print the hit rate.
                        end
                    case 'HIT RATE (%) IN FIRST 50 TRIALS'
                        if numel(data(s).outcome) > 50                      %If there's more than 50 trials.
                            fprintf(fid(d),'%1.1f%%',...
                                100*nanmean(data(s).outcome(1:50) == 'H')); %Print the hit rate for the first 50 trials.
                        else                                                %Otherwise, if there's 50 trials or fewer...
                            fprintf(fid(d),'%1.1f%%',...
                                100*nanmean(data(s).outcome == 'H'));       %Print the hit rate.
                        end
                    case 'HIT RATE (%) IN LAST 10 TRIALS'
                        if numel(data(s).outcome) > 10                      %If there's more than 10 trials...
                            fprintf(fid(d),'%1.1f%%',...
                                100*nanmean(data(s).outcome(end-9:end) == 'H')); %Print the hit rate for the first 10 trials.
                        else                                                %Otherwise, if there's 10 trials or fewer...
                            fprintf(fid(d),'%1.1f%%',...
                                100*nanmean(data(s).outcome == 'H'));       %Print the hit rate.
                        end
                    case 'HIT RATE (%) IN LAST 50 TRIALS'
                        if numel(data(s).outcome) > 50                      %If there's more than 50 trials.
                            fprintf(fid(d),'%1.1f%%',...
                                100*nanmean(data(s).outcome(end-49:end) == 'H')); %Print the hit rate for the first 50 trials.
                        else                                                %Otherwise, if there's 50 trials or fewer...
                            fprintf(fid(d),'%1.1f%%',...
                                100*nanmean(data(s).outcome == 'H'));       %Print the hit rate.
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
                            for k = 1:length(times) - 1                     %Step through each trial.
                                a(k,2) = ...
                                    sum(times(k:end) - times(k) <= 300);    %Count the number of trials within 5 minutes of each trial.
                                a(k,1) = ...
                                    sum(times(k:end) - times(k) <= 300 & ...
                                    data(s).outcome(k:end) == 'H');         %Count the number of hits within 5 minutes of each trial.
                            end
%                             fprintf(fid(d),'%1.0f',nanmax(a(:,1)));         %Print the maximum number of hits in any 5 minutes.
%                             fprintf(fid(d),'%1.0f',nanmax(a(:,2)));         %Print the maximum number of trials in any 5 minutes.
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
                    case 'MAX HITS IN ANY 5 MINUTES'                        %Print the maximum number of hits in any 5 minutes.
                        if any(times >= 300)                                %If the session lasted for at least 5 minutes...
                            a = zeros(length(times)-1,1);                   %Create a matrix to hold hit counts in any 5 minutes.                
                            for k = 1:length(times) - 1                     %Step through each trial.
                                a(k) = ...
                                    sum(times(k:end) - times(k) <= 300 & ...
                                    data(s).outcome(k:end) == 'H');         %Count the number of hits within 5 minutes of each trial.
                            end
                            fprintf(fid(d),'%1.3f',nanmax(a));              %Print the maximum number of hits rate within any 5 minutes.
                        else                                                %Otherwise, if the trial didn't last 5 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'MAX PEAK ANGLE (degrees)'
                        fprintf(fid(d),'%1.3f',nanmax(data(s).peak));       %Print the maximum signal peaks for each session.
                    case 'MAX PEAK FORCE (gm)'
                        fprintf(fid(d),'%1.3f',nanmax(data(s).peak));       %Print the maximum signal peaks for each session.
                    case 'MAX TRIALS IN ANY 5 MINUTES'
                        if any(times >= 300)                                %If the session lasted for at least 5 minutes...
                            a = zeros(length(times)-1,1);                   %Create a matrix to hold hit counts in any 5 minutes.                
                            for k = 1:length(times) - 1                     %Step through each trial.
                                a(k) = ...
                                    sum(times(k:end) - times(k) <= 300);    %Count the number of trials within 5 minutes of each trial.
                            end
                            fprintf(fid(d),'%1.3f',nanmax(a));              %Print the maximum trial count within any 5 minutes.
                        else                                                %Otherwise, if the trial didn't last 5 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'MEAN ATTEMPT ANGLE (degrees)'
                        fprintf(fid(d),'%1.3f',data(s).attempt_force);      %Print the mean peak force per attempt.    
                    case 'MEAN ATTEMPT FORCE (gm)'
                        fprintf(fid(d),'%1.3f',data(s).attempt_force);      %Print the mean peak force per attempt.    
                    case 'MEAN PEAK ANGLE (degrees)'
                        fprintf(fid(d),'%1.3f',nanmean(data(s).peak));      %Print the mean signal peaks for each session.
                    case 'MEAN PEAK FORCE (gm)'
                        fprintf(fid(d),'%1.3f',nanmean(data(s).peak));      %Print the mean signal peaks for each session.
                    case 'MEAN PEAK FORCE (gm) IN FIRST 10 TRIALS'
                        if numel(data(s).peak) > 10                         %If there were more than 10 trials...
                            fprintf(fid(d),'%1.3f',...
                                nanmean(data(s).peak(1:10)));               %Print the mean signal peaks for the first 10 trials of each session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'%1.3f',nanmean(data(s).peak));  %Print the mean signal peaks for each session.
                        end                        
                    case 'MEAN PEAK FORCE (gm) IN FIRST 50 TRIALS'
                        if numel(data(s).peak) > 50                         %If there were more than 50 trials...
                            fprintf(fid(d),'%1.3f',...
                                nanmean(data(s).peak(1:50)));               %Print the mean signal peaks for the first 50 trials of each session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'%1.3f',nanmean(data(s).peak));  %Print the mean signal peaks for each session.
                        end
                    case 'MEAN PEAK FORCE (gm) IN LAST 10 TRIALS'
                        if numel(data(s).peak) > 10                         %If there were more than 10 trials...
                            fprintf(fid(d),'%1.3f',...
                                nanmean(data(s).peak(end-9:end)));          %Print the mean signal peaks for the first 10 trials of each session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'%1.3f',nanmean(data(s).peak));  %Print the mean signal peaks for each session.
                        end                        
                    case 'MEAN PEAK FORCE (gm) IN LAST 50 TRIALS'
                        if numel(data(s).peak) > 50                         %If there were more than 50 trials...
                            fprintf(fid(d),'%1.3f',...
                                nanmean(data(s).peak(end-49:end)));         %Print the mean signal peaks for the first 50 trials of each session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'%1.3f',nanmean(data(s).peak));  %Print the mean signal peaks for each session.
                        end
                    case 'MEAN PEAK IMPULSE (gm/s)'
                        fprintf(fid(d),'%1.3f',nanmean(data(s).impulse));   %Print the mean signal impulse for each session.
                    case 'MEAN PEAK ROTATIONAL VELOCITY (degrees/s)'
                        fprintf(fid(d),'%1.3f',nanmean(data(s).impulse));   %Print the mean signal impulse for each session.
                    case 'MEAN PULL ATTEMPTS BETWEEN HITS'                       
                        fprintf(fid(d),'%1.3f',...
                            nanmean(data(s).long_attempts));                %Print the mean number of attempts before a hit.
                    case 'MEAN PULL DURATION (s)'
                        fprintf(fid(d),'%1.3f',data(s).all_peak_width);     %Print the mean peak duration for all peaks.
                    case 'MEAN SUB-CEILING ATTEMPT FORCE (gm)'
                        if ~isnan(data(s).ceiling(1))                       %If there was a ceiling on the current session...
                            fprintf(fid(d),'%1.3f',...
                                data(s).subceiling_attempt_force);          %Print the mean peak force per attempt.   
                        else                                                %Otherwise, if there was no ceiling for this session...
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end
                    case 'MEAN SUB-CEILING PEAK FORCE (gm)'
                        if ~isnan(data(s).ceiling(1))                       %If there was a ceiling on the current session...
                            fprintf(fid(d),'%1.3f',...
                                nanmean(data(s).subceiling_peak));          %Print the mean sub-ceiling signal peaks for each session.
                        else                                                %Otherwise, if there was no ceiling for this session...
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end
                    case 'MEAN SUCCESSFUL ATTEMPT PEAK FORCE (gm)'
                    	fprintf(fid(d),'%1.3f',...
                            nanmean(data(s).success_peak));                 %Print the mean sub-ceiling signal peaks for each session.
                    case 'MEAN TURN DURATION (s)'
                        fprintf(fid(d),'%1.3f',data(s).all_peak_width);     %Print the mean peak duration for all peaks.
                    case 'MEDIAN PEAK FORCE (gm)'
                        fprintf(fid(d),'%1.3f',nanmedian(data(s).peak));    %Print the median signal peaks for each session.
                    case 'MEDIAN PEAK FORCE (gm) IN FIRST 10 TRIALS'
                        if numel(data(s).peak) > 10                         %If there were more than 10 trials...
                            fprintf(fid(d),'%1.3f',...
                                nanmedian(data(s).peak(1:10)));             %Print the mean signal peaks for the first 10 trials of each session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'%1.3f',...
                                nanmedian(data(s).peak));                   %Print the mean signal peaks for each session.
                        end
                    case 'MEDIAN PEAK FORCE (gm) IN FIRST 50 TRIALS'
                        if numel(data(s).peak) > 50                         %If there were more than 50 trials...
                            fprintf(fid(d),'%1.3f',...
                                nanmedian(data(s).peak(1:50)));             %Print the mean signal peaks for the first 50 trials of each session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'%1.3f',...
                                nanmedian(data(s).peak));                   %Print the mean signal peaks for each session.
                        end
                    case 'MEDIAN PEAK FORCE (gm) IN LAST 10 TRIALS'
                        if numel(data(s).peak) > 10                         %If there were more than 10 trials...
                            fprintf(fid(d),'%1.3f',...
                                nanmedian(data(s).peak(end-9:end)));        %Print the mean signal peaks for the first 10 trials of each session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'%1.3f',...
                                nanmedian(data(s).peak));                   %Print the mean signal peaks for each session.
                        end
                    case 'MEDIAN PEAK FORCE (gm) IN LAST 50 TRIALS'
                        if numel(data(s).peak) > 50                         %If there were more than 50 trials...
                            fprintf(fid(d),'%1.3f',...
                                nanmedian(data(s).peak(end-49:end)));       %Print the mean signal peaks for the first 50 trials of each session.
                        else                                                %Otherwise...
                            fprintf(fid(d),'%1.3f',...
                                nanmedian(data(s).peak));                   %Print the mean signal peaks for each session.
                        end
                    case 'MEDIAN HIT THRESHOLD (gm)'
                        fprintf(fid(d),'%1.3f',nanmedian(data(s).thresh));  %Print the median hit threshold.   
                    case 'MIN. INTER-TRIAL INTERVAL (s)'
                        if length(times) > 1                                %If there's more than one trial...
                            ints = diff(times);                             %Calculate the inter-trial intervals.
                            ints = boxsmooth(ints,10);                      %Box-smooth the inter-trial intervals over 10 trials.
                            if length(ints) > 11                            %If there's more than 11 trials...
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
                    case 'NUMBER OF ATTEMPTS'
                        fprintf(fid(d),'%1.0f',data(s).total_attempts);     %Print the total number of attempts
                    case 'NUMBER OF TRIALS'
                        fprintf(fid(d),'%1.0f',...
                            sum(~isnan(data(s).outcome)));                  %Print the total number of trials.
                    case 'PERCENTAGE OF TRIALS EXCEEDING MAX HIT THRESHOLD (%)'
                        fprintf(fid(d),'%1.3f',...
                            100*nanmean(data(s).peak > nanmax(data(s).thresh)));       %Print the percentage of trials that exceed the maximum hit threshold.
                    case 'POSITION'
                        fprintf(fid(d),'%1.2f',data(s).position);           %Print the device position.  
                    case 'PRESS ATTEMPTS PER TRIAL'
                        fprintf(fid(d),'%1.3f',...
                            nanmean(data(s).pre_hit_attempts));             %Print the mean number of attempts before a hit.
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
                    case 'SUB-CEILING PULL ATTEMPTS PER TRIAL'
                        if ~isnan(data(s).ceiling(1))                       %If there was a ceiling on the current session...
                            fprintf(fid(d),'%1.3f',...
                                nanmean(data(s).subceiling_pre_hit_attempts));	%Print the mean number of attempts before a hit.
                        else                                                %Otherwise, if there was no ceiling for this session...
                            fprintf(fid(d),'%1.3f',...
                                nanmean(data(s).pre_hit_attempts));         %Print the mean number of attempts before a hit.
                        end
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
                    case 'TRIALS IN FIRST 10 MINUTES'
                        if any(times >= 600)                                %If the session lasted for at least 10 minutes...
                            fprintf(fid(d),'%1.0f',sum(times <= 600));      %Print the number of trials in the first 10 minutes.
                        else                                                %Otherwise, if the trial didn't last 10 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'TRIALS IN FIRST 15 MINUTES'
                        if any(times >= 900)                                %If the session lasted for at least 15 minutes...
                            fprintf(fid(d),'%1.0f',sum(times <= 900));      %Print the number of trials in the first 15 minutes.
                        else                                                %Otherwise, if the trial didn't last 15 minutes.
                            fprintf(fid(d),'NaN');                          %Print a NaN.
                        end   
                    case 'TURN ATTEMPTS PER TRIAL'
                        fprintf(fid(d),'%1.3f',...
                            nanmean(data(s).pre_hit_attempts));             %Print the mean number of attempts before a hit.

                        %fprintf(fid(d),'%1.3f',nanmean(data(s).hit_peak_width));     %Print the mean peak duration for hit peaks.
                end
                if j < N                                                    %If this isn't the last column...
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