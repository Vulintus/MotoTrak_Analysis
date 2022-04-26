function MotoTrak_Daily_Report(~,~,varargin)
%
%MOTOTRAK_DAILY_REPORT.m - Vulintus, Inc., 2017.
%
%   MOTOTRAK_DAILY_REPORT finds all behavioral sessions conducted on a
%   specified date and displays a report showing the number of sessions,
%   the number for feedings, and the hit rate for each animal.
%
%   UPDATE LOG:
%   04/17/2017 - Drew Sloan - Function first created.
%

%Check for any optional input arguments.
date = [];                                                                  %Create a variable to hold the specified date.
if nargin > 0 && ~isempty(varargin{1})                                      %If there's at least one optional input argument...
    date = varargin{1};                                                     %The specified date will be the first optional input argument.
    if ~isnumeric(date)                                                     %If the specified date is a string...
        try                                                                 %Use a try-catch statement to check for date formating errors...
            date = datenum(date);                                           %Convert the string to a date number.
        catch err                                                           %If an error occurred...
            errordlg(['ERROR IN MOTOTRAK_DAILY_REPORT: The specified '...
                'date string format was not recognized! ' err.message]);    %Show an error.
            date = [];                                                      %Reset the date value.
        end
    end
    if ~isempty(date) && (date < 734139 || date > ceil(now))                %If a valid numeric date number was specified, but is before 01/01/2010 or after today...
        if date < 734139                                                    %If the specified date is before 01/01/2010...
            errordlg(['ERROR IN MOTOTRAK_DAILY_REPORT: The specified '...
                'date of ' datestr(date, 'mm/dd/yyyy') ' occurs before '...
                'MotoTrak existed']);                                       %Show an error.
        else                                                                %Otherwise, if the specified date is a future date...
            errordlg(['ERROR IN MOTOTRAK_DAILY_REPORT: The specified '...
                'date of ' datestr(date, 'mm/dd/yyyy') ' hasn''t '...
                'happened yet!']);                                          %Show an error.
        end
    end
end
if nargin > 1                                                               %If there's more than one input argument..
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

%Load the configuration file, if one exists.
datapath = [];                                                              %Create a variable to hold the data path.
lastdate = now;                                                             %Create a variable to hold the date to default to.
filename = fullfile(configpath, 'mototrak_daily_report.config');            %Create the expected configuration filename.
if exist(filename,'file')                                                   %If a configuration file exists...
    fid = fopen(filename,'rt');                                             %Open the configuration file for reading.
    txt = fread(fid,'*char');                                               %Read in the data as characters.
    fclose(fid);                                                            %Close the configuration file.
    a = [0; find(txt == 10); length(txt) + 1];                              %Find all carriage returns in the text data.        
    for j = 1:length(a) - 1                                                 %Step through all lines in the data.
        ln = txt(a(j)+1:a(j+1)-1)';                                         %Grab the line of text.
        ln(ln == 0) = [];                                                   %Kick out all null characters.
        i = find(ln == ':',1,'first');                                      %Find the colon in the line.
        if isempty(i)                                                       %If no colon was found...
            continue                                                        %Skip to the next line.
        end
        param = ln(1:i-1);                                                  %Grab the parameter name.
        switch param                                                        %Switch between the recognized parameter names.
            case 'DATAPATH'                                                 %If the parameter is the previous data path...
                datapath = ln(i+2:end);                                     %Grab the specified datapath.
            case 'LAST_DATE'                                                %If the parameter was the previous date...
                lastdate = datenum(ln(i+2:end));                            %Grab the last specified date.
        end
    end
end
    
%Have the user choose a path containing data files to analyze.
if isempty(datapath)                                                        %If the data path isn't yet defined...
    datapath = 'C:\MotoTrak\';                                              %Set the default primary local data path for saving data files.
end
if ~exist(datapath,'dir')                                                   %If the primary local data path doesn't exist...
    datapath = pwd;                                                         %Set the default path to the current directory.
end
datapath = uigetdir(datapath,'Where is your MotoTrak data located?');       %Ask the user where their data is located.
if datapath(1) == 0                                                         %If the user pressed "cancel"...
    return                                                                  %Skip execution of the rest of the function.
end

%Have the user select a date for the report.
if isempty(date)                                                            %If no date is selected...
    date = uigetdate(lastdate);                                             %Have the user select a date.
    if isempty(date)                                                        %If no date was selected...
        return                                                              %Skip execution of the rest of the function.
    end
    date = fix(date);                                                       %Kick out the fractional parts of the date.
end

%Find all of the MotoTrak data files in the data path.
files = file_miner(datapath,{'*.ArdyMotor','*.MotoTrak'});                  %Find all LPS *.ArdyMotor files in the LPS folders.
pause(0.01);                                                                %Pause for 10 milliseconds.
if isempty(files)                                                           %If no files were found...
    errordlg('No MotoTrak data files were found in the that directory!');   %Show an error dialog box.
end

%Step through all of the data files and load them into a structure.
waitbar = big_waitbar('title',['Finding MotoTrak sessions on the '...
    'specified date...']);                                                  %Create a waitbar figure.
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
                session = ArdyMotorHeaderRead(files{f});                    %Read in the data from each file.
            case '.MotoTrak'                                                %If the data file is a *.MotoTrak file.
                session = MotoTrakHeaderRead(files{f});                       %Read in the data from each file.                
        end
    catch err                                                               %If an error occurs...
        warning(['ERROR READING: ' files{f}]);                              %Show which file had a read problem...
        warning(err.message);                                               %Show the actual error message.
        continue                                                            %Skip to the next file.
    end
    if session.starttime >= date && session.starttime < date + 1            %If the session occured on the specified date...
        try                                                                 %Try to read in the data file...
            switch ext                                                      %Switch between the possible file extensions.
                case '.ArdyMotor'                                           %If the data file is an *.ArdyMotor file.
                    session = ArdyMotorFileRead(files{f});                  %Read in the data from each file.
                case '.MotoTrak'                                            %If the data file is a *.MotoTrak file.
                    session = MotoTrakFileRead(files{f});                   %Read in the data from each file.                
            end
        catch err                                                           %If an error occurs...
            warning(['ERROR READING: ' files{f}]);                          %Show which file had a read problem...
            warning(err.message);                                           %Show the actual error message.
            continue                                                        %Skip to the next file.
        end        
        s = length(data) + 1;                                               %Create a new field index.
        if strcmpi(ext,'.MotoTrak')                                         %If the file is in the *.MotoTrak format...
            session = MotoTrak_to_ArdyMotor(session);                       %Convert the *.MotoTrak format data to *ArdyMotor format data.
        end
        for field = {'subject','device','stage','manual_feeds','booth',...
                'position'}                                                 %Step through the fields we want to save from the data file...
            if isfield(session,field{1})                                    %If the field exists in the session structure...
                data(s).(field{1}) = session.(field{1});                    %Grab each field from the data file and save it.
            end
        end
        if ~isfield(session,'trial')                                        %If there are no trials...     
            [~,str] = dos(['dir "' files{f} '"']);                          %Grab the file information from the windows command line.
            c = textscan(str,'%s');                                         %Parse the command line text into words.
            i = numel(c{1}) - 13;                                           %Find the entry for the date.
            data(s).timestamp = datenum([c{1}{i} ' ' c{1}{i+(1:2)}]);       %Grab the file creation time as the date number.
            continue                                                        %Skip to the next file.
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
            data(s).long_attempts = nan(length(session.trial),1);        %Create a matrix to hold the time to peak force.
            total_attempts = 0;                                             %Start a counter of the total number of attempts.
            long_attempt_counter = 0;                                       %Start a counter to count attempts across trials.
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
                    long_attempt_counter = long_attempt_counter + 1;        %Increment the longer attempt counter.
                    s1 = times(i(k)-1) + (times(i(k)) - ...
                        times(i(k)-1))*(init-signal(i(k)-1)) / ...
                        (signal(i(k))-signal(i(k)-1));                      %Interpolate the start time of the peak.
                    s2 = times(j(k)-1) + (times(j(k)) - ...
                        times(j(k)-1))*(init - signal(j(k)-1)) / ...
                        (signal(j(k))-signal(j(k)-1));                      %Interpolate the stop time of the peak.
                    [p, s3] = max(signal(i(k):j(k)));                       %Find the peak height.                    
                    s4 = times(i(k)+s3-1);                                  %Find the peak time.
                    s3 = i(k)+s3-1;                                         %Find the index for the peak time.
                    snippet = signal(i(k)-1:j(k));                          %Grab the signal for the peak.
                    if any(snippet >= thresh) && ...
                            isnan(data(s).hit_peak_width(t)) && ...
                            any(s3 == hit_i)                                %If this is the first supra-threshold peak for this trial...                        
                        data(s).hit_peak_width(t) = (s2-s1)/1000;           %Save the hit peak width for this trial.
                        data(s).time_to_peak(t) = (s4-s1)/1000;             %Save the hit time-to-peak for this trial.
                        data(s).pre_hit_attempts(t) = num_attempts - 1;     %Save the number of attempts preceding the hit.
                        data(s).long_attempts(t) = ...
                            long_attempt_counter - 1;                       %Save the long attempt count.
                        long_attempt_counter = 0;                           %Reset the long attempt count.                        
                    end
                    all_peak_width(end+1) = s2 - s1;                        %Add this peak's width to the all peaks width list.
                    attempt_force(end+1) = p;                               %Save the peak force for all peaks.
                end
                total_attempts = total_attempts + num_attempts;             %Add the attempts to the total attempts count.
            end
            data(s).total_attempts = total_attempts;                        %Save the total number of attempts.
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
                    'fatigue','timestamp','file','long_attempts'};          %Step through each field of the data structure.
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
waitbar.close();                                                            %Close the waitbar.
if isempty(data)                                                            %If no data files were found...
    msgbox(['There were no MotoTrak data files found on the '...
        'selected date!'],'No Sessions Found');                             %Show an error dialog box.
    return                                                                  %Skip execution of the rest of the function.
end
[~,i] = sort([data.timestamp]);                                             %Find the indices to sort all files chronologically.
data = data(i);                                                             %Sort all files chronologically.

%Grab the data for the daily report.
subjects = unique({data.subject});                                          %Find all unique subject names.
reportdata = struct([]);                                                    %Create a structure to hold the report data.
for s = 1:numel(subjects)                                                   %Step through each subject.
    reportdata(s).subject = subjects{s};                                    %Save the subject name.
    i = find(strcmpi(subjects{s},{data.subject}));                          %Find all sessions for each subject.
    if isfield(data,'outcome')                                              %If there is an outcome field...
        outcome = vertcat(data(i).outcome);                                 %Concatenate all of the trial outcomes.
    else                                                                    %Otherwise...
        outcome = [];                                                       %Create an empty matrix of outcomes.
    end
    reportdata(s).total_trials = numel(outcome);                            %Count the total number of trials.
    reportdata(s).feeds = [];                                               %Create a field to hold the number of feeds.
    for j = 1:length(i)                                                     %Step through each session.
        reportdata(s).feeds(j) = sum(data(i(j)).outcome == 'H') + ...
        numel(data(i(j)).manual_feeds);                                     %Find the total pellets feed per session.
    end
    reportdata(s).hit_rate = 100*nanmean(outcome == 'H');                   %Calculate the Hit Rate.
    reportdata(s).stages = unique({data(i).stage});                         %Grab the stages.
    reportdata(s).indices = i;                                              %Save the indices for each session.
end

%Update the configuration file.
filename = fullfile(configpath, 'mototrak_daily_report.config');            %Create the expected configuration filename.
fid = fopen(filename,'wt');                                                 %Open the configuration file for writing.
fprintf(fid,'DATAPATH: ');                                                  %Print the "DATAPATH" parameter label.
fprintf(fid,'%s\n',datapath);                                               %Print the current datapath.
fprintf(fid,'LAST_DATE: ');                                                 %Print the "LAST_DATE" parameter label.
fprintf(fid,'%s\n',datestr(date,'mm/dd/yyyy'));                             %Print the current report date.
fclose(fid);                                                                %Close the configuration file.
    
%Create the daily report file.
filename = fullfile(tempdir, ['MotoTrak_Daily_Report_' ...
    datestr(date,'yyyymmdd') '.tsv']);                                      %Create a filename.
fid = fopen(filename,'wt');                                                 %Open the file for writing as text.
if fid == -1                                                                %If the file opening was unsuccessful...
    filename = tempname('.tsv');                                            %Create a new file with a random name.
    fid = fopen(filename,'wt');                                             %Open the file for writing as text.
end
fprintf(fid,'%s SUMMARY:\n',datestr(date,'mm/dd/yyyy'));                    %Print the summary header.
fprintf(fid,'SUBJECT');                                                     %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'STAGE(S)');                                                    %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'TRIAL COUNT');                                                 %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'HIT RATE');                                                    %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'PELLETS FED');                                                 %Print a column label.
fprintf(fid,'\n');                                                          %Print a carriage return.
for s = 1:length(reportdata)                                                %Step through each subject.
    fprintf(fid,'%s\t',reportdata(s).subject);                              %Print the subject name.
    for i = 1:numel(reportdata(s).stages)                                   %Step through each tested stage.
        str = reportdata(s).stages{i};                                      %Grab the stage description.
        j = find(str == ':',1,'first') - 1;                                 %Find the colon in the stage description.
        fprintf(fid,'%s',str(1:j));                                         %Print the stage number.
        if i == numel(reportdata(s).stages)                                 %If this is the last stage to list...
            fprintf(fid,'\t');                                              %Print a tab.
        else                                                                %Otherwise...
            fprintf(fid,', ');                                              %Print a comma and a space.
        end
    end
    fprintf(fid,'%1.0f\t',reportdata(s).total_trials);                      %Print the total number of trials.
    fprintf(fid,'%1.1f%%\t',reportdata(s).hit_rate);                        %Print the hit rate.
    fprintf(fid,'%1.0f\n',sum(reportdata(s).feeds));                        %Print the total number of feedings.
end
fprintf(fid,'\n%s DETAIL:\n',datestr(date,'mm/dd/yyyy'));                   %Print the detail header.
fprintf(fid,'SUBJECT');                                                     %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'START TIME');                                                  %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'STAGE');                                                       %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'TRIAL COUNT');                                                 %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'HIT RATE');                                                    %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'TRIALS > MAX THRESHOLD');                                      %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'MEAN PEAK FORCE/ANGLE');                                       %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'MEDIAN PEAK FORCE/ANGLE');                                     %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'MEDIAN HIT THRESHOLD');                                        %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'TOTAL PELLETS FED');                                           %Print a column label.
fprintf(fid,'\t');                                                          %Print a tab.
fprintf(fid,'MANUAL FEEDS');                                                %Print a column label.
fprintf(fid,'\n');                                                          %Print a carriage return.

for s = 1:length(reportdata)                                                %Step through each subject.
    for i = reportdata(s).indices                                           %Step through each session.
        if i == reportdata(s).indices(1)                                    %If this is the first session of the day for the rat...
            fprintf(fid,'%s\t',data(i).subject);                            %Print the subject name.
        else                                                                %Otherwise...
            fprintf(fid,'\t');                                              %Just print a tab.
        end        
        fprintf(fid,'%s\t',datestr(data(i).timestamp,'HH:MM'));             %Print the start time.
        fprintf(fid,'%s\t',data(i).stage);                                  %Print the stage name.
        if isfield(data,'outcome')                                          %If there is an outcome field...
            outcome = data(i).outcome;                                      %Concatenate all of the trial outcomes.
            numfeeds = sum(outcome == 'H');                                 %Find the number of feeds.
        else                                                                %Otherwise...
            outcome = [];                                                   %Create an empty matrix of outcomes.
            numfeeds = 0;                                                   %Set the number of feeds to zero.
        end
        fprintf(fid,'%1.0f\t',length(outcome));                             %Print the trial count.
        fprintf(fid,'%1.1f%%\t',100*nanmean(outcome == 'H'));               %Print the hit rate.
        max_thresh = nanmax(data(i).thresh);                                %Grab the max threshold.
        fprintf(fid,'%1.0f (%1.2f)\t', sum(data(i).peak > max_thresh),...
            max_thresh);                                                    %Print the number of trials above the maximum threshold.
        if isfield(data,'peak')                                             %If there's a peak field...
            fprintf(fid,'%1.2f\t',nanmean(data(i).peak));                   %Print the mean peak force/angle.
            fprintf(fid,'%1.2f\t',nanmedian(data(i).peak));                 %Print the median peak force/angle.
        else                                                                %Otherwise...
            fprintf(fid,'NaN\tNaN\t');                                      %Print a NaN.
        end
        if isfield(data,'thresh')                                           %If there's a thresholds field...
            fprintf(fid,'%1.2f\t',nanmedian(data(i).thresh));               %Print the median peak threshold.
        else                                                                %Otherwise...
            fprintf(fid,'NaN\t');                                           %Print a NaN.
        end
        if isfield(data,'manual_feeds')                                     %If there's manual feeds field...
            numfeeds = numfeeds + numel(data(i).manual_feeds);              %Calculate the number of feeds.
            fprintf(fid,'%1.0f\t',numfeeds);                                %Print the total number of feeds.
            fprintf(fid,'%1.0f\n',numel(data(i).manual_feeds));             %Print the number of manual feeds.
        else                                                                %Otherwise...
            fprintf(fid,'%1.0f\t',numfeeds);                                %Print the total number of feeds.
            fprinf(fid,'0\n');                                              %Print a zero.
        end
    end
end
fclose(fid);                                                                %Close the report file.
   
%Open the report.
winopen(filename);                                                          %Open the TSV file using the default program.