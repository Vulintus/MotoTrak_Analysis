function MotoTrak_SessionData_to_TSV(varargin)

%
%MOTOTRAK_SESSION_TO_TSV.m - Vulintus, Inc., 2015.
%   MOTOTRAK_SESSIONDATA_TO_TSV has the user select one or multiple *.ArdyMotor files
%   and then exports a companion text file for each that is readable in
%   Microsoft Excel.  
%
%   Created July 6, 2015, by Drew Sloan.
%
%   UPDATE LOG:
%
%   11/16/2015 - Drew Sloan - Added a progress bar and auto-opening of the
%       destination directory in Windows Explorer at the end of conversion.
%   08/08/2016 - Drew Sloan - Renamed to "MotoTrak_Session_to_TSV" and
%       incorporated into combined analysis GUI, temporarily commenting out
%       the option to specify files in the function call.
%   09/26/2019 - Drew Sloan - Converted the if-then handling of module type
%       to switch-case and added press-time outputs for the lever module.
%

if nargin > 1 && ishandle(varargin{1})                                      %If the first input argument is a uicontrol handle...
    varargin(1:2) = [];                                                     %Kick out the first two input arguments.
    temp_nargin = nargin - 2;                                               %Subtract two from the number of input argumnets.
else                                                                        %Otherwise...
    temp_nargin = nargin;                                                   %Copy the number of input arguments to a temporary variable.
end
    
if temp_nargin > 1                                                          %If the user entered too many input arguments...
    error(['ERROR IN MOTOTRAK_SESSIONDATA_TO_TSV: Too many inputs! '...
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
        error(['ERROR IN MOTOTRAK_SESSIONDATA_TO_TSV: Input argument '...
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

%Check to see if any companion *.tsv files already exist for these files.
existing_files = zeros(1,length(files));                                    %Create a matrix to check for existing files.
for f = 1:length(files)                                                     %Step through each file.
    if ~exist(files{f},'file')                                              %If the file doesn't exist...
        error(['ERROR IN MOTOTRAK_SESSIONDATA_TO_TSV: The file "' ...
            files{f} '" does not exist!']);                                 %Show an error.
    end
    [path, newfile ,ext] = fileparts(files{f});                             %Grab the file extension.
    if ~any(strcmpi(ext,{'.Ardymotor','.MotoTrak'}))                        %If the file isn't an *.ArdyMotor file...
        error(['ERROR IN MOTOTRAK_SESSIONDATA_TO_TSV: The file "' ...
            files{f} '" is not a MotoTrak file!']);                         %Show an error.
    end
    newfile = fullfile(path, [newfile '.tsv']);                             %Create a filename for the text file.
    if exist(newfile,'file')                                                %If the new file already exists...
        existing_files(f) = 1;                                              %Show that this file already exists.
    end
end
overwrite = 0;                                                              %Keep track of whether the user wants to overwrite any existing files.
if any(existing_files == 1)                                                 %If any of the output files already exist...
    temp = questdlg(['A companion TSV file already exists for at '...
        'least one file, would you like to overwrite them or rename'...
        ' them?'],'Overwrite?','Overwrite','Rename Each','Cancel',...
        'Overwrite');                                                       %Ask the user if they want to overwrite or rename the files.
    if isempty(temp) || strcmpi(temp,'Cancel')                              %If the user cancelled the dialog box...
        return                                                              %Exit the function.
    else                                                                    %Otherwise...
        overwrite = strcmpi(temp,'Overwrite');                              %Set the overwriting setting to that chosen by the user.
    end
end

temp = questdlg('Would you like to export all trial signal samples?',...
    'Export Signals?','Yes (Export)','No (Don''t Export)','Cancel',...
    'No (Don''t Export)');                                                  %Ask the user if they want to export all of the trial signal samples.
if isempty(temp) || strcmpi(temp,'Cancel')                                  %If the user cancelled the dialog box...
    return                                                                  %Exit the function.
else                                                                        %Otherwise...
    export_all = strcmpi(temp,'Yes (Export)');                              %Set the signal export setting to that chosen by the user.
end

    
%Now step through each file and export a companion *.tsv file for each.
waitbar = big_waitbar('title','Loading MotoTrak Files...');                 %Create a waitbar figure.
for f = 1:length(files)                                                     %Step through each file.
    
    [~,filename,ext] = fileparts(files{f});                                 %Grab the filename and extension from the trial.
    if waitbar.isclosed()                                                   %If the user closed the waitbar figure...
        errordlg(['The MotoTrak export to TSV file was cancelled by '...
            'the user!'],'Export Cancelled');                               %Show an error.
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
        warning('ARDYMOTOR2TEXT:NoTrials',['WARNING FROM '...
            'ARDYMOTOR2TEXT: The file "' files{f} '" has zero trials '...
            'and will be skipped.']);                                       %Show a warning.
        continue                                                            %Skip to the next file.
    end
    
    %Create the companion *.tsv file name and check or overwriting...
    [path, shortfile, ~] = fileparts(files{f});                             %Grab the path and file root name from the file.
    newfile = fullfile(path, [shortfile '.tsv']);                           %Create a filename for the text file.
    if exist(newfile,'file') && overwrite == 0                              %If the new file already exists in this folder...
        [newfile, path] = uiputfile('*.tsv','Name Companion File',...
            newfile);                                                       %Ask the user for a new filename.
        if newfile(1) == 0                                                  %If the user selected cancel...
            return                                                          %Exit the function.
        else                                                                %Otherwise...
            cd(path);                                                       %Step into the specified directory.
        end
    end
    
    file_id = [fopen(newfile,'wt'), 1];                                     %Open a new text file to write the trial data to.
    
    clc;                                                                    %Clear the command window.
    fprintf(1,'MotoTrak_SessionData_to_TSV: Writing "%s"\n\n',shortfile);   %Show the user the filename being written.
    
    %Write the file header.
    for fid = file_id                                                       %Step through the file and command line.
        fprintf(fid,'%s\t','Subject:');                                     %Write a label for the subject name.
        fprintf(fid,'%s\n',data.subject);                                   %Write the subject's name.
        fprintf(fid,'%s\t','Date:');                                        %Write a label for the date.
        fprintf(fid,'%s\n',datestr(data.trial(1).starttime,23));            %Write the date.
        fprintf(fid,'%s\t','Stage:');                                       %Write a label for the stage.
        fprintf(fid,'%s\n',data.stage);                                     %Write the stage.
        fprintf(fid,'%s\t','Device:');                                      %Write a label for the device type.
        fprintf(fid,'%s\n',data.device);                                    %Write the device type.
        fprintf(fid,'%s\t','Device Position:');                             %Write a label for the device position.
        fprintf(fid,'%f\n',data.position);                                  %Write the device position.
        fprintf(fid,'%s\t','Constraint:');                                  %Write a label for the constraint.
        if isfield(data,'constraint')                                       %If there's a constraint field in the data structure...
            fprintf(fid,'%s\n',data.constraint);                            %Write the constraint.
        else                                                                %Otherwise...
            fprintf(fid,'%s\n','-');                                        %Write the constraint.
        end
        fprintf(fid,'%s\t','Trial Initiation Threshold:');                  %Write a label for the trial initiation threshold.
        fprintf(fid,'%1.2f ',data.trial(1).init);                           %Write the trial initiation threshold.
        if isfield(data,'threshtype')                                       %If there's a threshtype field in the data structure...
            fprintf(fid,'%s\n',data.threshtype);                            %Write the threshold units.
        end
        fprintf(fid,'%s\t','Hit Threshold:');                               %Write a label for the hit threshold.
        fprintf(fid,'%1.2f ',data.trial(1).thresh);                         %Write the hit threshold.
        if isfield(data,'threshtype')                                       %If there's a threshtype field in the data structure...
            fprintf(fid,'%s\n',data.threshtype);                            %Write the threshold units.
        end
        fprintf(fid,'%s\t','Hit Window:');                                  %Write a label for the hit threshold.
        fprintf(fid,'%1.2f ',data.trial(1).hitwin);                         %Write the hit window.
        fprintf(fid,'%s\n\n','s');                                          %Write the hit window units.
        
    end

    switch lower(data.device)                                               %Switch between the recognized devices...
        
% LEVER ANALYSIS ************************************************************************************************************
        case 'lever'
            
            %Write the overall session results.
            temp = mean([data.trial.outcome] == 'H');                       %Calculate the overall hit rate.
            for fid = file_id                                               %Step through the file and command line.
                fprintf(fid,'%s\t','Overall Hit Rate:');                    %Write a label for the overall hit rate.
                fprintf(fid,'%1.2f',100*temp);                              %Write the overall hit rate.
                fprintf(fid,'%s\n','%');                                    %Write a percentage label.
                fprintf(fid,'%s\t','Hits:');                                %Write a label for the number of hits.
                fprintf(fid,'%1.0f\n',sum([data.trial.outcome] == 'H'));    %Write the number of hits.
                fprintf(fid,'%s\t','Misses:');                              %Write a label for the number of misses.
                fprintf(fid,'%1.0f\n',sum([data.trial.outcome] == 'M'));    %Write the number of misses.
            end
            
            signal_max = 0;                                                 %Create a matrix to hold the historical signal maximum from the file.            
            for t = 1:length(data.trial)                                    %Step through each trial.
                if nanmax(data.trial(t).signal) > signal_max                %If the maximum of the signal is greater than the historical maximum...
                    signal_max = nanmax(data.trial(t).signal);              %Save that value as the new maximum.
                end
            end
            
            lever_press_pt = round(signal_max) * 0.75;                      %A "press" must be at least 3/4 of the range of motion of the lever.          
            lever_return_pt = round(signal_max) * 0.5;                      %Lever must return to the 50% point in its range before a new press begins
            
            for fid = file_id                                               %Step through the file and command line.
                fprintf(fid,'Press Threshold: %1.3f degrees\n',...
                    lever_press_pt);                                        %Write the press threshold.
                fprintf(fid,'Release Threshold: %1.3f degrees\n',...
                    lever_return_pt);                                       %Write the press threshold.
            end         

            for t = 1:length(data.trial)                                    %Step through each trial.
                a = (data.trial(t).sample_times >= 0 & ...
                    data.trial(t).sample_times <= ...
                    1000*data.trial(t).hitwin);                             %Find all samples within the hit window.
                times = data.trial(t).sample_times(a);                      %Grab only the sample times within the hit window.
                signal = data.trial(t).signal(a);                           %Grab only the device signal within the hit window.
                
                presses_signal = signal - lever_press_pt;                   %Subtract the press threshold from the signal.
                negative_bound = 0 - (lever_press_pt - lever_return_pt);    %Set a negative bound for the release threshold.

                presses_signal(presses_signal > 0) = 1;                     %Convert all positive values to 1.
                presses_signal((presses_signal <= 0) & ...
                    (presses_signal >= negative_bound)) = 0;                %Convert all values between the release threshold and zero the press threshold to zero.
                presses_signal(presses_signal < negative_bound) = -1;       %Convert all sub-release threshold values to -1.

                original_indices = find(presses_signal ~= 0);               %Grab the indices for all non-zero samples.
                modified_presses_signal = ...
                    presses_signal(presses_signal ~= 0);                    %Create a copy of the signal containing only the non-zero elements.
                modified_presses_signal(modified_presses_signal < 0) = 0;   %Set all negative values in the copied signal to zero.

                diff_presses_signal = [0; diff(modified_presses_signal)];   %Find the differential of the copied signal.

                %Find the position/time of each press
                data.trial(t).press_indices = ...
                    [1, original_indices(diff_presses_signal == 1)' - 1];   %Find the samples with upward-going press threshold crossings.
                data.trial(t).press_times = ...
                    times(data.trial(t).press_indices);                     %Save the press times.

                %Find the position/time of each release
                data.trial(t).release_indices = ...
                    original_indices(diff_presses_signal == -1)';           %Find the samles with download-going release threshold crossings.
                data.trial(t).release_times = ...
                    times(data.trial(t).release_indices);                   %Save the release times.
    
            end
            
            for fid = file_id                                               %Step through the file and command line.
                
                %Write the column labels.
                fprintf(fid,'\n%s\t','Trial');                              %Write a column label for the trial number.
                fprintf(fid,'%s\t','Time');                                 %Write a column label for the time.
                fprintf(fid,'%s\t','Outcome');                              %Write a column label for the outcome.
                fprintf(fid,'%s\t','Hit Threshold (Presses)');              %Write a column label for the hit threshold.
                fprintf(fid,'%s\t','Press Count');                          %Write a column label for the number of presses.
                fprintf(fid,'%s\n','Press Times (ms)');                     %Write a column label for the pull duration.
                
                %Write all of the trial data.
                for t = 1:length(data.trial)                                %Step through each trial.
                    fprintf(fid,'%1.0f\t',t);                               %Write the trial number.
                    fprintf(fid,'%s\t',...
                        datestr(data.trial(t).starttime,13));               %Write the trial time.                    
                    fprintf(fid,'%s\t',char(data.trial(t).outcome));        %Write the trial outcome.
                    fprintf(fid,'%1.0f\t',data.trial(t).thresh);            %Write the trial threshold.
                    fprintf(fid,'%1.0f\t',...
                        numel(data.trial(t).press_times));                  %Write the number of presses.
                    fprintf(fid,'[ ');                                       %Write left brackets.
                    fprintf(fid,'%1.0f ',data.trial(t).press_times);       %Write the press times.
                    fprintf(fid,']\n');                                     %Write right brackets.
                end
            end

        
        
% KNOB ANALYSIS *************************************************************************************************************
        case 'knob'
            
            %Write the overall session results.
            temp = mean([data.trial.outcome] == 'H');                       %Calculate the overall hit rate.
            for fid = file_id                                               %Step through the file and command line.
                fprintf(fid,'%s\t','Overall Hit Rate:');                    %Write a label for the overall hit rate.
                fprintf(fid,'%1.2f',100*temp);                              %Write the overall hit rate.
                fprintf(fid,'%s\n','%');                                    %Write a percentage label.
                fprintf(fid,'%s\t','Hits:');                                %Write a label for the number of hits.
                fprintf(fid,'%1.0f\n',sum([data.trial.outcome] == 'H'));    %Write the number of hits.
                fprintf(fid,'%s\t','Misses:');                              %Write a label for the number of misses.
                fprintf(fid,'%1.0f\n',sum([data.trial.outcome] == 'M'));    %Write the number of misses.
            end

            if ~isfield(data,'threshtype')                                  %If the threshold type isn't set...
                data.threshtype = 'degrees (total)';                        %Change the threshold type to "degrees (total)".        
            elseif strcmpi(data.threshtype,'bidirectional')                 %If the threshold type is "bidirectional"...
                data.threshtype = 'degrees (bidirectional)';                %Change the threshold type to "degrees (bidirectional)".                
            end

            for t = 1:length(data.trial)                                    %Step through each trial.
                a = find((data.trial(t).sample_times >= 0 & ...
                    data.trial(t).sample_times < ...
                    1000*data.trial(t).hitwin));                            %Find all samples within the hit window.
                if strcmpi(data.threshtype,'degrees (bidirectional)')       %If the threshold type is bidirectional knob-turning... 
                    signal = abs(data.trial(t).signal(a) - ....
                        data.trial(t).signal(1));                           %Subtract the starting degrees value from the trial signal and convert to absolute values.
                elseif any(strcmpi(data.threshtype,...                      %For any other threshold type...
                        {'supination','pronation','degrees (total)'}))
                    signal = data.trial(t).signal(a) - ...
                        data.trial(t).signal(1);                            %Subtract the starting degrees value from the trial signal, keeping the directionality.
                elseif strcmpi(data.threshtype,'# of spins')                %If the threshold type is the number of spins...
                    temp = diff(data.trial(t).signal);                      %Find the velocity profile for this trial.
                    temp = boxsmooth(temp,10);                              %Boxsmooth the wheel velocity with a 100 ms smooth.
                    [pks,i] = PeakFinder(temp,10);                          %Find all peaks in the trial signal at least 100 ms apart.
                    i(pks < 1) = [];                                        %Kick out all peaks that are less than 1 degree/sample.
                    i = intersect(a,i+1)-1;                                 %Find all peaks that are in the hit window.
                    signal = length(i);                                     %Set the trial signal to the number of wheel spins.
                else                                                        %Otherwise...
                    signal = data.trial(t).signal(a);                       %Grab the raw signal for the trial.
                end
                data.trial(t).range = range(signal);                        %Find the hit window range of each trial signal.
                data.trial(t).max = max(signal);                            %Find the hit window maximum of each trial signal.
                data.trial(t).min = min(signal);                            %Find the hit window minimum of each trial signal.
            end
            
            for fid = file_id                                               %Step through the file and command line.
                fprintf(fid,'%s\t','Trial Average:');                       %Write a label for the average response.
                fprintf(fid,'%1.2f ',mean([data.trial.max]));               %Write the average maximum.
                fprintf(fid,'%s\n\n',data.threshtype);                      %Write the threshold units.

                %Write the column labels.
                fprintf(fid,'%s\t','Trial');                                %Write a column label for the trial number.
                fprintf(fid,'%s\t','Time');                                 %Write a column label for the time.
                fprintf(fid,'%s\t','Outcome');                              %Write a column label for the outcome.
                fprintf(fid,'%s\n',data.threshtype);                        %Write a column for the signal units.

                %Write all of the trial data.
                for t = 1:length(data.trial)                                %Step through each trial.
                    fprintf(fid,'%1.0f\t',t);                               %Write the trial number.
                    fprintf(fid,'%s\t',...
                        datestr(data.trial(t).starttime,13));               %Write the trial time.
                    fprintf(fid,'%s\t',char(data.trial(t).outcome));        %Write the trial outcome.
                    fprintf(fid,'%1.2f\n',data.trial(t).max);               %Write the hit window signal maximum.
                end
            end
            
% PULL ANALYSIS *************************************************************************************************************
        case 'pull'
            
            ir_thresh = [1023, 0];                                          %Create a matrix to hold the IR signal bounds.
            for t = 1:length(data.trial)                                    %Step through each trial.
                ir_thresh(1) = min([data.trial(t).ir; ir_thresh(1)]);       %Find the new minimum for each trial.
                ir_thresh(2) = max([data.trial(t).ir; ir_thresh(2)]);       %Find the new maximum for each trial.
            end
            ir_thresh = mean(ir_thresh);                                    %Set the IR threshold to half the range.
            for t = 1:length(data.trial)                                    %Step through each trial.
                a = (data.trial(t).sample_times >= 0 & ...
                    data.trial(t).sample_times <= ...
                    1000*data.trial(t).hitwin);                             %Find all samples within the hit window.
                signal = data.trial(t).signal(a);                           %Grab only the device signal within the hit window.
                if any(signal > data.trial(t).init)                         %If the initiation threshold was broken...
                    data.trial(t).max = max(signal);                        %Find the hit window maximum of each trial signal.
                else                                                        %Otherwise...
                    data.trial(t).max = NaN;                                %Set the hit window maximum to NaN.
                end                
                times = [diff(data.trial(t).sample_times(a)); 0];           %Grab only the inter-sample intervales within the hit window.
                if data.trial(t).hittime ~= 0                               %If the trial resulted in a hit...
                    data.trial(t).hittime = ...
                        86400000*(data.trial(t).hittime...
                        - data.trial(t).starttime);                         %Convert the hit time to milliseconds.
                else                                                        %Otherwise...
                    data.trial(t).hittime = NaN;                            %Set the hit time to NaN.
                end 
                data.trial(t).pull_dur = ...
                    sum(times(signal >= data.trial(t).init));               %Find the pull duration.
                if any(signal > data.trial(t).thresh)                       %If there were any suprathreshold samples...
                    a = (data.trial(t).sample_times >= 0);                  %Find all samples after initiation.
                    signal = data.trial(t).signal(a);                       %Grab only the device signal after initation.
                    times = [diff(data.trial(t).sample_times(a)); 0];       %Grab only the inter-sample intervals after initiation.
                    i = find(signal >= data.trial(t).thresh,1,'first');     %Find the first suprathreshold sample.
                    j = i + find(signal(i:end) < ...
                        data.trial(t).thresh,1,'first') - 1;                %Find the next subthreshold sample.
                    data.trial(t).first_supra_dur = sum(times(i:j));        %Grab the pull duration.
                else                                                        %Otherwise...
                    data.trial(t).first_supra_dur = 0;                      %Set the first supra-threshold pull duration to zero.
                end
                signal = data.trial(t).ir(a);                               %Grab only the IR signal within the hit window.
                data.trial(t).ir_dur = sum(times(signal < ir_thresh));      %Find the IR blocking duration.
                a = find(data.trial(t).signal >= ...
                    data.trial(t).init,1,'first');                          %Find the latency to pull.
                if ~isempty(a)                                              %If there was any pull force greater than the initiation threshold.
                    data.trial(t).pull_lat = data.trial(t).sample_times(a); %Grab the pull latency.
                else                                                        %Otherwise...
                    data.trial(t).pull_lat = NaN;                           %Set the pull latency to NaN.
                end
                 a = find(data.trial(t).ir < ir_thresh,1,'first');          %Find the latency to pull.
                if ~isempty(a)                                              %If there was any IR signal less than the threshold...
                    data.trial(t).ir_lat = data.trial(t).sample_times(a);   %Grab the IR latency.
                else                                                        %Otherwise...
                    data.trial(t).ir_lat = NaN;                             %Set the IR latency to NaN.
                end            
            end

            %Write the overall session results.
            temp = [mean([data.trial.outcome] == 'H'), NaN];                %Calculate the overall hit rate.
            temp(2) = sum([data.trial.outcome] == 'H' & ...
                ~isnan([data.trial.max]))/sum(~isnan([data.trial.max]));    %Calculate the hit rate minus swipes.
            for fid = file_id                                               %Step through the file and command line.
                fprintf(fid,'%s\t','Hit Rate (overall):');                  %Write a label for the overall hit rate.
                fprintf(fid,'%1.2f',100*temp(1));                           %Write the overall hit rate.
                fprintf(fid,'%s\n','%');                                    %Write a percentage label.
                fprintf(fid,'%s\t',...
                    'Hit Rate (excluding swipe-only trials):');             %Write a label for the hit rate minus only-swipes.
                fprintf(fid,'%1.2f',100*temp(2));                           %Write the hit rate minus swipe-only trials.
                fprintf(fid,'%s\n','%');                                    %Write a percentage label.
                fprintf(fid,'%s\t','Hits:');                                %Write a label for the number of hits.
                fprintf(fid,'%1.0f\n',sum([data.trial.outcome] == 'H'));    %Write the number of hits.
                fprintf(fid,'%s\t','Misses:');                              %Write a label for the number of misses.
                fprintf(fid,'%1.0f\n',sum([data.trial.outcome] == 'M'));    %Write the number of misses.
            end

            %Write the column labels.
            for fid = file_id                                               %Step through the file and command line.
                fprintf(fid,'\n%s\t','Trial');                              %Write a column label for the trial number.
                fprintf(fid,'%s\t','Time');                                 %Write a column label for the time.
                fprintf(fid,'%s\t','Outcome');                              %Write a column label for the outcome.
                fprintf(fid,'%s\t','Peak Force (g)');                       %Write a column label for the peak force.
                fprintf(fid,'%s\t','Hit Threshold (g)');                    %Write a column label for the hit threshold.
                fprintf(fid,'%s\t','Pull Duration (ms)');                   %Write a column label for the pull duration.
                if export_all == 0                                          %If we're not exporting all samples.
                    fprintf(fid,'%s\n','1st Suprathresh. Pull Dur. (ms)');  %Write a column label for the pull duration and end the line.
                else                                                        %Otherwise...
                    fprintf(fid,'%s\t\t',...
                        '1st Suprathresh. Pull Dur. (ms)');                 %Write a column label for the pull duration and end the line.
                    if isfield(data,'trial')                                %If there's trials in this data file.
                        for i = 1:numel(data.trial(1).sample_times)         %Step through each sample time.
                            fprintf(fid,'Sample Time %1.0f (ms)\t',i);      %Print a column label for each sample time.
                        end
                        fprintf(fid,'\t');                                  %Print a tab to the file.
                        for i = 1:numel(data.trial(1).signal)               %Step through each sample time.
                            fprintf(fid,'Force Value %1.0f (gm)\t',i);      %Print a column label for each signal value.
                        end
                        fprintf(fid,'\t');                                  %Print a tab to the file.
                        for i = 1:numel(data.trial(1).sample_times)         %Step through each sample time.
                            fprintf(fid,'IR Value %1.0f\t',i);               %Print a column label for each signal value.
                        end
                        fprintf(fid,'\n');                                  %Print a carriage return to the file.
                    end
                end

                %Write all of the trial data.
                for t = 1:length(data.trial)                                %Step through each trial.
                    fprintf(fid,'%1.0f\t',t);                               %Write the trial number.
                    fprintf(fid,'%s\t',...
                        datestr(data.trial(t).starttime,13));               %Write the trial time.
                    fprintf(fid,'%s\t',char(data.trial(t).outcome));        %Write the trial outcome.
                    fprintf(fid,'%1.2f\t',data.trial(t).max);               %Write the hit window signal maximum.
                    fprintf(fid,'%1.2f\t',data.trial(t).thresh);            %Write the hit threshold.
                    fprintf(fid,'%1.0f\t',data.trial(t).pull_dur);          %Write the pull duration.
                    if export_all == 0                                      %If we're not exporting all samples.
                        fprintf(fid,'%1.0f\n',...
                            data.trial(t).first_supra_dur);                 %Write the 1st suprathreshold pull duration and end the line.
                    else                                                    %Otherwise...
                        fprintf(fid,'%1.0f\t\t',...
                            data.trial(t).first_supra_dur);                 %Write the 1st suprathreshold pull duration.
                        if isfield(data,'trial')                            %If there's trials in this data file.
                            for i = 1:numel(data.trial(t).sample_times)     %Step through each sample time.
                                fprintf(fid,'%1.0f\t',...
                                    data.trial(t).sample_times(i));         %Print the sample time.
                            end
                            fprintf(fid,'\t');                              %Print a tab to the file.
                            for i = 1:numel(data.trial(t).signal)           %Step through each sample time.
                                fprintf(fid,'%1.3f\t',...
                                    data.trial(t).signal(i));               %Print the IR signal value.
                            end
                            fprintf(fid,'\t');                              %Print a tab to the file.
                            for i = 1:numel(data.trial(t).signal)           %Step through each sample time.
                                fprintf(fid,'%1.0f\t',...
                                    data.trial(t).ir(i));                   %Print the IR signal value.
                            end
                            fprintf(fid,'\n');                              %Print a carriage return to the file.
                        end
                    end
                end
            end
            
% "BOTH" ANALYSIS ***********************************************************************************************************
        case 'both'
            ir_thresh = [1023, 0];                                          %Create a matrix to hold the IR signal bounds.
            for t = 1:length(data.trial)                                    %Step through each trial.
                ir_thresh(1) = min([data.trial(t).ir; ir_thresh(1)]);       %Find the new minimum for each trial.
                ir_thresh(2) = max([data.trial(t).ir; ir_thresh(2)]);       %Find the new maximum for each trial.
            end
            ir_thresh = mean(ir_thresh);                                    %Set the IR threshold to half the range.
            for t = 1:length(data.trial)                                    %Step through each trial.
                a = (data.trial(t).sample_times >= 0 & ...
                    data.trial(t).sample_times <= ...
                    1000*data.trial(t).hitwin);                             %Find all samples within the hit window.
                signal = data.trial(t).signal(a);                           %Grab only the device signal within the hit window.
                if any(signal > 10)                                         %If the initiation threshold was broken...
                    data.trial(t).max = max(signal);                        %Find the hit window maximum of each trial signal.
                else                                                        %Otherwise...
                    data.trial(t).max = NaN;                                %Set the hit window maximum to NaN.
                end
                times = [diff(data.trial(t).sample_times(a)); 0];           %Grab only the inter-sample intervales within the hit window.
                if data.trial(t).hittime ~= 0                               %If the trial resulted in a hit...
                    data.trial(t).hittime = ...
                        86400000*(data.trial(t).hittime...
                        - data.trial(t).starttime);                         %Convert the hit time to milliseconds.
                else                                                        %Otherwise...
                    data.trial(t).hittime = NaN;                            %Set the hit time to NaN.
                end 
                data.trial(t).pull_dur = ...
                    sum(times(signal >= 10));                               %Find the pull duration.
                signal = data.trial(t).ir(a);                               %Grab only the IR signal within the hit window.
                data.trial(t).ir_dur = sum(times(signal < ir_thresh));      %Find the IR blocking duration.
                a = find(data.trial(t).signal >= ...
                    data.trial(t).init,1,'first');                          %Find the latency to pull.
                if ~isempty(a)                                              %If there was any pull force greater than the initiation threshold.
                    data.trial(t).pull_lat = data.trial(t).sample_times(a); %Grab the pull latency.
                else                                                        %Otherwise...
                    data.trial(t).pull_lat = NaN;                           %Set the pull latency to NaN.
                end
                 a = find(data.trial(t).ir < ir_thresh,1,'first');          %Find the latency to pull.
                if ~isempty(a)                                              %If there was any IR signal less than the threshold...
                    data.trial(t).ir_lat = data.trial(t).sample_times(a);   %Grab the IR latency.
                else                                                        %Otherwise...
                    data.trial(t).ir_lat = NaN;                             %Set the IR latency to NaN.
                end            
            end

            %Write the overall session results.
            temp = [mean([data.trial.outcome] == 'H'), NaN];                %Calculate the overall hit rate.
            temp(2) = sum([data.trial.outcome] == 'H' & ...
                ~isnan([data.trial.max]))/sum(~isnan([data.trial.max]));    %Calculate the hit rate minus swipes.
            for fid = file_id                                               %Step through the file and command line.
                fprintf(fid,'%s\t','Hit Rate (overall):');                  %Write a label for the overall hit rate.
                fprintf(fid,'%1.2f',100*temp(1));                           %Write the overall hit rate.
                fprintf(fid,'%s\n','%');                                    %Write a percentage label.
                fprintf(fid,'%s\t',...
                    'Hit Rate (excluding swipe-only trials):');             %Write a label for the hit rate minus only-swipes.
                fprintf(fid,'%1.2f',100*temp(2));                           %Write the hit rate minus swipe-only trials.
                fprintf(fid,'%s\n','%');                                    %Write a percentage label.
                fprintf(fid,'%s\t','Hits:');                                %Write a label for the number of hits.
                fprintf(fid,'%1.0f\n',sum([data.trial.outcome] == 'H'));    %Write the number of hits.
                fprintf(fid,'%s\t','Misses:');                              %Write a label for the number of misses.
                fprintf(fid,'%1.0f\n',sum([data.trial.outcome] == 'M'));    %Write the number of misses.
            end

            %Write the column labels.
            for fid = file_id                                               %Step through the file and command line.
                fprintf(fid,'\n%s\t','Trial');                              %Write a column label for the trial number.
                fprintf(fid,'%s\t','Time');                                 %Write a column label for the time.
                fprintf(fid,'%s\t','Outcome');                              %Write a column label for the outcome.
                fprintf(fid,'%s\t','Peak Force (g)');                       %Write a column label for the peak force.
                fprintf(fid,'%s\t','Hit Threshold (g)');                    %Write a column label for the hit threshold.
                fprintf(fid,'%s\n','Pull Duration (ms)');                   %Write a column label for the pull duration.

                %Write all of the trial data.
                for t = 1:length(data.trial)                                %Step through each trial.
                    fprintf(fid,'%1.0f\t',t);                               %Write the trial number.
                    fprintf(fid,'%s\t',...
                        datestr(data.trial(t).starttime,13));               %Write the trial time.
                    fprintf(fid,'%s\t',char(data.trial(t).outcome));        %Write the trial outcome.
                    fprintf(fid,'%1.2f\t',data.trial(t).max);               %Write the hit window signal maximum.
                    fprintf(fid,'%1.2f\t',data.trial(t).thresh);            %Write the hit threshold.
                    fprintf(fid,'%1.0f\n',data.trial(t).pull_dur);          %Write the pull duration.
                end
            end
            
    end
    
    fclose(file_id(1));                                                     %Close the text file.
end

waitbar.close();                                                            %Close the waitbar.

if length(files) == 1 && exist(newfile,'file')                              %If there was only one file...
    winopen(newfile);                                                       %Open the new TSV file.
else                                                                        %Otherwise...
    str = ['explorer.exe ' path];                                           %Create a system command to open Windows explorer in the current directory.
    system(str);                                                            %Execute the system command.
end


%% This subfunction finds peaks in the signal, accounting for equality of contiguous samples.
function [pks,i] = PeakFinder(signal,minpkdist)
i = find(signal(2:end) - signal(1:end-1) > 0) + 1;                          %Find each point that's greater than the preceding point.
j = find(signal(1:end-1) - signal(2:end) >= 0);                             %Find each point that's greater than or equal to the following point.
i = intersect(i,j);                                                         %Find any points that meet both criteria.
checker = 1;                                                                %Make a variable to check for peaks too close together.
while checker == 1 && length(i) > 2                                         %Loop until no too-close together peaks are found.
    checker = 0;                                                            %Set the checker variable to a default of no too-close peaks found.
    j = i(2:end) - i(1:end-1);                                              %Find the time between peaks.
    if any(j < minpkdist)                                                   %If any too-close-together peaks were found...
        j = find(j < minpkdist,1,'first') + 1;                              %Find the first set of too-close-together peaks.
        i(j) = [];                                                          %Kick out the following peak of the too-close-together pair.
        checker = 1;                                                        %Set the checker variable back to one to loop around again.
    end
end
pks = signal(i);                                                            %Grab the value of the signal at each peak.