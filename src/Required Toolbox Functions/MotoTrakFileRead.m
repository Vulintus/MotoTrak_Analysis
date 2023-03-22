function data = MotoTrakFileRead ( file )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MotoTrakFileRead.m
% Vulintus, Inc., 2016.
% Date Created:         2016-08-16
% Last date modified:   2023-03-22
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

if (version == -5 || version == -6)
    
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
    
    %Read in the number of quantitative stage parameters that exist for this session
    N = fread(fid, 1, 'uint32');
    
    %Read in the stage parameters for this stage
    data.parameters = {};
    for i=1:N
        n_param_name = fread(fid, 1, 'uint8');
        param_name = fread(fid, n_param_name, '*char')';
        data.parameters{end+1} = param_name;
    end
    
    %Read in the number of nominal stage parameters that exist for this session
    data.nominal_parameters = {};
    
    %Nominal parameters only exist for file version -6 and later.  Not for -5.
    if (version == -6)
        
        %Read in the number of nominal parameters
        N = fread(fid, 1, 'uint32');
        
        %Read in the name of each nominal parameter
        for i=1:N
            n_param_name = fread(fid, 1, 'uint8');
            param_name = fread(fid, n_param_name, '*char')';
            data.nominal_parameters{end+1} = param_name;
        end
        
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
            [new_trial, ~] = mototrak_read_trial(fid, number_of_streams, version);
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
    

%% mototrak_read_trial - a subfunction that reads in individual trials from the MotoTrak session file

function [trial, trial_number] = mototrak_read_trial ( fid, num_streams, version )

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

%Read in the number of quantitative parameters that exist for the trial
N = fread(fid, 1, 'uint8');

%Read in each quantitative parameter value
variable_params = [];
for i=1:N
    %Read in the parameter value
    param_value = fread(fid, 1, 'float32');
    variable_params(end+1) = param_value;
end
trial.parameters = variable_params;

%Declare an empty list of nominal parameters
trial.nominal_parameters = {};

%If this is file version -6 or later, read in any nominal parameters for this trial
if (version == -6)
    
    %Read the number of nominal parameters
    N = fread(fid, 1, 'uint8');
    
    %Read in each nominal parameter
    for i=1:N
        n_chars = fread(fid, 1, 'uint8');
        nominal_param_value = fread(fid, n_chars, '*char')';
        trial.nominal_parameters{end+1} = nominal_param_value;
    end
    
end

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