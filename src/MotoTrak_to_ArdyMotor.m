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
thresh_index = strcmpi(data.parameters,'Hit Threshold') | ...
    strcmpi(data.parameters,'Lower bound force threshold');                 %Find the parameter index for the hit threshold.

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
        if data.version >= -5                                               %If this is a version -4 file format or older...
            times = data.trial(t).signal(1,:)/1000;                         %Convert the sample times to milliseconds.        
        else                                                                %Otherwise...
            times = data.trial(t).signal(1,:);                              %Copy the sample times from the trial.
        end
        times = times - times(1);                                           %Make the sample times relative to the start of the snippet.
        isi = diff(times);                                                  %Calculate the sampling intervals for all times.
        if nanmedian(isi) > 100                                             %If the sampling period is greater than 100...
            times = times/1000;                                             %Assume the sample times are in microseconds and convert them to milliseconds.
            isi = isi/1000;                                                 %Convert the sampling intervals to milliseconds.
        end
        isi_std = abs((isi - nanmedian(isi))/nanstd(isi));                  %Convert the sampling intervals to standard deviations from a median.
        if any(isi <= 0 | isi_std > 10)                                     %If any of the sample times are non-increasing...
            if t == 1                                                       %If this is the first trial...
                sample_period = 10;                                         %Set the sample period to 10 milliseconds.                        
            end
            times = sample_period*(0:size(data.trial(t).signal,2)-1);       %Create sample times based on the previous average sample period.
        else                                                                %Otherwise...
            sample_period = nanmedian(isi);                                 %Save the average sample period for the next loop, if needed.
        end                          
        times = times - 1000*data.trial(t).pre_trial_duration;              %Make the sample times relative to the trial initiation.  
        data.trial(t).sample_times = int16(times');                         %Copy the sample times to a field called "sample times".
        data.trial(t).ir = data.trial(t).signal(3,:)';                      %Copy the IR signal to a field called "ir".
        data.trial(t).signal = data.trial(t).signal(2,:)';                 	%Trim the "signal" field to just those values coming from the primary device.
        data.trial(t).init = data.trial(t).parameters(init_index);          %Copy the the initiation threshold to a field called "init".
        data.trial(t).thresh = data.trial(t).parameters(thresh_index);      %Copy the the hit threshold to a field called "thresh".
    end
else                                                                        %Otherwise...
    data.pre_trial_sampling_dur = NaN;                                      %Set the pre trial sampling duration to NaN.
    data.position = NaN;                                                    %Set the device position to NaN.
end