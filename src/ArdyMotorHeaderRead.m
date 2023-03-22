function [data, varargout] = ArdyMotorHeaderRead(file)

%
%ArdyMotorHeaderRead.m - Vulintus, Inc., 2017.
%
%   ARDYMOTORHEADERREAD reads in just the file header information from the
%   MotoTrak *.ArdyMotor behavioral session data file type.
%
%   data = ARDYMOTORFILEREAD(file) reads in the file header from the
%   *.ARDYMOTOR file specified by the string variable "file" into the
%   output "data" structure.
%
%   UPDATE LOG:
%   2017-04-17 - Drew Sloan - Function first created.
%   2023-03-22 - Drew Sloan - Changes the "daycode()" subfunction to
%       "ardymotorheaderread_get_daycode()" to reduce conflicts.
%

data = [];                                                                  %Start a data structure to receive the recordings.
fid = fopen(file,'r');                                                      %Open the file for read access.
fseek(fid,0,-1);                                                            %Rewind to the beginning of the file.

version = fread(fid,1,'int8');                                              %Read the file format version from the first byte as a signed integer.

data.version = version;                                                     %Save the file version to the data structure.

if version < 0                                                              %If the file format version indicates ArdyMotor V2.0...
    
    if version == -1 || version <= -3                                       %If the file format version is -1 or -2...
        data.daycode = fread(fid,1,'uint16');                               %Read in the daycode.
        if data.daycode > 366                                               %If the daycode is greater than 366...
            fseek(fid,2,-1);                                                %Rewind to the beginning of the file.
            data.daycode = fread(fid,1,'uint16');                           %Read in the daycode.
        end
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
    data.threshtype = fread(fid,N,'*char')';                                %Read in the characters of the threshold type description.
    if version == -1 || version <= -3                                       %If the file format version is -1...
        data.pre_trial_sampling_dur = 1000;                                 %Indicate 1000 milliseconds of pre-trial sampling.
    else                                                                    %Otherwise, for later versions...
        data.pre_trial_sampling_dur = fread(fid,1,'float32');               %Read in the pre-trial sampling duration (in milliseconds).
    end
    
    if ~feof(fid)                                                           %If we haven't yet reached the end of the file...
        trial = fread(fid,1,'uint32');                                      %Read in the trial number.
        if ~isempty(trial)                                                  %If a trial number was read...
            data.starttime = fread(fid,1,'float64');                        %Read in the first trial start time.
        else                                                                %Otherwise...
            [~,str] = dos(['dir "' file '"']);                              %Grab the file information from the windows command line.
            c = textscan(str,'%s');                                         %Parse the command line text into words.
            i = numel(c{1}) - 13;                                           %Find the entry for the date.
            data.starttime = datenum(c{1}{i});                              %Grab the file creation time as the date number.
        end        
    end
    
    if version < -1 && isfield(data,'trial') && ...
            isfield(data.trial,'starttime')                                 %If the file format version is newer than version -1 and the daycode function exists...
        data.daycode = ...
            ardymotorheaderread_get_daycode(data.trial(1).starttime);       %Find the daycode for this file.
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
end

fclose(fid);                                                                %Close the input file.
varargout{1} = version;                                                     %Output the format version number if the user asked for it.


%% This subfunction returns the daycode (1-365) for a given date.
function d = ardymotorheaderread_get_daycode(date)
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