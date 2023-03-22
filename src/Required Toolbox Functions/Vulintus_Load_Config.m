function handles = Vulintus_Load_Config(file,varargin)

%
%Vulintus_Load_Config.m - Vulintus, Inc.
%
%   MotoTrak_Load_Config loads the entries of a custom MotoTrak
%   configuration file and overwrites any existing default values.
%   
%   UPDATE LOG:
%   10/03/2017 - Drew Sloan - Function first created, adapted from
%       MotoTrak_Load_Config.m
%


%Initialize some expected variables.
handles = struct([]);                                                       %Create a structure to hold the configuration fields and values.
placeholder = 'off';                                                        %Set the function to NOT create a placeholder file by default.
abbrevs = {};                                                               %Create an empty cell array to hold field abbreviations.

%Step through any optional input parameters.
if nargin > 1                                                               %If a second input argument was specified...
    handles = varargin{1};                                                  %Assume the argument is an input structure to add to.
    if ~isstruct(handles) && ~isempty(handles)                              %If the argument isn't the right type...
    	error(['ERROR IN ' upper(mfilename) ': The second input '...
            'argument must be an existing structure to have fields '...
            'added to.']);                                                  %Show an error.
    end
end
str = {'placeholder','abbreviations'};                                      %List the recognized optional parameter names.
for i = 2:2:numel(varargin)                                                 %Step through any entered optional parameters.
	if ~ischar(varargin{i}) || ~any(strcmpi(str,varargin{i}))               %If the first optional input argument isn't one of the expected property names...
        cprintf('err',['ERROR IN ' upper(mfilename) ':  Property name '...
            'not recognized! Optional input properties are:\n']);           %Show an error.
        for j = 1:length(str)                                               %Step through each optional input argument name.
            cprintf('err',['\t''' str{j} '''\n']);                          %Print the optional input argument name.
        end
        beep;                                                               %Beep to alert the user to an error.
        return                                                              %Skip execution of the rest of the function.
    else                                                                    %Otherwise...
        temp = varargin{i};                                                 %Grab the parameter name.
        switch lower(temp)                                                  %Switch among possible parameter names.
            case 'placeholder'                                              %If the parameter name was "placeholder"...
                placeholder = varargin{i+1};                                %Set the placeholder creation option to the specified mode.
                if ~ischar(placeholder) || ...
                        ~any(strcmpi(placeholder,{'on','off'}))             %If the placeholder value isn't recognized.
                    error(['ERROR IN ' upper(mfilename) ': Invalid '...
                        '''placeholder'' value. Recognized values for '...
                        'the ''placeholder'' parameter are ''on'' and '...
                        '''off''.']);                                       %Show an error.
                end
            case 'abbreviations'                                            %If the parameter name was "abbreviations"...
                abbrevs = varargin{i+1};                                    %Save the cell array containing the expected field abbreviations.
                if ~iscell(abbrevs) || size(abbrevs,2) ~= 2                 %If the specified value isn't a valid cell array...
                    error(['ERROR IN ' upper(mfilename) ': The '...
                        'specified field name abbreviations must be a '...
                        '2-column cell array.']);                           %Show an error.
                end 
        end
    end
end

%Create a placeholder file, if one is specified.
if strcmpi(placeholder,'on')                                                %If the user specified the creation of a placeholder file...
    [path, file, ~] = fileparts(file);                                      %Grab the path, filename, and extension for the config file.
    placeholder_file = [path file '_placeholder.temp'];                     %Create a filename for the temporary placeholder file.
    while exist(placeholder_file,'file')                                    %Loop until a placeholder file no longer exists.
        [fid, errmsg] = fopen(placeholder_file,'r');                        %Open the placeholder file for reading as binary.
        if fid == -1                                                        %If a file could not be opened...
            warndlg(sprintf(['Could not open an existing placeholder '...
                'file in:\n\n%s\n\nError:\n\n%s'],placeholder_file,...
                errmsg),'Configuration File Read Error');                   %Show a warning.
            delete(placeholder_file);                                       %Delete the placeholder file.
        else                                                                %Otherwise...
            timestamp = fread(fid,'double');                                %Read in all data from the file as double precision values.
            fclose(fid);                                                    %Close the placeholder file.
            if now > timestamp(1)                                           %If the specified time in the placeholder file has passed...
                delete(placeholder_file);                                   %Delete the placeholder file.
            end
        end
    end   
    [fid, errmsg] = fopen(placeholder_file,'w');                            %Create a new temporary placeholder file.
    if fid == -1                                                            %If a file could not be created...
        warndlg(sprintf(['Could not create a placeholder file '...
            'in:\n\n%s\n\nError:\n\n%s'],placeholder_file,...
            errmsg),'Configuration File Write Error');                      %Show a warning.
    end
    timestamp = now + 1/86400;                                              %Set an expiration time for the placeholder in 1 second.
    fwrite(fid,timestamp,'double');                                         %Write the expiration time to the placeholder file.
    fclose(fid);                                                            %Close the placeholder file.
    fprintf(fid,'Placeholder file Created: %s\n',datestr(now,0));           %Write the file creation time to the placeholder file.
end

%Read in the configuration file data.
[fid, errmsg] = fopen(file,'rt');                                           %Open the specified configuration file for reading as text.
if fid == -1                                                                %If the file could not be opened...
    warndlg(sprintf(['Could not open the specified configuration file '...
        'in:\n\n%s\n\nError:\n\n%s'],handles.config_file,...
        errmsg),'Configuration File Read Error');                           %Show a warning.
end
txt = fread(fid,'*char');                                                   %Read in the data as characters.
fclose(fid);                                                                %Close the text file.

a = [0; find(txt == 10); length(txt) + 1];                                  %Find all carriage returns in the txt data.

for i = 1:length(a) - 1                                                     %Step through all lines in the data.
    ln = txt(a(i)+1:a(i+1)-1)';                                             %Grab the line of text.
    ln(ln == 0) = [];                                                       %Kick out all null characters.
    j = find(ln == ':',1,'first');                                          %Find the first colon separating the parameter name from the value.
    if ~isempty(j) && j > 1                                                 %If a parameter was found for this line.
        field = ln(1:j-1);                                                  %Grab the parameter name.
        val = ln(j+2:end);                                                  %Grab the parameter value.
        if ~isempty(abbrevs) && any(strcmpi(field,abbrevs(:,1)))            %If field abbreviations were specified...
            j = strcmpi(field,abbrevs(:,1));                                %Find the index for the matching abbreviation.
            field = abbrevs{j,2};                                           %Use the abbreviation as the field name.
        else                                                                %Otherwise...
            field = lower(field);                                           %Convert the field name to all lower-case.
            field(field < 'a' | field > 'z') = 95;                          %Set all non-text characters to underscores.
        end
        j = find(val > 32,1,'first') - 1;                                   %Find the first non-special character in the parameter value.
        if j > 0                                                            %If there were any preceding special characters...
            val(1:j) = [];                                                  %Kick out the leading special characters.
        end
        j = find(val > 32,1,'last') + 1;                                    %Find the last non-special character in the parameter value.
        if j <= length(val)                                                 %If there were any following special characters...
            val(j:end) = [];                                                %Kick out the trailing special characters.
        end
        if isempty(setdiff(val,[45:46,48:57]))                              %If all of the value characters are numeric characters...
            val = str2double(val);                                          %Convert the value string to a number.
        elseif isempty(setdiff(val,[32,39,44:59,91,93]))                    %If all of the value characters are evaluatable characters.      
        	eval(['val = ' val ';']);                                       %Set the field value by evaluating the string.
        end
        handles.(field) = val;                                              %Save the header value to a field with the parameter name.
    end
end

if strcmpi(placeholder,'on') && exist(placeholder_file,'file')              %If the user specified the creation of a placeholder file...
    delete(placeholder_file);                                               %Delete the placeholder file.
end