function Vulintus_Write_Config(variant,config_name,path,handles,varargin)

%
%Vulintus_Write_Config.m - Vulintus, Inc.
%
%   This function create a configuration file with the specified fields
%   from the specified 'config' structure.
%   
%   UPDATE LOG:
%   10/03/2017 - Drew Sloan - Function first created, adapted from
%       MotoTrak_Write_Config.m.

%Initialize some expected variables.
fields = fieldnames(handles);                                               %Grab all of the fieldnames in the configuration handles structure.
abbrevs = {};                                                               %Create an empty cell array to hold field abbreviations.

%Step through any optional input parameters.
str = {'fields','abbreviations'};                                           %List the recognized optional parameter names.
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
            case 'fields'                                                   %If the parameter name was "fields"...
                fields = varargin{i+1};                                     %Set the placeholder creation option to the specified mode.
                if ~iscell(fields)                                          %If the specified fields parameter value isn't a cell array.
                    error(['ERROR IN ' upper(mfilename) ': The '...
                        'specified field names must be in a cell array.']); %Show an error.
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

%Create the configuration file.
file = [path '\' variant '_' config_name];                                  %Create the configuration file name.
[fid, errmsg] = fopen(file,'wt');                                           %Create a new configuration file for writing as text.
if fid == -1                                                                %If the file could not be created...
    warndlg(sprintf(['Could not create the configuration file '...
        'in:\n\n%s\n\nError:\n\n%s'],file,...
        errmsg),'MotoTrak File Write Error');                               %Show a warning.
end

%Write the specified fields and field values to the configuration file.
for f = 1:numel(fields)                                                     %Step through each field name to write to the configuration file.
    if ~ischar(fields{f})                                                   %If the field value isn't a character array...
        warning(['A non-character field name input in the '...
            'configuration file write function was ignored.']);             %Show a warning.
    elseif ~any(strcmpi(fieldnames(handles),fields{f}))                     %If the field value doesn't match any fields in the handles structure..
        warning(['The specified field "' fields{f} '"  isn''t a '...
            'recognized field of the configuration structure, it will '...
            'be ignored by the configuration file write function']);        %Show a warning.
    else                                                                    %Otherwise...
        temp = upper(fields{f});                                            %Grab the specified field name.
        temp(temp == '_') = ' ';                                            %Replace all underscores with spaces.
        if ~isempty(abbrevs) && any(strcmpi(temp,abbrevs(:,2)))             %If field abbreviations were specified...
            j = strcmpi(temp,abbrevs(:,2));                                 %Find the index for the matching abbreviation.
            temp = upper(abbrevs{j,1});                                     %Use the abbreviation as the field name.
        end        
        fprintf(fid,'%s: ',temp);                                           %Print the field name to the configuration file.
        val = handles.(fields{f});                                          %Grab the value of the specified handles field.
        if ischar(val)                                                      %If the value is a string...
            fprintf(fid,'%s\n',val);                                        %Print the value to the configuration file.
        elseif isnumeric(val)                                               %If the value is numeric...
            if numel(val) > 1                                               %If there's more than one value...
                fprintf(fid,'[');                                           %Print a left bracket to the configuration file.
                for i = 1:numel(val)                                        %Step through each value...
                    fprintf(fid,'%s',num2str(val(i)));                      %Print each value as a string.
                    if i < numel(val)                                       %If this isn't the last value.
                        fprintf(fid,' ');                                   %Print a space to the configuration file.
                    end
                end
                fprintf(fid,']\n');                                         %Print a right bracket and carriage return to the configuration file.
            else                                                            %Otherwise...
                fprintf(fid,'%s\n',num2str(val));                           %Print the value and a carriage return to the configuration file.
            end
        end
    end
end
fclose(fid);                                                                %Close the configuration file.