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
    if folders{f}(end)  ~= filesep                                          %If a directory doesn't end in a forward slash...
        folders{f}(end+1) = filesep;                                            %Add a forward slash to the end of the main path.
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
        b = find(folders{a}(1:end-2) == filesep,1,'last');                  %Find the last forward slash.
        if isempty(b)                                                       %If no forward slash was found...
            b = 1;                                                          %Set the index to 1.
        end
        waitbar.string(sprintf('Checking: %s',folders{a}(b:end)));          %Update the waitbar text.
        waitbar.value(sum(checker)/length(checker));                        %Update the waitbar value.
    end
    for f = 1:length(temp)                                                  %Step through all of the returned contents.
        if ~all(temp(f).name == '.') && temp(f).isdir == 1                  %If an item is a folder, but not a system folder...
            subfolder = [folders{a} temp(f).name filesep];                  %Concatenate the full subfolder name...
            if ~any(strcmpi(subfolder,folders))                             %If the subfolder is not yet in the list of subfolders...                
                folders{end+1} = [folders{a} temp(f).name filesep];         %Add the subfolder to the list of subfolders.
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
        temp([temp.isdir] == 1) = [];                                       %Kick out all directories.
        N = N + length(temp);                                               %Add the number of files to the file counter.
        if waitbar.isclosed()                                               %If the user closed the waitbar figure...
            files = [];                                                     %Return an empty matrix.
            return                                                          %Skip execution of the rest of the function.
        else                                                                %Otherwise...
            b = find(folders{f}(1:end-2) == filesep,1,'last');              %Find the last forward slash.
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
            if ~all(temp(j).name == '.') && temp(j).isdir == 0              %If the filename isn't a hidden folder.
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