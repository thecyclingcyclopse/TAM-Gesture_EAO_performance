%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% create .m files with conditions, onsets and durations %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% function
function [] = container_gplus_01_conditions_function(events_files, crun, tgt_dir, spec_name, subj_ID)
    

   
    %% Initialize variables.
    filename = strcat(events_files(crun).folder, '/', events_files(crun).name);
    delimiter = '\t';
    startRow = 2;
    
    %% Format for each line of text:
    %   column1: double (%f)
    %	column2: double (%f)
    %   column3: categorical (%C)
    %	column4: double (%f)
    %   column5: text (%s)
    %	column6: double (%f)
    %   column7: double (%f)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%f%f%C%f%s%f%f%[^\n\r]';
    
    %% Open the text file.
    fileID = fopen(filename,'r');
    
    %% Read columns of data according to the format.
    % This call is based on the structure of the file used to generate this
    % code. If an error occurs for a different file, try regenerating the code
    % from the Import Tool.
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
    
    %% Close the text file.
    fclose(fileID);
    
    %% Post processing for unimportable data.
    % No unimportable data rules were applied during the import, so no post
    % processing code is included. To generate code which works for
    % unimportable data, select unimportable cells in a file and regenerate the
    % script.
    
    %% Create output variable
    events = table(dataArray{1:end-1}, 'VariableNames', {'onset','duration','trial_type','response_time','stim_file','response','pmod'});
    
    %% create a cell arrays & struct array containing the names ...
    % of the conditions, the onsets, the durations and parametric modifier
    
    names = categories(events.trial_type)';
    pmod(1).name{1}='eigenvalues_abs';
    pmod(2).name{1}='eigenvalues_con';

    for j = 1:length(names)
        for i = 1:length(events.trial_type)
            if names(j) == events.trial_type(i)
                onsets_x(i,j) = events.onset(i);
                durations_x(i,j) = events.duration(i);
                param_x(i,1) = events.pmod(i);
            else
                onsets_x(i,j) = 0;
                durations_x(i,j) = 0;
                param_x(i,1) = 0;
            end
        end
        onsets{1,j} = nonzeros(onsets_x(1:i,j));
        durations{1,j} = nonzeros(durations_x(1:i,j));
        param_y = nonzeros(param_x(1:i,1));
        pmod(j).param{1} = param_y;
        clear onsets_x durations_x param_x param;
    end
    
    pmod(1).poly{1}=1;
    pmod(2).poly{1}=1;

    %% save the cell arrays into a .mat-file which spm can read (useful for 1st level - multiple conditions)
    tgt_name = strcat(fullfile(tgt_dir, subj_ID), '/', 'ses-pre', '/', spec_name, '_keyword-times-conditions.mat');
    cd(tgt_dir)
    cd(subj_ID)
    mkdir("ses-pre")
    cd("ses-pre")
    save([tgt_name], 'names', 'onsets', 'durations', 'pmod');
    cd ..
    cd ..
    cd ..
    %% Clear temporary variables
    clear all
end
