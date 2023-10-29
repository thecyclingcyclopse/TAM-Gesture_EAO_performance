cd C:/bids/sub

files = dir('*.tsv');

for i = 1:numel(files)
    clear onset  duration trial_type response pmod;
    data = readtable(files(i).name, 'FileType', 'text', 'Delimiter', '\t');
    names = {'onset', 'duration', 'trial_type', 'response_time', 'stim_file', 'response' 'pmod'};
    % extract from table to cells
    % and divide by conditions abstract and concrete
    % repeat for number of data sets
    for j = 1:numel(data)
        if data.trial_type == "sg_abs"
            onset.abs(j,1) = {data.onset.(i)};
            duration.abs(j,1) = {data.duration(i)};
            trial_type.abs(j,1) = {data.trial_type(i)};
            response_time.abs(j,1) = {data.response_time(i)};
            stim_file.abs(j,1) = {data.stim_file(i)};
            response.abs(j,1) = {data.response(i)};
            pmod.abs(j,1) = {data.pmod(i)};
        else
            onset.con(j,2) = {data.onset(i)};
            duration.con(j,2) = {data.duration(i)};
            trial_type.con(j,2) = {data.trial_type(i)};
            response_time.con(j,2) = {data.response_time(i)};
            stim_file.con(j,2) = {data.stim_file(i)};
            response.con(j,2) = {data.response(i)};
            pmod.con(j,1) = {data.pmod(i)};
        end
    end

    filename = strrep(files(i).name, '.tsv', '.mat');
    save(filename, "names","onset","duration","trial_type","response_time","stim_file","response","pmod");
end