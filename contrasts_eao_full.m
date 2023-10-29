% List of open inputs
nrun = X; % enter the number of runs here
jobfile = {'C:\Users\Pathfinder\Desktop\work\MSc KIS\10 Abschlussarbeit - 30LP\Scripts\contrasts\EAO\contrasts_full_job.m'};
jobs = repmat(jobfile, 1, nrun);
inputs = cell(0, nrun);
for crun = 1:nrun
end
spm('defaults', 'FMRI');
spm_jobman('run', jobs, inputs{:});
