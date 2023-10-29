import pandas
import numpy as np
import os
files = os.listdir("C:/Users/Pathfinder/Desktop/work/MSc KIS/10 Abschlussarbeit - 30LP/data/1measurements/")
for file in files:
    dat=pandas.read_csv('C:/Users/Pathfinder/Desktop/work/MSc KIS/10 Abschlussarbeit - 30LP/data/1measurements/' + file, sep='\t')
    dat2=pandas.read_csv('C:/Users/Pathfinder/Desktop/work/MSc KIS/9 Vertiefungsmodul - MRT - 24LP/TAM-Similarities-Data/Datasets/evr_at_once_sim/sim_Audio_Video.csv', index_col=(0))
    transferlist=[]
    for idx in range(len(dat)):
        f_name = dat.loc[idx,'stim_file']
        if any(map(f_name.__contains__, ['ik_os_', 'mp_os_', 'ogm_', 'ogi_'])):
            transferlist.append(np.nan)
        else:
            if 'hell' in f_name:
                newfilename=f_name[:f_name.index('hell')]
            elif 'dunkel' in f_name:
                newfilename=f_name[:f_name.index('dunkel')]
            elif 'x_hell' in f_name:
                newfilename=f_name[:f_name.index('x_hell')]
            elif 'x_dunkel' in f_name:
                newfilename=f_name[:f_name.index('x_dunkel')]
            transferlist.append(newfilename)
    sim_list=[]
    for f_name in transferlist:
        if f_name is np.nan:
            sim_list.append(np.nan)
        else:
            sim_line=dat2[dat2.index.str.startswith(f_name)]
            sim_value = sim_line[sim_line.index.values[0]]
            sim_list.append(sim_value[0])
    dat['pmod'] = sim_list
    dat.to_csv('C:/Users/Pathfinder/Desktop/work/MSc KIS/10 Abschlussarbeit - 30LP/data/2combined/EAO/' +file, index = False, sep='\t')
