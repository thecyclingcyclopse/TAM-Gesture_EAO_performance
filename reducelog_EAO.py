import pandas as pd
import os

path = "C:/Users/Pathfinder/Desktop/work/MSc KIS/10 Abschlussarbeit - 30LP/data/3reduced/EAO/"

tsv_files = [file for file in os.listdir(path) if file.endswith('.tsv')]

os.chdir(path)

for tsv_file in tsv_files:
  df = pd.read_csv(tsv_file, sep='\t')

  df.dropna(subset=['pmod'], inplace=True)

  df.to_csv(tsv_file, sep='\t', index=False)