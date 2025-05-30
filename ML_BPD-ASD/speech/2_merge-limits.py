# -*- coding: utf-8 -*-
"""
Created on Mon Jun 20 13:54:49 2022

@author: Irene Sophia Plank

This script chooses the minimum pitch floor and maximum pitch ceiling for each 
participant over both of the tasks - this is not necessary if there a
The original OUT_pitch_limits.csv file is overwritten. 

"""

import pandas as pd
import os
import math

#### [!PARAMETERS TO ADJUST!] ####
# directory for input and output
directory = "/home/emba/Documents/AUD_preprocessed"
# list of task shorthands - empty if there are none
tasks = ["H", "M"]

#### PROCESSING

# read in the data
limits = pd.read_csv(os.path.join(directory,'OUT_pitch_limits.csv'),sep=',',dtype={'dirname': 'str'})

# loop through the tasks
name_short = limits['filename']
for t in tasks:
    name_short = name_short.str.replace(t, '')
name_short = name_short.str.replace('__', '_')
name_short = name_short.str.replace('.wav', '')
limits['name_short'] = name_short

ppl = limits.name_short.unique()

for p in ppl:
    limits.loc[limits['name_short'] == p,'floor_pp'] = math.floor(min(limits[limits['name_short'] == p]['floor']))
    limits.loc[limits['name_short'] == p,'ceiling_pp'] = math.ceil(max(limits[limits['name_short'] == p]['ceiling']))
    
limits.to_csv(os.path.join(directory,'OUT_pitch_limits.csv'),index=False,sep=',')
