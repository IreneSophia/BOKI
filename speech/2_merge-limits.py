# -*- coding: utf-8 -*-
"""
Created on Mon Jun 20 13:54:49 2022

@author: Irene Sophia Plank

This script chooses the minimum pitch floor and maximum pitch ceiling for each participant over both of the tasks. The original ML_pitch_limits.csv file is overwritten. 

"""

import pandas as pd
import os
import math

directory = "/home/emba/Documents/ML_BOKI/Data_speech/"

limits = pd.read_csv(os.path.join(directory,'ML_pitch_limits.csv'),sep=';',dtype={'dirname': 'str'})

limits['name_short'] = limits['filename'].str.rsplit('_',1).str[0]

ppl = limits.name_short.unique()

for p in ppl:
    limits.loc[limits['name_short'] == p,'floor_pp'] = math.floor(min(limits[limits['name_short'] == p]['floor']))
    limits.loc[limits['name_short'] == p,'ceiling_pp'] = math.ceil(max(limits[limits['name_short'] == p]['ceiling']))
    
limits.to_csv(os.path.join(directory,'ML_pitch_limits.csv'),index=False,sep=';')
