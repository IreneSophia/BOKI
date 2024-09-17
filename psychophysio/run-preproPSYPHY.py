#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

This is a preprocessing pipeline for psychophysiological data collected with 
Empatica wristbands, either E4 or E+. It uses EDA Explorer to detect 
artefacts in the data based on EDA, temperature and acceleration data using
a binary classifier (noise versus okay). For the preprocessing, it uses 
functions from the NeuroKit2 package. Specifically for the EDA preprocessing, 
it also draws inspiration from Ledalab. 

Arguments: 
    dir_path   : data input directory
    dir_out    : output directory for all the results
    tag_file   : full path to tag_file containing info on file names and blocks
    empatica   : either 'e4' or 'e+'
    exclude    : list of patterns to be excluded from preprocessing (default = [])
    winwidth   : width of the window for smoothing of EDA with Gaussian kernel (int, default = 8)
    lowpass    : lowpass filter frequency for EDA - has to be no larger than half the sample rate (int, default = 5)
    max_art    : maximum percent of artefacts when data is still preprocessed (0 - 100, default = 100/3)
    art_cor    : whether or not to perform artefact correction py interpolation (default = True)

The function creates preprocessed data files for EDA and BVP as well as plots 
to check the data quality. 

(c) Irene Sophia Plank, 10planki@gmail.com

"""

###### Settings

import os
from preproPSYPHY import preproPSYPHY

# path to tag file > see explanation in example files
tag_file = os.path.join(os.getcwd(), 'part_E4_sel.csv')
# path to the data directory > see explanation in README
dir_path = os.path.join(os.getcwd(), 'BOKI_e4')
# path to the output directory
dir_out = os.path.join(os.getcwd(), 'BOKI_e4/preprocessed_data')
# which empatica version was used: 'e4' or 'e+'?
empatica = 'e4'
# list of participants that should not be processed
exclude  = []

preproPSYPHY(dir_path, dir_out, tag_file, empatica, exclude, art_cor = False)
