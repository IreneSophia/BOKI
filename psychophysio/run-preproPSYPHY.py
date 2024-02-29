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
    exclude    : list of patterns to be excluded from preprocessing
    winwidth   : width of the window for smoothing of EDA with Gaussian kernel (int)
    lowpass    : lowpass filter frequency for EDA - has to be no larger than half the sample rate
    max_art    : maximum percent of artefacts when data is still preprocessed (0 - 100, default = 100/3)

The function creates preprocessed data files for EDA and BVP as well as plots 
to check the data quality. 

(c) Irene Sophia Plank, 10planki@gmail.com

"""

###### Settings

import os
from preproPSYPHY import preproPSYPHY

# window width for Gaussian smoothing
winwidth = 8 
# lowpass filter frequency
lowpass  = 5 
# path to tag file
tag_file = os.path.join(os.getcwd(), 'part_tags-short.csv')
# path to the data directory
dir_path = os.path.join(os.getcwd(), 'E+/1/1')
# path to the output directory
dir_out = os.path.join(os.getcwd(), 'E+/1/1/preprocessed_data')
# which empatica version was used: 'e4' or 'e+'?
empatica = 'e+'
# list of participants that should not be processed
exclude  = []

preproPSYPHY(dir_path, dir_out, tag_file, empatica, exclude, winwidth, lowpass)
