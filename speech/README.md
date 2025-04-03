# BOKI: speech

This folder contains the preprocessing pipeline from the *ML_speech* study (doi: 10.3389/fpsyt.2023.1257569; https://github.com/IreneSophia/MLSPE) which will also be used in the BOKI study for the extraction of speech features. This pipeline has been developed by me, Irene Sophia Plank, at the NEVIA lab (https://nevialab.com/). The study has been preregistrated on OSF (https://osf.io/jhetr), and investigates speech and interactional features of autistic and non-autistic people in dyadic communication. In this project, we aim to use automatic extraction of speech (acoustic and prosodic) as well as interactional features.

The preprocessing pipeline uses praat, python and R. It was developed by Irene Sophia Plank based on an earlier version from Afton Nelson. It uses the uhm-o-meter by De Jonge and colleagues (https://sites.google.com/view/uhm-o-meter). In each script, the paths need to be adjusted to where the data is stored. The data should be in .wav format and contain one channel per participant or alternatively two .wav files containing one participant each > if this is the case, step 0 can be skipped. All output is saved in the same folder as the data. 

## Overview
![speech_pipeline](https://user-images.githubusercontent.com/40594634/214036786-319e1aea-abe2-4e58-b33f-9e32c91d1447.PNG)


## Scripts

The scripts start with a number indicating the order of execution:

0. This praat script separates audio channels. If the participants' audio is already saved in two separate files, this step can be skipped
1. This praat script estimates pitch floor and ceiling
2. This python script chooses one floor and ceiling per participant: since there are two conditions in this study, separate limits are extracted for both conditions. This script always chooses one per participant by taking the more extreme option.
3. This praat script extracts:
	- composite pitch and intensity information based on individual settings (Hirst, 2011)
	- continuous pitch and intensity using the same parameters for all individuals
  
Between the third and the fouth script, the uhm-o-meter should be used to extract syllable nuclei, thereby, giving information about sound and silence (De Jong, N.H., Pacilly, J., & Heeren, W., 2021: https://doi.org/10.1080/0969594X.2021.1951162; scripts from https://sites.google.com/view/uhm-o-meter/home).

4. This python scripts transforms the praat output textgrids to csvs
5. This R script identifies turn-based information
6. This R script computes synchrony values
7. This R script merges all the available information to two dataframes, one on the dyad and one on the individual level
