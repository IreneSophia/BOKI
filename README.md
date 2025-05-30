# BOKI

This repository contains the preprocessing pipeline from the *BOKI* project. The scripts have been developed by me, Irene Sophia Plank, at the NEVIA lab (https://nevialab.com/). Some of the scripts are inspired by or further developed from https://github.com/IreneSophia/ML_speech or Jana Koehler's https://github.com/jckoe/MLASS-study. This project contains two sub-projects: 

1. **BPD_sync** assesses the influence of borderline personality disorder on interpersonal synchrony in heart rate and electrodermal activity. This project was preregistered on [OSF](https://osf.io/7jtzr) and the preregistration will be made public when results are submitted for publication or posted as a preprint.
2. **ML_BPD-ASD** develops a machine learning algorithm for digitally assisted diagnostics of ASD and BPD. Features are extracted in an automated manner from video and audio recordings. 

Scripts for each project are in the folders of the same name. 

The data was collected at the Medical Faculty of the LMU Munich. Strangers were paired in either heterogeneous (one interaction partner with and one without BPD) or homogeneous (two interaction partners without BPD) dyads and asked to have two ten-minute conversations, one about their hobbies and one to collaboratively plan a menu with foods and drinks that they both dislike. We collected audio, video and psychophysiological data during these conversation. Previously, we have collected similar data with heterogeneous dyads including one autistic interaction partner (Koehler et al., 2024: https://doi.org/10.1038/s41398-024-02802-5; Plank et al., 2023: https://doi.org/10.3389/fpsyt.2023.1257569).

The folders contain preprocessing scripts for the different modalities as well as synchrony computation within and across channels. For the synchrony computation, the rMEA package and specifically MEA objects and the CCF function which computes windowed lagged cross correlation are used. Some of these scripts rely on already preprocessed data, e.g., with OpenFace or MEA. Details can be found in the README of the individual folders. Paths and settings need to be adjusted where indicated. 
