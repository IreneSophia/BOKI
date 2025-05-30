# BOKI: psychophysio

This folder contains preprocessing scripts for BVP and EDA data collected with Empatica wristbands, E4 and Embrace Plus. 

It is necessary to install all packages listed in "requirements.txt" to use this preprocessing pipeline. 

The only file that has to be adjusted is "run-preproPSYPHY.py". This file then loads the relevant functions from the other files. 

The tag files have to be created manually and contain information for the script on which files to process and how to cut them. The possible settings are explained in the example files. The preprocessing pipeline differs between data collected with E4 and Embrace Plus, therefore, there are separate example files. 

Whether one uses E4 or E+ also affects the required data path. 

* E4: there should be a data folder which contains one folder per participant containing all csv files. There can be levels between that, for example, the participant folders can be nested within dyad folders. 
* E+: there should be a data folder which contains the downloaded participant folders containing the `raw_data` folder which in turn contains a folder, i.e., `v6`, which contains the avro files. There can be levels between that, for example, the participant folders can be nested within dyad folders. 

In both cases, the tag file should contain the participant folder names in the `part` column. The path to the data folder should be used for `dir_path` in `run-preproPSYPHY.py`. 

This pipeline was originally created for the BOKI project.
