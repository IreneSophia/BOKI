####################################################
########## Irene Sophia Plank (c)2022 ##############
####### Includes code by Afton Nelson (c)2020 ######
####################################################
####### Separate channels from .wav files ##########
####################################################

####################################################
############### Initialise settings ################
####################################################

# specify directory containing the data and the output
folder$ = "/home/emba/Documents/ML_BOKI/Data_audio/"
# please enter the file separator from your OS
filesep$ = "/" 

clearinfo


####################################################
###### Read in and separate audio channels #########
####################################################

appendInfoLine: "read in and separate audio channels"

fls = Create Strings as file list: "list", folder$ + "*.wav"
nof = Get number of strings
for ifile to nof
    selectObject: fls
    fileName$ = Get string: ifile
    appendInfoLine: "Starting: " + fileName$
    flShort$ = left$ (fileName$, length (fileName$)-4)
    Read separate channels from sound file: folder$ + fileName$
    selectObject: "Sound " + flShort$ + "_ch1"
    Save as WAV file: folder$ + "ch_L_" + flShort$ + ".wav"
    selectObject: "Sound " + flShort$ + "_ch2"
    Save as WAV file: folder$ + "ch_R_" + flShort$ + ".wav"
    selectObject: "Sound " + flShort$ + "_ch1"
    plusObject: "Sound " + flShort$ + "_ch2"
    Remove
endfor
removeObject: fls

appendInfoLine: "Done."