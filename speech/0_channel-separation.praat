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
folder$ = "/media/emba/emba-2/ML_BOKI/audio_checks/"
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
    Save as WAV file: folder$ + flShort$ + "_ch_L.wav"
    selectObject: "Sound " + flShort$ + "_ch2"
    Save as WAV file: folder$ + flShort$ + "_ch_R.wav"
    selectObject: "Sound " + flShort$ + "_ch1"
    plusObject: "Sound " + flShort$ + "_ch2"
    Remove
endfor
removeObject: fls

appendInfoLine: "Done."