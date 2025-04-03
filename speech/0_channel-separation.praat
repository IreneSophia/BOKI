####################################################
########## Irene Sophia Plank (c)2022 ##############
####### Includes code by Afton Nelson (c)2020 ######
####################################################
####### Separate channels from .wav files ##########
####################################################

####################################################
############### Initialise settings ################
####################################################

#### [!PARAMETERS TO ADJUST!] ####
# specify directory containing the data
folder$ = "/media/emba/emba-2/ML_BOKI/Data_cut/"
# specify directory for the output > should be different!
folder_out$ = "/home/emba/Documents/AUD_preprocessed"
# please enter the file separator from your OS
filesep$ = "/" 

# specify the time window to be processed (-1 if whole duration)
start = 10
end   = 610

clearinfo


####################################################
###### Read in and separate audio channels #########
####################################################


# add file separator to folder
folder$ = folder$ + filesep$
folder_out$ = folder_out$ + filesep$

# interpret start and end
if end == -1
  end = Get end time
endif
if start == -1
  start = Get start time
endif

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
    name1$ = selected$("Sound")
    Extract part: start, end, "rectangular",1, "yes"
    selectObject: "Sound 'name1$'_part"
    nowarn Save as WAV file: folder_out$ + flShort$ + "_ch_L.wav"
    selectObject: "Sound " + flShort$ + "_ch2"
    name2$ = selected$("Sound")
    Extract part: start, end, "rectangular",1, "yes"
    selectObject: "Sound 'name2$'_part"
    nowarn Save as WAV file: folder_out$ + flShort$ + "_ch_R.wav"
    selectObject: "Sound " + flShort$ + "_ch1"
    plusObject: "Sound " + flShort$ + "_ch2"
    plusObject: "Sound 'name2$'_part"
    plusObject: "Sound 'name1$'_part"
    Remove
endfor
removeObject: fls

appendInfoLine: "Done."
