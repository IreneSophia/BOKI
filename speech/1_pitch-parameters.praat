####################################################
########## Irene Sophia Plank (c)2022 ##############
####### Includes code by Afton Nelson (c)2020 ######
####################################################
########## Preprocess audio .wav files #############
####################################################

####################################################
############### Initialise settings ################
####################################################

# specify directory containing the data and the output
folder$ = "/home/emba/Documents/ML_BOKI/Data_audio/"
# please enter the file separator from your OS
filesep$ = "/" 

### set variables

# set the timestep 
tstep = 0.015

# set the pitch floor
p_floor = 50

# set the pitch ceiling
p_ceil  = 700

clearinfo

####################################################
##### Finding pitch floor and ceiling per wav ######
####################################################

appendInfoLine: "finding pitch floor and ceiling"

# print a single header line with column names
writeFileLine: "'folder$'ML_pitch_limits.csv", "filename;floor;ceiling"

# read files
fls = Create Strings as file list... list 'folder$'ch_*.wav
numberOfFiles = Get number of strings
for ifile to numberOfFiles

    select Strings list
    fileName$ = Get string... ifile
    appendInfoLine: fileName$
    sound = Read from file... 'folder$''fileName$'
    # use object name
    shortName$ = selected$("Sound")

    # get individual pitch floor and ceiling
    To Pitch: tstep, p_floor, p_ceil
    q1 = Get quantile: 0, 0, 0.25, "Hertz"
    q3 = Get quantile: 0, 0, 0.75, "Hertz"
    Remove
    floor = q1*0.75
    ceiling = q3*2.5

    removeObject: sound

    appendFileLine: "'folder$'ML_pitch_limits.csv", "'fileName$';'floor';'ceiling'"

endfor

removeObject: fls

appendInfoLine: "Done."