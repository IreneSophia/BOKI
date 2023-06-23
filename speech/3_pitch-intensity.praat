####################################################
########## Irene Sophia Plank (c)2022 ##############
####### Includes code by Afton Nelson (c)2020 ######
####################################################
######## Analyses preprocessed .wav files ##########
####################################################

####################################################
############### Initialise settings ################
####################################################

# specify directory containing the data and the output
# this file needs to contain "ML_pitch_limits.csv"
folder$ = "/home/emba/Documents/ML_BOKI/Data_speech/"
# please enter the file separator from your OS
filesep$ = "/" 

### set variables

## composite pitch and intensity

# pitch settings are determined individually according to Hirst (2011)

# get information about limits to calculate pitch time step
limits = Read Table from semicolon-separated file: "'folder$'ML_pitch_limits.csv"
selectObject: limits
n = Get number of rows
min_floor = Get minimum: "floor_pp"
pstep = 0.75 / min_floor

# set the timestep for intensity
istep = 0.01

# set the pitch minimum for intensity
p_min = 100

## continuous pitch and intensity

tstep = 0.001
tmin  = 10
tmax  = 610
floor = 50
ceil  = 700

clearinfo

####################################################
####   Extract pitch and intensity composites   ####
####################################################

# print a single header line with column names
writeFileLine: "'folder$'ML_pitch_intensity.csv", "soundname;mean_pitch;sd_pitch;min_pitch;max_pitch;min_pitch_none;max_pitch_none;mean_int;sd_int;min_int;max_int;min_int_none;max_int_none"

appendInfoLine: "extracting pitch and intensity composites"

for i to n
    
    selectObject: limits
    fileName$ = object$[limits, i, "filename"]

    appendInfoLine: fileName$
    sound = Read from file: folder$ + fileName$

    # get individual limits
    selectObject: limits
    p_floor = object[limits, i, "floor_pp"]
    p_ceil = object[limits, i, "ceiling_pp"]
    
    # get pitch composites
    selectObject: sound
    shortName$ = selected$("Sound")
    pitch = To Pitch (ac): pstep, p_floor, 4, "no", 0.03, 0.45, 0.01, 0.35, 0.14, p_ceil
    meanp = Get mean: tmin, tmax, "Hertz"
    sdp = Get standard deviation: tmin, tmax, "Hertz"
    minp = Get minimum: tmin, tmax, "Hertz", "Parabolic"
    maxp = Get maximum: tmin, tmax, "Hertz", "Parabolic"
    minpn = Get minimum: tmin, tmax, "Hertz", "None"
    maxpn = Get maximum: tmin, tmax, "Hertz", "None"

    # selecting the sound object and extracting continuous pitch and intensity
    selectObject: sound
    intensity = To Intensity: p_min, istep, "yes"
    meani = Get mean: tmin, tmax, "energy"
    sdi = Get standard deviation: tmin, tmax
    mini = Get minimum: tmin, tmax, "Parabolic"
    maxi = Get maximum: tmin, tmax, "Parabolic"
    minin = Get minimum: tmin, tmax, "None"
    maxin = Get maximum: tmin, tmax, "None"

    appendFileLine: "'folder$'ML_pitch_intensity.csv", "'shortName$';'meanp';'sdp';'minp';'maxp';'minpn';'maxpn';'meani';'sdi';'mini';'maxi';'minin';'maxin'"

    removeObject: sound, pitch, intensity

endfor

####################################################
####   Extract pitch and intensity continuous   ####
####################################################

appendInfoLine: "extracting pitch and intensity continuous"

for i to n
    
    selectObject: limits
    fileName$ = object$[limits, i, "filename"]
    
    appendInfoLine: fileName$
    sound = Read from file: folder$ + fileName$
    
    sound = selected ("Sound")
    shortName$ = selected$("Sound")
    
    # print a single header line with column names
    writeFileLine: "'folder$''shortName$'_cont.csv", "time;pitch;int"
    
    selectObject: sound
    pitch_cont = To Pitch: tstep, floor, ceil
    selectObject: sound
    intensity_cont = To Intensity: p_min, tstep
    
    for j to (tmax-tmin)/tstep
        time = tmin + j * tstep
        selectObject: pitch_cont
        pitch = Get value at time: time, "Hertz", "linear"
        selectObject: intensity_cont
        intensity = Get value at time: time, "cubic"
        appendFileLine: "'folder$''shortName$'_cont.csv", fixed$ (time, 3), ";", fixed$ (pitch, 3), ";", fixed$ (intensity, 3)
    endfor

    removeObject: pitch_cont, intensity_cont, sound

endfor

removeObject: limits

appendInfoLine: "Done."